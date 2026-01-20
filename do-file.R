setwd("C:/Users/fdgir/projects/ns-business-analyse")

library("plm")
library(lmtest)
library(car)
library("tidyverse")
library('dplyr') 
library('readxl')
library("corrplot")
library("stargazer")
library(psych)
library(flextable)
library(officer)
library(lubridate)
library(ggplot2)
library(stringr)
library(tidyr)
library(scales)

#IMPORT DATA
bezettingsregistraties <- read_csv("data/bezettingsregistraties.csv")
materieelplanning <- read_csv("data/materieelplanning.csv")
concessie_kpis <- read_csv("data/concessie_kpis.csv")
station_metadata <- read_csv("data/station_metadata.csv")
trein_dienstregeling <- read_csv("data/trein_dienstregeling.csv")

# 1) Datacontrole (snelle sanity checks)
glimpse(trein_dienstregeling)
glimpse(bezettingsregistraties)
glimpse(materieelplanning)
glimpse(station_metadata)
glimpse(concessie_kpis)

# Check op duplicates op de belangrijkste keys (trein_id + dienst_datum)
trein_dienstregeling %>%
  count(trein_id, dienst_datum) %>%
  filter(n > 1) %>%
  head()

bezettingsregistraties %>%
  count(trein_id, dienst_datum) %>%
  filter(n > 1) %>%
  head()

materieelplanning %>%
  count(trein_id, dienst_datum) %>%
  filter(n > 1) %>%
  head()

# 2) Types goed zetten
trein_dienstregeling2 <- trein_dienstregeling %>%
  mutate(
    dienst_datum = as.Date(dienst_datum),
    vertrek_uur = as.integer(vertrek_uur),
    traject = as.character(traject),
    vertrek_station = as.character(vertrek_station),
    aankomst_station = as.character(aankomst_station)
  )

bezettingsregistraties2 <- bezettingsregistraties %>%
  mutate(
    dienst_datum = as.Date(dienst_datum),
    reizigers = as.integer(reizigers),
    zitplaatsen_beschikbaar = as.integer(zitplaatsen_beschikbaar),
    bezettingsgraad_pct = as.numeric(bezettingsgraad_pct)
  )

materieelplanning2 <- materieelplanning %>%
  mutate(
    dienst_datum = as.Date(dienst_datum),
    materieeltype = as.character(materieeltype),
    aantal_bakken = as.integer(aantal_bakken),
    totaal_zitplaatsen = as.integer(totaal_zitplaatsen),
    staanplaatsen = as.integer(staanplaatsen)
  )

# 3) Samenvoegen naar 1 analyse-tabel (fact)
fact <- trein_dienstregeling2 %>%
  left_join(bezettingsregistraties2, by = c("trein_id", "dienst_datum")) %>%
  left_join(materieelplanning2, by = c("trein_id", "dienst_datum")) %>%
  mutate(
    weekday = wday(dienst_datum, label = TRUE, abbr = TRUE, week_start = 1),
    week = isoweek(dienst_datum),
    month = floor_date(dienst_datum, "month"),
    tijdvak = case_when(
      vertrek_uur %in% 7:9 ~ "Ochtendspits",
      vertrek_uur %in% 16:18 ~ "Avondspits",
      TRUE ~ "Dal"
    )
  ) %>%
  mutate(
    zitplaatsen = coalesce(totaal_zitplaatsen, zitplaatsen_beschikbaar),
    bezettingsgraad = 100 * reizigers / zitplaatsen,
    overbezet = if_else(reizigers > zitplaatsen, 1L, 0L),
    overbezettingsgraad = 100 * pmax(0, (reizigers - zitplaatsen) / zitplaatsen),
    zitplaatskans_pct = 100 * pmin(1, zitplaatsen / reizigers),
    buffer_zitplaatsen = zitplaatsen - reizigers
  )

# 4) Check op NA’s na joins (indicatie van ontbrekende koppelingen)
colSums(is.na(fact[c("reizigers","zitplaatsen","materieeltype","vertrek_station","aankomst_station")]))

# 5) KPI’s overall
kpi_overall <- fact %>%
  summarise(
    ritten = n(),
    reizigers_totaal = sum(reizigers, na.rm = TRUE),
    zitplaatsen_totaal = sum(zitplaatsen, na.rm = TRUE),
    gemiddelde_bezettingsgraad = mean(bezettingsgraad, na.rm = TRUE),
    pct_ritten_overbezet = mean(overbezet, na.rm = TRUE) * 100,
    gemiddelde_overbezettingsgraad = mean(overbezettingsgraad, na.rm = TRUE),
    zitplaatskans_reizigers_pct = weighted.mean(zitplaatskans_pct, w = reizigers, na.rm = TRUE)
  )

kpi_overall

# 6) KPI’s per tijdvak (spits vs dal)
kpi_tijdvak <- fact %>%
  group_by(tijdvak) %>%
  summarise(
    ritten = n(),
    reizigers = sum(reizigers, na.rm = TRUE),
    pct_ritten_overbezet = mean(overbezet, na.rm = TRUE) * 100,
    zitplaatskans_reizigers_pct =
      100 * sum(pmin(zitplaatsen, reizigers), na.rm = TRUE) /
      sum(reizigers, na.rm = TRUE),
    gemiddelde_bezettingsgraad = mean(bezettingsgraad, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(reizigers))

kpi_tijdvak

# 7) KPI’s per materieeltype (welk materieel “werkt” beter?)
kpi_materieel <- fact %>%
  group_by(materieeltype) %>%
  summarise(
    ritten = n(),
    reizigers_totaal = sum(reizigers, na.rm = TRUE),
    pct_ritten_overbezet = mean(overbezet, na.rm = TRUE) * 100,
    zitplaatskans_reizigers_pct =
      100 * sum(pmin(zitplaatsen, reizigers), na.rm = TRUE) /
      sum(reizigers, na.rm = TRUE),
    gemiddelde_bezettingsgraad = mean(bezettingsgraad, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(reizigers_totaal))


kpi_materieel

# 8) Top knelpunten: trajecten met laagste zitplaatskans (reizigersgewogen)
top_knelpunten <- fact %>%
  group_by(traject, tijdvak) %>%
  summarise(
    ritten = n(),
    reizigers_totaal = sum(reizigers, na.rm = TRUE),
    zitplaatskans_reizigers_pct =
      100 * sum(pmin(zitplaatsen, reizigers), na.rm = TRUE) /
      sum(reizigers, na.rm = TRUE),
    pct_ritten_overbezet = mean(overbezet, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  filter(ritten >= 30) %>% # stabiliteit
  arrange(zitplaatskans_reizigers_pct) %>%
  slice_head(n = 15)

top_knelpunten

# 9) Trendanalyse per maand (zitplaatskans & overbezetting)
trend_maand <- fact %>%
  group_by(month) %>%
  summarise(
    reizigers_totaal = sum(reizigers, na.rm = TRUE),
    zitplaatskans_reizigers_pct =
      100 * sum(pmin(zitplaatsen, reizigers), na.rm = TRUE) /
      sum(reizigers, na.rm = TRUE),
    pct_ritten_overbezet = mean(overbezet, na.rm = TRUE) * 100,
    .groups = "drop"
  )

trend_maand

# 10) Visuals
# 10a) Trend: Zitplaatskans per maand
ggplot(trend_maand, aes(x = month, y = zitplaatskans_reizigers_pct)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
  labs(
    title = "Zitplaatskans (reizigersgewogen) per maand",
    x = "Maand", y = "Zitplaatskans (%)"
  ) +
  theme_minimal()

# 10b) Trend: % overbezette ritten per maand
ggplot(trend_maand, aes(x = month, y = pct_ritten_overbezet)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "% overbezette ritten per maand",
    x = "Maand", y = "% overbezet"
  ) +
  theme_minimal()

# 10c) Materieel: zitplaatskans vs % overbezet
ggplot(kpi_materieel, aes(x = pct_ritten_overbezet, y = zitplaatskans_reizigers_pct, label = materieeltype)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.8) +
  scale_x_continuous(labels = percent_format(scale = 1)) +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
  labs(
    title = "Materieeltype performance",
    x = "% ritten overbezet", y = "Zitplaatskans reizigersgewogen (%)"
  ) +
  theme_minimal()