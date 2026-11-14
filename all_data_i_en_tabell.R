# Lager all data samlet i et dok

library(tidyverse)
library(lubridate)


# 1. ELHUB: Samler målerdata for alle 5 prisområder

omraader <- c("NO1", "NO2", "NO3", "NO4", "NO5")

maal_samlet <- map_dfr(omraader, ~ beregn_prisområde(maalerdata, .x)) |> 
  as_tibble()


# 2. PRISER OG STRØMSTØTTE: Beregner og omformer til langt panelformat

# Kjører støtteberegningen på dagsdataene
dogn_stotte <- stotteutregner(dognstatistikk_w)

names(dogn_stotte)

view(dogn_stotte)

# Endrer kolonnenavnene for spotpris så de blir entydige før pivotering
priser_long <- dogn_stotte |> 
  rename_with(
    ~ paste0("spotpris_", .x),
    .cols = matches("^NO\\d+ \\(NOK\\)")
  ) |> 
  pivot_longer(
    cols = matches("^(spotpris_|stotte_|slutt_)"),
    names_to = c("PRISTYPE", "PRISOMRÅDE", "MÅLING"),
    names_pattern = "^(spotpris|stotte|slutt)_(NO\\d) \\(NOK\\)_(Min:|Max:|Average:)$",
    values_to = "VERDI"
  ) |> 
  pivot_wider(
    names_from = c(PRISTYPE, MÅLING),
    values_from = VERDI,
    names_glue = "{PRISTYPE}_{MÅLING}"
  )

view(priser_long)

# 3. MAGASINFYLLING (NVE): Henter klargjorte data

fyll_long <- fyll_2020_2026 |>
  select(Date, år, Uke, PRISOMRÅDE, fyllingsgrad)


# 4. SAMMENSTILLING: Bygger det endelige masterdatasettet. +
# 5. Ser at NordPool sine aggregerte prisdata fra 2020 -26 mangler 
# legger de til


all_data_samlet <- priser_long |>
  inner_join(
    maal_samlet,
    by = c("Date", "PRISOMRÅDE")
  ) |>
  mutate(
    år = year(Date),
    Uke = isoweek(Date)
  ) |>
  left_join(
    fyll_long |>
      select(Date, PRISOMRÅDE, fyllingsgrad),
    by = c("Date", "PRISOMRÅDE")
  ) |>
  arrange(PRISOMRÅDE, Date) |>
  group_by(PRISOMRÅDE) |>
  fill(fyllingsgrad, .direction = "down") |>
  ungroup() |>
  rename(slutt_Average = `slutt_Average:`) |> 
  left_join(
    spotpris_historisk_long,
    by = c("Date", "PRISOMRÅDE")
  )

glimpse(all_data_samlet)


