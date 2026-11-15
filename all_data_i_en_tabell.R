# Lager all data samlet i et dok
#krever at man laster inn (i rekkefølge)
# strompriser_nordpool
# fylling
# pris_per_time
# funksjon_for_maaler 

# Så: 
# stottekalkulator
# stottefunk


library(tidyverse)
library(lubridate)
library(janitor)


# 1. ELHUB: Samler målerdata for alle 5 prisområder

omraader <- c("NO1", "NO2", "NO3", "NO4", "NO5")

maal_samlet <- map_dfr(omraader, ~ beregn_prisområde(maalerdata, .x)) |> 
  as_tibble()


# 2. PRISER OG STRØMSTØTTE: Beregner og omformer til langt panelformat

# Kjører støtteberegningen på dagsdataene
dogn_stotte <- stotteutregner(dognstatistikk_w) |> 
  clean_names()

#names(dogn_stotte)

#view(dogn_stotte)

# Endrer kolonnenavnene for spotpris så de blir entydige før pivotering
priser_long <- dogn_stotte |> 
  rename_with(
    ~ paste0("spotpris_", .x),
    .cols = matches("^no[1-5]_nok_(min|max|average)$")
  ) |> 
  pivot_longer(
    cols = matches(
      "^(spotpris|stotte|slutt|fordel)_no[1-5]_nok_(min|max|average)$"
    ),
    names_to = c("pristype", "prisomrade", "maling"),
    names_pattern =
      "^(spotpris|stotte|slutt|fordel)_(no[1-5])_nok_(min|max|average)$",
    values_to = "verdi"
  ) |> 
  pivot_wider(
    names_from = c(pristype, maling),
    values_from = verdi,
    names_glue = "{pristype}_{maling}"
  ) |>
  mutate(
    prisomrade = toupper(prisomrade)
  ) |> 
  clean_names()


#view(priser_long)

# 3. MAGASINFYLLING (NVE): Henter klargjorte data

fyll_long <- fyll_2020_2026 |>
  select(date, ar, uke, prisomrade, fyllingsgrad)


# 4. SAMMENSTILLING: Bygger det endelige masterdatasettet. +
# 5. Ser at NordPool sine aggregerte prisdata fra 2020 -26 mangler 
# legger de til

# 4. SAMMENSTILLING

all_data_samlet <- priser_long |>
  inner_join(
    maal_samlet,
    by = c("date", "prisomrade")
  ) |>
  mutate(
    ar = year(date),
    uke = isoweek(date)
  ) |>
  left_join(
    fyll_long |>
      select(date, prisomrade, fyllingsgrad),
    by = c("date", "prisomrade")
  ) |>
  arrange(prisomrade, date) |>
  group_by(prisomrade) |>
  fill(fyllingsgrad, .direction = "down") |>
  ungroup() |>
  left_join(
    spotpris_historisk_long,
    by = c("date", "prisomrade")
  )

#glimpse(all_data_samlet)

#names(all_data_samlet)


