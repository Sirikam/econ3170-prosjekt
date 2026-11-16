# Laste inn pakker 

library(tidyverse)
library(readxl)
library(janitor)


#pris er oppgitt i kr per MWh

# Finn alle Excel-filer i mappen
#filer <- list.files(
 # "data-kilde/Nordpool",
#  pattern = "\\.xlsx$",
 # full.names = TRUE
#)

filer <- list.files(
  "data-kilde/Nordpool",
  pattern = "^[^~].*\\.xlsx$",
  full.names = TRUE
)

# Les inn filene og hopp over de fire første radene
data_liste <- lapply(filer, function(fil) {
  read_excel(fil, skip = 4)
})

strompriser <- bind_rows(data_liste) |> 
  select(-2) |> 
  mutate(`Delivery Date (CET)` = as.Date(`Delivery Date (CET)`))  |> 
  arrange(`Delivery Date (CET)`) |>
  mutate(across(starts_with("NO"), ~ .x / 1000))  #Reduserer fra NOK/MWh til NOK/kWh

#names(strompriser)
#glimpse(strompriser)

strompriser |>
  summarise(
    fra = min(`Delivery Date (CET)`, na.rm = TRUE),
    til = max(`Delivery Date (CET)`, na.rm = TRUE),
    antall_datoer = n_distinct(`Delivery Date (CET)`)
  )

# glimpse(strompriser)

# Historisk aggregert Nord Pool-spotpris i langt format:
# Én rad per dato og prisområde

spotpris_historisk_long <- strompriser |>
  rename(date = `Delivery Date (CET)`) |>
  pivot_longer(
    cols = starts_with("NO"),
    names_to = "prisomrade",
    values_to = "spotpris_aggregert_historisk"
  ) |>
  mutate(
    prisomrade = substr(prisomrade, 1, 3)
  ) |>
  arrange(prisomrade, date) |> 
  clean_names()


#glimpse(spotpris_historisk_long) 
  #names(strompriser)

