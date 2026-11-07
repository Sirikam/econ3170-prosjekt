# Laste inn pakker 

library(tidyverse)
library(readxl)

#pris er oppgitt i kr per MWh

# Finn alle Excel-filer i mappen
filer <- list.files(
  "data-kilde/Nordpool",
  pattern = "\\.xlsx$",
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
  mutate(across(.cols = c(2:6),.fns = ~.x /1000)) # reduserer til KWH fra MWH
