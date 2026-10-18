# Laste inn pakker 

library(tidyverse)
library(readxl)

library(tidyverse)
library(readxl)

# Finn alle Excel-filer i mappen
filer <- list.files(
  "~/Documents/Nordpool",
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
  arrange(`Delivery Date (CET)`)
