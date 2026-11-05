library(tidyverse)
library(readxl)
library(lubridate)

mappe <- "~/Desktop/Nordpool_2025_2026"

filer <- list.files(
  path = mappe,
  pattern = "\\.(xlsx|xls|csv)$",
  full.names = TRUE,
  ignore.case = TRUE
)

length(filer) #sjekke at jeg ikke har glemt å laste ned noen av dagene

priser_per_time <- map_dfr(filer, function(fil) {
  
  dato <- str_extract(basename(fil), "\\d{4}-\\d{2}-\\d{2}")
  
  read_excel(fil, skip = 4) |>  #fjerne metadata
    mutate(Date = as.Date(dato)) |> 
    select(-2) |> # kolonne uten data/NA
    rename(hour = `Delivery period (CET)`) |> 
    relocate(Date, hour)
})

#data settet inneholder avergae, min og max fra Nordpool. Lager egen tabel til dem
#Ønsker samme format på tabellen som prisene

dognstatistikk <- priser_per_time |>
  filter(hour %in% c("Min:", "Max:", "Average:")) |>
  rename(Statistic = hour) |> 
  pivot_longer(cols = starts_with("NO"),
               names_to = "Price_area",
               values_to = "Price")

#fjerne min, max og average fra hoveddata
timepriser <- priser_per_time |>
  filter(!hour %in% c("Min:", "Max:", "Average:"))

