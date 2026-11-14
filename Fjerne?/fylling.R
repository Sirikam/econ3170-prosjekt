# Funksjon for å laste inn data fra fyllingsgrad av vannmagasin i et
# Strømprisområde med data fra NVE

# Det er brukt Google AI-modus for spørsmål tilknyttet koden som en del av 
# arbeidsflyten, hvor noen av inspillene har blitt brukt i den endelige 
# løsningen
library(readxl)
library(tidyverse)
library(purrr)
library(dplyr)
library(ISOweek)

#les in filer som skal emd i en liste
filer <- list.files(
  "data-kilde/fylling",
  pattern = "\\.(xls|xlsx|csv)$",
  full.names = TRUE
)

length(filer)
filer

# Les inn filene, og formatter dem slika t de er på ønsket format
data_liste <- lapply(filer, function(fil) {
  
  #mellomlagrer formatert filnavn slik at den kan brukes som tittel på kolonne
  navn <- basename(fil)|>
    str_remove("\\, 2020, 2021, 2022, 2023, 2024, 2025, 2026.xlsx")
  
  read_excel(fil,skip = 2, col_names = TRUE)[,c(1,5:11)]|>
    #fjerner tegn slik at års.label kan brukes som verdi
  rename_with(~ str_extract(.x, "\\d{4}"), .cols = -Uke)|> 
    arrange(Uke) |> pivot_longer(
      cols = -Uke,
      names_to = "år",
      values_to = navn
    )|>
    mutate(år = as.numeric(år))|>
    arrange(år, Uke)
})

#formattering av det endelige resultatet
fyll_2020_2026 <- data_liste |> 
  reduce(inner_join, by = c("Uke", "år"))|>
  drop_na() |>
  relocate(år, Uke)

# Omformer til langt format og legger til dato
fyll_2020_2026 <- fyll_2020_2026 |>
  pivot_longer(
    cols = -c(år, Uke),
    names_to = "PRISOMRÅDE",
    values_to = "fyllingsgrad"
  ) |>
  mutate(
    PRISOMRÅDE = str_extract(PRISOMRÅDE, "NO\\d+"),
    Date = ISOweek::ISOweek2date(
      sprintf("%d-W%02d-1", år, Uke)
    )
  ) |>
  select(Date, år, Uke, PRISOMRÅDE, fyllingsgrad) |>
  arrange(PRISOMRÅDE, Date)

view(fyll_2020_2026)


