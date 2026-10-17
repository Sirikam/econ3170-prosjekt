# Funksjon for å laste inn data fra fyllingsgrad av vannmagasin i et
# Strømprisområde med data fra NVE
library(readr)
library(readxl)
library(tidyverse)
library(fs)
library(purrr)
library(dplyr)

fyllingsliste <- dir_ls('data-kilde/fylling')

#instantier dataframes med størrelse og behov
fg_Samlet <- read_excel('data-kilde/fylling/Fyllingsgrad (%) Norge, 2020, 2021, 2022, 2023, 2024, 2025, 2026.xlsx',skip = 2, col_names = TRUE)[,c(1:11,37)]|>
  arrange(Uke)

fg_no1 <-read_excel('data-kilde/fylling/Fyllingsgrad (%) NO1, 2020, 2021, 2022, 2023, 2024, 2025, 2026.xlsx',skip = 2, col_names = TRUE)[,c(1:11,37)] |>
  arrange(Uke)

fg_no2 <-read_excel('data-kilde/fylling/Fyllingsgrad (%) NO2, 2020, 2021, 2022, 2023, 2024, 2025, 2026.xlsx',skip = 2, col_names = TRUE)[,c(1:11,37)] |>
  arrange(Uke)
fg_no3 <-read_excel('data-kilde/fylling/Fyllingsgrad (%) NO3, 2020, 2021, 2022, 2023, 2024, 2025, 2026.xlsx',skip = 2, col_names = TRUE)[,c(1:11,37)] |>
  arrange(Uke)
fg_no4 <-read_excel('data-kilde/fylling/Fyllingsgrad (%) NO4, 2020, 2021, 2022, 2023, 2024, 2025, 2026.xlsx',skip = 2, col_names = TRUE)[,c(1:11,37)] |>
  arrange(Uke)
fg_no5 <-read_excel('data-kilde/fylling/Fyllingsgrad (%) NO5, 2020, 2021, 2022, 2023, 2024, 2025, 2026.xlsx',skip = 2, col_names = TRUE)[,c(1:11,37)] |>
  arrange(Uke)

#print(length(fyllingsliste)) #skal bli en funksjonsinnlasting, dette er kommentert ut så vi kommer igang

#fyllefaktor <- function(fyllingsliste){
#  for (n in 1:length(fyllingsliste)) {
#    fyll  <- read_excel(fyllingsliste[n],skip = 2, col_names = TRUE)[,c(1:11,37)] |>
#      arrange(Samlet$Uke)
#    print(fyll)
#    DFs[n] <- list(fyll)
#  }
#}

#fyllefaktor(fyllingsliste=fyllingsliste)