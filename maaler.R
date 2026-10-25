# funksjon for import og formattering ac strmmmaalerdata for de ulike
# strømprisområdene
library(readr)
library(readxl)
library(tidyverse)

#forbruk i KWH, antall i totalt antall og antall 

maalerdata <- read_csv2('data-kilde/maaler/strommaalere.csv')


#splitter dataen inn i hvert sitt prisområde dag for dag iløpet av perioden
maal_no1 <- filter(maalerdata, PRISOMRÅDE == "NO1") |>
  pivot_wider(names_from = c(EAC_GRUPPE, FORBRUKSGRUPPE),
              values_from = c(ANTALL_NORGESPRIS, ANTALL, FORBRUK, FORBRUK_NORGESPRIS)) |>
  rowwise() |>
  mutate(sum_hus_NP = sum(c_across(3:6)))|>
  mutate(sum_fri_NP = sum(c_across(7:10))) |>
  mutate(sum_hus_tot = sum(c_across(11:14))) |>
  mutate(sum_fri_tot = sum(c_across(15:18))) |>
  mutate(forbruk_hus = sum(c_across(19:22))) |>
  mutate(forbruk_fri = sum(c_across(23:26))) |>
  mutate(forbuk_hus_NP = sum(c_across(27:30))) |>
  mutate(forbruk_fri_NP = sum(c_across(31:34))) |>
  mutate(prosent_hus_NP = sum_hus_NP/sum_hus_tot) |>
  mutate(prosent_fri_NP = sum_fri_NP/sum_fri_tot)

maal_no2 <- filter(maalerdata, PRISOMRÅDE == "NO2") |>
  pivot_wider(names_from = c(EAC_GRUPPE, FORBRUKSGRUPPE),
              values_from = c(ANTALL_NORGESPRIS, ANTALL, FORBRUK, FORBRUK_NORGESPRIS)) |>
  rowwise() |>
  mutate(sum_hus_NP = sum(c_across(3:6)))|>
  mutate(sum_fri_NP = sum(c_across(7:10))) |>
  mutate(sum_hus_tot = sum(c_across(11:14))) |>
  mutate(sum_fri_tot = sum(c_across(15:18))) |>
  mutate(forbruk_hus = sum(c_across(19:22))) |>
  mutate(forbruk_fri = sum(c_across(23:26))) |>
  mutate(forbuk_hus_NP = sum(c_across(27:30))) |>
  mutate(forbruk_fri_NP = sum(c_across(31:34))) |>
  mutate(prosent_hus_NP = sum_hus_NP/sum_hus_tot) |>
  mutate(prosent_fri_NP = sum_fri_NP/sum_fri_tot)
  
maal_no3 <- filter(maalerdata, PRISOMRÅDE == "NO3") |>
  pivot_wider(names_from = c(EAC_GRUPPE, FORBRUKSGRUPPE),
              values_from = c(ANTALL_NORGESPRIS, ANTALL, FORBRUK, FORBRUK_NORGESPRIS)) |>
  rowwise() |>
  mutate(sum_hus_NP = sum(c_across(3:6)))|>
  mutate(sum_fri_NP = sum(c_across(7:10))) |>
  mutate(sum_hus_tot = sum(c_across(11:14))) |>
  mutate(sum_fri_tot = sum(c_across(15:18))) |>
  mutate(forbruk_hus = sum(c_across(19:22))) |>
  mutate(forbruk_fri = sum(c_across(23:26))) |>
  mutate(forbuk_hus_NP = sum(c_across(27:30))) |>
  mutate(forbruk_fri_NP = sum(c_across(31:34))) |>
  mutate(prosent_hus_NP = sum_hus_NP/sum_hus_tot) |>
  mutate(prosent_fri_NP = sum_fri_NP/sum_fri_tot)

maal_no4 <- filter(maalerdata, PRISOMRÅDE == "NO4") |>
  pivot_wider(names_from = c(EAC_GRUPPE, FORBRUKSGRUPPE),
              values_from = c(ANTALL_NORGESPRIS, ANTALL, FORBRUK, FORBRUK_NORGESPRIS)) |>
  rowwise() |>
  mutate(sum_hus_NP = sum(c_across(3:6)))|>
  mutate(sum_fri_NP = sum(c_across(7:10))) |>
  mutate(sum_hus_tot = sum(c_across(11:14))) |>
  mutate(sum_fri_tot = sum(c_across(15:18))) |>
  mutate(forbruk_hus = sum(c_across(19:22))) |>
  mutate(forbruk_fri = sum(c_across(23:26))) |>
  mutate(forbuk_hus_NP = sum(c_across(27:30))) |>
  mutate(forbruk_fri_NP = sum(c_across(31:34))) |>
  mutate(prosent_hus_NP = sum_hus_NP/sum_hus_tot) |>
  mutate(prosent_fri_NP = sum_fri_NP/sum_fri_tot)

maal_no5 <- filter(maalerdata, PRISOMRÅDE == "NO5") |>
  pivot_wider(names_from = c(EAC_GRUPPE, FORBRUKSGRUPPE),
              values_from = c(ANTALL_NORGESPRIS, ANTALL, FORBRUK, FORBRUK_NORGESPRIS)) |>
  rowwise() |>
  mutate(sum_hus_NP = sum(c_across(3:6)))|>
  mutate(sum_fri_NP = sum(c_across(7:10))) |>
  mutate(sum_hus_tot = sum(c_across(11:14))) |>
  mutate(sum_fri_tot = sum(c_across(15:18))) |>
  mutate(forbruk_hus = sum(c_across(19:22))) |>
  mutate(forbruk_fri = sum(c_across(23:26))) |>
  mutate(forbuk_hus_NP = sum(c_across(27:30))) |>
  mutate(forbruk_fri_NP = sum(c_across(31:34))) |>
  mutate(prosent_hus_NP = sum_hus_NP/sum_hus_tot) |>
  mutate(prosent_fri_NP = sum_fri_NP/sum_fri_tot)


