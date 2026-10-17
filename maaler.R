# funksjon for import og formattering ac strmmmaalerdata for de ulike
# strømprisområdene
library(readr)
library(readxl)
library(tidyverse)

maalerdata <- read_csv2('data-kilde/maaler/strommaalere.csv')

maal_no1 <- filter(maalerdata, PRISOMRÅDE == "NO1") |>
  pivot_wider(names_from = c(EAC_GRUPPE, FORBRUKSGRUPPE),
              values_from = c(ANTALL_NORGESPRIS, ANTALL, FORBRUK, FORBRUK_NORGESPRIS)) |>
  mutate(sum_hus_NP = maal_no1[3]+maal_no1[4]+maal_no1[5]+maal_no1[6]) |>
  mutate(sum_fri_NP = maal_no1[7]+maal_no1[8]+maal_no1[9]+maal_no1[10]) |>
  mutate(sum_hus_tot = maal_no1[11]+maal_no1[12]+maal_no1[13]+maal_no1[14]) |>
  mutate(sum_fri_tot = maal_no1[15]+maal_no1[16]+maal_no1[17]+maal_no1[18]) |>
  mutate(forbruk_hus = maal_no1[19]+maal_no1[20]+maal_no1[21]+maal_no1[22]) |>
  mutate(forbruk_fri = maal_no1[23]+maal_no1[24]+maal_no1[25]+maal_no1[26]) |>
  mutate(forbuk_hus_NP = maal_no1[27]+maal_no1[28]+maal_no1[29]+maal_no1[30]) |>
  mutate(forbruk_fri_NP = maal_no1[31]+maal_no1[32]+maal_no1[33]+maal_no1[34])

forbrukstest <- maalerdata |>
  group_by(FORBRUKSGRUPPE)

maal_no2 <- filter(maalerdata, PRISOMRÅDE == "NO2")
maal_no3 <- filter(maalerdata, PRISOMRÅDE == "NO3")
maal_no4 <- filter(maalerdata, PRISOMRÅDE == "NO4")
maal_no5 <- filter(maalerdata, PRISOMRÅDE == "NO5")