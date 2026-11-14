# funksjon for import og formattering a strommmaalerdata for de ulike
# strømprisområdene
library(readr)
library(readxl)
library(tidyverse)

#forbruk i KWH, antall i totalt antall og antall 

maalerdata <- read_csv2('data-kilde/maaler/strommaalere.csv')

# Finn alle Excel-filer i mappen
filer <- list.files(
  path = "data-kilde/maaler",
  pattern = "\\.(xls|xlsx|csv)$",
  full.names = TRUE,
  ignore.case = TRUE
)

maalerdata <- read_csv2(filer)|>rename(Date = `DATO`)


beregn_prisområde <- function(data, omrade_navn) {
  data |>
    filter(PRISOMRÅDE == omrade_navn) |>
    pivot_wider(
      names_from = c(EAC_GRUPPE, FORBRUKSGRUPPE),
      values_from = c(ANTALL_NORGESPRIS, ANTALL, FORBRUK, FORBRUK_NORGESPRIS),
      values_fill = 0  # Sørger for at manglende grupper blir 0 i stedet for NA
    ) |>
    rowwise() |>
    mutate(
      sum_hus_NP      = sum(c_across(starts_with("ANTALL_NORGESPRIS_HUSHOLDNING")), na.rm = TRUE),
      sum_fri_NP      = sum(c_across(starts_with("ANTALL_NORGESPRIS_FRITID")), na.rm = TRUE),
      sum_hus_tot     = sum(c_across(starts_with("ANTALL_HUSHOLDNING")), na.rm = TRUE),
      sum_fri_tot     = sum(c_across(starts_with("ANTALL_FRITID")), na.rm = TRUE),
      forbruk_hus     = sum(c_across(starts_with("FORBRUK_HUSHOLDNING")), na.rm = TRUE),
      forbruk_fri     = sum(c_across(starts_with("FORBRUK_FRITID")), na.rm = TRUE),
      forbruk_hus_NP  = sum(c_across(starts_with("FORBRUK_NORGESPRIS_HUSHOLDNING")), na.rm = TRUE),
      forbruk_fri_NP  = sum(c_across(starts_with("FORBRUK_NORGESPRIS_FRITID")), na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
      prosent_hus_NP         = sum_hus_NP / sum_hus_tot,
      prosent_fri_NP         = sum_fri_NP / sum_fri_tot,
      prosent_forbruk_hus_NP = forbruk_hus_NP / forbruk_hus,
      prosent_forbruk_fri_NP = forbruk_fri_NP / forbruk_fri
    )
}

#Henter ut de 5 tabellene
maal_no1_test <- beregn_prisområde(maalerdata, "NO1")
maal_no2_test <- beregn_prisområde(maalerdata, "NO2")
maal_no3_test <- beregn_prisområde(maalerdata, "NO3")
maal_no4_test <- beregn_prisområde(maalerdata, "NO4")
maal_no5_test <- beregn_prisområde(maalerdata, "NO5")


# Slår sammen dataene for alle prisområdene
maal_alle <- bind_rows(
  maal_no1_test,
  maal_no2_test,
  maal_no3_test,
  maal_no4_test,
  maal_no5_test
)


# Beregn én samlet tabell og splitt i 5 deltabeller
maal_liste <- maal_alle |> 
  group_split(PRISOMRÅDE) |> 
  set_names(c("NO1", "NO2", "NO3", "NO4", "NO5"))


names(maal_liste)
glimpse(maal_liste)

