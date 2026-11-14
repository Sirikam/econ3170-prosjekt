

library(tidyverse)
library(dplyr)


#funksjonen er laget ved hjelp av google AI, ved implementering av across
#test_oversikt <- select(dognstatistikk_w, c(Date, starts_with("NO1"))) |>
  
stotteutregner <- function(cc){  
  cc_ny <- cc |> 
    mutate(
    across(.cols = starts_with("NO"),
      
      ~ case_when(
        # 2020-21, før støtte (støtten startet des 2021, så her blir det 0)
        `Date` < as.Date("2021-12-01") & .x >= 0.7 ~ 0,
        
        # 2021.12 - 2022.03 (Desember 2021 var støtten 55 %)
        `Date` < as.Date("2022-01-01") & 
          `Date` >= as.Date("2021-12-01") &
          .x >= 0.7 ~ (.x - 0.7) * 0.55,
        
        # 2022.01 - 2022.03 (Januar-mars 2022 økte den til 80 %)
        `Date` < as.Date("2022-04-01") & 
          `Date` >= as.Date("2022-01-01") &
          .x >= 0.7 ~ (.x - 0.7) * 0.8,
        
        # 2022.4 - 2022.8 (April-august 2022 var den 80 %)
        `Date` < as.Date("2022-09-01") & 
          `Date` >= as.Date("2022-04-01") &
          .x >= 0.7 ~ (.x - 0.7) * 0.8,
        
        # 2022.09 - 2022.12 
        `Date` < as.Date("2023-01-01") & 
          `Date` >= as.Date("2022-09-01") &
          .x >= 0.7 ~ (.x - 0.7) * 0.9,
        
        # 2023.01 - 2023.09
        `Date` < as.Date("2023-10-01") & 
          `Date` >= as.Date("2023-01-01") &
          .x >= 0.7 ~ (.x - 0.7) * 0.9,
        
        # 2023.10 - 2023.12 timespris begynner her, trenger bedre data
        `Date` < as.Date("2024-01-01") & 
          `Date` >= as.Date("2023-10-01") &
          .x >= 0.7 ~ (.x - 0.7) * 0.9,
        
        # 2024.01 - 2024.12 (Terskel økte til 73 øre)
        `Date` < as.Date("2025-01-01") & 
          `Date` >= as.Date("2024-01-01") &
          .x >= 0.73 ~ (.x - 0.73) * 0.9,
        
        # 2025.01 - 2025.12 (Terskel økte til 75 øre)
        `Date` < as.Date("2026-01-01") & 
          `Date` >= as.Date("2025-01-01") &
          .x >= 0.75 ~ (.x - 0.75) * 0.9,
        
        # 2026.01 -> (Terskel økte til 77 øre)
        `Date` >= as.Date("2026-01-01") &
          .x >= 0.77 ~ (.x - 0.77) * 0.9,
        
        TRUE ~ 0
      ), 
      
      .names = "stotte_{.col}"
    )
  ) |> # beregner endelig pris ved å trekke fra stotten på prisen
    mutate(
      across(.cols = starts_with("NO"),
             .fns = \(x) x - pick(paste0("stotte_", cur_column()))[[1]],
             .names = "slutt_{.col}"
      )
    )|> #beregner pris for omraadene gitt at det er valgt norgespris
  mutate(
    Norgespris_uMVA
    = case_when(
      #foer stromstotte
      `Date` < as.Date("2025-10-01") ~ 0,
      TRUE ~ 0.4
    )
  ) |> 
  mutate(
    Norgespris_mMVA
    = case_when(
      #foer stromstotte
      `Date` < as.Date("2025-10-01") ~ 0,
      TRUE ~ 0.5
      )
    )
  return(cc_ny)
  }


