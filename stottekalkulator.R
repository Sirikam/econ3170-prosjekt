# Utregning av terskelverdier for strømstøtte for hvert prisområde

 library(tidyverse)
 
 
 terskelverdier <- c(0.7, 0.7, 0.7, 0.75, 0.77) #2021-2026
 #dekningsgrad 
 #2021, 0.55, 12
 #2022, 0.9, 0.8, 0.9; 1-3,4-9, 10-12
 #2023, 0.9, timebasert; 1-9, 10-12
 #2024, 0.9; 1-12, 5000 kwh
 #2025, 0.9; 1-12, 5000 kwh
 #2026, 0.9; 1-12, 5000 kwh
 
 
 #funksjonen er laget ved hjelp av google AI, ved implementering av across
 df <- strompriser |>
   mutate(
     across(
       c(`NO1 (NOK)`, `NO2 (NOK)`, `NO3 (NOK)`, `NO4 (NOK)`, `NO5 (NOK)`),
       
       # 1. FJERNET "stotte = " OG BEHOLDT KUN TILDEN (~) HER:
       ~ case_when(
         # 2020-21, før støtte (støtten startet des 2021, så her blir det 0)
         `Delivery Date (CET)` < as.Date("2021-12-01") & .x >= 0.7 ~ 0,
         
         # 2021.12 - 2022.03 (Desember 2021 var støtten 55 %)
         `Delivery Date (CET)` < as.Date("2022-01-01") & 
           `Delivery Date (CET)` >= as.Date("2021-12-01") &
           .x >= 0.7 ~ (.x - 0.7) * 0.55,
         
         # 2022.01 - 2022.03 (Januar-mars 2022 økte den til 80 %)
         `Delivery Date (CET)` < as.Date("2022-04-01") & 
           `Delivery Date (CET)` >= as.Date("2022-01-01") &
           .x >= 0.7 ~ (.x - 0.7) * 0.8,
         
         # 2022.4 - 2022.8 (April-august 2022 var den 80 %)
         `Delivery Date (CET)` < as.Date("2022-09-01") & 
           `Delivery Date (CET)` >= as.Date("2022-04-01") &
           .x >= 0.7 ~ (.x - 0.7) * 0.8,
         
         # 2022.09 - 2022.12 
         `Delivery Date (CET)` < as.Date("2023-01-01") & 
           `Delivery Date (CET)` >= as.Date("2022-09-01") &
           .x >= 0.7 ~ (.x - 0.7) * 0.9,
         
         # 2023.01 - 2023.09
         `Delivery Date (CET)` < as.Date("2023-10-01") & 
           `Delivery Date (CET)` >= as.Date("2023-01-01") &
           .x >= 0.7 ~ (.x - 0.7) * 0.9,
         
         # 2023.10 - 2023.12 timespris begynner her, trenger bedre data
         `Delivery Date (CET)` < as.Date("2024-01-01") & 
           `Delivery Date (CET)` >= as.Date("2023-10-01") &
           .x >= 0.7 ~ (.x - 0.7) * 0.9,
         
         # 2024.01 - 2024.12 (Terskel økte til 73 øre)
         `Delivery Date (CET)` < as.Date("2025-01-01") & 
           `Delivery Date (CET)` >= as.Date("2024-01-01") &
           .x >= 0.73 ~ (.x - 0.73) * 0.9,
         
         # 2025.01 - 2025.12 (Terskel økte til 75 øre)
         `Delivery Date (CET)` < as.Date("2026-01-01") & 
           `Delivery Date (CET)` >= as.Date("2025-01-01") &
           .x >= 0.75 ~ (.x - 0.75) * 0.9,
         
         # 2026.01 -> (Terskel økte til 77 øre)
         `Delivery Date (CET)` >= as.Date("2026-01-01") &
           .x >= 0.77 ~ (.x - 0.77) * 0.9,
         
         TRUE ~ 0
       ), 
     
     .names = "stotte_{.col}"
    )
   ) |> # beregner endelig pris ved å trekke fra stotten på prisen
   mutate(
     sluttpris_NO1 = `NO1 (NOK)` - `stotte_NO1 (NOK)`,
     sluttpris_NO2 = `NO2 (NOK)` - `stotte_NO2 (NOK)`,
     sluttpris_NO3 = `NO3 (NOK)` - `stotte_NO3 (NOK)`,
     sluttpris_NO4 = `NO4 (NOK)` - `stotte_NO4 (NOK)`,
     sluttpris_NO5 = `NO5 (NOK)` - `stotte_NO5 (NOK)`
   )
