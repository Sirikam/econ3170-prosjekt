library(tidyverse)

masterdata <- inner_join(dognstatistikk_w,stotteoversikt, by= "Date")

#lager spesifikke datasett for de ulike prisomraadene

# NO1
NO1_stotte <- stotteutregner(select(dognstatistikk_w, c(Date, starts_with("NO1"))))
NO1_master <- inner_join(NO1_stotte,maal_no1, by = "Date")

#Dato, maks, min, avg, %np_hus, %np_fri, forbruk_hus, forbruk_hus_np

#No1_NP inneholder %np_hus, %np_fri, sjekkes opp mot maks, min, avg.
#No1_FOrbruk forbruk_hus, forbruk_hus_np <- maks, min, avg
#Fyllingsgrad

NO2_stotte <- stotteutregner(select(dognstatistikk_w, c(Date, starts_with("NO2"))))
NO2_master <- inner_join(NO2_stotte,maal_no2, by = "Date")

NO3_stotte <- stotteutregner(select(dognstatistikk_w, c(Date, starts_with("NO3"))))
NO3_master <- inner_join(NO3_stotte,maal_no3, by = "Date")

NO4_stotte <- stotteutregner(select(dognstatistikk_w, c(Date, starts_with("NO4"))))
NO4_master <- inner_join(NO4_stotte,maal_no4, by = "Date")

NO5_stotte <- stotteutregner(select(dognstatistikk_w, c(Date, starts_with("NO5"))))
NO5_master <- inner_join(NO5_stotte,maal_no5, by = "Date")