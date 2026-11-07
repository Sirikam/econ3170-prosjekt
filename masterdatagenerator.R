library(tidyverse)

masterdata <- inner_join(dognstatistikk_w,stotteoversikt, by= "Date")

#lager spesifikke datasett for de ulike prisomraadene

#NO1_master <- 

#Dato, maks, min, avg, %np_hus, %np_fri, forbruk_hus, forbruk_hus_np

#No1_NP inneholder %np_hus, %np_fri, sjekkes opp mot maks, min, avg.
#No1_FOrbruk forbruk_hus, forbruk_hus_np <- maks, min, avg
#Fyllingsgrad