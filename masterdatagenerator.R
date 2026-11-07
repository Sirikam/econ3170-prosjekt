library(tidyverse)

masterdata <- inner_join(dognstatistikk_w,stotteoversikt, by= "Date")

#lager spesifikke datasett for de ulike prisomraadene

#NO1_master <- 