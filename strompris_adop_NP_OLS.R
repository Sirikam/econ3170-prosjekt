# problemstilling 2: strømpris og adapsjon av NP 

# strømpris og adapsjon 

ols_spotpris <- lm(
  prosent_hus_NP ~ slutt_Average,
  data = all_data_samlet
)
summary(ols_spotpris)


#Strømpris, magasinfylling og prisområde

ols_spotpris_kontroller <- lm(
  prosent_hus_NP ~
    slutt_Average +
    fyllingsgrad +
    factor(PRISOMRÅDE),
  data = all_data_samlet
)

summary(ols_spotpris_kontroller)


# Kontrollerer også for tidsutvikling
ols_spotpris_tid <- lm(
  prosent_hus_NP ~ slutt_Average +
    fyllingsgrad +
    factor(PRISOMRÅDE) +
    as.numeric(Date),
  data = all_data_samlet
)

summary(ols_spotpris_tid)

# Problemstilling 3: Magasinfylling og strømpriser

ols_fyllingsgrad <- lm(
  spotpris_Average ~ fyllingsgrad +
    factor(PRISOMRÅDE) +
    as.numeric(Date),
  data = all_data_samlet
)

summary(ols_fyllingsgrad)

