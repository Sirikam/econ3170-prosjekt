# Problemstilling 2  eget script

# problemstilling 2: strømpris og adapsjon av NP 

# Modell 2A: Kun nettopris
modell_2A <- lm(
  prosent_hus_NP ~ slutt_Average,
  data = all_data_samlet
)

# Modell 2B: Nettopris, magasinfylling og område
modell_2B <- lm(
  prosent_hus_NP ~
    slutt_Average +
    fyllingsgrad +
    factor(PRISOMRÅDE),
  data = all_data_samlet
)

# Modell 2C: Utvider med lineær tidstrend
modell_2C <- lm(
  prosent_hus_NP ~
    slutt_Average +
    fyllingsgrad +
    factor(PRISOMRÅDE) +
    as.numeric(Date),
  data = all_data_samlet
)
