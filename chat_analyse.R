#Denne koden er skapt med prompting i chagpt

library(tidyverse)
library(slider)

NP_panel <- bind_rows(
  NO1_master %>% mutate(omrade = "NO1"),
  NO2_master %>% mutate(omrade = "NO2"),
  NO3_master %>% mutate(omrade = "NO3"),
  NO4_master %>% mutate(omrade = "NO4"),
  NO5_master %>% mutate(omrade = "NO5")
) %>%
  arrange(omrade, Date) %>%
  group_by(omrade) %>%
  mutate(
    # Antall nye signeringer
    nye_signeringer =
      sum_hus_NP - lag(sum_hus_NP),
    
    # Antall som kunne signere ved periodens start
    mulige_signeringer =
      lag(sum_hus_tot - sum_hus_NP),
    
    # Signeringsrate
    signeringsrate =
      if_else(
        mulige_signeringer > 0,
        nye_signeringer / mulige_signeringer,
        NA_real_
      ),
    
    # Gårsdagens strømpris
    pris_lag1 = lag(avg_pris, 1),
    
    # Gjennomsnitt siste 7 dager,
    # ekskludert dagens pris
    pris_7d = lag(
      slide_dbl(
        avg_pris,
        mean,
        .before = 6,
        .complete = TRUE
      ),
      1
    )
  ) %>%
  ungroup()


ggplot(
  NP_panel,
  aes(
    x = Date,
    y = signeringsrate,
    color = omrade
  )
) +
  geom_line() +
  labs(
    title = "Daglig signeringsrate for Norgespris",
    x = "Dato",
    y = "Andel av gjenværende husholdninger",
    color = "Prisområde"
  ) +
  theme_minimal()