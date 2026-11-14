library(dplyr)
library(broom)
library(knitr)

# OLS
ols_tabell <- tidy(modell_p1) |>
  filter(term %in% c("prosent_hus_NP", "slutt_Average")) |>
  mutate(
    Variabel = recode(term,
                      "prosent_hus_NP" = "Andel husholdninger med Norgespris",
                      "slutt_Average" = "Spotpris"
    ),
    OLS = sprintf("%.0f (%.0f)", estimate, std.error)
  ) |>
  select(Variabel, OLS)

# LASSO
lasso_tabell <- koeffisienter_lasso |>
  filter(term %in% c("prosent_hus_NP", "slutt_Average")) |>
  mutate(
    Variabel = recode(term,
                      "prosent_hus_NP" = "Andel husholdninger med Norgespris",
                      "slutt_Average" = "Spotpris"
    ),
    LASSO = round(estimate, 0)
  ) |>
  select(Variabel, LASSO)

# Lager tabellen
tabell_41 <- left_join(ols_tabell, lasso_tabell, by = "Variabel")

kable(
  tabell_41,
  caption = "Tabell 4.1. Regresjonsresultater for modell P1",
  align = "lrr"
)