library(dplyr)
library(broom)
library(knitr)


# OLS
ols_tabell <- tidy(modell_p1) |>
  filter(term %in% c("prosent_forbruk_hus_np", "slutt_average")) |>
  mutate(
    Variabel = recode(term,
                      "prosent_forbruk_hus_np" = "Andel av forbruket knyttet til Norgespris",
                      "slutt_average" = "Spotpris"
    ),
    OLS = sprintf("%.0f (%.0f)", estimate, std.error)
  ) |>
  select(Variabel, OLS)

# LASSO
lasso_tabell <- koeffisienter_lasso |>
  filter(term %in% c("prosent_forbruk_hus_np", "slutt_average")) |>
  mutate(
    Variabel = recode(term,
                      "prosent_forbruk_hus_np" = "Andel av forbruket knyttet til Norgespris",
                      "slutt_average" = "Spotpris"
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