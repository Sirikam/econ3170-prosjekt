# Problemstilling 3: Magasinfylling og strømpriser

# 1 setter sammen mini datasett for å få historikken
prob_3_data_fyllingsgrad <- spotpris_historisk_long |>
  inner_join(
    fyll_2020_2026 |>
      select(
        Date,
        PRISOMRÅDE,
        fyllingsgrad
      ),
    by = c("Date", "PRISOMRÅDE")
  ) |>
  drop_na(
    spotpris_aggregert_historisk,
    fyllingsgrad
  )

# 2. reggresjon 

ols_fyllingsgrad <- lm(
  spotpris_aggregert_historisk ~ fyllingsgrad +
    factor(PRISOMRÅDE) +
    as.numeric(Date),
  data = all_data_samlet
)

summary(ols_fyllingsgrad)


# prob 3 grafisk

ggplot(
  fyll_2020_2026,
  aes(
    x = Date,
    y = fyllingsgrad,
    color = PRISOMRÅDE,
    group = PRISOMRÅDE
  )
) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) + 
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    title = "Magasinfylling per prisområde 2020-2026",
    x = "Dato",
    y = "Fyllingsgrad",
    color = "Prisområde"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

# 4.4.2 prob 3

ggplot(
  prob_3_data_fyllingsgrad,
  aes(
    x = fyllingsgrad,
    y = spotpris_aggregert_historisk,
    color = PRISOMRÅDE
  )
) +
  geom_point(alpha = 0.25) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE
  ) +
  facet_wrap(~ PRISOMRÅDE) +
  labs(
    title = "Sammenhengen mellom magasinfylling og aggregert spotpris 2020-2026",
    x = "Magasinfylling per prisområde",
    y = "Spotpris (kr/kWh)",
    color = "Prisområde"
  ) +
  theme_minimal()


