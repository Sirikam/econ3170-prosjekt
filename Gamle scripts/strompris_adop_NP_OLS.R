
# Problemstilling 3: Magasinfylling og strømpriser

ols_fyllingsgrad <- lm(
  spotpris_aggregert_historisk ~ fyllingsgrad +
    factor(PRISOMRÅDE) +
    as.numeric(Date),
  data = all_data_samlet
)

summary(ols_fyllingsgrad)


#4.4 prob 3

ggplot(
  all_data_samlet,
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
  labs(
    title = "Magasinfylling per prisområde",
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
  all_data_samlet,
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
    title = "Sammenhengen mellom magasinfylling og spotpris",
    x = "Magasinfylling",
    y = "Spotpris (kr/kWh)",
    color = "Prisområde"
  ) +
  theme_minimal()
