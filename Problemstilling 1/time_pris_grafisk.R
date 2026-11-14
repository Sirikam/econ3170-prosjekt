#historisk endring i time priser 1.10.25 - 30.11.26
#nordpool 

timepriser_long <- priser_per_time |>
  pivot_longer(
    cols = `NO1 (NOK)`:`NO5 (NOK)`,
    names_to = "region",
    values_to = "pris"
  )

timepriser_mnd <- timepriser_long |>
  mutate(
    maaned = floor_date(Date, unit = "month")
  ) |>
  group_by(maaned, region) |>
  summarise(
    gjennomsnittspris = mean(pris, na.rm = TRUE),
    .groups = "drop"
  ) 

ggplot(timepriser_mnd, aes(
  x = maaned,
  y = gjennomsnittspris,
  color = region
)) +
  geom_line(
    linewidth = 0.4
  ) +
  geom_hline(
    yintercept = mean(timepriser_mnd$gjennomsnittspris, na.rm = TRUE),
    color = "black",
    linetype = "dashed",
    linewidth = 0.25,
    alpha = 0.7
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 8
    )
  ) +
  labs(
    title = "Utvikling i gjennomsnittlig døgnpris per prisområde 1.10.2025-30.09.2026",
    x = "Måned",
    y = "Strømpris (NOK/kWh)",
    color = "Prisområde"
  )
