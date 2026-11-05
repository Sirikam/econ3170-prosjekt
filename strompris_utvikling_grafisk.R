#Historisk strømpris per prisområde  

#nordpool data
#gjennomsnittspriser 


strompriser_long <- strompriser |>
  pivot_longer(
    cols = `NO1 (NOK)`:`NO5 (NOK)`,
    names_to = "region",
    values_to = "pris i ore"
  )

summary(strompriser_long$`pris i ore`) 
#finne min og max for å vite om jeg skal justere y-aksen


ggplot(strompriser_long, aes(
  x = `Delivery Date (CET)`,
  y = `pris i ore`,
  color = region
)) +
  geom_line(
    linewidth = 0.25,
    alpha = 0.8
  ) +
  geom_hline(
    yintercept = mean(strompriser_long$`pris i ore`, na.rm = TRUE),
    color = "black",
    linetype = "dashed",
    linewidth = 0.25,
    alpha = 0.7
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    title = "Utvikling i gjennomsnittlig døgnpris per prisområde",
    x = "Dato",
    y = "Strømpris (NOK/kWh)",
    color = "Prisområde"
  )

