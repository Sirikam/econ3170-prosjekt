# fylling dag for dag ikke på uke 

fyll_long <- fyll_long |>
  mutate(
    PRISOMRÅDE = str_extract(PRISOMRÅDE, "NO\\d+")
  )
# er nå sorter på uke, ikke dato, så må ender det 
install.packages("ISOweek")

fyll_long <- fyll_long |>
  mutate(
    Date = ISOweek::ISOweek2date(
      sprintf("%d-W%02d-1", år, Uke)
    )
  )

all_data_samlet <- all_data_samlet |>
  left_join(
    fyll_long |>
      select(Date, PRISOMRÅDE, fyllingsgrad),
    by = c("Date", "PRISOMRÅDE")
  )

range(fyll_long$Date) #kontrollerer at det er riktig 
range(all_data_samlet$Date) #kontrollerer at det er riktig  

fyll_daglig <- all_data_samlet |>
  select(Date, PRISOMRÅDE) |>
  left_join(
    fyll_long |>
      select(Date, PRISOMRÅDE, fyllingsgrad),
    by = c("Date", "PRISOMRÅDE")
  ) |>
  arrange(PRISOMRÅDE, Date) |>
  group_by(PRISOMRÅDE) |>
  fill(fyllingsgrad, .direction = "down") |>
  ungroup()

all_data_samlet <- all_data_samlet |>
  left_join(
    fyll_daglig,
    by = c("Date", "PRISOMRÅDE")
  )

summary(all_data_samlet$fyllingsgrad)

all_data_samlet |>
  group_by(PRISOMRÅDE) |>
  summarise(
    antall = n(),
    manglende = sum(is.na(fyllingsgrad)),
    gjennomsnitt = mean(fyllingsgrad, na.rm = TRUE)
  )


