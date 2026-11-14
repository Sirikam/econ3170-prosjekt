#Hvordan varierer strømforbruket mellom husholdninger med og uten Norgespris?

modell_p1 <- lm(
  forbruk_hus ~ prosent_hus_NP +
    slutt_Average +
    factor(PRISOMRÅDE) +
    factor(month(Date)),
  data = all_data_samlet
)

summary(modell_p1)

ggplot(all_data_samlet, 
       aes(x = prosent_hus_NP, y = forbruk_hus)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", 
              se = TRUE, 
              color = "blue") +
  labs(
    title = "Sammenhengen mellom Norgespris og strømforbruk",
    x = "Andel husholdninger med Norgespris",
    y = "Samlet strømforbruk i husholdninger"
  ) +
  theme_minimal()


ggplot(all_data_samlet, 
       aes(x = prosent_hus_NP, 
           y = forbruk_hus,
           color = PRISOMRÅDE)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", 
              se = TRUE) +
  labs(
    title = "Sammenhengen mellom Norgespris og strømforbruk",
    x = "Andel husholdninger med Norgespris",
    y = "Samlet strømforbruk i husholdninger",
    color = "Prisområde"
  ) +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal()


