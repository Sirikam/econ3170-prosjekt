#Hvordan varierer strømforbruket mellom husholdninger med og uten Norgespris?

modell_p1 <- lm(
  forbruk_hus ~ prosent_forbruk_hus_np +
    slutt_average +
    factor(prisomrade) +
    factor(month(date)),
  data = all_data_samlet
)

summary(modell_p1)


ggplot(all_data_samlet, 
       aes(x = prosent_forbruk_hus_np, 
           y = forbruk_hus,
           color = prisomrade)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", 
              se = TRUE) +
  labs(
    title = "Sammenhengen mellom Norgespris og strømforbruk",
    x = "Prosent forbruk husholdninger med Norgespris",
    y = "Samlet strømforbruk i husholdninger",
    color = "Prisområde"
  ) +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal()


