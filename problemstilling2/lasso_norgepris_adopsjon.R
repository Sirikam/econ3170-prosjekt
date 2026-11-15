# #Hvordan varierer strømforbruket mellom husholdninger med og uten Norgespris? 

#LASSO 
library(glmnet)
library(dplyr)
library(tidymodels)

lasso_data <- all_data_samlet |>
  select(
    nye_signeringer,
    slutt_max,
    slutt_min,
    slutt_average,
    prisomrade,
    date,
    stotte_min,
    stotte_max,
    stotte_average,
    fordel_average,
    fordel_max,
    fordel_min
  ) |>
  drop_na() |>
  mutate(
    prisomrade = factor(prisomrade),
    måned = factor(month(date))
  ) |>
  select(-date)

# Deler data i trening (80 %) og test (20 %)

data_split <- initial_split(lasso_data, prop = 0.8)

train_data <- training(data_split)
test_data  <- testing(data_split)

# Oppretter Recipe

lasso_rec <- recipe(nye_signeringer ~ ., data = train_data) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

# Spesifiserer LASSO-modellen

lasso_spec <- linear_reg(
  penalty = tune(),
  mixture = 1
) |>
  set_engine("glmnet")


lasso_wf <- workflow() |>
  add_recipe(lasso_rec) |>
  add_model(lasso_spec)

# velger hvor mange deler den skal ha 

set.seed(3170)

folds <- vfold_cv(train_data, v = 10)

grid_lambda <- grid_regular(
  penalty(range = c(-4, 1)),
  levels = 50
)

lasso_res <- tune_grid(
  lasso_wf,
  resamples = folds,
  grid = grid_lambda,
  metrics = metric_set(rmse, rsq)
)


# finner beste lambda 

best_lambda_p2 <- select_best(
  lasso_res,
  metric = "rmse"
)

best_lambda_p2

#ser at lambda er veldig veldig lav, redd dette lager en overfitted modell 

# tester modellen på testdaten 

final_lasso_wf <- finalize_workflow(
  lasso_wf,
  best_lambda_p2
)

final_lasso_fit_p2 <- last_fit(
  final_lasso_wf,
  split = data_split
)

collect_metrics(final_lasso_fit_p2)

# finner koeffesientene 

final_model_train <- fit(
  final_lasso_wf,
  data = train_data
)

koeffisienter_lasso_p2 <- final_model_train |>
  extract_fit_parsnip() |>
  tidy() |>
  filter(estimate != 0)

koeffisienter_lasso_p2



# Sammenlikne OLS modellen med testdata fra denne LASSOen

# 1. Estimerer OLS på treningsdataene
modell_p2_pred <- lm(
  nye_signeringer ~ slutt_max+
  slutt_min +
  slutt_average +
  stotte_min +
  stotte_max +
  stotte_average +
  fordel_average +
  fordel_max +
  fordel_min +
  prisomrade +
  måned,
  data = train_data
)

summary(modell_p2_pred)

# 2. Predikerer forbruk på testdataene
prediksjoner_ols_p2 <- test_data |>
  mutate(
    .pred = predict(modell_p2_pred, newdata = test_data)
  )

# 3. Beregner prediksjonsmål for OLS
ols_prediksjonsmal_p2 <- metric_set(rmse, rsq, mae)(
  prediksjoner_ols,
  truth = nye_signeringer,
  estimate = .pred
) |>
  mutate(Modell = "OLS")

# 4. Henter LASSO-prediksjonene fra samme testsett
prediksjoner_lasso_p2 <- collect_predictions(final_lasso_fit_p2)

# 5. Beregner prediksjonsmål for LASSO
lasso_prediksjonsmal_p2 <- metric_set(rmse, rsq, mae)(
  prediksjoner_lasso_p2,
  truth = nye_signeringer,
  estimate = .pred
) |>
  mutate(Modell = "LASSO")

# 6. Samler resultatene i én tabell
sammenligning_prediksjon_p2 <- bind_rows(
  ols_prediksjonsmal_p2,
  lasso_prediksjonsmal_p2
) |>
  select(Modell, .metric, .estimate)

sammenligning_prediksjon_p2
