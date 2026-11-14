# #Hvordan varierer strømforbruket mellom husholdninger med og uten Norgespris? 

#LASSO 
library(glmnet)
library(dplyr)
library(tidymodels)

lasso_data <- all_data_samlet |>
  select(
    forbruk_hus,
    prosent_hus_NP,
    slutt_Average,
    PRISOMRÅDE,
    Date
  ) |>
  drop_na() |>
  mutate(
    PRISOMRÅDE = factor(PRISOMRÅDE),
    måned = factor(month(Date))
  ) |>
  select(-Date)

# Deler data i trening (80 %) og test (20 %)

data_split <- initial_split(lasso_data, prop = 0.8)

train_data <- training(data_split)
test_data  <- testing(data_split)

# Oppretter Recipe

lasso_rec <- recipe(forbruk_hus ~ ., data = train_data) |>
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

best_lambda <- select_best(
  lasso_res,
  metric = "rmse"
)

best_lambda

#ser at lambda er veldig veldig lav, redd dette lager en overfitted modell 

# tester modellen på testdaten 

final_lasso_wf <- finalize_workflow(
  lasso_wf,
  best_lambda
)

final_lasso_fit <- last_fit(
  final_lasso_wf,
  split = data_split
)

collect_metrics(final_lasso_fit)

# finner koeffesientene 

final_model_train <- fit(
  final_lasso_wf,
  data = train_data
)

koeffisienter_lasso <- final_model_train |>
  extract_fit_parsnip() |>
  tidy() |>
  filter(estimate != 0)

koeffisienter_lasso



# Sammenlikne OLS modellen med testdata fra denne LASSO 

# 1. Estimerer OLS på treningsdataene
modell_p1_pred <- lm(
  forbruk_hus ~ prosent_hus_NP +
    slutt_Average +
    PRISOMRÅDE +
    måned,
  data = train_data
)

summary(modell_p1_pred)

# 2. Predikerer forbruk på testdataene
prediksjoner_ols <- test_data |>
  mutate(
    .pred = predict(modell_p1_pred, newdata = test_data)
  )

# 3. Beregner prediksjonsmål for OLS
ols_prediksjonsmal <- metric_set(rmse, rsq, mae)(
  prediksjoner_ols,
  truth = forbruk_hus,
  estimate = .pred
) |>
  mutate(Modell = "OLS")

# 4. Henter LASSO-prediksjonene fra samme testsett
prediksjoner_lasso <- collect_predictions(final_lasso_fit)

# 5. Beregner prediksjonsmål for LASSO
lasso_prediksjonsmal <- metric_set(rmse, rsq, mae)(
  prediksjoner_lasso,
  truth = forbruk_hus,
  estimate = .pred
) |>
  mutate(Modell = "LASSO")

# 6. Samler resultatene i én tabell
sammenligning_prediksjon <- bind_rows(
  ols_prediksjonsmal,
  lasso_prediksjonsmal
) |>
  select(Modell, .metric, .estimate)

sammenligning_prediksjon
