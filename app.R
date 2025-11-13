library(tidyverse)
library(lubridate)
library(tidymodels)

# READ DATA
bike <- read_csv("SeoulBikeData.csv", locale = locale(encoding = "latin1"))

# CLEAN DATA
bike <- bike %>%
  mutate(
    date = dmy(Date),
    seasons = factor(Seasons),
    holiday = factor(Holiday),
    functioning_day = factor(`Functioning Day`)
  ) %>%
  select(
    date,
    bike_count = `Rented Bike Count`,
    Hour,
    temperature = `Temperature(°C)`,
    humidity = `Humidity(%)`,
    windspeed = `Wind speed (m/s)`,
    visibility = `Visibility (10m)`,
    dew_point = `Dew point temperature(°C)`,
    solar = `Solar Radiation (MJ/m2)`,
    rainfall = `Rainfall(mm)`,
    snowfall = `Snowfall (cm)`,
    seasons,
    holiday,
    functioning_day
  ) %>%
  filter(functioning_day == "Fun")

# DAILY DATA
daily <- bike %>%
  group_by(date, seasons, holiday) %>%
  summarise(
    bike_count = sum(bike_count),
    rainfall = sum(rainfall),
    snowfall = sum(snowfall),
    temperature = mean(temperature),
    humidity = mean(humidity),
    windspeed = mean(windspeed),
    visibility = mean(visibility),
    dew_point = mean(dew_point),
    solar = mean(solar),
    .groups = "drop"
  )

# TRAIN / TEST SPLIT
set.seed(123)
split <- initial_split(daily, prop = 0.75)
train <- training(split)
test  <- testing(split)

# RECIPE
rec <- recipe(
  bike_count ~ seasons + holiday +
    rainfall + snowfall + temperature +
    humidity + windspeed + visibility +
    dew_point + solar,
  data = train
) %>%
  step_dummy(seasons, holiday) %>%
  step_normalize(all_numeric_predictors())

# MODEL
lm_spec <- linear_reg() %>% set_engine("lm")

wf <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(lm_spec)

# FIT DIRECTLY TO TRAIN SET
final_model <- fit(wf, train)

# PREDICT ON TEST SET
preds <- predict(final_model, test) %>%
  bind_cols(test %>% select(bike_count))

# RMSE
rmse(preds, truth = bike_count, estimate = .pred)

# COEFFICIENTS
final_model %>%
  extract_fit_parsnip() %>%
  tidy()
