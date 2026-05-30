library(tidyverse)
library(ggplot2)
library(tidymodels)
library(vip)


# PREPROCESSING

spotify <- read.csv("/Users/romygou/Documents/winter 2025/stats 140xp/spotify.csv")

spotify <- spotify %>%
  select(acousticness, danceability, energy, duration_ms, instrumentalness, valence, tempo, liveness, loudness, speechiness, key, year) %>%
  na.omit()
spotify <- spotify %>%
  mutate(era = case_when(
    year >= 1920 & year <= 1949  ~ "1920-40s",
    year >= 1950 & year <= 1969  ~ "1950-60s",
    year >= 1970 & year <= 1999  ~ "1970-90s",
    year >= 2000 & year <= 2020  ~ "2000-10s")) %>%
  select(-year)
spotify$era <- as.factor(spotify$era)

# normalized data for EDA
spotify_norm <- spotify %>%
  mutate(across(where(is.numeric), ~ (. - min(.)) / (max(.) - min(.))))


# EDA

# histogram
ggplot(spotify_norm) +
  aes(x = era) +
  geom_bar(fill = "#4682B4") +
  ggtitle(paste("Distribution of Songs by Era")) +
  theme_minimal()

# box plots
features <- c("acousticness", "danceability", "energy", "duration_ms", "instrumentalness", "valence", "tempo", "liveness", "loudness", "speechiness", "key")
for (i in 1:11) {
  feature_plot <- ggplot(spotify_norm, aes(x = era, y = .data[[features[i]]])) +
    geom_boxplot(fill = "#4682B4") +
    theme_minimal() +
    ggtitle(paste("Distribution of Normalized", toTitleCase(features[i]), "by Era"))
  print(feature_plot)}

# standard deviation table
spotify %>%
  group_by(era) %>%
  summarise(across(where(is.numeric), sd))


# MODEL

# training
set.seed(123)
data_split <- initial_split(spotify, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

rf_recipe <- recipe(era ~., data = train_data) %>%
  step_normalize(all_numeric())
rf_spec <- rand_forest() %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")
rf_workflow <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_spec)
rf_fit <- rf_workflow %>%
  fit(data = train_data)

# performance
predictions <- predict(rf_fit, new_data = test_data) %>%
  pull(.pred_class)
metrics <- metric_set(accuracy)
performance <- test_data %>%
  mutate(predictions = predictions) %>%
  metrics(truth = era, estimate = predictions)
print(performance)

# confusion matrix
confusionMatrix(predictions, test_data$era)

# feature importance
vip(rf_fit)