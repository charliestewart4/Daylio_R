library(randomForest)
library(tidymodels)

#model <- randomForest(Species ~ .,data = daylio ,ntree = 100, proximity = TRUE)

daylio_split <- df%>%
  select(-c(moodVal))%>%
  initial_split(prop = 0.6)


daylio_split %>%
  training() %>%
  glimpse()

daylio_recipe <- training(daylio_split) %>%
  recipe(mood ~.) %>%
  step_rm(contains(c('full_date','time'))) %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()

daylio_testing <- daylio_recipe %>%
  bake(testing(daylio_split)) 

glimpse(daylio_testing)

daylio_training <- juice(daylio_recipe)

glimpse(daylio_training)

daylio_rf <-  rand_forest(trees = 100, mode = "classification") %>%
  set_engine("randomForest") %>%
  fit(mood ~ ., data = daylio_training)

daylio_probs <- daylio_rf %>%
          predict(daylio_testing, type = "prob") %>%
          bind_cols(daylio_testing)
#metrics(truth = mood, estimate = .pred_class)

daylio_probs%>%
  gain_curve(mood, .pred_awful:.pred_rad) %>%
  autoplot()
daylio_rf$fit$importance

plot(daylio_rf$fit, log="y")
varImpPlot(daylio_rf$fit)
