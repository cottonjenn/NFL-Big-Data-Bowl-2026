library(tidyverse)
library(tidymodels)
library(vroom)
library(dplyr)
library(DataExplorer)
# library(patchwork)
library(lubridate)  # for hour extraction
library(bonsai)
library(lightgbm)

# Load data
train <- vroom("C:/Users/Jenna/OneDrive/Desktop/Statistics/Stat 348/BikeShare/train.csv")
test <- vroom("C:/Users/Jenna/OneDrive/Desktop/Statistics/Stat 348/BikeShare/test.csv")

# Old factor conversions (commented out)
train$weather <- as.factor(train$weather)
train$season <- as.factor(train$season)
train$holiday <- as.factor(train$holiday)
train$workingday <- as.factor(train$workingday)

# Old exploratory plots (commented out)
glimpse(train)
DataExplorer::plot_correlation(train)
DataExplorer::plot_intro(train)
DataExplorer::plot_bar(train)
DataExplorer::plot_missing(train)
GGally::ggpairs(train)
DataExplorer::plot_histogram(train)

# Old visualizations (commented out)
p1 <- ggplot(train, aes(x=humidity)) + geom_histogram(bins=30, fill="skyblue")
p2 <- ggplot(data= train)+ geom_bar(aes(x=weather, fill=season))
p3 <- ggplot(data=train)+geom_point(aes(x=atemp, y=temp))
p4 <- ggplot(data=train)+geom_point(aes(x=casual, y=count))
(p1 + p2) / (p3 + p4)

# HW Step 1: Data Cleaning
train_clean <- train %>%
   select(-casual, -registered) %>% # Remove columns as per HW
   mutate(count = log(count))        # Log-transform target variable

#  # HW Step 2: Feature Engineering
 bike_recipe <- recipe(count ~ ., data = train_clean) %>%
   step_mutate(weather = ifelse(weather == 4, 3, weather)) %>% # Recode weather 4->3
   # step_mutate(weather = factor(weather)) %>%
   # step_mutate(holiday = factor(holiday)) %>%
   # step_mutate(workingday = factor(workingday)) %>%
   # step_mutate(season = factor(season)) %>%
   # step_mutate(hour = hour(datetime)) %>%
   step_time(datetime, features="hour") %>%
   step_date(datetime, features="month") %>%
   step_date(datetime, features = "doy") %>%
   step_rm(datetime) %>%
   step_normalize(all_numeric_predictors()) %>%
   step_dummy(all_nominal_predictors()) %>%
   step_zv(all_predictors()) # Remove zero-variance predictors

# Prepare and bake recipe (optional check)
prepped_recipe <- prep(bike_recipe)
baked_data <- bake(prepped_recipe, new_data = train_clean)
head(baked_data, 5)  # Show first 5 rows for HW

# # first penalty=0.01, mixture=0.01
# # second penalty=0.5, mixture=0.05
# # 3 penalty=0.1, mixture=0.1
# # 4 penalty=0.5, mixture=0.1

## new penalized regression ----------------------------------
preg_model <- linear_reg(penalty=0.1, mixture=0.1) %>%
  set_engine("glmnet") # Function to fit in R11
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model) %>%
  fit(data=train_clean)
bike_preds <- predict(preg_wf, new_data=test) %>%
  mutate(count = exp(.pred))

kaggle_submission <- bike_preds %>%
  bind_cols(test %>% select(datetime)) %>%  # Keep original test datetime
  select(datetime, count) %>%               # Column order
  mutate(count = pmax(0, count)) %>%
  mutate(datetime=as.character(format(datetime)))

vroom_write(kaggle_submission, "LinearPreds_v2.csv", delim = ",")
