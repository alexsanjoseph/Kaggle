library(data.table)
library(dplyr)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

source("expedia/fnsExpedia.R")

destinations = fread("../Data/kaggle/expedia/destinations.csv")
test_data = fread("../Data/kaggle/expedia/test.csv")
train_data = fread("../Data/kaggle/expedia/train.csv")

# sample_sub = fread("../Data/kaggle/expedia/sample_submission.csv")̧
train_data_book = train_data %>% filter(is_booking == 1)
train_data_sub = sample_frac(train_data, 0.1)

train_label = train_data_sub$hotel_cluster
train_data_sub = train_data_sub %>% select(-hotel_cluster)
train_data_sub_rel = train_data_sub %>% select(-date_time)

head(train_data_sub)
head(test_data)
ohe_feats = c('gender', 'education', 'employer')

names(train_data_sub)
