# Rossmann Exploratory Analysis
library(data.table)
library(zoo)
library(forecast)
library(ggplot2)
library(dplyr)

# Get Data ----
test <- fread("~/Learning/Data/Kaggle/rossman/test.csv")
train <- fread("~/Learning/Data/Kaggle/rossman/train.csv")
store <- fread("~/Learning/Data/Kaggle/rossman/store.csv")

samp_sub <- fread("~/Learning/Data/Kaggle/rossman/sample_submission.csv")

# Massage Data ----
train$Date = as.Date(train$Date)
train_subset = sample_n(train, 10000)
single_store = filter(train, Store == 1)

# Exploratory ----
ggplot(single_store, aes(x = Date, y = Customers)) + geom_line()

# Prediction ----
samp_sub$Sales = mean(train$Sales)

# Submission ----
write.csv(samp_sub, "~/Learning/Data/Kaggle/rossman/subs/sub_0.csv", row.names = F)
