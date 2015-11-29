# Rossmann Exploratory Analysis
library(data.table)
library(zoo)
library(forecast)
library(ggplot2)
library(dplyr)
library(xgboost)

# Get Data ----
test <- fread("~/Learning/Data/Kaggle/rossman/test.csv")
train <- fread("~/Learning/Data/Kaggle/rossman/train.csv")
store <- fread("~/Learning/Data/Kaggle/rossman/store.csv")

samp_sub <- fread("~/Learning/Data/Kaggle/rossman/sample_submission.csv")

# Massage Data ----
train$Date = as.Date(train$Date)
train_subset = train %>% select(-Customers) #%>% sample_n(10000) 
single_store = filter(train, Store == 1)

# Exploratory ----
# ggplot(single_store, aes(x = Date, y = Customers)) + geom_line()

# Prediction ----
# samp_sub$Sales = mean(train$Sales)

xgb_data_train = model.matrix( ~ . - 1, data = train_subset %>% select(-Date, -Id)) 
dtrain <- xgb.DMatrix(xgb_data_train[,-3], label = xgb_data_train[,3])
xgb_model = xgboost(data = dtrain, nrounds = 100)

test_subset = test %>% select(-Date, -Id) %>% mutate(Open = replace(Open, is.na(Open), 1))
xgb_data_test = model.matrix( ~ . - 1, data = test_subset)
nrow(xgb_data_test)

dtest <- xgb.DMatrix(xgb_data_test)
pred = predict(xgb_model, dtest)
plot(pred)
pred



# Submission ----
sub = data.frame(Id = test$Id, Sales = pred)
head(samp_sub)
write.csv(samp_sub, "~/Learning/Data/Kaggle/rossman/subs/sub_0.csv", row.names = F)
