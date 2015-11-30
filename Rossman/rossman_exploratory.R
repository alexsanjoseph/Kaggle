# Rossmann Exploratory Analysis
library(data.table)
library(zoo)
library(forecast)
library(ggplot2)
library(dplyr)
library(xgboost)
library(Matrix)

# Get Data ----
test <- fread("~/Learning/Data/Kaggle/rossman/test.csv")
train <- fread("~/Learning/Data/Kaggle/rossman/train.csv")
store <- fread("~/Learning/Data/Kaggle/rossman/store.csv")

samp_sub <- fread("~/Learning/Data/Kaggle/rossman/sample_submission.csv")

# Massage Data ----
train$Date = as.Date(train$Date)
holdout_sample = sample(1:nrow(train), floor(nrow(train)/3))
train_massaged = train %>% select(-Customers) %>%
  mutate(Store = as.factor(Store), StateHoliday = as.factor(StateHoliday), SchoolHoliday = as.factor(SchoolHoliday)) %>% 
  select(-Date)
train_train = train_massaged[-holdout_sample,]
holdout_subset = train_massaged[holdout_sample,]

# single_store = filter(train, Store == 2)

# Exploratory ----
# ggplot(single_store, aes(x = Date, y = Customers)) + geom_line()

# Prediction ----
xgb_data_train <- sparse.model.matrix(Sales ~ ., train_train)
dtrain <- xgb.DMatrix(xgb_data_train, label = train_train$Sales)

xgb_data_holdout = model.matrix(Sales ~ ., data = holdout_subset)
dholdout <- xgb.DMatrix(xgb_data_holdout, label = holdout_subset$Sales)
train.gdbt<-xgb.train(params=list(objective = "reg:linear", eval_metric="rmse", 
                                  eta=0.2, max_depth=5, subsample=1, colsample_bytree=0.5), 
                      data=dtrain, nrounds=150, watchlist=list(eval=holdout, train=dtrain))

# Let's think of the domain
# train_subset_dt = data.table(train_subset, key =  c("Store", "DayOfWeek"))
# store_mean = train_subset[,StoreMean:=mean(Sales),by = c("Store", "DayOfWeek")] %>% 
#   select(Store, DayOfWeek, StoreMean) %>% unique

# Submission ----

test_subset = test %>% select(-Id) %>% mutate(Open = replace(Open, is.na(Open), 1)) %>% 
  mutate(Store = as.factor(Store), StateHoliday = as.factor(StateHoliday), SchoolHoliday = as.factor(SchoolHoliday)) %>% 
  select(-Date)
xgb_data_test <- sparse.model.matrix( ~ ., test_subset)
dtest <- xgb.DMatrix(xgb_data_test)

pred = predict(train.gdbt, dtest)
plot(pred)

sub = test %>% transmute(Id, Sales = pred)

write.csv(sub, "~/Learning/Data/Kaggle/rossman/subs/sub_5.csv", row.names = F)
