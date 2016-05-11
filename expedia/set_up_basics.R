library(data.table)
library(dplyr)
library(lubridate)
source("expedia/fnsExpedia.R")

destinations = fread("../Data/kaggle/expedia/destinations.csv")
test_data = fread("../Data/kaggle/expedia/test.csv")
train_data = fread("../Data/kaggle/expedia/train.csv")

# Best Five

train_freq = train_data[ , list(length(site_name)), by = c('hotel_cluster')]
train_freq = train_freq %>% arrange(desc(V1))
best_five = train_freq$hotel_cluster[1:5] %>% paste(collapse = " ")

write(best_five, "../Data/kaggle/expedia/best_five.txt")

##### Data Subsets
test_data_subset = test_data [1:10000]
train_data_subset = train_data [user_id %in% test_data_subset$user_id]

train_data_subset$month = month(train_data_subset$date_time)
train_data_subset_first = train_data_subset[month %in% 1:9]
train_data_subset_sec = train_data_subset[month %in% 10:12]

## Map functions in Metrics package

write.csv(test_data_subset, "../Data/kaggle/expedia/test_data_subset.csv", row.names = F)
write.csv(train_data_subset, "../Data/kaggle/expedia/train_data_subset.csv", row.names = F)
