library(data.table)
library(dplyr)

source("expedia/fnsExpedia.R")

destinations = fread("../Data/kaggle/expedia/destinations.csv")
test_data = fread("../Data/kaggle/expedia/test.csv")
train_data = fread("../Data/kaggle/expedia/train.csv")

# sample_sub = fread("../Data/kaggle/expedia/sample_submission.csv")̧

###### Best Five

# head(sample_sub)
# 
# train_freq = train_data[ , list(length(site_name)), by = c('hotel_cluster')]
# train_freq = train_freq %>% arrange(desc(V1))
# head(train_freq)
# 
# best_five = train_freq$hotel_cluster[1:5] %>% paste(collapse = " ")
# 
# head(train_data)
# test_data$id %>% head

##### Data Leak
train_data_subset = train_data[1:1000000,]
setkeyv(train_data_subset, c('user_location_country', 'user_location_region', 'user_location_city', 'hotel_market', ))

system.time({
  hotel_cluster_obs = train_data_subset[ , list(most_common = most_common(hotel_cluster)), by = key(train_data_subset)] 
})

A = train_data[user_id == 12]
A
A = train_data_subset[, key(train_data_subset), with = F]
A1 = unique(A)
                                                                  
write.csv(data.frame(id = test_data$id, hotel_cluster = best_five),
          "../Data/kaggle/expedia/submissions/sub1.csv", row.names = F)

                                                                  