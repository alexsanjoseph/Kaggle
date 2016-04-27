library(data.table)
library(dplyr)

source("expedia/fnsExpedia.R")

destinations = fread("../Data/kaggle/expedia/destinations.csv")
test_data = fread("../Data/kaggle/expedia/test.csv")
train_data = fread("../Data/kaggle/expedia/train.csv")

# sample_sub = fread("../Data/kaggle/expedia/sample_submission.csv")̧

###### Best Five

# 
train_freq = train_data[ , list(length(site_name)), by = c('hotel_cluster')]
train_freq = train_freq %>% arrange(desc(V1))
# 
best_five = train_freq$hotel_cluster[1:5] %>% paste(collapse = " ")
# 
# write.csv(data.frame(id = test_data$id, hotel_cluster = best_five),
          # "../Data/kaggle/expedia/submissions/sub2.csv", row.names = F)


##### Data Leak
names(train_data)
train_data_subset = train_data #[1:1000000,]
setkeyv(train_data_subset, c('user_location_country', 'user_location_region', 'user_location_city', 'hotel_market', 'orig_destination_distance'))

system.time({
  hotel_cluster_obs = train_data_subset[ , list(most_common = most_common(hotel_cluster)), by = key(train_data_subset)] 
})

output = test_data %>% merge(hotel_cluster_obs, by = key(hotel_cluster_obs), all.x = T)
head(output)                                                                  
submission = output %>% select(id, hotel_cluster = most_common) %>% 
  mutate(hotel_cluster = replace(hotel_cluster, is.na(hotel_cluster), best_five)) %>% 
  arrange(id)

write.csv(submission, "../Data/kaggle/expedia/submissions/sub3.csv", row.names = F)

                                                                  