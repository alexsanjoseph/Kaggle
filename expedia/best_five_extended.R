library(data.table)
library(dplyr)

source("expedia/fnsExpedia.R")

destinations = fread("../Data/kaggle/expedia/destinations.csv")
test_data = fread("../Data/kaggle/expedia/test.csv")
train_data = fread("../Data/kaggle/expedia/train.csv")

##### Data Leak
test_data_subset = test_data #[1:10000]
train_data_subset = train_data #[user_id %in% test_data_subset$user_id]
# Using the full data leak
leak_columns = list(c('user_id', 'user_location_country', 'user_location_region',
                      'user_location_city', 'hotel_market', 'orig_destination_distance'),
                    c('user_location_country', 'user_location_region',
                      'user_location_city', 'hotel_market', 'orig_destination_distance'),
                    c('user_location_country', 'user_location_region',
                      'user_location_city', 'hotel_market'),
                    c('user_location_country', 'user_location_region',
                      'user_location_city', 'orig_destination_distance'),
                    c('user_location_country', 'user_location_region',
                      'user_location_city'),
                    c('user_location_country', 'user_location_region',
                      'hotel_market'),
                    c('user_location_country', 'user_location_region',
                      'orig_destination_distance'),
                    c('user_location_country', 'user_location_region'),
                    c('user_location_country'))

submission = NULL
l = leak_columns[[1]]

for(l in leak_columns){
  print(l)
  setkeyv(train_data_subset, l)
  system.time({
    hotel_cluster_obs = train_data_subset[ , list(most_common = most_common(hotel_cluster)), by = key(train_data_subset)] 
  })
  output_leak_n = test_data_subset %>% merge(hotel_cluster_obs, by = key(hotel_cluster_obs), all.x = F)
  submission = submission %>% rbind(output_leak_n)
  test_data_subset = test_data_subset %>% subset(!(test_data_subset$id %in% submission$id))
}

test_data_subset$most_common = "91 41 48 64 65"
submission_final = submission %>% rbind(test_data_subset) %>% 
  select(id, hotel_cluster = most_common) %>% 
  arrange(id)

z <- gzfile("../Data/kaggle/expedia/submissions/sub8.csv.gz")
write.csv(submission_final, z, row.names = F)

                                                                  