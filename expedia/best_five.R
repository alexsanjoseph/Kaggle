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

# Using the full data leak
setkeyv(train_data_subset, c('user_location_country', 'user_location_region', 'user_location_city', 'hotel_market', 'orig_destination_distance'))

system.time({
  hotel_cluster_obs = train_data_subset[ , list(most_common = most_common(hotel_cluster)), by = key(train_data_subset)] 
})

output_leak_1 = test_data %>% merge(hotel_cluster_obs, by = key(hotel_cluster_obs), all.x = F)
system.time({
  hotel_cluster_obs = train_data_subset[ , list(most_common = most_common(hotel_cluster)), by = key(train_data_subset)] 
})

test_non_leak = test_data %>% subset(!(id %in% output_leak_1$id))

# Using a smaller grouping variable excluding the orig_dest_distance to see if that gets us anywhere
train_data_subset = train_data

setkeyv(train_data_subset, c('user_location_country', 'user_location_region', 'user_location_city', 'hotel_market'))

system.time({
  hotel_cluster_obs = train_data_subset[ , list(most_common = most_common(hotel_cluster)), by = key(train_data_subset)] 
})

output_leak_2 = test_non_leak %>% merge(hotel_cluster_obs, by = key(hotel_cluster_obs), all.x = F)
test_non_leak = test_data %>% subset(!(id %in% c(output_leak_1$id, output_leak_2$id)))

test_non_leak$most_common = best_five

submission = output_leak_1 %>% rbind(output_leak_2) %>% rbind(test_non_leak) %>% 
  select(id, hotel_cluster = most_common) %>% 
  # mutate(hotel_cluster = replace(hotel_cluster, is.na(hotel_cluster), best_five)) %>% 
  arrange(id)

z <- gzfile("../Data/kaggle/expedia/submissions/sub5.csv.gz")
write.csv(submission, z, row.names = F)

                                                                  