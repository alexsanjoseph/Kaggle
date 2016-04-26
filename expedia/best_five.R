library(data.table)
library(dplyr)

destinations = fread("../Data/kaggle/expedia/destinations.csv")
test_data = fread("../Data/kaggle/expedia/test.csv")
train_data = fread("../Data/kaggle/expedia/train.csv")

# sample_sub = fread("../Data/kaggle/expedia/sample_submission.csv")̧
head(sample_sub)

train_freq = train_data[ , list(length(site_name)), by = c('hotel_cluster')]
train_freq = train_freq %>% arrange(desc(V1))
head(train_freq)

best_five = train_freq$hotel_cluster[1:5] %>% paste(collapse = " ")
sa
test_data$id %>% head

write.csv(data.frame(id = test_data, hotel_cluster = best_five),
          "../Data/kaggle/expedia/submissions/sub1.csv", row.names = F)

