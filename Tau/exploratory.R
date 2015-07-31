
source("bootstrap.R")

dir_name = "../Kaggle-Data/tau/"

train_file = paste0(dir_name, "training.csv")
test_file = paste0(dir_name, "test.csv")
check_agreement_file = paste0(dir_name, "check_agreement.csv")
check_cor_file = paste0(dir_name, "check_correlation.csv")

sample_subm = read.csv("../Kaggle-Data/tau/sample_submission.csv")

train = read.csv(train_file)
test = read.csv(test_file)
check_cor = read.csv(check_cor_file)
check_agr = read.csv(check_agreement_file)

names(train)
names(check_cor)
names(test)
