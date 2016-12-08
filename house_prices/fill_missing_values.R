library(dplyr)
packages_needed = c('mice', 'Amelia', 'missForest', 'VIM', 'usdm', 'mi')
if (length(setdiff(packages_needed, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages_needed, rownames(installed.packages())))  
}
library(mice)
source("house_prices/fnshouse_prices.R")

# Setting Data
DATA.DIR = "../Data/kaggle/house_prices"
train_raw <- read.csv(file.path(DATA.DIR,"train.csv"),stringsAsFactors = FALSE)
test_raw <- read.csv(file.path(DATA.DIR,"test.csv"), stringsAsFactors = FALSE)

# Only continuous variables for evaluation
train_raw_numeric = train_raw[sapply(train_raw, is.numeric)]

# Finding collinear variables
Amelia::amelia(train_raw_numeric, m=5, parallel = "multicore")
train_raw_numeric = train_raw_numeric %>% select(-TotalBsmtSF, -GrLivArea)

impute_test = train_raw_numeric %>% na.omit()
missing_ratio = find_missing_ratio(train_raw_numeric)
impute_test_missing = create_missing_values(impute_test, missing_ratio)

# Naive zero
impute_test_imputed_naive_zero <- lapply(impute_test_missing, function(x){
  x[is.na(x)] = 0
  x
}) %>% data.frame()
find_impute_performance(impute_test_imputed_naive_zero, impute_test) %>% sum


# Naive means
impute_test_imputed_naive_mean <- lapply(impute_test_missing, function(x){
  x[is.na(x)] = mean(x, na.rm = T)
  x
}) %>% data.frame()
find_impute_performance(impute_test_imputed_naive_mean, impute_test) %>% sum

## Mice
impute_test_imputed_mice <- mice::mice(impute_test_missing, m = 5, maxit = 50, method = 'pmm', seed = 500)
sapply(1:5, function(i) find_impute_performance(complete(impute_test_imputed_mice,i), impute_test) %>% sum)

## Amelia
impute_test_imputed_amelia = Amelia::amelia(impute_test_missing, m=5, parallel = "multicore")
sapply(1:5, function(i) find_impute_performance(impute_test_imputed_amelia$imputations[[i]], impute_test) %>% sum)

## missForest
impute_test_imputed_missForest <- missForest::missForest(impute_test_missing)
find_impute_performance(impute_test_imputed_missForest$ximp, impute_test) %>% sum

## mi
impute_test_imputed_mi <- mi::mi(impute_test_missing)
find_impute_performance(impute_test_imputed_mi %>% data.frame, impute_test) %>% sum

A = impute_test_imputed_mi %>% as.data.frame
