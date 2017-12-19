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

# Maybe do normalization?


## collinear
# train_raw_numeric$TotalBsmtSF[1] = 855
# A = lm(SalePrice ~ .,train_raw_numeric)
# summary(A)

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
  x[is.na(x)] = median(x, na.rm = T)
  x
}) %>% data.frame()
find_impute_performance(impute_test_imputed_naive_mean, impute_test) %>% sum

## Mice
impute_test_imputed_mice <- mice::mice(impute_test_missing, m = 15, maxit = 50, method = 'pmm', seed = 500)
impute_test_imputed_mice_df = (lapply(1:10, function(i) complete(impute_test_imputed_mice, i)) %>% Reduce('+', .))/10

find_impute_performance(impute_test_imputed_mice_df, impute_test) %>% sum

## Amelia
impute_test_imputed_amelia = Amelia::amelia(impute_test_missing, m=25, parallel = "multicore")
impute_test_imputed_amelia_df = (impute_test_imputed_amelia$imputations %>% Reduce('+', .))/25
find_impute_performance(impute_test_imputed_amelia_df, impute_test) %>% sum

## missForest
impute_test_imputed_missForest <- missForest::missForest(impute_test_missing, maxiter = 10, ntree = 500)
?missForest::missForest
impute_test_imputed_missForest_df = impute_test_imputed_missForest$ximp
find_impute_performance(impute_test_imputed_missForest_df, impute_test) %>% sum

## mi
impute_test_missing_mi <- mi::missing_data.frame(impute_test_missing)
impute_test_imputed_mi <- mi::mi(impute_test_missing_mi)
impute_test_imputed_mi = mi::complete(impute_test_imputed_mi, 5)
impute_test_imputed_mi_df =  (lapply(impute_test_imputed_mi, function(x)
  lapply(x, as.numeric) %>% data.frame() %>% vimana::round_num_cols(4)
  ) %>% Reduce('+', .) %>% select(-starts_with("missing_")))/5

find_impute_performance(impute_test_imputed_mi_df, impute_test) %>% sum

all_miss = list(zero = impute_test_imputed_naive_zero,
                       mean = impute_test_imputed_naive_mean,
                       mice = impute_test_imputed_mice_df,
                       amelia = impute_test_imputed_amelia_df,
                       missF = impute_test_imputed_missForest_df,
                       mi = impute_test_imputed_mi_df,
                       actual = impute_test)
all_miss$ensemble = (all_miss[4:6] %>% Reduce('+', .))/3
errors = sapply(all_miss, missForest::nrmse, impute_test_missing, impute_test) %>% round(4)

current_var = 'LotFrontage'
missing_indices = which(is.na(impute_test_missing[[current_var]]))
missing_data_df = lapply(all_miss, function(x) x[[current_var]][missing_indices]) %>% do.call(cbind, .) %>% data.frame
names(missing_data_df) = names(all_miss)

# missing_data_df = data.frame(zero = 0,
#                              mean = impute_test_imputed_naive_mean[[current_var]][missing_indices],
#                              mice = impute_test_imputed_mice_df[[current_var]][missing_indices],
#                              amelia = impute_test_imputed_amelia_df[[current_var]][missing_indices],
#                              missF = impute_test_imputed_missForest_df[[current_var]][missing_indices],
#                              mi = impute_test_imputed_mi_df[[current_var]][missing_indices],
#                              actual = impute_test[[current_var]][missing_indices]
#                              )
