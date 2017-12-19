library(dplyr)
packages_needed = c('Boruta')
if (length(setdiff(packages_needed, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages_needed, rownames(installed.packages())))  
}
library(Boruta)
source("house_prices/fnshouse_prices.R")

# Setting Data
DATA.DIR = "../Data/kaggle/house_prices"
train_raw <- read.csv(file.path(DATA.DIR,"train.csv"),stringsAsFactors = FALSE)
test_raw <- read.csv(file.path(DATA.DIR,"test.csv"), stringsAsFactors = FALSE)

# Only continuous variables for evaluation


names(train_raw_numeric)
?Boruta
