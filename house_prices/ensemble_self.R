# library(knitr)

library('Boruta')
library('mice')


DATA.DIR = "../Data/kaggle/house_prices"
train.raw <- read.csv(file.path(DATA.DIR,"train.csv"),stringsAsFactors = FALSE)
test.raw <- read.csv(file.path(DATA.DIR,"test.csv"), stringsAsFactors = FALSE)

#Impute missing data with MICE



#Boruta analysis for feature selection
