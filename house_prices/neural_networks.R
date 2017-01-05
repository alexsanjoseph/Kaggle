library(dplyr)
packages_needed = c('neuralnet')
if (length(setdiff(packages_needed, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages_needed, rownames(installed.packages())))  
}
source("house_prices/fnshouse_prices.R")

# Setting Data
DATA.DIR = "../Data/kaggle/house_prices"
train_raw <- read.csv(file.path(DATA.DIR,"train.csv"),stringsAsFactors = FALSE)
# test_raw <- read.csv(file.path(DATA.DIR,"test.csv"), stringsAsFactors = FALSE)

# Only continuous variables for evaluation
train_raw_numeric = train_raw[sapply(train_raw, is.numeric)]

# Imputing the missing variables.
train_raw_numeric_miss <- missForest::missForest(train_raw_numeric)
train_full = train_raw_numeric_miss$ximp

# Creating train and cross validation datasets
index <- sample(1:nrow(train_full),round(0.8*nrow(train_full)))
train_data <- train_full[index,]
cv_data <- train_full[-index,]

# Testing with LM
lm.fit <- glm(SalePrice ~ ., data = train_data)
summary(lm.fit)
pr.lm <- predict(lm.fit, cv_data)
MSE.lm <- sum((pr.lm - cv_data$SalePrice)^2)/nrow(cv_data)

# Normalizing using min, max
maxs <- apply(train_full, 2, max) 
mins <- apply(train_full, 2, min)

scaled <- as.data.frame(scale(train_full, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

# Implementing neural networks
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("SalePrice ~", paste(n[!n %in% "SalePrice"], collapse = " + ")))
nn <- neuralnet(f, data = train_,hidden = c(15,5), linear.output=T)

plot(nn)

# Predicting value

pr.nn <- compute(nn,test_[,1:(ncol(test_)-1)])
pr.nn_ <- pr.nn$net.result*(max(train_full$SalePrice)-min(train_full$SalePrice))+min(train_full$SalePrice)
test.r <- (test_$SalePrice)*(max(train_full$SalePrice)-min(train_full$SalePrice))+min(train_full$SalePrice)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

print(paste(MSE.lm,MSE.nn))
