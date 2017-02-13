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

# Doing feature selection
boruta_output = Boruta::Boruta(SalePrice ~ ., data = train_full, doTrace = 2)
final.boruta <- TentativeRoughFix(boruta_output)
select_columns = getSelectedAttributes(final.boruta, withTentative = F)
train_full = train_full %>% select_(.dots = select_columns, "SalePrice")

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
nn <- neuralnet(f, data = train_,hidden = c(), linear.output=T)

plot(nn)

# Predicting value

pr.nn <- compute(nn,test_[,1:(ncol(test_)-1)])
pr.nn_ <- pr.nn$net.result*(max(train_full$SalePrice)-min(train_full$SalePrice))+min(train_full$SalePrice)
test.r <- (test_$SalePrice)*(max(train_full$SalePrice)-min(train_full$SalePrice))+min(train_full$SalePrice)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

sqrt(c(MSE.lm,MSE.nn))

# Plotting the values
par(mfrow=c(1,2))

plot(cv_data$SalePrice,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(cv_data$SalePrice,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

# Cross Validation
library(boot)
set.seed(200)
lm.fit <- glm(SalePrice~.,data=train_full)
cv.glm(train_full,lm.fit,K=10)$delta[1] / 1e9


