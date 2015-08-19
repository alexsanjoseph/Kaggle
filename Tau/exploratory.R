
source("bootstrap.R")
library(xgboost)
source("Tau/evaluation.R")
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
names(check_agr)
names(test)

setdiff(names(train), names(test))

train_ratio = 0.9
train_indices = sample(1:nrow(train), floor(train_ratio * nrow(train)))

traindata = train[train_indices,]
cv_data = train[-train_indices]

bst <- xgboost(data = traindata %>% select(one_of(names(test))[1:40]) %>% as.matrix, label = traindata$signal, max.depth = 2, eta = 1, nround = 2,
               nthread = 2, objective = "binary:logistic")

bst <- train(x = traindata %>% select(one_of(names(test))[1:40]), y = traindata$signal)
?lm
## check cvm
cvm_pred = predict(bst, check_cor %>% as.matrix)
compute_cvm(cvm_pred, check_cor$mass, n_neighbours = 200,step = 50)



## Check test
pred = predict(bst, test %>% as.matrix)
submission = data.frame(id = test$id, prediction = pred)
head(submission)
write.csv(submission, "../Kaggle-Data/tau/submission1.csv", row.names = F)










pred %>% head
cv_data$signal %>% head(10000)

traindata$

data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train1 <- agaricus.train
test1 <- agaricus.test
bst <- xgboost(data = train$data, label = train$label, max.depth = 2,
               eta = 1, nthread = 2, nround = 2, objective = "binary:logistic")
pred <- predict(bst, test$data)

