import pandas as pd
import os

dir_name = "/home/alex/Learning/Kaggle-Data/tau/"
os.chdir(dir_name)
import evaluation

train_file = dir_name +  "training.csv"
test_file = dir_name + "test.csv"
check_agreement_file = dir_name + "check_agreement.csv"
check_cor_file = dir_name + "check_correlation.csv"

train = pd.read_csv(train_file)
check_agreement = pd.read_csv(check_agreement_file)
check_cor = pd.read_csv(check_cor_file)

print train.head(10)
print check_agreement.head(10)
print check_cor.head(10)
