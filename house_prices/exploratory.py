import pandas as pd
import matplotlib.pyplot as plt
#import numpy as np
from sklearn import datasets, linear_model, model_selection
from sklearn.model_selection import train_test_split#, cross_val_predict
import scipy.stats as stats
from ggplot import *
import seaborn as sns
# import xgboost as xgb

test_data_final  = pd.read_csv("~/Learning/Data/kaggle/house_prices/test.csv")
test_sub   = pd.read_csv("~/Learning/Data/kaggle/house_prices/sample_submission.csv")

train_data = pd.read_csv("~/Learning/Data/kaggle/house_prices/train.csv")

#train_data.columns.values


# Exploratory
current_column = 'LotArea'
ggplot(train_data, aes(x = current_column, y = 'SalePrice')) + geom_point()

numerics = ['int16', 'int32', 'int64', 'float16', 'float32', 'float64']
train_data_numerics = train_data.select_dtypes(include=numerics).sort_values("SalePrice", ascending = False)


current_column = 'YearRemodAdd'

plot_df = train_data[[current_column, 'SalePrice']]

ggplot(plot_df, aes(x = current_column, y = 'SalePrice')) + geom_point()




# Modeling
numerics = ['int16', 'int32', 'int64', 'float16', 'float32', 'float64']
train_data = train_data.select_dtypes(include=numerics)

train_data = train_data.select_dtypes(include=numerics)
train_data.fillna(train_data.mean(), inplace = True)

train_data, test_data = train_test_split(train_data, test_size = 0.2)
train_data_X = train_data.drop('SalePrice', axis = 1)
train_data_Y = train_data[['SalePrice']]

test_data_X = test_data.drop('SalePrice', axis = 1)
test_data_Y = test_data[['SalePrice']]

regr = linear_model.LinearRegression()
regr.fit(train_data_X, train_data_Y)

print('Coefficients: \n', regr.coef_)

print("Mean squared error: %.2f" % np.mean((regr.predict(test_data_X) - test_data_Y) ** 2))
# Explained variance score: 1 is perfect prediction
print('Variance score: %.2f' % regr.score(test_data_X, test_data_Y))

# Plot outputs
plot_df = pd.concat([test_data[['SalePrice']], pd.DataFrame({'test': regr.predict(test_data_X)})


##### Submission
test_data_final = test_data_final.select_dtypes(include=numerics)
test_data_final.fillna(test_data_final.mean(), inplace = True)

final_submission = cross_val_predict(regr, train_data_X, train_data_Y, cv=10)
final_submission = regr.predict(test_data_final).transpose()
sub_output = pd.concat([test_data_final[['Id']], pd.DataFrame(final_submission[0], columns=['SalePrice'])], axis = 1)

sub_output[sub_output < 0] = 0
sub_output.to_csv("../../Data/kaggle/house_prices/sub2.csv", index = False)

min(sub_output[['SalePrice']])

sub_output.SalePrice
final_submission.min()
