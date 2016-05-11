import pandas as pd

destinations = pd.read_csv("../../Data/kaggle/expedia/destinations.csv")
test = pd.read_csv("../../Data/kaggle/expedia/test.csv")
train = pd.read_csv("../../Data/kaggle/expedia/train.csv", chunksize = None)

train.shape
train["hotel_cluster"].value_counts()

test_ids = set(test.user_id.unique())
train_ids = set(train.user_id.unique())
intersection_count = len(test_ids & train_ids)
intersection_count == len(test_ids)[]

train["date_time"] = pd.to_datetime(train["date_time"])
train["year"] = train["date_time"].dt.year
train["month"] = train["date_time"].dt.month

import random

unique_users = train.user_id.unique()

sel_user_ids = [unique_users[i] for i in sorted(random.sample(range(len(unique_users)), 10000)) ]
sel_train = train[train.user_id.isin(sel_user_ids)]

t1 = sel_train[((sel_train.year == 2013) | ((sel_train.year == 2014) & (sel_train.month < 8)))]
t2 = sel_train[((sel_train.year == 2014) & (sel_train.month >= 8))]

t2 = t2[t2.is_booking == True]

most_common_clusters = list(train.hotel_cluster.value_counts().head().index)

predictions = [most_common_clusters for i in range(t2.shape[0])]

import ml_metrics as metrics

target = [[l] for l in t2["hotel_cluster"]]
metrics.mapk(target, predictions, k=5)
