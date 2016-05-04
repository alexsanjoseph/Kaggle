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
