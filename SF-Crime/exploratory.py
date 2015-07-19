import pandas as pd
import zipfile
import matplotlib.pyplot as pl

drive = '/home/alex/Learning/Kaggle-Data/SF-Crime/'
z = zipfile.ZipFile(drive+'train.csv.zip')
print(z.namelist())

train = pd.read_csv(z.open('train.csv'), parse_dates=['Dates'])

train['Year'] = train['Dates'].map(lambda x: x.year)
train['Week'] = train['Dates'].map(lambda x: x.week)
train['Hour'] = train['Dates'].map(lambda x: x.hour)

train
print(train.head())

