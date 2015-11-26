#Rossmann competition benchmark
#Libraries, directories
using BinDeps
using DataFrames
using Dates
using XGBoost

#Load data; If you want to run this script locally, you might want to change the following 4 lines.
train = readtable("../input/train.csv")
test = readtable("../input/test.csv")
store = readtable("../input/store.csv")
sample_submission = readtable("../input/sample_submission.csv")

#Reduce the training set to ignore the closed stores
train = train[train[:Open] .== 1, :]

#Tables inner join
train = join(train, store, on = :Store, kind = :inner)
test = join(test, store, on = :Store, kind = :inner)

#Test Ids & closed stores indexes 
tesdIdx = test[:Id]
closedStoreIdx = find(test[:Open] .== 0)

#Remove NAs from competition distance data
train[isna(train[:CompetitionDistance]), :CompetitionDistance] = 20000
test[isna(test[:CompetitionDistance]), :CompetitionDistance] = 20000
#accumulatedNAInfo = colwise(isna, train)
#numberOfNAsPerColumn = map((x) -> sum(x) / length(x), accumulatedNAInfo)

#Date transformation to "Date" object
train[:Date] = Date(train[:Date], "y-m-d")
test[:Date] = Date(test[:Date], "y-m-d")

#Make new date related features
train[:year] = year(train[:Date])
train[:month] = month(train[:Date])
train[:day] = day(train[:Date])
test[:year] = year(test[:Date])
test[:month] = month(test[:Date])
test[:day] = day(test[:Date])

#Define Categorical and Numeric Columns
colummTypes = map(x -> string(x), eltypes(train))
categoricalColumns = names(train)[colummTypes .== "UTF8String"]
categoricalColumns = categoricalColumns[1:3] #Exclude promo interval
numericalColumns = [:DayOfWeek, :Promo, :SchoolHoliday, :CompetitionDistance, :Promo2,
                    :year, :month, :day]

#Helper Function
function categorical2frequency(dataVector, vectorDict)
  #This functions counts the occurences of a category and maps it back to the data provided
  newVector = [vectorDict[dataPoint] for dataPoint in dataVector]
  return newVector
end

#Categorical Columns to Frequency (Counts)
for singleCol in categoricalColumns
  #singleCol = categoricalColumns[1]
  #countmap is similar to the function "table()" in R, the only difference is that countmap() returns a dictionary
  vectorDict = countmap(train[singleCol])
  #Apply counts to factors
  train[singleCol] = categorical2frequency(train[singleCol], vectorDict)
  test[singleCol] = categorical2frequency(test[singleCol], vectorDict)
  #Print progress
  print(string(singleCol) * " Column Processed")
end

#Define target
costTrainingLog = convert(Array{Float32}, train[:Sales])

#Transform data to XGBoost matrices
trainArray = convert(Array{Float32},  train[:, vcat(numericalColumns, categoricalColumns)])
testArray = convert(Array{Float32}, test[:, vcat(numericalColumns, categoricalColumns)])
dtrain = DMatrix(trainArray, label = costTrainingLog)
dtest = DMatrix(testArray)

#XGBoost training
num_round = 250
param = ["eta" => 0.2, "max_depth" => 20, "objective" => "reg:linear",
         "silent" => 1]

XGBoostModel = xgboost(dtrain, num_round, param = param)

#Predictions using test data
preds = predict(XGBoostModel, dtest)
#Round to zero closed stores
preds[closedStoreIdx] = 0

#Write Results
sampleSubmission = DataFrame(Id = tesdIdx, Sales = preds)
writetable("juliaBenchmark.csv", sampleSubmission)
                