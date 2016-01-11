# libraries
library("dplyr")
library(devtools)

#install necessary ubuntu libraries
# sudo apt-get install fftw3 fftw3-dev pkg-config
# install_url("http://cran.r-project.org/src/contrib/Archive/biOps/biOps_0.2.2.tar.gz")


if (!require("pacman")) install.packages("pacman")
pacman::p_load(png, raster, data.table, gbm, foreach, doSNOW, biOps, xgboost, Ckmeans.1d.dp)

if (!require("EBImage"))
{
  source("http://bioconductor.org/biocLite.R")
  biocLite("EBImage")
}
library(EBImage)

source("web-scrape/denoising_dirty_docs/fns_denoising.R")
dirtyFolder = "../Data/Kaggle/denoising_dirty/train"
cleanFolder = "../Data/Kaggle/denoising_dirty/train_cleaned"
outFolder = "../Data/Kaggle/denoising_dirty/train_predicted"

outPath = file.path(outFolder, "trainingdata.csv")
filenames = list.files(dirtyFolder)
unlink(outPath)

for (f in filenames)
{
  print(f)
  imgX = readPNG(file.path(dirtyFolder, f))
  imgY = readPNG(file.path(cleanFolder, f))
  
  # turn the images into vectors
  x = matrix(imgX, nrow(imgX) * ncol(imgX), 1)
  y = matrix(imgY, nrow(imgY) * ncol(imgY), 1)
  
  # threshold the image
  x2 = kmeansThreshold(imgX)
  
  # adaptive thresholding
  x3 = img2vec(adaptiveThresholding(imgX))
  
  # canny edge detector and related features
  x4 = img2vec(cannyEdges(imgX))
  x5 = img2vec(cannyDilated1(imgX))
  x6 = img2vec(cannyDilated2(imgX))
  
  # median filter and related features
  x7 = img2vec(median_Filter(imgX, 5))
  x8 = img2vec(background_Removal(imgX))
  
  dat = data.table(cbind(y, x, x2, x3, x4, x5, x6, x7, x8))
  setnames(dat,c("y", "raw", "thresholded", "adaptive", "canny", "cannyDilated1", "cannyDilated2", "median17", "backgroundRemoval"))
  write.table(dat, file=outPath, append=(f != filenames[1]), sep=",", row.names=FALSE, col.names=(f == filenames[1]), quote=FALSE)
}

# read in the full data table
dat = read.csv(outPath)

# fit an xgboost model to a subset of the data
set.seed(1)
rows = sample(nrow(dat), 2000000)
dat[is.na(dat)] = 0
dtrain <- xgb.DMatrix(as.matrix(dat[rows,-1]), label = as.matrix(dat[rows,1]))
# do cross validation first
xgb.tab = xgb.cv(data = dtrain, nthread = 8, eval_metric = "rmse", nrounds = 10000, early.stop.round = 50, nfold = 5, print.every.n = 10)
# what is the best number of rounds?
min.error.idx = which.min(xgb.tab[, test.rmse.mean]) 
# now fit an xgboost model 
xgb.mod = xgboost(data = dtrain, nthread = 8, eval_metric = "rmse", nrounds = min.error.idx, print.every.n = 10)

# get the predictions
dtrainFull <- xgb.DMatrix(as.matrix(dat[,-1]), label = as.matrix(dat[,1]))
yHat = predict(xgb.mod, newdata=dtrainFull)
# what score do we get on the training data?
rmse = sqrt(mean( (yHat - dat$y) ^ 2 ))
print(rmse) # 2.4% vs 4.1%

# get the trained model
model = xgb.dump(xgb.mod, with.stats=TRUE)
# get the feature real names
names = names(dat)[-1]
# compute feature importance matrix
importance_matrix = xgb.importance(names, model=xgb.mod)
# plot the variable importance
gp = xgb.plot.importance(importance_matrix)
print(gp)


# show the predicted result for a sample image
img = readPNG("C:\\Users\\Colin\\dropbox\\Kaggle\\Denoising Dirty Documents\\data\\train\\3.png")
x = data.table(matrix(img, nrow(img) * ncol(img), 1), kmeansThreshold(img), img2vec(adaptiveThresholding(img)), img2vec(cannyEdges(img)), 
               img2vec(cannyDilated1(img)), img2vec(cannyDilated2(img)),img2vec(median_Filter(img, 17)), img2vec(background_Removal(img)) )
setnames(x, c("raw", "thresholded", "adaptive", "canny", "cannyDilated1", "cannyDilated2", "median17", "backgroundRemoval"))
yHatImg = predict(xgb.mod, newdata=as.matrix(x))
yHatImg[yHatImg < 0] = 0 yHatImg[yHatImg > 1] = 1
imgOut = matrix(yHatImg, nrow(img), ncol(img))
writePNG(imgOut, "C:\\Users\\Colin\\dropbox\\Kaggle\\Denoising Dirty Documents\\data\\sample.png")
plot(raster(imgOut))