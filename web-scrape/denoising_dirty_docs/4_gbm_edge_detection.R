# libraries
library("dplyr")
library(devtools)

#install necessary ubuntu libraries
# sudo apt-get install fftw3 fftw3-dev pkg-config
# install_url("http://cran.r-project.org/src/contrib/Archive/biOps/biOps_0.2.2.tar.gz")


if (!require("pacman")) install.packages("pacman")
pacman::p_load(png, raster, data.table, gbm, foreach, doSNOW)

if (!require("EBImage"))
{
  source("http://bioconductor.org/biocLite.R")
  biocLite("EBImage")
}
library(EBImage)
library(biOps)

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
  
  dat = data.table(cbind(y, x, x2, x3, x4, x5, x6))
  setnames(dat,c("y", "raw", "thresholded", "adaptive", "canny", "cannyDilated1", "cannyDilated2"))
  write.table(dat, file=outPath, append=(f != filenames[1]), sep=",", row.names=FALSE, col.names=(f == filenames[1]), quote=FALSE)
}

# read in the full data table
dat = read.csv(outPath)

# fit a model to a subset of the data
set.seed(1)
rows = sample(nrow(dat), 100000)
gbm.mod = gbm(y ~ raw + thresholded + adaptive + canny + cannyDilated1 + cannyDilated2, data = dat[rows,], n.trees = 1000, train.fraction = 0.5, interaction.depth = 5)
best.iter <- gbm.perf(gbm.mod)

s = summary(gbm.mod)

# get the predictions - using parallel processing to save time
numCores = 1 #change the 6 to your number of CPU cores. or maybe lower due to RAM limits
cl = makeCluster(numCores)
registerDoSNOW(cl)
num_splits = numCores
split_testing = sort(rank(1:nrow(dat))%%numCores)
yHat = foreach(i=unique(split_testing),.combine=c,.packages=c("gbm")) %dopar% {
  as.numeric(predict(gbm.mod, newdata=dat[split_testing==i,], n.trees = best.iter))
}
stopCluster(cl)
yHat[yHat < 0] = 0
yHat[yHat > 1] = 1
# what score do we get on the training data?
rmse = sqrt(mean( (yHat - dat$y) ^ 2 ))
print(rmse) # 4.1%

# show the predicted result for a sample image
img = readPNG("../Data/Kaggle/denoising_dirty/train/3.png")
x = data.table(matrix(img, nrow(img) * ncol(img), 1), kmeansThreshold(img), img2vec(adaptiveThresholding(img)), img2vec(cannyEdges(img)), img2vec(cannyDilated1(img)), img2vec(cannyDilated2(img)))
setnames(x, c("raw", "thresholded", "adaptive", "canny", "cannyDilated1", "cannyDilated2"))
yHatImg = predict(gbm.mod, newdata=x, n.trees = best.iter)
yHatImg[yHatImg < 0] = 0
yHatImg[yHatImg > 1] = 1
imgOut = matrix(yHatImg, nrow(img), ncol(img))
writePNG(imgOut, "../Data/Kaggle/denoising_dirty/data/sample.png")
plot(raster(imgOut))
