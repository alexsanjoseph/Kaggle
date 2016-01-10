# libraries
library("dplyr")

#install necessary ubuntu libraries
# sudo apt-get install fftw3 fftw3-dev pkg-config
# sudo apt-get install libtiff5-dev              

if (!require("pacman")) install.packages("pacman")
pacman::p_load(png, raster, data.table, gbm, foreach, doSNOW)

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
f = filenames[1]

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
  
  dat = data.table(cbind(y, x, x2, x3))
  setnames(dat,c("y", "raw", "thresholded", "adaptive"))
  write.table(dat, file=outPath, append=(f != filenames[1]), sep=",", row.names=FALSE, col.names=(f == filenames[1]), quote=FALSE)
}

# read in the full data table
dat = read.csv(outPath)

# fit a model to a subset of the data
set.seed(1)
rows = sample(nrow(dat), 1000000)
gbm.mod = gbm(y ~ raw + thresholded + adaptive, data = dat[rows,], n.trees = 1500, cv.folds = 3, train.fraction = 0.5, interaction.depth = 5)
best.iter <- gbm.perf(gbm.mod,method="cv",oobag.curve = FALSE)

s = summary(gbm.mod)

# get the predictions - using parallel processing to save time
numCores = 6 #change the 6 to your number of CPU cores. or maybe lower due to RAM limits
cl = makeCluster(numCores)
registerDoSNOW(cl)
num_splits = numCores
split_testing = sort(rank(1:nrow(dat))%%numCores)
yHat = foreach(i=unique(split_testing),.combine=c,.packages=c("gbm")) %dopar% {
  as.numeric(predict(gbm.mod, newdata=dat[split_testing==i,], n.trees = best.iter))
}
stopCluster(cl)

# what score do we get on the training data?
rmse = sqrt(mean( (yHat - dat$y) ^ 2 ))
print(rmse)

# show the predicted result for a sample image
img = readPNG("../Data/Kaggle/denoising_dirty/train/3.png")
x = data.table(matrix(img, nrow(img) * ncol(img), 1), kmeansThreshold(img), img2vec(adaptiveThresholding(img)))
setnames(x, c("raw", "thresholded", "adaptive"))
yHat = predict(gbm.mod, newdata=x, n.trees = best.iter)
imgOut = matrix(yHat, nrow(img), ncol(img))
writePNG(imgOut, "../Data/Kaggle/denoising_dirty/data/sample.png")
plot(raster(imgOut))