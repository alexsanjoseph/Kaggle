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

Image2Mat = . %>% matrix(., nrow(.), ncol(.)) %>% t
img2vec = . %>% matrix(nrow(.) * ncol(.), 1)

# a function to do k-means thresholding
kmeansThreshold = function(img)
{
  # fit 3 clusters
  v = img2vec(img)
  km.mod = kmeans(v, 3)
  # allow for the random ordering of the clusters
  oc = order(km.mod$centers)
  # the higher threshold is the halfway point between the top of the middle cluster and the bottom of the highest cluster
  hiThresh = 0.5 * (max(v[km.mod$cluster == oc[2]]) + min(v[km.mod$cluster == oc[3]]))
  
  # using upper threshold
  imgHi = v
  imgHi[imgHi <= hiThresh] = 0 
  imgHi[imgHi > hiThresh] = 1
  
  return (imgHi)
}



# a function that applies adaptive thresholding
adaptiveThresholding = function(img)
{
  img.eb <- Image(t(img))
  img.thresholded.3 = thresh(img.eb, 3, 3)
  img.thresholded.5 = thresh(img.eb, 5, 5)
  img.thresholded.7 = thresh(img.eb, 7, 7)
  img.thresholded.9 = thresh(img.eb, 9, 9)
  img.thresholded.11 = thresh(img.eb, 11, 11)
  img.kmThresh = kmeansThreshold(img)
  
  # combine the adaptive thresholding
  ttt.1 = cbind(img2vec(Image2Mat(img.thresholded.3)), img2vec(Image2Mat(img.thresholded.5)), img2vec(Image2Mat(img.thresholded.7)), img2vec(Image2Mat(img.thresholded.9)), img2vec(Image2Mat(img.thresholded.11)), img2vec(kmeansThreshold(img)))
  ttt.2 = apply(ttt.1, 1, max)
  ttt.3 = matrix(ttt.2, nrow(img), ncol(img))
  return (ttt.3)
}

dirtyFolder = "../Data/Kaggle/denoising_dirty/train"
cleanFolder = "../Data/Kaggle/denoising_dirty/train_cleaned"
outFolder = "../Data/Kaggle/denoising_dirty/train_predicted"

outPath = file.path(outFolder, "trainingdata.csv")
filenames = list.files(dirtyFolder)
unlink(outPath)

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