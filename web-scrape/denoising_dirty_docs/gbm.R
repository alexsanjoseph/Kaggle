# libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(png, raster, data.table, gbm)

# a function to do k-means thresholding
kmeansThreshold = function(img)
{
  browser()
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

dirtyFolder = "../Data/Kaggle/denoising_dirty/train"
cleanFolder = "../Data/Kaggle/denoising_dirty/train_cleaned"
outFolder = "../Data/Kaggle/denoising_dirty/train_predicted"

outPath = file.path(outFolder, "trainingdata.csv")
filenames = list.files(dirtyFolder)
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
  x2 = matrix(x2, nrow(x2) * ncol(x2), 1)
  
  dat = data.table(cbind(y, x, x2))
  setnames(dat,c("y", "raw", "thresholded"))
  write.table(dat, file=outPath, append=(f != filenames[1]), sep=",", row.names=FALSE, col.names=(f == filenames[1]), quote=FALSE)
}

# view the data
dat = read.csv(outPath)
rows = sample(nrow(dat), 10000)
d1 = dat[rows,]
plot(d1$raw[dat$thresholded == 0], d1$y[dat$thresholded == 0], col = "blue")
lines(d1$raw[dat$thresholded == 1], d1$y[dat$thresholded == 1], col = "red", type="p")

# fit a model to a subset of the data
rows = sample(nrow(dat), 100000)
gbm.mod = gbm(y ~ raw + thresholded, data = dat[rows,], n.trees = 5000, cv.folds = 10, train.fraction = 0.5)
best.iter <- gbm.perf(gbm.mod,method="cv")

# what score do we get on the training data?
yHat = predict(gbm.mod, newdata=dat, n.trees = best.iter)
rmse = sqrt(mean( (yHat - dat$y) ^ 2 ))
print(rmse)

# show the predicted result for a sample image
img = readPNG("C:\\Users\\Colin\\Kaggle\\Denoising Dirty Documents\\data\\train\\6.png")
x = data.table(matrix(img, nrow(img) * ncol(img), 1), kmeansThreshold(img))
setnames(x, c("raw", "thresholded"))
yHat = predict(gbm.mod, newdata=x, n.trees = best.iter)
imgOut = matrix(yHat, nrow(img), ncol(img))
writePNG(imgOut, "C:\\Users\\Colin\\Kaggle\\Denoising Dirty Documents\\data\\sample.png")
plot(raster(imgOut))