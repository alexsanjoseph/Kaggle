# libraries
library(data.table)
if (!require("pacman")) install.packages("pacman")

pacman::p_load(png, raster)

# img = readPNG("../Data/Kaggle/denoising_dirty/train/6.png")

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
  
  dat = data.table(cbind(y, x))
  setnames(dat,c("y", "x"))
  write.table(dat, file=outPath, append=(f != filenames[1]), sep=",", row.names=FALSE, col.names=(f == filenames[1]), quote=FALSE)
}

# view the data
dat = read.csv(outPath)
head(dat)
rows = sample(nrow(dat), 10000)
plot(dat$x[rows], dat$y[rows])

# fit a linear model, ignoring the data points at the extremes
lm.mod.1 = lm(y ~ x, data=dat[dat$y > 0.05 & dat$y < 0.95,])

summary(lm.mod.1)
dat$predicted = sapply(predict(lm.mod.1, newdata=dat), function(x) max(min(x, 1),0))
plot(dat$predicted[rows], dat$y[rows])
rmse1 = sqrt(mean( (dat$y - dat$x) ^ 2))
rmse2 = sqrt(mean( (dat$predicted - dat$y) ^ 2))
c(rmse1, rmse2)


# show the predicted result for a sample image
img = readPNG("../Data/Kaggle/denoising_dirty/train/6.png")
x = data.table(matrix(img, nrow(img) * ncol(img), 1))
setnames(x, "x")
yHat = sapply(predict(lm.mod.1, newdata=x), function(x) max(min(x, 1),0))
imgOut = matrix(yHat, nrow(img), ncol(img))
plot(raster(imgOut))
