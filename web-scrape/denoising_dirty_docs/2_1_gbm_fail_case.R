# libraries
library("dplyr")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(png, raster, data.table, gbm)

img2vec = . %>% matrix(nrow(.) * ncol(.), 1)

# Load prediction from gbm case

# here's a sample image that doesn't perform as well
img = readPNG("../Data/Kaggle/denoising_dirty/train/3.png")
x = data.table(matrix(img, nrow(img) * ncol(img), 1), kmeansThreshold(img))
setnames(x, c("raw", "thresholded"))
yHat = predict(gbm.mod, newdata=x)
imgOut = matrix(yHat, nrow(img), ncol(img))
plot(raster(imgOut))
