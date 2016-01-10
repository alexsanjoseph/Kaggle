# libraries
library(data.table)
pacman::p_load(png, raster)

img = readPNG("../Data/Kaggle/denoising_dirty/train/6.png")

# libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(png, raster)

# turn the image into a vector
img2vec = function(img)
{
  return (matrix(img, nrow(img) * ncol(img), 1))
}

# show a histogram
hist(img2vec(img))

# fit 3 clusters
v = img2vec(img)
km.mod = kmeans(v, 3)

# allow for the random ordering of the clusters
oc = order(km.mod$centers)
# the lower threshold is the halfway point between the top of the lowest cluster and the bottom of the middle cluster
loThresh = 0.5 * (max(v[km.mod$cluster == oc[1]]) + min(v[km.mod$cluster == oc[2]]))
# the higher threshold is the halfway point between the top of the middle cluster and the bottom of the highest cluster
hiThresh = 0.5 * (max(v[km.mod$cluster == oc[2]]) + min(v[km.mod$cluster == oc[3]]))

# using lower threshold
imgLo = img
imgLo[imgLo <= loThresh] = 0 
imgLo[imgLo > loThresh] = 1
plot(raster(imgLo))

# using upper threshold
imgHi = img
imgHi[imgHi <= hiThresh] = 0
imgHi[imgHi > hiThresh] = 1
plot(raster(imgHi))
