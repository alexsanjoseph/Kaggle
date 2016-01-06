dirtyFolder = "../Data/Kaggle/denoising_dirty/train"
cleanFolder = "../Data/Kaggle/denoising_dirty/train_cleaned"
outFolder = "../Data/Kaggle/denoising_dirty/train_predicted"

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
