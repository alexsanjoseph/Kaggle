# libraries

if (!require("pacman")) install.packages("pacman")

pacman::p_load(png, raster)

img = readPNG("../Data/Kaggle/denoising_dirty/train/6.png")
head(img)
plot(raster(img))
