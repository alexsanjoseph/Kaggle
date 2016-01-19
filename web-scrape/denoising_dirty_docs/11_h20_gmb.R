
# libraries
library("dplyr")

install.packages("h2o")
library(h2o)
library(png)
library(data.table)

# a function to turn a matrix image into a vector


source("web-scrape/denoising_dirty_docs/fns_denoising.R")
dirtyFolder = "../Data/Kaggle/denoising_dirty/train"
cleanFolder = "../Data/Kaggle/denoising_dirty/train_cleaned"
outFolder = "../Data/Kaggle/denoising_dirty/train_predicted"

outPath = file.path(outFolder, "trainingdata.csv")
filenames = list.files(dirtyFolder)
unlink(outPath)
foregroundFolder = "./foreground/train foreground"

outPath = file.path(outFolder, "trainingdata.csv")
outPath2 = file.path(outFolder, "testdata.csv")
filenames = list.files(dirtyFolder)
padding = 2
set.seed(1)

h2oServer = h2o.init(nthreads = 15, max_mem_size = "110G")

trainData = h2o.importFile(h2oServer, path = outPath)
testData = h2o.importFile(h2oServer, path = outPath2)

model.dl.median <- h2o.deeplearning(x = 2:ncol(trainData), y = 1, training_frame = trainData, validation_frame = testData,
                                    score_training_samples = 0,
                                    overwrite_with_best_model = TRUE,
                                    activation = "Rectifier", seed = 1,
                                    hidden = c(200, 200,200), epochs = 15,
                                    adaptive_rate = TRUE, initial_weight_distribution = "UniformAdaptive", loss = "MeanSquare",
                                    fast_mode = T, diagnostics = T, ignore_const_cols = T,
                                    force_load_balance = T)

summary(model.dl)

modelPath = h2o.saveModel(model.dl.median, dir = "./model", name = "model_dnn_median", force = TRUE)

outFolder = "./model/training data"

img2tab = function(imgX, f)
{
  median5 = img2vec(median_Filter(imgX, 5))
  median17 = img2vec(median_Filter(imgX, 17))
  median25 = img2vec(median_Filter(imgX, 25))
  backgroundRemoval = img2vec(background_Removal(imgX))
  foreground = readPNG(file.path(foregroundFolder, f))
  
  # pad out imgX
  padded = matrix(0, nrow(imgX) + padding * 2, ncol(imgX) + padding * 2)
  offsets = expand.grid(seq_len(2*padding+1), seq_len(2*padding+1))
  
  # raw pixels window
  padded[padding + seq_len(nrow(imgX)), padding + seq_len(ncol(imgX))] = imgX
  x = sapply(seq_len((2*padding+1)^2), function(x) img2vec(padded[offsets[x, 2] - 1 + seq_len(nrow(imgX)), offsets[x, 1] - 1 + seq_len(ncol(imgX))]))
  
  # x2 window
  padded[padding + seq_len(nrow(imgX)), padding + seq_len(ncol(imgX))] = median5
  x2 = sapply(seq_len((2*padding+1)^2), function(x) img2vec(padded[offsets[x, 2] - 1 + seq_len(nrow(imgX)), offsets[x, 1] - 1 + seq_len(ncol(imgX))]))
  
  # x3 window
  padded[padding + seq_len(nrow(imgX)), padding + seq_len(ncol(imgX))] = median17
  x3 = sapply(seq_len((2*padding+1)^2), function(x) img2vec(padded[offsets[x, 2] - 1 + seq_len(nrow(imgX)), offsets[x, 1] - 1 + seq_len(ncol(imgX))]))
  
  # x4 window
  padded[padding + seq_len(nrow(imgX)), padding + seq_len(ncol(imgX))] = median25
  x4 = sapply(seq_len((2*padding+1)^2), function(x) img2vec(padded[offsets[x, 2] - 1 + seq_len(nrow(imgX)), offsets[x, 1] - 1 + seq_len(ncol(imgX))]))
  
  # x5 window
  padded[padding + seq_len(nrow(imgX)), padding + seq_len(ncol(imgX))] = backgroundRemoval
  x5 = sapply(seq_len((2*padding+1)^2), function(x) img2vec(padded[offsets[x, 2] - 1 + seq_len(nrow(imgX)), offsets[x, 1] - 1 + seq_len(ncol(imgX))]))
  
  # x6 window
  padded[padding + seq_len(nrow(imgX)), padding + seq_len(ncol(imgX))] = foreground
  x6 = sapply(seq_len((2*padding+1)^2), function(x) img2vec(padded[offsets[x, 2] - 1 + seq_len(nrow(imgX)), offsets[x, 1] - 1 + seq_len(ncol(imgX))]))
  
  dat = data.table(cbind(x, x2, x3, x4, x5, x6))
  setnames(dat,c(
    paste("x", seq_len((2*padding+1)^2), sep=""),
    paste("median5", seq_len((2*padding+1)^2), sep=""),
    paste("median17", seq_len((2*padding+1)^2), sep=""),
    paste("median25", seq_len((2*padding+1)^2), sep=""),
    paste("backgroundRemoval", seq_len((2*padding+1)^2), sep=""),
    paste("foreground", seq_len((2*padding+1)^2), sep="")
  ))
  
  return (dat)
}

dirtyFolder = "./data/test"
outFolder = "./model/test data"
foregroundFolder = "./foreground/test foreground"
filenames = list.files(dirtyFolder)
for (f in filenames)
{
  print(f)
  imgX = readPNG(file.path(dirtyFolder, f))
  
  dat = img2tab(imgX, f)
  
  x.h2o = as.h2o(h2oServer, dat)
  predict.dl = as.data.frame(h2o.predict(model.dl.median, newdata = x.h2o))
  imgOut = matrix(as.numeric(predict.dl$predict), nrow(imgX), ncol(imgX))
  
  # correct the pixel brightnesses that are out of bounds
  imgOut[imgOut > 1] = 1
  imgOut[imgOut < 0] = 0
  
  writePNG(imgOut, file.path(outFolder, f))
}

h2o.shutdown()