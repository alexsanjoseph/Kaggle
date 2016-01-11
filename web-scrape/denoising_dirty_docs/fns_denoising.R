
# Initial 1-3 tutorials  

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


### Edge detection

# a function to do canny edge detector
cannyEdges = function(img)
{
  img.biOps = imagedata(img * 255)
  img.canny = imgCanny(img.biOps, 0.7)
  return (matrix(img.canny / 255, nrow(img), ncol(img)))
}

# a function combining canny edge detector with morphology
cannyDilated1 = function(img)
{
  img.biOps = imagedata(img * 255)
  img.canny = imgCanny(img.biOps, 0.7)
  # do some morphology on the edges to fill the gaps between them
  mat <- matrix (0, 3, 3)
  mask <- imagedata (mat, "grey", 3, 3)
  img.dilation = imgBinaryDilation(img.canny, mask)
  img.erosion = imgBinaryErosion(img.dilation, mask)
  return(matrix(img.erosion / 255, nrow(img), ncol(img)))
}

# a function combining canny edge detector with morphology
cannyDilated2 = function(img)
{
  img.biOps = imagedata(img * 255)
  img.canny = imgCanny(img.biOps, 0.7)
  # do some morphology on the edges to fill the gaps between them
  mat <- matrix (0, 3, 3)
  mask <- imagedata (mat, "grey", 3, 3)
  img.dilation = imgBinaryDilation(img.canny, mask)
  img.erosion = imgBinaryErosion(img.dilation, mask)
  img.erosion.2 = imgBinaryErosion(img.erosion, mask)
  img.dilation.2 = imgBinaryDilation(img.erosion.2, mask)
  return(matrix(img.dilation.2 / 255, nrow(img), ncol(img)))
}

##### 5. background removal
background_Removal = function(img)
{
  w = 5
  
  # the background is found via a median filter
  background = median_Filter(img, w)
  
  # the foreground is darker than the background
  foreground = img - background
  foreground[foreground > 0] = 0
  m1 = min(foreground)
  m2 = max(foreground)
  foreground = (foreground - m1) / (m2 - m1)
  
  return (matrix(foreground, nrow(img), ncol(img)))
}

median_Filter = function(img, filterWidth)
{
  pad = floor(filterWidth / 2)
  padded = matrix(NA, nrow(img) + 2 * pad, ncol(img) + 2 * pad)
  padded[pad + seq_len(nrow(img)), pad + seq_len(ncol(img))] = img
  
  tab = NULL
  for (i in seq_len(filterWidth))
  {
    for (j in seq_len(filterWidth))
    {
      if (i == 1 && j == 1)
      {
        tab = img2vec(padded[i - 1 + seq_len(nrow(img)), j - 1 + seq_len(ncol(img))])
      } else {
        tab = cbind(tab, img2vec(padded[i - 1 + seq_len(nrow(img)), j - 1 + seq_len(ncol(img))]))
      }
    }
  }
  
  filtered = unlist(apply(tab, 1, function(x) median(x[!is.na(x)])))
  return (matrix(filtered, nrow(img), ncol(img)))
}

### Proximal pixels
# a function that groups together the pixels contained within a sliding window around each pixel of interest
proximalPixels = function(img)
{
  pad = 2
  width = 2 * pad + 1
  padded = matrix(median(img), nrow(img) + 2 * pad, ncol(img) + 2 * pad)
  padded[pad + seq_len(nrow(img)), pad + seq_len(ncol(img))] = img
  
  tab = matrix(1, nrow(img) * ncol(img), width ^ 2)
  k = 1
  for (i in seq_len(width))
  {
    for (j in seq_len(width))
    {
      tab[,k] = img2vec(padded[i - 1 + seq_len(nrow(img)), j - 1 + seq_len(ncol(img))])
      k = k + 1
    }
  }
  
  return (tab)
}
