#Rolling window: take window with definite size through the array
#param data: array-like
#param window_size: size
#return: the sequence of windows
#Example: data = array(1, 2, 3, 4, 5, 6), window_size = 4
#Then this function return array(array(1, 2, 3, 4), array(2, 3, 4, 5), array(3, 4, 5, 6))

rolling_window <- function(data,window_size){
  rw <- NULL
  for(i in 1:(length(data)-window_size+1)){
    rw[[i]] <- data[i:(i+window_size-1)]
  }
  rw
}

#Following function has been copied from R package "WeightedROC
# because it is not available on CRAN. and not available on Kaggle R Scripts
# for using on a local machine, users are recommended to install the relevant package 
# by running following command:
# devtools::install_github("tdhock/WeightedROC")
# Author: Toby Dylan Hocking
# library(WeightedROC)

WeightedROC <- function (guess, label, weight = rep(1, length(label))) 
{
  if (is.factor(label)) {
    label <- as.integer(label)
  }
  stopifnot(is.numeric(label))
  if (all(label %in% c(0, 1))) {
    label[label == 0] <- -1
  }
  if (all(label %in% c(1, 2))) {
    label[label == 1] <- -1
    label[label == 2] <- 1
  }
  stopifnot(label %in% c(-1, 1))
  stopifnot(is.numeric(guess))
  stopifnot(length(label) == length(guess))
  stopifnot(is.numeric(weight))
  stopifnot(length(label) == length(weight))
  stopifnot(weight > 0)
  ord <- order(guess)
  y <- label[ord]
  w <- weight[ord]
  y.hat <- guess[ord]
  is.positive <- y == 1
  is.negative <- y == -1
  w.positive <- w.negative <- w
  w.positive[is.negative] <- 0
  w.negative[is.positive] <- 0
  cum.positive <- cumsum(w.positive)
  cum.negative <- cumsum(w.negative)
  is.end <- c(diff(y.hat) != 0, TRUE)
  n <- length(y)
  TPR <- c(1, 1 - cum.positive[is.end]/cum.positive[n])
  FPR <- c(1, 1 - cum.negative[is.end]/cum.negative[n])
  d <- data.frame(TPR, FPR)
  d
}


cvm <- function (subindices,total_events){
  #   Compute Cramer-von Mises metric.
  #   Compared two distributions, where first is subset of second one.
  #   Assuming that second is ordered by ascending
  #   :param subindices: indices of events which will be associated with the first distribution
  #   :param total_events: count of events in the second distribution
  #   :return: cvm metric
  target_distribution = (1:(1+total_events))/total_events
  subarray_distribution = cumsum((hist(x = subindices,breaks = seq(0.5,total_events+1.5,1),plot = FALSE))$counts)
  subarray_distribution = subarray_distribution/subarray_distribution[length(subarray_distribution)]
  mean((target_distribution-subarray_distribution)^2)
}


compute_cvm <- function(predictions,masses, n_neighbours = 200,step = 50){
  # Computing Cramer-von Mises (cvm) metric on background events: take average of cvms calculated for each mass bin.
  # In each mass bin global prediction's cdf is compared to prediction's cdf in mass bin.
  # :param predictions: array-like, predictions
  # :param masses: array-like, in case of Kaggle tau23mu this is reconstructed mass
  # :param n_neighbours: count of neighbours for event to define mass bin
  # :param step: step through sorted mass-array to define next center of bin
  # :return: average cvm value
  
  # First, reorder by masses
  predictions = predictions[order(masses)]
  # Second, replace probabilities with order of probability among other events
  predictions = order(order(predictions))
  # Now, each window forms a group, and we can compute contribution of each group to CvM
  cvms = c()
  rw = rolling_window(predictions,window_size = n_neighbours)
  for (window in rw[seq(1,length(rw),step)]){
    cvms = c(cvms,cvm(subindices = window,total_events = length(predictions)))
  }
  mean(cvms)
}

roc_curve_splitted <- function(data_zero, data_one, sample_weights_zero, sample_weights_one){
  #   Compute roc curve
  #   :param data_zero: 0-labeled data
  #   :param data_one:  1-labeled data
  #   :param sample_weights_zero: weights for 0-labeled data
  #   :param sample_weights_one:  weights for 1-labeled data
  #   :return: roc curve
  labels = c(rep(0,length(data_zero)),rep(1,length(data_one)))
  weights = c(sample_weights_zero, sample_weights_one)
  data_all = c(data_zero, data_one)
  tpr.fpr = WeightedROC(guess = data_all,label = labels,weight = weights)
  tpr.fpr
}
compute_ks <- function(data_prediction, mc_prediction, weights_data, weights_mc){
  #   Compute Kolmogorov-Smirnov (ks) distance between real data predictions cdf and Monte Carlo one.
  #   :param data_prediction: array-like, real data predictions
  #   :param mc_prediction: array-like, Monte Carlo data predictions
  #   :param weights_data: array-like, real data weights
  #   :param weights_mc: array-like, Monte Carlo weights
  #   :return: ks value
  weights_data = weights_data/sum(weights_data)
  weights_mc = weights_mc/sum(weights_mc)
  tpr.fpr = roc_curve_splitted(data_prediction,mc_prediction,weights_data,weights_mc)
  Dnm = max(abs(tpr.fpr[,1]-tpr.fpr[,2]))
  Dnm
}

roc_auc_truncated <- function(labels, predictions, tpr_thresholds=c(0.2, 0.4, 0.6, 0.8),
                              roc_weights=c(4, 3, 2, 1, 0)){
  #   labels = as.numeric(valResponse)
  #   predictions = predValxgbTree_caret[,2]
  #   tpr_thresholds=c(0.2, 0.4, 0.6, 0.8)
  #   roc_weights=c(4, 3, 2, 1, 0)
  #   Compute weighted area under ROC curve.
  #   :param labels: array-like, true labels
  #   :param predictions: array-like, predictions
  #   :param tpr_thresholds: array-like, true positive rate thresholds delimiting the ROC segments
  #   :param roc_weights: array-like, weights for true positive rate segments
  #   :return: weighted AUC
  tpr.fpr = WeightedROC(guess = predictions,label = labels)
  tpr.fpr = tpr.fpr[order(tpr.fpr$TPR,tpr.fpr$FPR),]
  area = 0
  tpr_thresholds = c(0,tpr_thresholds,1)
  for(index in (2:length(tpr_thresholds))){
    #     index = 6
    tpr.cut = sapply(tpr.fpr[,1],function(x){min(x,tpr_thresholds[index])})
    tpr.previous = sapply(tpr.fpr[,1],function(x){min(x,tpr_thresholds[index-1])})
    area = area+roc_weights[index-1]*(trapz(tpr.fpr$FPR,tpr.cut)-trapz(tpr.fpr$FPR,tpr.previous))
  }
  area = area/sum((tpr_thresholds[-1] - tpr_thresholds[-length(tpr_thresholds)])*roc_weights)
  area
}

