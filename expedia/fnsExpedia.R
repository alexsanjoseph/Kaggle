
most_common <- function(data){
  data_sorted = sort(table(data), decreasing = T)
  # paste(data_sorted[1:5], names(data_sorted[1:5]), sep = "-", collapse = " ")
  names(data_sorted)[1]
}