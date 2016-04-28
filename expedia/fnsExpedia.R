
most_common <- function(data){
  data_sorted = sort(table(data), decreasing = T)
  paste(head(names(data_sorted), 5), sep = "-", collapse = " ")
  # names(data_sorted)[1]
}