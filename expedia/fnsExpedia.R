
most_common <- function(data){
  data_sorted = sort(table(data), decreasing = T)
  paste(head(c(names(data_sorted), c("91", "41", "48", "64", "65")), 5), sep = "-", collapse = " ")
  # names(data_sorted)[1]
}