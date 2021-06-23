normalize <- function(x) {
  x <- x[!is.na(x)]
  return ((x - min(x)) / (max(x) - min(x)))
}