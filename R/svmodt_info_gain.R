entropy <- function(y) {
  p <- prop.table(table(y))
  -sum(p * log2(p + 1e-9))
}

info_gain <- function(parent_y, left_y, right_y) {
  n <- length(parent_y)
  entropy(parent_y) -
    (length(left_y)/n)  * entropy(left_y) -
    (length(right_y)/n) * entropy(right_y)
}


best_column <- function(dec_matrix, y) {
  gains <- sapply(seq_len(ncol(dec_matrix)), function(j) {
    vals <- dec_matrix[, j]
    info_gain(y, y[vals > 0], y[vals <= 0])
  })
  which.max(gains)
}
