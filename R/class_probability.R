class_probabilities <- function(vec, all_levels) {
  f <- factor(vec, levels = all_levels)
  counts <- tabulate(f, nbins = length(all_levels))
  probs <- counts / sum(counts)
  names(probs) <- all_levels
  # Return as 1-row matrix (transposed)
  matrix(probs, nrow = 1, dimnames = list(NULL, all_levels))
}
