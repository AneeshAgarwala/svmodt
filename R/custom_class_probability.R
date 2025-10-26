class_probabilities <- function(vec, all_levels) {
  f <- factor(vec, levels = all_levels)
  counts <- tabulate(f, nbins = length(all_levels))
  if (sum(counts) == 0) {
    # fallback to uniform if empty (shouldn't normally happen)
    probs <- rep(1 / length(all_levels), length(all_levels))
  } else {
    probs <- counts / sum(counts)
  }
  names(probs) <- all_levels
  probs
}
