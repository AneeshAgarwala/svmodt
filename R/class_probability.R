# Helper: Safe class probabilities
class_probabilities <- function(vec) {
  f <- factor(vec)
  counts <- tabulate(f)
  probs <- counts / sum(counts)
  names(probs) <- levels(f)
  return(probs)
}
