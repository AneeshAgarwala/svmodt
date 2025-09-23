stop_conditions_met <- function(data, y, depth, max_depth, min_samples, verbose) {
  reason <- NULL
  if (depth > max_depth) {
    reason <- "max depth reached"
  } else if (length(unique(y)) == 1) {
    reason <- "pure node"
  } else if (nrow(data) < min_samples) {
    reason <- "too few samples"
  }
  if (!is.null(reason) && verbose) cat("Stopping:", reason, "\n")
  !is.null(reason)
}
