tree_stats <- function(tree) {
  stats <- list(
    n_nodes = 0,
    n_leaves = 0,
    max_depth = 0,
    avg_samples_per_leaf = 0,
    memory_usage_mb = 0
  )

  traverse <- function(node, depth = 1) {
    if (is.null(node)) return()
    stats$n_nodes <<- stats$n_nodes + 1
    stats$max_depth <<- max(stats$max_depth, depth)

    if (node$is_leaf) {
      stats$n_leaves <<- stats$n_leaves + 1
      stats$avg_samples_per_leaf <<- stats$avg_samples_per_leaf + node$n
    } else {
      traverse(node$left, depth + 1)
      traverse(node$right, depth + 1)
    }
  }

  traverse(tree)

  if (stats$n_leaves > 0) {
    stats$avg_samples_per_leaf <- stats$avg_samples_per_leaf / stats$n_leaves
  }

  # Memory estimate
  stats$memory_usage_mb <- as.numeric(object.size(tree)) / (1024^2)

  return(stats)
}
