library(e1071)

# --- Minimal binary dataset (linearly separable) ---
make_binary_data <- function(n = 40, seed = 42) {
  set.seed(seed)
  data.frame(
    x1    = c(rnorm(n / 2, mean = -2), rnorm(n / 2, mean =  2)),
    x2    = c(rnorm(n / 2, mean = -2), rnorm(n / 2, mean =  2)),
    label = factor(rep(c("A", "B"), each = n / 2))
  )
}

# --- Three-class dataset ---
make_multiclass_data <- function(n_per_class = 20, seed = 42) {
  set.seed(seed)
  data.frame(
    x1    = c(rnorm(n_per_class, -3), rnorm(n_per_class, 0), rnorm(n_per_class, 3)),
    x2    = c(rnorm(n_per_class,  0), rnorm(n_per_class, 3), rnorm(n_per_class, 0)),
    label = factor(rep(c("A", "B", "C"), each = n_per_class))
  )
}

# --- Highly imbalanced binary dataset ---
make_imbalanced_data <- function(seed = 42) {
  set.seed(seed)
  data.frame(
    x1    = c(rnorm(90, -1), rnorm(10, 3)),
    x2    = c(rnorm(90, -1), rnorm(10, 3)),
    label = factor(c(rep("majority", 90), rep("minority", 10)))
  )
}

# --- Dataset with a constant column ---
make_data_with_constant <- function(n = 30, seed = 42) {
  d <- make_binary_data(n = n, seed = seed)
  d$constant_col <- 5.0
  d
}

# --- Very small dataset (edge case) ---
make_tiny_data <- function() {
  data.frame(
    x1    = c(1, 2, 3, 4, 5),
    x2    = c(2, 1, 4, 3, 5),
    label = factor(c("A", "A", "B", "B", "A"))
  )
}

# Pre-built instances used across multiple test files
binary_data     <- make_binary_data()
multiclass_data <- make_multiclass_data()
imbalanced_data <- make_imbalanced_data()
