# helpers

# Shared call signature <U+2014> all the boring boilerplate in one place
call_handle <- function(left_idx, right_idx,
                        min_samples = 5,
                        data = binary_data,
                        verbose = FALSE) {
  handle_small_children(
    left_idx                    = left_idx,
    right_idx                   = right_idx,
    min_samples                 = min_samples,
    data                        = data,
    response                    = "label",
    depth                       = 1,
    max_depth                   = 5,
    max_features                = 2,
    feature_method              = "random",
    impurity_measure            = "gini",
    max_features_strategy       = "constant",
    max_features_decrease_rate  = 0.8,
    max_features_random_range   = c(0.3, 1.0),
    penalize_used_features      = FALSE,
    feature_penalty_weight      = 0.5,
    n_subsets                   = 1,
    used_features               = character(0),
    class_weights               = "none",
    custom_class_weights        = NULL,
    min_impurity_decrease       = 0.001,
    features                    = c("x1", "x2"),
    scaler                      = scale_node(data[, c("x1", "x2")]),
    all_classes                 = c("A", "B"),
    verbose                     = verbose
  )
}

# <U+2500><U+2500> both children too small <U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500>

test_that("returns stop = TRUE and a leaf when both children are too small", {
  result <- call_handle(left_idx = 1:2, right_idx = 3:4, min_samples = 10)

  expect_true(result$stop)
  expect_true(result$node$is_leaf)
})

test_that("leaf node from both-too-small case has valid class probabilities", {
  result <- call_handle(left_idx = 1:2, right_idx = 3:4, min_samples = 10)

  expect_equal(sum(result$node$class_prob), 1, tolerance = 1e-9)
})

# <U+2500><U+2500> left child too small <U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500>

test_that("returns stop = FALSE and a node when only the left child is too small", {
  # Give the right side enough samples, left side is tiny
  result <- call_handle(left_idx = 1:2, right_idx = 5:30, min_samples = 5)

  expect_false(result$stop)
  expect_false(is.null(result$node))
})

test_that("node produced for small-left case has is_leaf = FALSE", {
  result <- call_handle(left_idx = 1:2, right_idx = 5:30, min_samples = 5)
  expect_false(result$node$is_leaf)
})

test_that("node for small-left case has a valid right child but left child is a leaf", {
  result <- call_handle(left_idx = 1:2, right_idx = 5:30, min_samples = 5)

  expect_false(is.null(result$node$right))
  expect_true(result$node$left$is_leaf)
})

# <U+2500><U+2500> right child too small <U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500>

test_that("returns stop = FALSE and a node when only the right child is too small", {
  result <- call_handle(left_idx = 1:30, right_idx = 31:32, min_samples = 5)

  expect_false(result$stop)
  expect_false(is.null(result$node))
})

test_that("node for small-right case has a valid left child but right child is a leaf", {
  result <- call_handle(left_idx = 1:30, right_idx = 31:32, min_samples = 5)

  expect_false(is.null(result$node$left))
  expect_true(result$node$right$is_leaf)
})

# <U+2500><U+2500> neither child too small <U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500>

test_that("returns stop = FALSE and node = NULL when both children are large enough", {
  result <- call_handle(left_idx = 1:20, right_idx = 21:40, min_samples = 5)

  expect_false(result$stop)
  expect_null(result$node)
})

# <U+2500><U+2500> verbose output <U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500>

test_that("prints a message when both children are too small and verbose = TRUE", {
  expect_output(
    call_handle(left_idx = 1:2, right_idx = 3:4, min_samples = 10, verbose = TRUE),
    regexp = "both child"
  )
})
