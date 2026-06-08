test_that("returns a list node for a clean binary dataset", {
  tree <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)

  expect_type(tree, "list")
  expect_true("is_leaf" %in% names(tree))
})

test_that("returns a leaf when max_depth = 0", {
  tree <- svm_split(binary_data, "label", max_depth = 0, min_samples = 5)
  expect_true(tree$is_leaf)
})

test_that("returns a leaf for a single-class dataset", {
  pure <- binary_data[binary_data$label == "A", ]
  tree <- svm_split(pure, "label", max_depth = 3, min_samples = 2)

  expect_true(tree$is_leaf)
  expect_equal(tree$prediction, "A")
})

test_that("returns a leaf when dataset is smaller than min_samples", {
  tree <- svm_split(make_tiny_data(), "label", max_depth = 5, min_samples = 10)
  expect_true(tree$is_leaf)
})

test_that("handles three-class data without error", {
  expect_no_error(
    svm_split(multiclass_data, "label", max_depth = 2, min_samples = 5)
  )
})

test_that("internal node contains all required fields", {
  tree <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  if (!tree$is_leaf) {
    required <- c("model", "features", "scaler", "left", "right", "depth", "n")
    expect_true(all(required %in% names(tree)))
  }
})

test_that("no node in the tree has depth greater than max_depth", {
  max_d <- 2
  tree  <- svm_split(binary_data, "label", max_depth = max_d, min_samples = 3)

  check_depth <- function(node) {
    if (node$is_leaf) return(invisible())
    expect_lte(node$depth, max_d)
    if (!is.null(node$left))  check_depth(node$left)
    if (!is.null(node$right)) check_depth(node$right)
  }
  check_depth(tree)
})

test_that("feature_method = 'cor' produces a valid tree", {
  expect_no_error(
    svm_split(binary_data, "label",
              max_depth = 2, min_samples = 5,
              feature_method = "cor")
  )
})

test_that("class_weights = 'balanced' runs without error", {
  expect_no_error(
    svm_split(imbalanced_data, "label",
              max_depth = 2, min_samples = 5,
              class_weights = "balanced")
  )
})

test_that("penalize_used_features = TRUE runs without error", {
  expect_no_error(
    svm_split(binary_data, "label",
              max_depth = 2, min_samples = 5,
              penalize_used_features = TRUE,
              feature_penalty_weight = 0.5)
  )
})

test_that("max_features_strategy = 'decrease' runs without error", {
  expect_no_error(
    svm_split(binary_data, "label",
              max_depth = 2, min_samples = 5,
              max_features_strategy      = "decrease",
              max_features_decrease_rate = 0.5)
  )
})

test_that("returns a leaf node when all features are constant", {
  const_data <- data.frame(
    x1    = rep(1, 20),
    x2    = rep(2, 20),
    label = factor(rep(c("A", "B"), 10))
  )
  tree <- expect_no_error(
    svm_split(const_data, "label", max_depth = 3, min_samples = 2)
  )
  expect_true(tree$is_leaf)
})

test_that("errors with an informative message when the response column is absent", {
  expect_error(
    svm_split(binary_data, response = "nonexistent",
              max_depth = 2, min_samples = 2),
    regexp = "not found"
  )
})
