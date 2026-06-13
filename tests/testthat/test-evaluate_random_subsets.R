test_that("evaluate_random_subsets returns a data frame with two columns", {
  set.seed(1)
  result <- evaluate_random_subsets(
    binary_data,
    predictors = c("x1", "x2"),
    response = "label", n_subsets = 3, subset_size = 1
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("info_gain", "features"))
})

test_that("result has exactly n_subsets rows", {
  set.seed(1)
  result <- evaluate_random_subsets(
    binary_data, c("x1", "x2"), "label",
    n_subsets = 4, subset_size = 1
  )
  expect_equal(nrow(result), 4)
})

test_that("result is sorted in descending order by info_gain", {
  set.seed(42)
  result <- evaluate_random_subsets(
    binary_data, c("x1", "x2"), "label",
    n_subsets = 5, subset_size = 1
  )
  expect_true(all(diff(result$info_gain) <= 0))
})

test_that("features column is a list of character vectors", {
  set.seed(1)
  result <- evaluate_random_subsets(
    binary_data, c("x1", "x2"), "label",
    n_subsets = 2, subset_size = 1
  )
  expect_type(result$features, "list")
  expect_type(result$features[[1]], "character")
})

test_that("subset_size is clamped to the number of available predictors", {
  set.seed(1)
  # subset_size = 100 >> 2 available predictors
  result <- evaluate_random_subsets(
    binary_data, c("x1", "x2"), "label",
    n_subsets = 2, subset_size = 100
  )
  expect_lte(length(result$features[[1]]), 2)
})

test_that("warns and returns empty result for zero-length predictor vector", {
  expect_warning(
    result <- evaluate_random_subsets(
      binary_data, character(0), "label",
      n_subsets = 3, subset_size = 1
    ),
    regexp = "No predictors"
  )
  expect_equal(nrow(result), 0)
})

test_that("invalid n_subsets is corrected to 1 with a warning", {
  expect_warning(
    result <- evaluate_random_subsets(
      binary_data, c("x1", "x2"), "label",
      n_subsets = -1, subset_size = 1
    ),
    regexp = "Invalid n_subsets"
  )
  expect_equal(nrow(result), 1)
})
