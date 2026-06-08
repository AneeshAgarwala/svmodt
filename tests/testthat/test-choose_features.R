test_that("random method returns the requested number of features", {
  set.seed(1)
  result <- choose_features(binary_data, "label", max_features = 1, method = "random")
  expect_length(result, 1)
  expect_true(result %in% c("x1", "x2"))
})

test_that("returns all predictors when max_features exceeds predictor count", {
  result <- choose_features(binary_data, "label", max_features = 10, method = "random")
  expect_setequal(result, c("x1", "x2"))
})

test_that("cor method returns features that exist in the data", {
  result <- choose_features(binary_data, "label", max_features = 1, method = "cor")
  expect_length(result, 1)
  expect_true(all(result %in% names(binary_data)))
})

test_that("mutual method falls back to correlation when FSelectorRcpp is unavailable", {
  with_mocked_bindings(
    requireNamespace = function(pkg, ...) FALSE,
    .package = "base",
    {
      expect_warning(
        result <- choose_features(
          binary_data, "label",
          max_features = 1, method = "mutual"
        ),
        regexp = "FSelectorRcpp not available"
      )
      expect_length(result, 1)
    }
  )
})

test_that("the response variable is never included in the returned features", {
  result <- choose_features(binary_data, "label", max_features = 2, method = "random")
  expect_false("label" %in% result)
})
