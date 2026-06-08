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

test_that("n_subsets = 3 with random method returns a character vector", {
  set.seed(1)
  result <- choose_features(
    multiclass_data, "label",
    max_features = 1,
    method       = "random",
    n_subsets    = 3
  )

  expect_type(result, "character")
  expect_length(result, 1)
  expect_true(all(result %in% c("x1", "x2")))
})

test_that("n_subsets = 1 (default) and n_subsets = 5 return valid features", {
  set.seed(2)
  r1 <- choose_features(binary_data, "label", max_features = 1,
                        method = "random", n_subsets = 1)
  r5 <- choose_features(binary_data, "label", max_features = 1,
                        method = "random", n_subsets = 5)

  expect_true(r1 %in% c("x1", "x2"))
  expect_true(r5 %in% c("x1", "x2"))
})

test_that("cor method works on multiclass data and returns valid features", {
  result <- choose_features(
    multiclass_data, "label",
    max_features = 1, method = "cor"
  )

  expect_length(result, 1)
  expect_true(result %in% c("x1", "x2"))
})

test_that("response never appears in choose_features result for any method", {
  for (m in c("random", "cor")) {
    result <- choose_features(binary_data, "label", max_features = 2, method = m)
    expect_false("label" %in% result)
  }
})

test_that("choose_features cor method warns and falls back when all associations are NA", {
  df <- data.frame(
    x1    = rep(0, 20),
    x2    = rep(0, 20),
    label = factor(rep(c("A", "B"), 10))
  )

  expect_warning(
    result <- choose_features(df, "label", max_features = 1, method = "cor"),
    regexp = "No valid features|random"
  )
  expect_type(result, "character")
  expect_gte(length(result), 1)
})
