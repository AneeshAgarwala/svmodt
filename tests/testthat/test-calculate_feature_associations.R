test_that("returns a named numeric vector", {
  result <- calculate_feature_associations(binary_data, "label", c("x1", "x2"))

  expect_type(result, "double")
  expect_named(result)
})

test_that("all association values are between 0 and 1", {
  result <- calculate_feature_associations(binary_data, "label", c("x1", "x2"))

  expect_true(all(result >= 0))
  expect_true(all(result <= 1))
})

test_that("a highly informative feature scores higher than pure noise", {
  set.seed(42)
  y    <- factor(rep(c("A", "B"), each = 50))
  data <- data.frame(
    informative = c(rnorm(50, -3), rnorm(50, 3)),
    noise       = rnorm(100),
    label       = y
  )
  result <- calculate_feature_associations(data, "label", c("informative", "noise"))

  expect_gt(result["informative"], result["noise"])
})

test_that("constant column is excluded from the result", {
  df     <- make_data_with_constant()
  result <- calculate_feature_associations(
    df, "label", c("x1", "x2", "constant_col")
  )
  expect_false("constant_col" %in% names(result))
})

test_that("returns an empty vector when all predictors are constant", {
  df <- data.frame(
    c1    = rep(1, 20),
    c2    = rep(2, 20),
    label = factor(rep(c("A", "B"), 10))
  )
  result <- calculate_feature_associations(df, "label", c("c1", "c2"))
  expect_length(result, 0)
})
