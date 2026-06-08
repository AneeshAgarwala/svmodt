test_that("returns the requested number of features when no penalty is applied", {
  set.seed(1)
  result <- choose_features_with_penalty(
    binary_data, "label",
    max_features   = 1,
    method         = "random",
    penalize_used  = FALSE
  )
  expect_length(result, 1)
})

test_that("returns all predictors when max_features exceeds predictor count", {
  result <- choose_features_with_penalty(
    binary_data, "label",
    max_features   = 10,
    method         = "random",
    penalize_used  = TRUE,
    used_features  = c("x1")
  )
  expect_setequal(result, c("x1", "x2"))
})

test_that("out-of-range penalty_weight does not cause an error", {
  expect_no_error(
    choose_features_with_penalty(
      binary_data, "label",
      max_features   = 1,
      method         = "cor",
      penalize_used  = TRUE,
      penalty_weight = 5.0,
      used_features  = c("x1")
    )
  )
})

test_that("the response variable is never returned even with penalty active", {
  result <- choose_features_with_penalty(
    binary_data, "label",
    max_features   = 2,
    method         = "cor",
    penalize_used  = TRUE,
    used_features  = c("x1")
  )
  expect_false("label" %in% result)
})

test_that("cor method with penalty returns features that exist in the data", {
  result <- choose_features_with_penalty(
    binary_data, "label",
    max_features   = 1,
    method         = "cor",
    penalize_used  = TRUE,
    penalty_weight = 0.5,
    used_features  = c("x1")
  )
  expect_true(all(result %in% c("x1", "x2")))
})
