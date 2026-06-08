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

test_that("mutual method with penalty falls back to cor when FSelectorRcpp unavailable", {
  with_mocked_bindings(
    requireNamespace = function(pkg, ...) FALSE,
    .package = "base",
    {
      expect_warning(
        result <- choose_features_with_penalty(
          binary_data, "label",
          max_features   = 1,
          method         = "mutual",
          penalize_used  = TRUE,
          penalty_weight = 0.5,
          used_features  = c("x1")
        ),
        regexp = "FSelectorRcpp not available"
      )
      expect_true(result %in% c("x1", "x2"))
    }
  )
})

test_that("verbose = TRUE produces console output when penalty is active", {
  expect_output(
    choose_features_with_penalty(
      binary_data, "label",
      max_features   = 1,
      method         = "cor",
      penalize_used  = TRUE,
      penalty_weight = 0.5,
      used_features  = c("x1"),
      verbose        = TRUE
    ),
    regexp = "penalty"
  )
})

test_that("penalty cor path falls back to random when all cor_vals are NA", {
  # x1 is constant -> cor returns NA and gets filtered; only x1 available
  # Use a dataset with one near-constant and one zero-variance predictor
  df <- data.frame(
    x1    = rep(1.0, 30),      # constant -> NA association
    x2    = rep(2.0, 30),      # constant -> NA association
    label = factor(rep(c("A", "B"), 15))
  )

  # With all predictors constant, cor_vals will be empty after NA removal.
  # The function should fall back to random selection without error.
  expect_no_error(
    result <- choose_features_with_penalty(
      df, "label",
      max_features   = 1,
      method         = "cor",
      penalize_used  = TRUE,
      penalty_weight = 0.5,
      used_features  = c("x1")
    )
  )
  # Must still return something (random fallback)
  expect_type(result, "character")
  expect_gte(length(result), 1)
})

# ── choose_features_with_penalty: random method + penalty + n_subsets > 1 ────

test_that("random method with penalty and n_subsets > 1 returns valid features", {
  set.seed(5)
  result <- choose_features_with_penalty(
    multiclass_data, "label",
    max_features   = 1,
    method         = "random",
    penalize_used  = TRUE,
    penalty_weight = 0.5,
    used_features  = c("x1"),
    n_subsets      = 3
  )

  expect_type(result, "character")
  expect_gte(length(result), 1)
  expect_true(all(result %in% c("x1", "x2")))
})
