library(testthat)
test_that("apply_scaler scales data correctly with valid scaler", {
  df <- data.frame(a = 1:5, b = 6:10)
  scaler <- list(
    transform = function(data) {
      scale(data)
    }
  )

  scaled_df <- apply_scaler(df, scaler)

  expect_equal(nrow(scaled_df), nrow(df))
  expect_equal(ncol(scaled_df), ncol(df))
  expect_true(all(abs(colMeans(scaled_df)) < 1e-8)) # Means should be close to 0
})
test_that("apply_scaler returns original data with invalid scaler", {
  df <- data.frame(a = 1:5, b = 6:10)
  scaler <- NULL

  expect_warning(
    result <- apply_scaler(df, scaler),
    "Invalid scaler provided, returning unscaled data"
  )

  expect_equal(result, df)
})
test_that("apply_scaler returns original data when scaling fails", {
  df <- data.frame(a = 1:5, b = 6:10)
  scaler <- list(
    transform = function(data) {
      stop("Simulated scaling error")
    }
  )

  expect_warning(
    result <- apply_scaler(df, scaler),
    "Scaling failed: Simulated scaling error"
  )

  expect_equal(result, df)
})
test_that("apply_scaler handles NULL or empty data frame", {
  scaler <- list(
    transform = function(data) {
      scale(data)
    }
  )

  result_null <- apply_scaler(NULL, scaler)
  expect_equal(nrow(result_null), 0)

  result_empty <- apply_scaler(data.frame(), scaler)
  expect_equal(nrow(result_empty), 0)
})
test_that("apply_scaler handles data frame with non-numeric columns", {
  df <- data.frame(a = 1:5, b = letters[1:5])
  scaler <- list(
    transform = function(data) {
      scale(data)
    }
  )

  expect_warning(
    result <- apply_scaler(df, scaler),
    "Scaling failed:"
  )

  expect_equal(result, df)
})
test_that("apply_scaler works with scaler having different transform implementation", {
  df <- data.frame(a = 1:5, b = 6:10)
  scaler <- list(
    transform = function(data) {
      data * 2
    }
  )

  scaled_df <- apply_scaler(df, scaler)

  expect_equal(scaled_df, df * 2)
})
