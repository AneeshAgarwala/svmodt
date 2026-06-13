test_that("returns a data frame with the same dimensions as the input", {
  df <- binary_data[, c("x1", "x2")]
  scaler <- scale_node(df)
  result <- apply_scaler(df, scaler)

  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), dim(df))
})

test_that("returns an empty data frame when given an empty input", {
  df <- binary_data[, c("x1", "x2")]
  scaler <- scale_node(df)
  result <- apply_scaler(data.frame(), scaler)

  expect_equal(nrow(result), 0)
})

test_that("warns and returns the original data when scaler is NULL", {
  df <- binary_data[, c("x1", "x2")]

  expect_warning(
    result <- apply_scaler(df, NULL),
    regexp = "Invalid scaler"
  )
  expect_equal(result, df)
})

test_that("warns and returns the original data when scaler has no transform field", {
  df <- binary_data[, c("x1", "x2")]
  bad_scaler <- list(not_transform = function(x) x)

  expect_warning(
    result <- apply_scaler(df, bad_scaler),
    regexp = "Invalid scaler"
  )
  expect_equal(result, df)
})

test_that("apply_scaler warns and returns original df when transform throws", {
  df <- binary_data[, c("x1", "x2")]
  bad_scaler <- list(
    transform = function(x) stop("deliberate transform error")
  )

  expect_warning(
    result <- apply_scaler(df, bad_scaler),
    regexp = "Scaling failed"
  )
  expect_equal(result, df)
})
