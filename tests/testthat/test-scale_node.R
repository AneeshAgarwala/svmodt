test_that("scale_node returns a list with 'train' and 'transform'", {
  df <- binary_data[, c("x1", "x2")]
  result <- scale_node(df)

  expect_type(result, "list")
  expect_named(result, c("train", "transform"))
  expect_true(is.function(result$transform))
  expect_s3_class(result$train, "data.frame")
})

test_that("scale_node produces zero-mean, unit-variance columns", {
  df <- binary_data[, c("x1", "x2")]
  result <- scale_node(df)
  scaled <- result$train

  expect_lt(abs(mean(scaled$x1)), 1e-10)
  expect_lt(abs(mean(scaled$x2)), 1e-10)
  expect_lt(abs(sd(scaled$x1) - 1), 1e-10)
  expect_lt(abs(sd(scaled$x2) - 1), 1e-10)
})

test_that("scale_node removes constant columns and warns", {
  df <- make_data_with_constant()[, c("x1", "x2", "constant_col")]

  expect_warning(
    result <- scale_node(df),
    regexp = "constant"
  )
  expect_false("constant_col" %in% names(result$train))
})

test_that("scale_node transform applies consistent scaling to new data", {
  df <- binary_data[, c("x1", "x2")]
  scaler <- scale_node(df)

  new_df <- data.frame(x1 = c(0, 1), x2 = c(0, 1))
  new_scaled <- scaler$transform(new_df)

  mu <- colMeans(df)
  sds <- apply(df, 2, sd)
  expected <- sweep(sweep(new_df, 2, mu, "-"), 2, sds, "/")

  expect_equal(as.numeric(new_scaled$x1), as.numeric(expected$x1), tolerance = 1e-9)
  expect_equal(as.numeric(new_scaled$x2), as.numeric(expected$x2), tolerance = 1e-9)
})

test_that("scale_node returns empty data frame for zero-column input", {
  result <- scale_node(data.frame())
  expect_equal(ncol(result$train), 0)
})

test_that("scale_node transform returns empty data frame when called on empty input", {
  df <- binary_data[, c("x1", "x2")]
  scaler <- scale_node(df)
  out <- scaler$transform(data.frame())
  expect_equal(ncol(out), 0)
})
