test_that("svm_info_gain returns a single non-negative numeric", {
  result <- svm_info_gain(c("x1", "x2"), binary_data, "label")

  expect_type(result, "double")
  expect_length(result, 1)
  expect_gte(result, 0)
})

test_that("svm_info_gain is higher for informative features than noise", {
  set.seed(42)
  df <- data.frame(
    informative = c(rnorm(30, -3), rnorm(30, 3)),
    noise1      = rnorm(60),
    noise2      = rnorm(60),
    label       = factor(rep(c("A", "B"), each = 30))
  )

  ig_signal <- svm_info_gain(c("informative"), df, "label")
  ig_noise  <- svm_info_gain(c("noise1", "noise2"), df, "label")

  expect_gt(ig_signal, ig_noise)
})

test_that("svm_info_gain returns 0 and warns for an empty feature subset", {
  expect_warning(
    result <- svm_info_gain(character(0), binary_data, "label"),
    regexp = "Empty feature subset"
  )
  expect_equal(result, 0)
})

test_that("svm_info_gain returns 0 and warns when features are absent from data", {
  expect_warning(
    result <- svm_info_gain(c("nonexistent"), binary_data, "label"),
    regexp = "not found"
  )
  expect_equal(result, 0)
})

test_that("svm_info_gain errors informatively when response is missing", {
  expect_error(
    svm_info_gain(c("x1"), binary_data, "nonexistent_response"),
    regexp = "not found"
  )
})

test_that("svm_info_gain accepts metric = 'gini' without error", {
  expect_no_error(
    svm_info_gain(c("x1", "x2"), binary_data, "label", metric = "gini")
  )
})

test_that("svm_info_gain works on multiclass data", {
  result <- svm_info_gain(c("x1", "x2"), multiclass_data, "label")

  expect_type(result, "double")
  expect_gte(result, 0)
})
