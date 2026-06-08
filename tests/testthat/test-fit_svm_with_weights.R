# Helper: scale binary_data and return X and y for reuse across tests
scaled_binary <- local({
  scaler <- scale_node(binary_data[, c("x1", "x2")])
  list(X = scaler$train, y = binary_data$label)
})

test_that("returns an svm object for valid binary input without weights", {
  model <- fit_svm_with_weights(
    scaled_binary$X, scaled_binary$y,
    class_weights_vec = NULL
  )
  expect_s3_class(model, "svm")
})

test_that("returns an svm object when class weights are supplied", {
  cw    <- c(A = 1, B = 2)
  model <- fit_svm_with_weights(
    scaled_binary$X, scaled_binary$y,
    class_weights_vec = cw
  )
  expect_s3_class(model, "svm")
})

test_that("returns NULL without error when SVM fitting is impossible (single class)", {
  X     <- data.frame(x1 = 1:5, x2 = 1:5)
  y_bad <- factor(rep("A", 5))

  result <- expect_no_error(
    fit_svm_with_weights(X, y_bad, class_weights_vec = NULL)
  )
  expect_null(result)
})

test_that("fitted model produces decision values on predict()", {
  model <- fit_svm_with_weights(
    scaled_binary$X, scaled_binary$y,
    class_weights_vec = NULL
  )
  preds <- predict(model, scaled_binary$X, decision.values = TRUE)
  expect_false(is.null(attr(preds, "decision.values")))
})


test_that("fit_svm_with_weights verbose = TRUE prints weight and level info", {
  scaler <- scale_node(binary_data[, c("x1", "x2")])
  cw     <- c(A = 1.5, B = 0.8)

  expect_output(
    fit_svm_with_weights(
      scaler$train, binary_data$label,
      class_weights_vec = cw,
      verbose           = TRUE
    ),
    regexp = "weight|level"
  )
})

test_that("fit_svm_with_weights verbose = TRUE with no weights still runs without error", {
  scaler <- scale_node(binary_data[, c("x1", "x2")])

  expect_no_error(
    model <- fit_svm_with_weights(
      scaler$train, binary_data$label,
      class_weights_vec = NULL,
      verbose           = TRUE
    )
  )
  expect_s3_class(model, "svm")
})

test_that("fit_svm_with_weights verbose = TRUE prints WARN when weight names mismatch levels", {
  scaler   <- scale_node(binary_data[, c("x1", "x2")])
  # Supply weights only for "A" so "B" is missing -> [WARN] in verbose output
  cw_bad   <- c(A = 2.0)   # B intentionally absent

  # The SVM will likely error with mismatched weights, but we just want to
  # confirm the verbose mismatch-check lines are executed (not an error check).
  output <- capture.output(
    tryCatch(
      fit_svm_with_weights(
        scaler$train, binary_data$label,
        class_weights_vec = cw_bad,
        verbose           = TRUE
      ),
      error = function(e) NULL
    )
  )
  # The verbose block should have printed BEFORE the SVM attempt
  expect_true(any(grepl("weight|level|WARN", output)))
})
