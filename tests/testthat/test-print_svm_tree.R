test_that("produces console output containing 'Leaf' for a leaf node", {
  leaf <- leaf_node(factor(c("A", "B")), 2, c("A", "B"))
  expect_output(print_svm_tree(leaf), regexp = "Leaf")
})

test_that("produces console output for an internal node tree", {
  tree <- svm_split(binary_data, "label", max_depth = 1, min_samples = 5)
  expect_output(print_svm_tree(tree), regexp = "Node|Leaf")
})

test_that("returns NULL invisibly", {
  leaf   <- leaf_node(factor("A"), 1, c("A", "B"))
  result <- print_svm_tree(leaf)
  expect_null(result)
})

test_that("shows class probabilities when show_probabilities = TRUE", {
  leaf <- leaf_node(factor(c("A", "B")), 2, c("A", "B"))
  expect_output(
    print_svm_tree(leaf, show_probabilities = TRUE),
    regexp = "probs"
  )
})

test_that("print_svm_tree shows 'no left child' when left is NULL", {
  leaf  <- leaf_node(factor("B"), 1, c("A", "B"))
  scaler <- scale_node(binary_data[, c("x1", "x2")])
  model  <- fit_svm_with_weights(scaler$train, binary_data$label,
                                 class_weights_vec = NULL)

  node <- list(
    is_leaf          = FALSE,
    model            = model,
    features         = c("x1", "x2"),
    scaler           = scaler,
    left             = NULL,    # deliberately absent
    right            = leaf,
    depth            = 1,
    n                = nrow(binary_data),
    hyperplane_class = NULL,
    impurity         = 0.4,
    penalty_applied  = FALSE
  )

  expect_output(
    print_svm_tree(node),
    regexp = "no left child"
  )
})

test_that("print_svm_tree shows 'no right child' when right is NULL", {
  leaf  <- leaf_node(factor("A"), 1, c("A", "B"))
  scaler <- scale_node(binary_data[, c("x1", "x2")])
  model  <- fit_svm_with_weights(scaler$train, binary_data$label,
                                 class_weights_vec = NULL)

  node <- list(
    is_leaf          = FALSE,
    model            = model,
    features         = c("x1", "x2"),
    scaler           = scaler,
    left             = leaf,
    right            = NULL,    # deliberately absent
    depth            = 1,
    n                = nrow(binary_data),
    hyperplane_class = NULL,
    impurity         = 0.4,
    penalty_applied  = FALSE
  )

  expect_output(
    print_svm_tree(node),
    regexp = "no right child"
  )
})

test_that("print_svm_tree shows hyperplane_class when set", {
  leaf  <- leaf_node(factor("A"), 1, c("A", "B", "C"))
  leaf2 <- leaf_node(factor("B"), 1, c("A", "B", "C"))
  scaler <- scale_node(binary_data[, c("x1", "x2")])
  model  <- fit_svm_with_weights(scaler$train, binary_data$label,
                                 class_weights_vec = NULL)

  node <- list(
    is_leaf          = FALSE,
    model            = model,
    features         = c("x1", "x2"),
    scaler           = scaler,
    left             = leaf,
    right            = leaf2,
    depth            = 1,
    n                = 60,
    hyperplane_class = "A",     # OVR split label
    impurity         = 0.6,
    penalty_applied  = FALSE
  )

  expect_output(
    print_svm_tree(node),
    regexp = "A vs rest|split"
  )
})
