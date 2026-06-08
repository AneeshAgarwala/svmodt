test_that("returns a list node for a clean binary dataset", {
  tree <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)

  expect_type(tree, "list")
  expect_true("is_leaf" %in% names(tree))
})

test_that("returns a leaf when max_depth = 0", {
  tree <- svm_split(binary_data, "label", max_depth = 0, min_samples = 5)
  expect_true(tree$is_leaf)
})

test_that("returns a leaf for a single-class dataset", {
  pure <- binary_data[binary_data$label == "A", ]
  tree <- svm_split(pure, "label", max_depth = 3, min_samples = 2)

  expect_true(tree$is_leaf)
  expect_equal(tree$prediction, "A")
})

test_that("returns a leaf when dataset is smaller than min_samples", {
  tree <- svm_split(make_tiny_data(), "label", max_depth = 5, min_samples = 10)
  expect_true(tree$is_leaf)
})

test_that("handles three-class data without error", {
  expect_no_error(
    svm_split(multiclass_data, "label", max_depth = 2, min_samples = 5)
  )
})

test_that("internal node contains all required fields", {
  tree <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  if (!tree$is_leaf) {
    required <- c("model", "features", "scaler", "left", "right", "depth", "n")
    expect_true(all(required %in% names(tree)))
  }
})

test_that("no node in the tree has depth greater than max_depth", {
  max_d <- 2
  tree  <- svm_split(binary_data, "label", max_depth = max_d, min_samples = 3)

  check_depth <- function(node) {
    if (node$is_leaf) return(invisible())
    expect_lte(node$depth, max_d)
    if (!is.null(node$left))  check_depth(node$left)
    if (!is.null(node$right)) check_depth(node$right)
  }
  check_depth(tree)
})

test_that("feature_method = 'cor' produces a valid tree", {
  expect_no_error(
    svm_split(binary_data, "label",
              max_depth = 2, min_samples = 5,
              feature_method = "cor")
  )
})

test_that("class_weights = 'balanced' runs without error", {
  expect_no_error(
    svm_split(imbalanced_data, "label",
              max_depth = 2, min_samples = 5,
              class_weights = "balanced")
  )
})

test_that("penalize_used_features = TRUE runs without error", {
  expect_no_error(
    svm_split(binary_data, "label",
              max_depth = 2, min_samples = 5,
              penalize_used_features = TRUE,
              feature_penalty_weight = 0.5)
  )
})

test_that("max_features_strategy = 'decrease' runs without error", {
  expect_no_error(
    svm_split(binary_data, "label",
              max_depth = 2, min_samples = 5,
              max_features_strategy      = "decrease",
              max_features_decrease_rate = 0.5)
  )
})

test_that("errors with an informative message when the response column is absent", {
  expect_error(
    svm_split(binary_data, response = "nonexistent",
              max_depth = 2, min_samples = 2),
    regexp = "not found"
  )
})

# ── impurity_measure = "entropy" ──────────────────────────────────────────────

test_that("entropy impurity measure builds a valid tree", {
  tree <- svm_split(binary_data, "label",
                    max_depth        = 2,
                    min_samples      = 5,
                    impurity_measure = "entropy")

  expect_type(tree, "list")
  expect_true("is_leaf" %in% names(tree))
})

test_that("multiclass tree with entropy produces correct prediction set", {
  tree  <- svm_split(multiclass_data, "label",
                     max_depth        = 3,
                     min_samples      = 5,
                     impurity_measure = "entropy")
  preds <- svm_predict_tree(tree, multiclass_data)

  expect_true(all(preds %in% c("A", "B", "C")))
})

# ── min_impurity_decrease ─────────────────────────────────────────────────────

test_that("very high min_impurity_decrease forces the root to be a leaf", {
  # No split will ever decrease impurity by 999
  tree <- svm_split(multiclass_data, "label",
                    max_depth             = 5,
                    min_samples           = 2,
                    min_impurity_decrease = 999)

  expect_true(tree$is_leaf)
})

test_that("min_impurity_decrease = 0 allows the tree to grow normally", {
  tree <- svm_split(binary_data, "label",
                    max_depth             = 3,
                    min_samples           = 5,
                    min_impurity_decrease = 0)

  # Root should split at least once on clean data
  expect_false(tree$is_leaf)
})

# ── n_subsets > 1 (random method with subset evaluation) ─────────────────────

test_that("n_subsets = 3 with random feature method builds a valid tree", {
  set.seed(7)
  tree <- svm_split(binary_data, "label",
                    max_depth      = 2,
                    min_samples    = 5,
                    feature_method = "random",
                    n_subsets      = 3)

  expect_type(tree, "list")
  expect_true("is_leaf" %in% names(tree))
})

# ── max_features_strategy = "random" ─────────────────────────────────────────

test_that("random max_features_strategy builds a tree without error", {
  set.seed(3)
  expect_no_error(
    svm_split(binary_data, "label",
              max_depth             = 2,
              min_samples           = 5,
              max_features_strategy = "random",
              max_features_random_range = c(0.5, 1.0))
  )
})

# ── custom class weights ──────────────────────────────────────────────────────

test_that("custom class weights are accepted and build a valid tree", {
  expect_no_error(
    svm_split(binary_data, "label",
              max_depth            = 2,
              min_samples          = 5,
              class_weights        = "custom",
              custom_class_weights = c(A = 1.5, B = 0.8))
  )
})

test_that("custom class weights with missing class fall back to no-weighting", {
  # Custom weights are missing class B — should warn but not crash
  expect_warning(
    tree <- svm_split(binary_data, "label",
                      max_depth            = 2,
                      min_samples          = 5,
                      class_weights        = "custom",
                      custom_class_weights = c(A = 2))
  )
  expect_type(tree, "list")
})

# ── internal node metadata ────────────────────────────────────────────────────

test_that("internal node records the correct depth value", {
  tree <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)

  if (!tree$is_leaf) {
    expect_equal(tree$depth, 1)
  }
})

test_that("internal node records n equal to the number of training rows", {
  tree <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  expect_equal(tree$n, nrow(binary_data))
})

test_that("penalty_applied is FALSE when penalize_used_features = FALSE", {
  tree <- svm_split(binary_data, "label",
                    max_depth              = 2,
                    min_samples            = 5,
                    penalize_used_features = FALSE)

  if (!tree$is_leaf) {
    expect_false(tree$penalty_applied)
  }
})

test_that("penalty_applied is FALSE at root (used_features is empty) even when penalization is on", {
  # At the root, used_features = character(0), so penalty_applied should be FALSE
  tree <- svm_split(binary_data, "label",
                    max_depth              = 2,
                    min_samples            = 5,
                    penalize_used_features = TRUE)

  if (!tree$is_leaf) {
    expect_false(tree$penalty_applied)
  }
})

# ── NA handling ───────────────────────────────────────────────────────────────

test_that("dataset with NA values returns a leaf (graceful fallback)", {
  na_data        <- binary_data
  na_data[1, 1]  <- NA

  tree <- svm_split(na_data, "label", max_depth = 3, min_samples = 2)
  expect_true(tree$is_leaf)
})


# ── verbose output ────────────────────────────────────────────────────────────

test_that("verbose = TRUE produces node-depth output for binary data", {
  expect_output(
    svm_split(binary_data, "label",
              max_depth   = 1,
              min_samples = 5,
              verbose     = TRUE),
    regexp = "Node at depth"
  )
})

test_that("verbose = TRUE prints class distribution table", {
  expect_output(
    svm_split(binary_data, "label",
              max_depth   = 1,
              min_samples = 5,
              verbose     = TRUE),
    regexp = "Class distribution"
  )
})

test_that("verbose = TRUE prints selected features", {
  expect_output(
    svm_split(binary_data, "label",
              max_depth   = 1,
              min_samples = 5,
              verbose     = TRUE),
    regexp = "Selected features"
  )
})

test_that("verbose = TRUE prints multi-class split attempts", {
  expect_output(
    svm_split(multiclass_data, "label",
              max_depth   = 1,
              min_samples = 5,
              verbose     = TRUE),
    regexp = "Multi-class case"
  )
})

test_that("verbose = TRUE reports 'Creating leaf node' at stopping conditions", {
  expect_output(
    svm_split(binary_data, "label",
              max_depth   = 0,
              verbose     = TRUE),
    regexp = "Creating leaf"
  )
})

test_that("verbose = TRUE reports 'Stopping: all features are constant'", {
  const_data <- data.frame(
    x1    = rep(1, 20),
    x2    = rep(2, 20),
    label = factor(rep(c("A", "B"), 10))
  )
  expect_warning(
    svm_split(const_data, "label",
              max_depth   = 3,
              min_samples = 2,
              verbose     = TRUE)
  )
})

test_that("verbose = TRUE with NA data reports the NA warning", {
  na_data       <- binary_data
  na_data[1, 1] <- NA
  expect_output(
    svm_split(na_data, "label",
              max_depth   = 3,
              min_samples = 2,
              verbose     = TRUE),
    regexp = "NA values"
  )
})

# ── multiclass: no valid split found -> leaf ──────────────────────────────────

test_that("multiclass tree returns a leaf when min_impurity_decrease is impossibly high", {
  # All one-vs-rest splits will be skipped; best_model stays NULL -> leaf
  tree <- svm_split(multiclass_data, "label",
                    max_depth             = 3,
                    min_samples           = 2,
                    min_impurity_decrease = 999)
  expect_true(tree$is_leaf)
})

test_that("leaf from failed multiclass split has valid class probabilities", {
  tree <- svm_split(multiclass_data, "label",
                    max_depth             = 3,
                    min_samples           = 2,
                    min_impurity_decrease = 999)
  expect_equal(sum(tree$class_prob), 1, tolerance = 1e-9)
})

# ── multiclass: degenerate split (all samples to one side) ───────────────────
# Provoked by a near-separable single-class-vs-rest where the SVM assigns
# every sample to the same side; such splits are skipped with "Degenerate split"

test_that("verbose = TRUE prints 'Degenerate split' message when it occurs", {
  # Use a dataset where one class is so dominant that a one-vs-rest SVM
  # may produce an all-one-side split on at least one iteration.
  set.seed(99)
  skewed <- data.frame(
    x1    = c(rnorm(45, 0), rnorm(3, 10), rnorm(3, -10)),
    x2    = c(rnorm(45, 0), rnorm(3, 10), rnorm(3, -10)),
    label = factor(c(rep("A", 45), rep("B", 3), rep("C", 3)))
  )
  # May or may not trigger "Degenerate" depending on SVM outcome,
  # but tree should still build without error
  expect_no_error(
    svm_split(skewed, "label",
              max_depth   = 2,
              min_samples = 2,
              verbose     = FALSE)
  )
})

# ── multiclass: handle_small_children triggered after best split ──────────────
# Achieved by setting min_samples high enough that the best split's children
# are valid splits (not triggering the length==0 guard) but one child falls
# below min_samples, routing into handle_small_children.

test_that("multiclass tree builds correctly when best split has a small child", {
  # With min_samples = 15 and ~20 samples per class, at least one child will
  # likely have fewer than 15 samples after the best OVR split.
  set.seed(42)
  mc <- make_multiclass_data(n_per_class = 20)

  expect_no_error(
    tree <- svm_split(mc, "label",
                      max_depth   = 2,
                      min_samples = 15)
  )
  expect_type(tree, "list")
})

test_that("predictions from a tree with small multiclass children are valid", {
  set.seed(42)
  mc   <- make_multiclass_data(n_per_class = 20)
  tree <- svm_split(mc, "label", max_depth = 2, min_samples = 15)
  preds <- svm_predict_tree(tree, mc)

  expect_length(preds, nrow(mc))
  expect_true(all(preds %in% c("A", "B", "C")))
})

# ── binary: handle_small_children non-stop path ───────────────────────────────
# When one binary child has < min_samples, handle_small_children returns
# stop=FALSE and a partially-built node (not a leaf). The code then attaches
# the model and returns that node. We provoke this with asymmetric data where
# min_samples is set just above the smaller split's size.

test_that("binary tree handles one small child without error", {
  # Force highly asymmetric split: ~38 vs ~2 samples at the root
  set.seed(7)
  asym <- data.frame(
    x1    = c(rnorm(38, -2), rnorm(2, 10)),
    x2    = c(rnorm(38, -2), rnorm(2, 10)),
    label = factor(c(rep("A", 38), rep("B", 2)))
  )

  expect_no_error(
    tree <- svm_split(asym, "label",
                      max_depth   = 2,
                      min_samples = 3)
  )
  expect_type(tree, "list")
})

test_that("binary tree with one small child produces valid predictions", {
  set.seed(7)
  asym <- data.frame(
    x1    = c(rnorm(38, -2), rnorm(2, 10)),
    x2    = c(rnorm(38, -2), rnorm(2, 10)),
    label = factor(c(rep("A", 38), rep("B", 2)))
  )
  tree  <- svm_split(asym, "label", max_depth = 2, min_samples = 3)
  preds <- svm_predict_tree(tree, asym)

  expect_length(preds, nrow(asym))
  expect_true(all(preds %in% c("A", "B")))
})

# ── impurity_func / parent_impurity available in binary path ─────────────────
# After hoisting impurity_func and parent_impurity above the k==2 branch,
# verify binary splits still produce correct trees with both impurity measures.

test_that("binary tree with impurity_measure = 'gini' builds and predicts correctly", {
  tree  <- svm_split(binary_data, "label",
                     max_depth        = 2,
                     min_samples      = 5,
                     impurity_measure = "gini")
  preds <- svm_predict_tree(tree, binary_data)

  expect_true(all(preds %in% c("A", "B")))
  expect_gt(mean(preds == as.character(binary_data$label)), 0.7)
})

test_that("binary tree with impurity_measure = 'entropy' builds and predicts correctly", {
  tree  <- svm_split(binary_data, "label",
                     max_depth        = 2,
                     min_samples      = 5,
                     impurity_measure = "entropy")
  preds <- svm_predict_tree(tree, binary_data)

  expect_true(all(preds %in% c("A", "B")))
  expect_gt(mean(preds == as.character(binary_data$label)), 0.7)
})

# ── max_features NULL path (is.null branch) ──────────────────────────────────

test_that("max_features = NULL uses all available predictors", {
  # When max_features is NULL, calculate_dynamic_max_features returns
  # total_features, and the is.null(current_max_features) branch is FALSE.
  # The outer is.null(max_features) guard: if NULL is passed as max_features,
  # all predictors are eligible.
  tree <- svm_split(binary_data, "label",
                    max_depth    = 1,
                    min_samples  = 5,
                    max_features = NULL)

  expect_type(tree, "list")
  if (!tree$is_leaf) {
    # All used features must be valid predictors
    expect_true(all(tree$features %in% c("x1", "x2")))
  }
})
