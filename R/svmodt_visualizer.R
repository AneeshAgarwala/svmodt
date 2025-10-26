# Create a grid for decision boundary plotting
#' @keywords internal
create_decision_grid <- function(data, features, resolution = 50) {
  if (length(features) == 2) {
    # 2D grid
    x_range <- range(data[, features[1]], na.rm = TRUE)
    y_range <- range(data[, features[2]], na.rm = TRUE)

    # Add padding
    x_padding <- diff(x_range) * 0.1
    y_padding <- diff(y_range) * 0.1

    x_seq <- seq(x_range[1] - x_padding, x_range[2] + x_padding, length.out = resolution)
    y_seq <- seq(y_range[1] - y_padding, y_range[2] + y_padding, length.out = resolution)

    grid <- expand.grid(x = x_seq, y = y_seq)
    names(grid) <- features[1:2]

    return(grid)
  } else if (length(features) == 3) {
    # 3D grid - create 2D slices
    ranges <- sapply(features, function(f) range(data[, f], na.rm = TRUE))

    x_seq <- seq(ranges[1, 1], ranges[2, 1], length.out = resolution)
    y_seq <- seq(ranges[1, 2], ranges[2, 2], length.out = resolution)
    z_fixed <- median(data[, features[3]], na.rm = TRUE)

    grid <- expand.grid(x = x_seq, y = y_seq)
    names(grid) <- features[1:2]
    grid[, features[3]] <- z_fixed

    return(grid)
  }
}

# Visualize a single SVM decision boundary
#' @keywords internal
plot_svm_boundary <- function(data, features, svm_model, scaler,
                              response_col = NULL,
                              title = "SVM Decision Boundary",
                              use_tree_prediction = FALSE,
                              tree = NULL) {
  if (length(features) < 2) {
    stop("Need at least 2 features for plotting")
  }

  # FIXED: Identify response column properly
  if (is.null(response_col)) {
    # Try to find response: should be factor/character column not in features
    non_feature_cols <- setdiff(names(data), features)
    factor_cols <- sapply(data[non_feature_cols], function(x) is.factor(x) || is.character(x))
    if (any(factor_cols)) {
      response_col <- non_feature_cols[factor_cols][1]
      cat("Auto-detected response column:", response_col, "\n")
    } else {
      warning("Could not identify response column, using first column")
      response_col <- names(data)[1]
    }
  }

  # Validate response exists
  if (!response_col %in% names(data)) {
    stop("Response column '", response_col, "' not found in data")
  }

  # Use first two features for plotting
  plot_features <- features[1:2]
  grid <- create_decision_grid(data, plot_features)

  # Add other features if they exist (set to median)
  if (length(features) > 2) {
    for (feat in features[3:length(features)]) {
      grid[, feat] <- median(data[, feat], na.rm = TRUE)
    }
  }

  # FIXED: Ensure proper column order
  grid <- grid[, features, drop = FALSE]

  # Scale using provided scaler
  grid_scaled <- scaler$transform(grid)

  # CRITICAL FIX: Handle decision values properly for multiclass
  svm_result <- predict(svm_model, grid_scaled, decision.values = TRUE)
  dec_attr <- attr(svm_result, "decision.values")

  # Use first column (or best_col if available from tree)
  if (is.matrix(dec_attr)) {
    decision_values <- dec_attr[, 1]
  } else {
    decision_values <- as.numeric(dec_attr)
  }

  # Get SVM predictions directly
  predictions <- predict(svm_model, grid_scaled)

  # OPTIONAL: Compare with tree predictions
  if (use_tree_prediction && !is.null(tree)) {
    # Create dummy response for grid
    grid_with_response <- grid
    grid_with_response[[response_col]] <- levels(data[[response_col]])[1]

    # Get tree predictions
    tree_predictions <- svm_predict_tree(tree, grid_with_response)

    # Add to plot data
    grid$tree_prediction <- tree_predictions
    grid$svm_agrees <- (predictions == tree_predictions)
  }

  # Prepare plot data
  plot_data <- grid
  plot_data$svm_prediction <- predictions
  plot_data$decision_value <- decision_values
  plot_data$boundary <- ifelse(decision_values > 0, "Left", "Right")

  # FIXED: Use actual response column for point colors
  data_for_plot <- data[, c(plot_features, response_col)]
  names(data_for_plot)[3] <- "response"

  # Create the plot
  p <- ggplot2::ggplot() +
    # Decision boundary regions
    ggplot2::geom_tile(
      data = plot_data,
      ggplot2::aes(
        x = .data[[plot_features[1]]],
        y = .data[[plot_features[2]]],
        fill = boundary
      ),
      alpha = 0.3
    ) +
    # Decision boundary line (where decision_value = 0)
    ggplot2::geom_contour(
      data = plot_data,
      ggplot2::aes(
        x = .data[[plot_features[1]]],
        y = .data[[plot_features[2]]],
        z = decision_value
      ),
      breaks = 0, color = "black", linewidth = 1.5
    ) +
    # FIXED: Original data points with CORRECT response coloring
    ggplot2::geom_point(
      data = data_for_plot,
      ggplot2::aes(
        x = .data[[plot_features[1]]],
        y = .data[[plot_features[2]]],
        color = response
      ),
      size = 2, alpha = 0.8
    ) +
    ggplot2::scale_fill_manual(
      values = c("Left" = "lightblue", "Right" = "lightcoral"),
      name = "SVM Side"
    ) +
    ggplot2::scale_color_brewer(palette = "Set1", name = "Actual Class") +
    ggplot2::labs(
      title = title,
      x = plot_features[1],
      y = plot_features[2]
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(size = 10, face = "bold"),
      aspect.ratio = 1
    )

  return(list(plot = p, grid_data = plot_data, response_col = response_col))
}


#' Visualize a Decision Tree SVM
#'
#' Generates plots of SVM decision boundaries for each node in a decision tree SVM.
#' Optionally, it can calculate accuracy at each node and return the data used for plotting.
#'
#' @param tree A decision tree SVM object created using `svm_split()`.
#' @param original_data A data frame containing the original training data.
#' @param features A character vector of feature names to be used for plotting.
#' @param response_col Optional. Name of the response column. If `NULL`, it will be auto-detected.
#' @param max_depth Optional. Maximum depth to visualize. If `NULL`, the entire tree is visualized.
#' @param check_accuracy Logical. Whether to compute node-level accuracy for the original data.
#'
#' @return A list containing:
#'   \item{plots}{A named list of ggplot2 objects for each node.}
#'   \item{grid_data}{A named list of data frames used for plotting decision boundaries.}
#'   \item{accuracy_info}{A named list of node-level accuracy information.}
#'   \item{response_col}{The name of the response column used.}
#'
#' @examples
#' \dontrun{
#' # tree <- svm_split(train_data, response = "Class")
#' viz <- visualize_dtsvm_tree(tree, train_data, features = c("X1", "X2"))
#' viz$plots$depth_1_Root
#' }
#'
#' @export
visualize_svm_tree <- function(tree, original_data, features,
                                 response_col = NULL,
                                 max_depth = NULL,
                                 check_accuracy = TRUE) {
  plots <- list()
  plot_data_list <- list()
  accuracy_info <- list()

  # Auto-detect response if not provided
  if (is.null(response_col)) {
    non_feature_cols <- setdiff(names(original_data), features)
    factor_cols <- sapply(
      original_data[non_feature_cols],
      function(x) is.factor(x) || is.character(x)
    )
    if (any(factor_cols)) {
      response_col <- non_feature_cols[factor_cols][1]
      cat("Using response column:", response_col, "\n")
    }
  }

  # Recursive function to traverse tree and create plots
  traverse_tree <- function(node, data_subset, depth = 1, path = "Root") {
    if (is.null(max_depth) || depth <= max_depth) {
      if (!node$is_leaf && !is.null(node$model)) {
        # Check accuracy at this node
        node_accuracy <- NA
        if (check_accuracy && !is.null(response_col) && response_col %in% names(data_subset)) {
          # Get predictions for this subset
          subset_preds <- predict(
            node$model,
            node$scaler$transform(data_subset[, node$features])
          )
          node_accuracy <- mean(subset_preds == data_subset[[response_col]])
        }

        # Create title with path and accuracy information
        title <- paste0(
          "Depth ", depth, " - ", path
          #                        "\nFeatures: ", paste(node$features, collapse = ", "),
          #                        "\nSamples: ", nrow(data_subset)
        )

        if (!is.na(node_accuracy)) {
          title <- paste0(
            title, "\nNode Accuracy: ",
            round(node_accuracy * 100, 1), "%"
          )
        }

        # Create plot
        plot_result <- plot_svm_boundary(
          data = data_subset,
          features = node$features,
          svm_model = node$model,
          scaler = node$scaler,
          response_col = response_col,
          title = title,
          use_tree_prediction = FALSE
        )

        plot_name <- paste0("depth_", depth, "_", gsub(" ", "_", path))
        plots[[plot_name]] <<- plot_result$plot
        plot_data_list[[plot_name]] <<- plot_result$grid_data

        # Store accuracy info
        accuracy_info[[plot_name]] <<- list(
          depth = depth,
          path = path,
          n_samples = nrow(data_subset),
          accuracy = node_accuracy
        )

        # Get decision values for the actual data to split it
        if (nrow(data_subset) > 0) {
          # Scale data using node's scaler
          X_scaled <- node$scaler$transform(data_subset[, node$features, drop = FALSE])

          # Get decision values
          dec <- attr(
            predict(node$model, X_scaled, decision.values = TRUE),
            "decision.values"
          )
          decision_values <- if (is.matrix(dec)) dec[, 1] else as.numeric(dec)

          # CRITICAL: Use same split logic as predict_tree
          left_idx <- which(decision_values > 0)
          right_idx <- which(decision_values <= 0)

          # Recurse to children
          if (!is.null(node$left) && length(left_idx) > 0) {
            traverse_tree(
              node$left, data_subset[left_idx, , drop = FALSE],
              depth + 1, paste(path, "\u2192 L")
            )
          }

          if (!is.null(node$right) && length(right_idx) > 0) {
            traverse_tree(
              node$right, data_subset[right_idx, , drop = FALSE],
              depth + 1, paste(path, "\u2192 R")
            )
          }
        }
      }
    }
  }

  # Start traversal
  traverse_tree(tree, original_data)

  return(list(
    plots = plots,
    grid_data = plot_data_list,
    accuracy_info = accuracy_info,
    response_col = response_col
  ))
}
