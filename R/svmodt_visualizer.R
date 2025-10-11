library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(dplyr)


# Create a grid for decision boundary plotting
create_decision_grid <- function(data, features, resolution = 50) {

  if (length(features) == 2) {
    # 2D grid
    x_range <- range(data[, features[1]])
    y_range <- range(data[, features[2]])

    x_seq <- seq(x_range[1] - 0.5, x_range[2] + 0.5, length.out = resolution)
    y_seq <- seq(y_range[1] - 0.5, y_range[2] + 0.5, length.out = resolution)

    grid <- expand.grid(x = x_seq, y = y_seq)
    names(grid) <- features[1:2]

    return(grid)

  } else if (length(features) == 3) {
    # 3D grid - we'll create 2D slices
    ranges <- sapply(features, function(f) range(data[, f]))

    # Create grid for first two features, fix third at median
    x_seq <- seq(ranges[1,1] - 0.5, ranges[2,1] + 0.5, length.out = resolution)
    y_seq <- seq(ranges[1,2] - 0.5, ranges[2,2] + 0.5, length.out = resolution)
    z_fixed <- median(data[, features[3]])

    grid <- expand.grid(x = x_seq, y = y_seq)
    names(grid) <- features[1:2]
    grid[, features[3]] <- z_fixed

    return(grid)
  }
}

# Visualize a single SVM decision boundary
# Visualize a single SVM decision boundary
plot_svm_boundary <- function(data, features, svm_model, scaler, title = "SVM Decision Boundary") {

  if (length(features) < 2) {
    stop("Need at least 2 features for plotting")
  }

  # Use first two features for plotting
  plot_features <- features[1:2]
  grid <- create_decision_grid(data, plot_features)

  # Add other features if they exist (set to median)
  if (length(features) > 2) {
    for (feat in features[3:length(features)]) {
      grid[, feat] <- median(data[, feat])
    }
  }

  # FIXED: Use the scaler parameter, not node$scaler
  grid_scaled <- scaler$transform(grid[, features])

  # Get SVM predictions and decision values
  predictions <- predict(svm_model, grid_scaled)
  decision_values <- as.numeric(attr(predict(svm_model, grid_scaled, decision.values = TRUE),
                                     "decision.values"))

  # Prepare plot data
  plot_data <- grid
  plot_data$prediction <- predictions
  plot_data$decision_value <- decision_values
  plot_data$boundary <- ifelse(decision_values > 0, "Left", "Right")

  # Create the plot
  p <- ggplot() +
    # Decision boundary regions
    geom_tile(data = plot_data,
              aes_string(x = plot_features[1], y = plot_features[2],
                         fill = "boundary"),
              alpha = 0.3) +
    # Decision boundary line (where decision_value ≈ 0)
    geom_contour(data = plot_data,
                 aes_string(x = plot_features[1], y = plot_features[2],
                            z = "decision_value"),
                 breaks = 0, color = "black", linewidth = 1.5) +
    # Original data points
    geom_point(data = data,
               aes_string(x = plot_features[1], y = plot_features[2],
                          color = names(data)[1]),  # Use first column as response
               size = 1, alpha = 0.8) +
    scale_fill_manual(values = c("Left" = "lightblue", "Right" = "lightcoral")) +
    scale_color_manual(values = c("red", "blue")) +
    labs(subtitle = title,
         x = plot_features[1],
         y = plot_features[2]) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.subtitle = element_text(size = 8),
          aspect.ratio = 1)

  return(list(plot = p, grid_data = plot_data))
}

visualize_dtsvm_tree <- function(tree, original_data, features, max_depth = NULL) {

  plots <- list()
  plot_data_list <- list()

  # Recursive function to traverse tree and create plots
  traverse_tree <- function(node, data_subset, depth = 1, path = "") {

    if (is.null(max_depth) || depth <= max_depth) {

      if (!node$is_leaf && !is.null(node$model)) {

        # Create title with path information
        title <- paste0("Depth ", depth, " - ", path,
                        "\nFeatures: ", paste(node$features, collapse = ", "),
                        "\nSamples: ", nrow(data_subset))

        # FIXED: Pass node$scaler as a parameter
        plot_result <- plot_svm_boundary(
          data = data_subset,
          features = node$features,
          svm_model = node$model,
          scaler = node$scaler,  # Pass the scaler!
          title = title
        )

        plots[[paste0("depth_", depth, "_", gsub(" ", "", path))]] <<- plot_result$plot
        plot_data_list[[paste0("depth_", depth, "_", gsub(" ", "", path))]] <<- plot_result$grid_data

        # Get decision values for the actual data to split it
        if (nrow(data_subset) > 0) {

          # Scale data using node's scaler
          X_scaled <- node$scaler$transform(data_subset[, node$features, drop = FALSE])

          # Get decision values
          dec <- attr(predict(node$model, X_scaled, decision.values = TRUE), "decision.values")
          dec_vals <- if (is.matrix(dec)) dec[, 1] else as.numeric(dec)

          left_idx <- which(dec_vals > 0)
          right_idx <- which(dec_vals <= 0)

          # Recurse to children
          if (!is.null(node$left) && length(left_idx) > 0) {
            traverse_tree(node$left, data_subset[left_idx, , drop = FALSE],
                          depth + 1, paste(path, "Left"))
          }

          if (!is.null(node$right) && length(right_idx) > 0) {
            traverse_tree(node$right, data_subset[right_idx, , drop = FALSE],
                          depth + 1, paste(path, "Right"))
          }
        }
      }
    }
  }

  # Start traversal
  traverse_tree(tree, original_data)

  return(list(plots = plots, grid_data = plot_data_list))
}
