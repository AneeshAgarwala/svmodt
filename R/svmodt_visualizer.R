utils::globalVariables(c("boundary", "decision_value", "response"))


# Internal helpers


#' Collect every feature name used anywhere in the tree (depth-first)
#' @keywords internal
get_tree_features <- function(tree) {
  if (tree$is_leaf) {
    return(tree$features)
  }
  unique(c(
    tree$features,
    if (!is.null(tree$left)) get_tree_features(tree$left),
    if (!is.null(tree$right)) get_tree_features(tree$right)
  ))
}

#' Check whether a decision-value vector crosses zero
#' @keywords internal
boundary_in_grid <- function(dec_values) {
  any(dec_values > 0) && any(dec_values <= 0)
}

#' Build a 2-D prediction grid in ORIGINAL (unscaled) feature space
#'
#' The two plot features vary over their observed range plus padding; every
#' other node feature is fixed at its median.  Returned unscaled so axis
#' labels stay readable; callers scale it themselves before predicting.
#'
#' @keywords internal
create_decision_grid <- function(data, plot_features, all_node_features,
                                 resolution = 100, pad_factor = 0.5) {
  x_range <- range(data[[plot_features[1]]], na.rm = TRUE)
  y_range <- range(data[[plot_features[2]]], na.rm = TRUE)

  x_seq <- seq(x_range[1] - diff(x_range) * pad_factor,
    x_range[2] + diff(x_range) * pad_factor,
    length.out = resolution
  )
  y_seq <- seq(y_range[1] - diff(y_range) * pad_factor,
    y_range[2] + diff(y_range) * pad_factor,
    length.out = resolution
  )

  grid <- expand.grid(x_seq, y_seq)
  names(grid) <- plot_features[1:2]

  for (feat in setdiff(all_node_features, plot_features[1:2])) {
    grid[[feat]] <- stats::median(data[[feat]], na.rm = TRUE)
  }

  grid[, all_node_features, drop = FALSE]
}

#' Plot the SVM decision boundary for a single internal node
#'
#' Internal workhorse called by \code{plot_boundary} for each node during
#' tree traversal.  Builds the grid in original space, scales it with the
#' node's own scaler, predicts decision values, and returns a ggplot2 object
#' together with metadata.  The grid is expanded automatically (up to
#' \code{pad_factor = 3}) if the hyperplane falls outside the data range.
#'
#' @keywords internal
plot_node_boundary <- function(data, node_features, svm_model, scaler,
                               response_col,
                               title = "SVM Decision Boundary",
                               resolution = 100) {
  if (length(node_features) < 2) {
    stop("Need at least 2 features for plotting")
  }
  if (!response_col %in% names(data)) {
    stop("Response column '", response_col, "' not found in data")
  }

  plot_features <- node_features[1:2]

  #  Expand grid until the hyperplane is visible
  pad_factor <- 0.5
  repeat {
    grid_original <- create_decision_grid(
      data, plot_features, node_features,
      resolution, pad_factor
    )
    grid_scaled <- scaler$transform(grid_original)
    if (ncol(grid_scaled) == 0) {
      stop("Scaler returned empty data frame - node features may not match scaler columns")
    }

    svm_res <- stats::predict(svm_model, grid_scaled, decision.values = TRUE)
    dec_attr <- attr(svm_res, "decision.values")
    dec_values <- if (is.matrix(dec_attr)) dec_attr[, 1] else as.numeric(dec_attr)

    if (boundary_in_grid(dec_values) || pad_factor >= 3) break
    pad_factor <- pad_factor + 0.5
  }

  boundary_visible <- boundary_in_grid(dec_values)

  #  Display window = data range + 10 %
  x_rng <- range(data[[plot_features[1]]], na.rm = TRUE)
  y_rng <- range(data[[plot_features[2]]], na.rm = TRUE)
  x_disp <- diff(x_rng) * 0.1
  y_disp <- diff(y_rng) * 0.1

  plot_data <- grid_original[, plot_features, drop = FALSE]
  plot_data$decision_value <- dec_values
  plot_data$boundary <- factor(ifelse(dec_values > 0, "Left", "Right"),
    levels = c("Left", "Right")
  )

  # Clip tile background to display window (contour uses full grid)
  tile_data <- plot_data[
    plot_data[[plot_features[1]]] >= x_rng[1] - x_disp &
      plot_data[[plot_features[1]]] <= x_rng[2] + x_disp &
      plot_data[[plot_features[2]]] >= y_rng[1] - y_disp &
      plot_data[[plot_features[2]]] <= y_rng[2] + y_disp,
  ]

  data_pts <- data[, c(plot_features, response_col), drop = FALSE]
  names(data_pts)[3] <- "response"

  present_sides <- unique(as.character(tile_data$boundary))
  fill_vals <- c("Left" = "lightblue", "Right" = "lightcoral")
  fill_vals <- fill_vals[names(fill_vals) %in% present_sides]

  subtitle <- if (!boundary_visible) {
    majority <- names(which.max(table(plot_data$boundary)))
    paste0(
      "\u26a0 Boundary outside plotted range (all points \u2192 ",
      majority, " branch)"
    )
  } else {
    NULL
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = tile_data,
      ggplot2::aes(
        x = .data[[plot_features[1]]],
        y = .data[[plot_features[2]]],
        fill = boundary
      ),
      alpha = 0.35
    )

  if (boundary_visible) {
    p <- p + ggplot2::geom_contour(
      data = plot_data,
      ggplot2::aes(
        x = .data[[plot_features[1]]],
        y = .data[[plot_features[2]]],
        z = decision_value
      ),
      breaks = 0, color = "black", linewidth = 1.5
    )
  }

  p <- p +
    ggplot2::geom_point(
      data = data_pts,
      ggplot2::aes(
        x = .data[[plot_features[1]]],
        y = .data[[plot_features[2]]],
        color = response
      ),
      size = 2, alpha = 0.85
    ) +
    ggplot2::scale_fill_manual(
      values = fill_vals, name = "SVM Side",
      drop = FALSE
    ) +
    ggplot2::scale_color_brewer(palette = "Set1", name = "Actual Class") +
    ggplot2::coord_cartesian(
      xlim = c(x_rng[1] - x_disp, x_rng[2] + x_disp),
      ylim = c(y_rng[1] - y_disp, y_rng[2] + y_disp)
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = plot_features[1],
      y = plot_features[2]
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      plot.title      = ggplot2::element_text(size = 10, face = "bold"),
      plot.subtitle   = ggplot2::element_text(size = 8, color = "darkorange"),
      aspect.ratio    = 1
    )

  list(
    plot = p,
    grid_data = plot_data,
    response_col = response_col,
    boundary_visible = boundary_visible,
    pad_factor_used = pad_factor
  )
}


# plot_boundary - all-node hyperplane panels


#' Plot SVM decision boundaries for every node in the tree
#'
#' Traverses the tree recursively and produces one plot per internal node,
#' showing the SVM hyperplane for that node's binary split, the background
#' region colouring, and the actual data points (coloured by true class).
#' Each node receives only the subset of data that reaches it during training.
#'
#' @param tree An \code{svmodt_node} object returned by \code{\link{svm_split}}.
#' @param data The original training data frame.
#' @param response_col Character string naming the response column in
#'   \code{data}.  Auto-detected when \code{NULL} (first factor/character
#'   column not used as a predictor).
#' @param max_depth Maximum tree depth to visualise.  \code{NULL} (default)
#'   shows all nodes.
#' @param check_accuracy Logical; if \code{TRUE} (default), compute and
#'   display training accuracy at each node.
#' @param resolution Integer; grid resolution per axis (default \code{100}).
#'   Increase for smoother boundaries at the cost of speed.
#'
#' @return Invisibly returns a list with four elements:
#' \describe{
#'   \item{\code{plots}}{Named list of \pkg{ggplot2} objects, one per node.
#'     Names encode depth and path, e.g. \code{"depth_1_Root"},
#'     \code{"depth_2_Root_L"}.}
#'   \item{\code{grid_data}}{Named list of data frames (full expanded grid
#'     used for each node's contour calculation).}
#'   \item{\code{accuracy_info}}{Named list of per-node metadata: depth, path,
#'     sample count, accuracy, features, whether the boundary was visible, and
#'     the pad factor that was needed.}
#'   \item{\code{response_col}}{The response column name used.}
#' }
#'
#' @examples
#' \dontrun{
#' tree <- svm_split(wdbc, response = "diagnosis", max_depth = 3)
#'
#' viz <- plot_boundary(tree, wdbc, response_col = "diagnosis")
#'
#' # Access individual node plots
#' viz$plots[[1]] # root node
#' viz$plots[[2]] # first child
#'
#' # Display all node plots in a grid
#' library(patchwork)
#' patchwork::wrap_plots(viz$plots, ncol = 2)
#' }
#' @export
plot_boundary <- function(tree, data,
                          response_col = NULL,
                          max_depth = NULL,
                          check_accuracy = TRUE,
                          resolution = 100) {
  plots <- list()
  plot_data_list <- list()
  accuracy_info <- list()

  #  Auto-detect response column
  if (is.null(response_col)) {
    non_feat <- setdiff(names(data), tree$features)
    is_fac <- sapply(
      data[non_feat],
      function(x) is.factor(x) || is.character(x)
    )
    if (any(is_fac)) {
      response_col <- non_feat[is_fac][1]
      message("plot_boundary: using response column '", response_col, "'")
    } else {
      stop("plot_boundary: could not detect response column; supply 'response_col'")
    }
  }

  #  Recursive traversal
  traverse <- function(node, data_subset, depth = 1, path = "Root") {
    if (!is.null(max_depth) && depth > max_depth) {
      return(invisible(NULL))
    }
    if (node$is_leaf || is.null(node$model)) {
      return(invisible(NULL))
    }

    node_features <- node$features

    # Per-node accuracy
    node_accuracy <- NA_real_
    if (check_accuracy &&
      response_col %in% names(data_subset) &&
      length(node_features) > 0) {
      X_sub <- node$scaler$transform(
        data_subset[, node_features, drop = FALSE]
      )
      preds <- stats::predict(node$model, X_sub)
      node_accuracy <- mean(as.character(preds) ==
        as.character(data_subset[[response_col]]))
    }

    # Build title
    title <- paste0("Depth ", depth, " \u2014 ", path)
    if (!is.na(node_accuracy)) {
      title <- paste0(
        title, "\nNode accuracy: ",
        round(node_accuracy * 100, 1), "%"
      )
    }
    if (!is.null(node$hyperplane_class)) {
      title <- paste0(title, "\nSplit: ", node$hyperplane_class, " vs rest")
    }

    # Plot this node
    result <- tryCatch(
      plot_node_boundary(
        data_subset, node_features, node$model, node$scaler,
        response_col, title, resolution
      ),
      error = function(e) {
        warning(
          "plot_boundary: could not plot depth ", depth,
          " (", path, "): ", e$message
        )
        NULL
      }
    )

    if (!is.null(result)) {
      key <- paste0(
        "depth_", depth, "_",
        gsub("[^A-Za-z0-9]", "_", path)
      )
      plots[[key]] <<- result$plot
      plot_data_list[[key]] <<- result$grid_data
      accuracy_info[[key]] <<- list(
        depth            = depth,
        path             = path,
        n_samples        = nrow(data_subset),
        accuracy         = node_accuracy,
        features         = node_features,
        boundary_visible = result$boundary_visible,
        pad_factor_used  = result$pad_factor_used
      )
    }

    # Propagate correct data subsets to children
    if (nrow(data_subset) > 0 && length(node_features) > 0) {
      X_sc <- node$scaler$transform(
        data_subset[, node_features, drop = FALSE]
      )
      dec <- attr(
        stats::predict(node$model, X_sc, decision.values = TRUE),
        "decision.values"
      )
      dv <- if (is.matrix(dec)) dec[, 1] else as.numeric(dec)
      left_idx <- which(dv > 0)
      right_idx <- which(dv <= 0)

      if (!is.null(node$left) && length(left_idx) > 0) {
        traverse(
          node$left,
          data_subset[left_idx, , drop = FALSE],
          depth + 1, paste0(path, " L")
        )
      }
      if (!is.null(node$right) && length(right_idx) > 0) {
        traverse(
          node$right,
          data_subset[right_idx, , drop = FALSE],
          depth + 1, paste0(path, " R")
        )
      }
    }
  }

  traverse(tree, data)

  invisible(list(
    plots = plots,
    grid_data = plot_data_list,
    accuracy_info = accuracy_info,
    response_col = response_col
  ))
}


# plot_surface  - global multiclass decision surface


#' Plot the global decision surface of the full tree
#'
#' Predicts class labels across a 2-D grid using the complete tree (not
#' individual node SVMs), then overlays the original data points.  Because
#' predictions come from \code{\link{svm_predict_tree}}, multiclass trees are
#' handled correctly - each grid cell receives the final leaf prediction which
#' respects all OVR splits along the path.
#'
#' All features not used as plot axes are held fixed at their in-sample median
#' (numeric) or mode (categorical).  You choose which two features to plot via
#' \code{features}; if omitted the first two features used at the root node are
#' used.
#'
#' @param tree An \code{svmodt_node} object returned by \code{\link{svm_split}}.
#' @param data The original training data frame.
#' @param response Character string naming the response column in \code{data}.
#' @param features Character vector of length 2 giving the two features to plot
#'   on the x and y axes.  Defaults to the first two features used at the root.
#' @param resolution Integer; grid resolution per axis (default \code{200}).
#'   Higher values give smoother region boundaries.
#'
#' @return A \pkg{ggplot2} object.  The background tiles show the predicted
#'   class for each grid cell; points show true class labels.
#'
#' @examples
#' \dontrun{
#' tree <- svm_split(wdbc, response = "diagnosis", max_depth = 3)
#'
#' # Default: uses first two root features
#' plot_surface(tree, wdbc, response = "diagnosis")
#'
#' # Explicit feature choice
#' plot_surface(tree, wdbc,
#'   response = "diagnosis",
#'   features = c("radius_mean", "concavity_mean")
#' )
#'
#' # Multiclass (wine dataset)
#' tree_w <- svm_split(wine, response = "class", max_depth = 4)
#' plot_surface(tree_w, wine,
#'   response = "class",
#'   features = c("flavanoids", "color_intensity")
#' )
#' }
#' @export
plot_surface <- function(tree, data, response,
                         features = NULL,
                         resolution = 200) {
  if (!response %in% names(data)) {
    stop("plot_surface: response column '", response, "' not found in data")
  }

  #  Choose plot axes
  if (is.null(features)) {
    root_feats <- tree$features # features at the root node
    if (length(root_feats) < 2) {
      stop(
        "plot_surface: root node uses fewer than 2 features; ",
        "supply 'features' explicitly"
      )
    }
    features <- root_feats[1:2]
  }

  if (length(features) != 2) {
    stop("plot_surface: 'features' must be a character vector of length 2")
  }
  if (!all(features %in% names(data))) {
    stop(
      "plot_surface: features not found in data: ",
      paste(setdiff(features, names(data)), collapse = ", ")
    )
  }

  #  Build grid
  other_features <- setdiff(names(data), c(response, features))

  grid <- expand.grid(
    x = seq(min(data[[features[1]]], na.rm = TRUE),
      max(data[[features[1]]], na.rm = TRUE),
      length.out = resolution
    ),
    y = seq(min(data[[features[2]]], na.rm = TRUE),
      max(data[[features[2]]], na.rm = TRUE),
      length.out = resolution
    )
  )
  names(grid) <- features

  for (feat in other_features) {
    if (is.numeric(data[[feat]])) {
      grid[[feat]] <- stats::median(data[[feat]], na.rm = TRUE)
    } else {
      grid[[feat]] <- names(sort(table(data[[feat]]),
        decreasing = TRUE
      ))[1]
    }
  }

  # Reorder to match data column order (scalers are order-sensitive)
  grid <- grid[, intersect(names(data), names(grid)), drop = FALSE]

  #  Predict using the FULL tree
  pred_result <- svm_predict_tree(tree, grid,
    return_probs    = TRUE,
    calibrate_probs = TRUE
  )

  plot_data <- data.frame(
    x          = grid[[features[1]]],
    y          = grid[[features[2]]],
    prediction = factor(pred_result$predictions)
  )
  names(plot_data)[1:2] <- features

  #  Plot
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[[features[1]]],
      y = .data[[features[2]]]
    )
  ) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = prediction),
      alpha = 0.25
    ) +
    ggplot2::geom_point(
      data = data,
      ggplot2::aes(
        x = .data[[features[1]]],
        y = .data[[features[2]]],
        color = .data[[response]],
        shape = .data[[response]]
      ),
      size = 1.5,
      alpha = 0.6
    ) +
    ggplot2::scale_color_brewer(palette = "Dark2", name = "True class") +
    ggplot2::scale_fill_brewer(palette = "Dark2", name = "Predicted class") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(x = features[1], y = features[2]) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid      = ggplot2::element_blank(),
      panel.border    = ggplot2::element_rect(fill = NA),
      legend.position = "right",
      legend.text     = ggplot2::element_text(size = 7),
      legend.title    = ggplot2::element_text(size = 8),
      axis.title      = ggplot2::element_text(size = 8),
      aspect.ratio    = 1
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 1),
      color = ggplot2::guide_legend(order = 2)
    )

  p
}


# S3 plot method  - dispatches on plot.type


#' Plot method for svmodt_node objects
#'
#' Thin S3 wrapper that dispatches to \code{\link{plot_boundary}} or
#' \code{\link{plot_surface}} depending on \code{plot.type}.
#'
#' @param x An \code{svmodt_node} returned by \code{\link{svm_split}}.
#' @param y Ignored; present only to satisfy the \code{graphics::plot}
#'   generic signature.
#' @param ... Currently unused.
#' @param data The original training data frame (required).
#' @param response Character string naming the response column (required).
#' @param plot.type One of \code{"boundary"} (default) or \code{"surface"}.
#' @param features Length-2 character vector of axis features
#'   (\code{"surface"} only; default uses root node features).
#' @param max_depth Maximum depth to visualise
#'   (\code{"boundary"} only; default \code{NULL} = full tree).
#' @param check_accuracy Logical; show per-node accuracy
#'   (\code{"boundary"} only; default \code{TRUE}).
#' @param resolution Grid resolution per axis.
#'   Default \code{100} for \code{"boundary"}, \code{200} for
#'   \code{"surface"}.
#'
#' @return
#' \itemize{
#'   \item \code{"boundary"}: invisibly returns the list from
#'     \code{\link{plot_boundary}}.
#'   \item \code{"surface"}: invisibly returns the \pkg{ggplot2} object from
#'     \code{\link{plot_surface}}.
#' }
#'
#' @examples
#' \dontrun{
#' tree <- svm_split(wdbc, response = "diagnosis", max_depth = 3)
#'
#' # All-node boundary panels - prints first, returns list
#' viz <- plot(tree,
#'   data = wdbc, response = "diagnosis",
#'   plot.type = "boundary"
#' )
#' viz$plots[[2]] # second node
#'
#' # Global decision surface
#' plot(tree,
#'   data = wdbc, response = "diagnosis",
#'   plot.type = "surface"
#' )
#'
#' # Surface with explicit feature axes
#' plot(tree,
#'   data = wdbc, response = "diagnosis",
#'   plot.type = "surface",
#'   features = c("radius_mean", "concavity_mean")
#' )
#' }
#'
#' @method plot svmodt_node
#' @export
plot.svmodt_node <- function(x, y = NULL, ...,
                             data = NULL,
                             response = NULL,
                             plot.type = c("boundary", "surface"),
                             features = NULL,
                             max_depth = NULL,
                             check_accuracy = TRUE,
                             resolution = NULL) {
  if (!is.null(y)) {
    stop(
      "plot.svmodt_node: unexpected positional argument 'y'.\n",
      "Use named arguments: plot(tree, data=..., response=...)"
    )
  }
  if (is.null(data)) {
    stop("plot.svmodt_node: 'data' must be supplied.")
  }
  if (is.null(response)) {
    stop("plot.svmodt_node: 'response' must be supplied.")
  }

  plot.type <- match.arg(plot.type)

  if (plot.type == "surface") {
    res <- if (is.null(resolution)) 200L else as.integer(resolution)
    p <- plot_surface(x, data, response,
      features = features, resolution = res
    )
    print(p)
    return(invisible(p))
  }

  # "boundary"
  res <- if (is.null(resolution)) 100L else as.integer(resolution)
  result <- plot_boundary(x, data,
    response_col   = response,
    max_depth      = max_depth,
    check_accuracy = check_accuracy,
    resolution     = res
  )
  if (length(result$plots) > 0) print(result$plots[[1]])
  invisible(result)
}
