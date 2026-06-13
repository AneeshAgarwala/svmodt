# ── Constructors ──────────────────────────────────────────────────────────────

#' @keywords internal
svmodt_leaf <- function(x) {
  stopifnot(is.list(x), isTRUE(x$is_leaf))
  structure(x, class = c("svmodt_leaf", "svmodt_node"))
}

#' @keywords internal
svmodt_tree <- function(x) {
  stopifnot(is.list(x), !isTRUE(x$is_leaf))
  structure(x, class = c("svmodt_tree", "svmodt_node"))
}

# ── print generic + methods ───────────────────────────────────────────────────

#' Print method for svmodt_node objects
#' @param x An object of class \code{svmodt_node}.
#' @param ... Further arguments passed to \code{\link{print_svm_tree}}.
#' @method print svmodt_node
#' @export
print.svmodt_node <- function(x, ...) {
  print_svm_tree(x, ...)
  invisible(x)
}

#' @method print svmodt_leaf
#' @export
print.svmodt_leaf <- function(x, ...) {
  print_svm_tree(x, ...)
  invisible(x)
}

#' @method print svmodt_tree
#' @export
print.svmodt_tree <- function(x, ...) {
  print_svm_tree(x, ...)
  invisible(x)
}

# ── predict method ────────────────────────────────────────────────────────────

#' Predict method for svmodt_node objects
#'
#' @param object An object of class \code{svmodt_node}.
#' @param newdata A data frame of new predictor values.
#' @param return_probs Logical; if \code{TRUE}, returns predictions and probabilities.
#' @param calibrate_probs Logical; if \code{TRUE}, uses logistic calibration on decision values.
#' @param ... Currently unused.
#' @method predict svmodt_node
#' @export
predict.svmodt_node <- function(object, newdata, return_probs = FALSE,
                                calibrate_probs = TRUE, ...) {
  svm_predict_tree(object, newdata,
                   return_probs = return_probs,
                   calibrate_probs = calibrate_probs
  )
}

# ── format method ─────────────────────────────────────────────────────────────

#' @method format svmodt_node
#' @export
format.svmodt_node <- function(x, ...) {
  cls   <- if (inherits(x, "svmodt_leaf")) "leaf" else "tree"
  depth <- if (!is.null(x$depth)) x$depth else 1L
  n     <- x$n
  paste0("<svmodt_node [", cls, "] depth=", depth, " n=", n, ">")
}


# ─── S3 trace method ──────────────────────────────────────────────────────────
#' Trace the prediction path of a sample through an svmodt tree
#'
#' Generic function that walks the tree for a single row of new data, printing
#' the SVM decision value and chosen branch at every internal node and the
#' final predicted class at the leaf.
#'
#' @param object An object for which a method exists (currently
#'   \code{svmodt_node}).
#' @param ... Further arguments passed to the method.
#'
#' @return Invisibly returns the predicted class label (character string).
#' @export
trace_path <- function(object, ...) UseMethod("trace_path")

#' @describeIn trace_path Method for \code{svmodt_node} objects.
#'
#' @param object An \code{svmodt_node} returned by \code{\link{svm_split}}.
#' @param sample_data A data frame of new predictor values (one or more rows).
#' @param sample_idx Integer; which row to trace (default \code{1}).
#' @param ... Currently unused.
#'
#' @examples
#' \dontrun{
#' tree <- svm_split(wdbc, response = "diagnosis", max_depth = 3)
#' trace_path(tree, wdbc, sample_idx = 5)
#' }
#'
#' @method trace_path svmodt_node
#' @export
trace_path.svmodt_node <- function(object, sample_data, sample_idx = 1, ...) {
  invisible(trace_prediction_path(object, sample_data, sample_idx))
}
