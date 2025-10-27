#' Cube Dataset
#'
#' A synthetic dataset consisting of 5,000 points in three-dimensional space. The data is generated to simulate a cube-shaped distribution, with a subset of points distinctly labeled or colored to represent a separate class or region.
#'
#' @format A data frame with 5,000 rows and 4 columns:
#' \describe{
#'   \item{x_rot}{Numeric. Rotated X-coordinate of the point.}
#'   \item{y_rot}{Numeric. Rotated Y-coordinate of the point.}
#'   \item{z_rot}{Numeric. Rotated Z-coordinate of the point.}
#'   \item{feature}{Factor. Class label indicating whether the point belongs to the distinct colored region.}
#' }
#'
#' @keywords internal
"cube"
