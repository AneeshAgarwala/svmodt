make_square <- function(n = 2000, side = 2, noise = 0.05) {
  set.seed(123)
  x <- runif(n, -side, side) + rnorm(n, 0, noise)
  y <- runif(n, -side, side) + rnorm(n, 0, noise)
  data.frame(x, y)
}

# Generate Data
df <- make_square(n = 5000, side = 1, noise = 0.02)

df <- df |>
  dplyr::mutate(feature = ifelse(x < 0 & y < 0, "A", "B")) |>
  dplyr::mutate(feature = as.factor(feature))


# plotly::plot_ly(data = df, x = ~x, y = ~y, color =  ~ feature)

rotate_2d <- function(df, angle) {
  # Rotation matrix
  R <- matrix(
    c(
      cos(angle), -sin(angle),
      sin(angle), cos(angle)
    ),
    2, 2,
    byrow = TRUE
  )

  # Apply rotation and keep column names
  coords <- as.matrix(df) %*% t(R)
  colnames(coords) <- c("x_rot", "y_rot")
  return(as.data.frame(coords))
}

# Rotate the square
rotated_coords <- rotate_2d(df[, c("x", "y")], -pi / 4)

# Bind rotated coordinates with feature
square <- cbind(df["feature"], rotated_coords)

# Plot rotated square
# plotly::plot_ly(data = square, x = ~x_rot, y = ~y_rot, color = ~feature)


usethis::use_data(square, overwrite = TRUE)
