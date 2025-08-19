make_cube <- function(n = 2000, side = 2, noise = 0.05) {
  set.seed(123)
  x <- runif(n, -side, side) + rnorm(n, 0, noise)
  y <- runif(n, -side, side) + rnorm(n, 0, noise)
  z <- runif(n, -side, side) + rnorm(n, 0, noise)
  data.frame(x, y, z)
}

# Generate Data
df <- make_cube(n = 5000, side = 1, noise = 0.02)

df <- df |>
  dplyr::mutate(feature = ifelse(x<0 & y <0 & z < 0, "A", "B")) |>
  dplyr::mutate(feature = as.factor(feature))


#plotly::plot_ly(data = df, x = ~x, y = ~y, z = ~z, color =  ~ feature)

rotate_x <- function(df, angle) {
  Rx <- matrix(c(1, 0, 0,
                 0, cos(angle), -sin(angle),
                 0, sin(angle), cos(angle)), 3, 3, byrow = TRUE)
  as.matrix(df) %*% t(Rx)
}

rotate_y <- function(df, angle) {
  Ry <- matrix(c(cos(angle), 0, sin(angle),
                 0, 1, 0,
                 -sin(angle), 0, cos(angle)), 3, 3, byrow = TRUE)
  as.matrix(df) %*% t(Ry)
}

rotate_z <- function(df, angle) {
  Rz <- matrix(c(cos(angle), -sin(angle), 0,
                 sin(angle), cos(angle), 0,
                 0, 0, 1), 3, 3, byrow = TRUE)
  as.matrix(df) %*% t(Rz)
}


coords <- as.matrix(df[, c("x","y","z")])
coords <- rotate_z(coords, pi/4)  # 45° Z
coords <- rotate_x(coords, pi/6)  # 30° X
df$x_rot <- coords[,1]
df$y_rot <- coords[,2]
df$z_rot <- coords[,3]

cube <- df[,c("feature", "x_rot", "y_rot", "z_rot")]

usethis::use_data(cube, overwrite = TRUE)
