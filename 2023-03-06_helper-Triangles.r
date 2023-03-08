#| Helpers ----
radian <- function(degree) {
  #| input degrees
  #| output radians (Vielfaches von pi)
  (degree / 360) * 2 * pi
}

degree <- function(radian) {
  (360 * radian) / (2 * pi)
}

angle <- function(x, y) {
  #| input: as point-matrix
  #| norm requires matrix
  #| output: as radians
  #| norm(..., type = "2") entspricht: sqrt(sum( x%*%x))
  #| norm(..., type = "1") entspricht: sum(sqrt( x%*%x))
  dot_prod <- x %*% y
  norm_x <- norm(x, type = "2")
  norm_y <- norm(y, type = "2")
  theta <- acos(dot_prod / (norm_x * norm_y))
  as.numeric(theta)
}


sample_points <- function(x) {
  sample(x, 1, replace = TRUE)
}
