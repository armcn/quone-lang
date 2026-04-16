#' @param x [Double]
#' @param y [Double]
#' @return [{ x : Double, y : Double }]
point <- function(x, y) {
  list(x = x, y = y)

}

#' @param a [{ x : Double, y : Double }]
#' @param b [{ x : Double, y : Double }]
#' @return [Double]
distance <- function(a, b) {
  dx <- a$x - b$x
  dy <- a$y - b$y
  sqrt(dx * dx + dy * dy)

}

#' @param factor [Double]
#' @param p [{ x : Double, y : Double }]
#' @return [{ x : Double, y : Double }]
scale <- function(factor, p) {
  list(x = p$x * factor, y = p$y * factor)

}

origin <- point(0.0, 0.0)

p1 <- point(3.0, 4.0)

dist <- distance(origin, p1)

scaled <- scale(2.0, p1)

