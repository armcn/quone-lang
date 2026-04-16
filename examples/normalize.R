#' @param xs [Vector Double]
#' @return [Double]
std_dev <- function(xs) {
  avg <- mean(xs)
  n <- as.numeric(length(xs))
  sum_sq <- ((xs - avg) * (xs - avg)) |>
    sum()
  sqrt(sum_sq / n)

}

#' @param xs [Vector Double]
#' @return [Vector Double]
z_score <- function(xs) {
  avg <- mean(xs)
  sd <- std_dev(xs)
  (xs - avg) / sd

}

scores <- c(55.0, 70.0, 85.0, 100.0, 40.0)

result <- z_score(scores)

