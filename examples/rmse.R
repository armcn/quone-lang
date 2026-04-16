#' @param predicted [Vector Double]
#' @param actual [Vector Double]
#' @return [Double]
rmse <- function(predicted, actual) {
  (predicted - actual) |>
    (\(.x) .x * .x)() |>
    mean() |>
    sqrt()

}

predicted <- c(2.5, 0.0, 2.1, 1.6)

actual <- c(3.0, 0.5, 2.0, 1.0)

result <- rmse(predicted, actual)

