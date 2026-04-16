#' Compile and evaluate a Quone file
#'
#' Compiles a `.Q` file to R and immediately evaluates the result, similar to
#' [base::source()] for R files. Objects defined in the Quone file become
#' available in the calling environment.
#'
#' @param file Path to a `.Q` source file.
#' @param envir The environment in which to evaluate the compiled R code.
#'   Defaults to the caller's environment.
#' @return The result of evaluating the compiled R code (invisibly).
#' @export
#' @examples
#' \dontrun{
#' source_quone("analysis.Q")
#' }
source_quone <- function(file, envir = parent.frame()) {
  r_code <- compile(file)
  exprs <- parse(text = r_code)
  result <- eval(exprs, envir = envir)
  invisible(result)
}
