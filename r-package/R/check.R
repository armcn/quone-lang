#' Type-check a Quone file
#'
#' Runs the Quone compiler on a `.Q` file and reports any type errors.
#' Returns `TRUE` (invisibly) if the file is error-free, or prints errors
#' and returns `FALSE`.
#'
#' @param file Path to a `.Q` source file.
#' @return `TRUE` if no errors, `FALSE` otherwise (invisibly).
#' @export
#' @examples
#' \dontrun{
#' check("analysis.Q")
#' }
check <- function(file) {
  bin <- assert_quone()
  file <- normalizePath(file, mustWork = TRUE)
  result <- processx::run(bin, args = file, error_on_status = FALSE)

  if (result$status != 0) {
    message(trimws(result$stdout))
    return(invisible(FALSE))
  }

  message("OK: no type errors in ", basename(file))
  invisible(TRUE)
}
