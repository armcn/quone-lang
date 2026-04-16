#' Format a Quone source file
#'
#' Formats a `.Q` file using the Quone formatter and returns the result.
#'
#' @param file Path to a `.Q` source file.
#' @param in_place If `TRUE`, overwrites the file with the formatted output.
#'   Default is `FALSE`.
#' @return The formatted source code as a character string (invisibly).
#' @export
#' @examples
#' \dontrun{
#' format_source("analysis.Q")
#' format_source("analysis.Q", in_place = TRUE)
#' }
format_source <- function(file, in_place = FALSE) {
  bin <- assert_quone()
  file <- normalizePath(file, mustWork = TRUE)
  result <- processx::run(bin, args = c(file, "--format"), error_on_status = FALSE)

  if (result$status != 0) {
    stop(trimws(result$stdout), call. = FALSE)
  }

  formatted <- result$stdout

  if (in_place) {
    writeLines(formatted, file, sep = "")
    message("Formatted: ", file)
  }

  invisible(formatted)
}
