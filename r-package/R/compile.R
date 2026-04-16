#' Compile a Quone file to R
#'
#' Compiles a `.Q` file and returns the generated R code as a string.
#'
#' @param file Path to a `.Q` source file.
#' @return The generated R code as a character string (invisibly).
#' @export
#' @examples
#' \dontrun{
#' r_code <- compile("analysis.Q")
#' cat(r_code)
#' }
compile <- function(file) {
  bin <- assert_quone()
  file <- normalizePath(file, mustWork = TRUE)
  result <- processx::run(bin, args = file, error_on_status = FALSE)

  if (result$status != 0) {
    stop(trimws(result$stdout), call. = FALSE)
  }

  r_code <- result$stdout
  cat(r_code)
  invisible(r_code)
}

#' Compile and write R output to a file
#'
#' Compiles a `.Q` file and writes the generated R code to an `.R` file.
#'
#' @param file Path to a `.Q` source file.
#' @param output Path for the output `.R` file. Defaults to replacing the
#'   `.Q` extension with `.R`.
#' @return The output file path (invisibly).
#' @export
compile_file <- function(file, output = sub("\\.Q$", ".R", file)) {
  r_code <- compile(file)
  writeLines(r_code, output, sep = "")
  message("Compiled: ", file, " -> ", output)
  invisible(output)
}
