#' Launch the Quone REPL
#'
#' Starts an interactive Quone REPL session. In RStudio, the REPL opens in a
#' new terminal pane. Otherwise it runs in the current R session's terminal.
#'
#' @export
#' @examples
#' \dontrun{
#' repl()
#' }
repl <- function() {
  bin <- assert_quone()

  if (rstudio_available()) {
    rstudioapi::terminalExecute(paste(shQuote(bin), "repl"))
    message("Quone REPL launched in RStudio terminal.")
  } else {
    system2(bin, args = "repl")
  }

  invisible()
}

rstudio_available <- function() {
  requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()
}
