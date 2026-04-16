#' Find the quone binary
#'
#' Returns the path to the `quone` binary. Searches in order:
#' 1. `QUONE_BIN` environment variable
#' 2. The package's bundled binary (`inst/bin/`)
#' 3. The system `PATH`
#' 4. Common install locations (`~/.cabal/bin`, `~/.ghcup/bin`, `~/.local/bin`)
#'
#' @return Path to the quone binary, or an error if not found.
#' @export
quone_bin <- function() {
  env <- Sys.getenv("QUONE_BIN", unset = "")
  if (nzchar(env) && file.exists(env)) return(env)

  pkg_bin <- system.file("bin", quone_exe_name(), package = "quone")
  if (nzchar(pkg_bin) && file.exists(pkg_bin)) return(pkg_bin)

  sys_bin <- Sys.which("quone")
  if (nzchar(sys_bin)) return(unname(sys_bin))

  known <- file.path(
    path.expand("~"),
    c(".cabal/bin", ".ghcup/bin", ".local/bin"),
    quone_exe_name()
  )
  for (p in known) {
    if (file.exists(p)) return(p)
  }

  stop(
    "quone binary not found. Install it with quone::install_quone() ",
    "or set the QUONE_BIN environment variable.",
    call. = FALSE
  )
}

quone_exe_name <- function() {
  if (.Platform$OS.type == "windows") "quone.exe" else "quone"
}

assert_quone <- function() {
  bin <- quone_bin()
  if (!file.exists(bin)) {
    stop("quone binary not found at: ", bin, call. = FALSE)
  }
  bin
}
