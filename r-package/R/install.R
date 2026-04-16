#' Install the quone compiler binary
#'
#' Downloads a pre-built quone binary from GitHub Releases and installs it
#' into the package directory. If no pre-built binary is available for your
#' platform, falls back to building from source (requires GHC and Cabal).
#'
#' @param version Version to install. Use `"latest"` for the most recent
#'   release, or a specific version like `"0.1.0"`.
#' @param force Re-install even if the binary already exists.
#' @return The path to the installed binary (invisibly).
#' @export
#' @examples
#' \dontrun{
#' install_quone()
#' install_quone(version = "0.1.0")
#' }
install_quone <- function(version = "latest", force = FALSE) {
  dest_dir <- quone_install_dir()
  dest <- file.path(dest_dir, quone_exe_name())

  if (!force && file.exists(dest)) {
    message("quone is already installed at: ", dest)
    message("Use install_quone(force = TRUE) to reinstall.")
    return(invisible(dest))
  }

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  url <- binary_url(version)
  if (!is.null(url)) {
    install_from_release(url, dest)
  } else {
    message("No pre-built binary for this platform. Building from source...")
    install_from_source(dest)
  }

  invisible(dest)
}

quone_install_dir <- function() {
  file.path(system.file(package = "quone"), "bin")
}

binary_url <- function(version) {
  base <- "https://github.com/armcn/quone-lang/releases"
  tag <- if (version == "latest") "latest/download" else paste0("download/v", version)

  os <- tolower(Sys.info()[["sysname"]])
  arch <- Sys.info()[["machine"]]

  platform <- if (os == "darwin" && arch == "arm64") {
    "quone-macos-arm64"
  } else if (os == "darwin") {
    "quone-macos-x86_64"
  } else if (os == "linux" && arch == "x86_64") {
    "quone-linux-x86_64"
  } else if (os == "windows") {
    "quone-windows-x86_64.exe"
  } else {
    return(NULL)
  }

  paste(base, tag, platform, sep = "/")
}

install_from_release <- function(url, dest) {
  message("Downloading quone from: ", url)
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  tryCatch({
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    file.copy(tmp, dest, overwrite = TRUE)
    Sys.chmod(dest, mode = "0755")
    message("Installed quone to: ", dest)
  }, error = function(e) {
    message("Download failed: ", conditionMessage(e))
    message("Falling back to building from source...")
    install_from_source(dest)
  })
}

install_from_source <- function(dest) {
  cabal <- Sys.which("cabal")
  if (!nzchar(cabal)) {
    stop(
      "Cannot build from source: 'cabal' not found.\n",
      "Install GHCup (https://www.haskell.org/ghcup/) and try again,\n",
      "or download a pre-built binary from:\n",
      "  https://github.com/armcn/quone-lang/releases",
      call. = FALSE
    )
  }

  src_dir <- find_source_dir()

  message("Building quone from source (this may take a few minutes)...")
  result <- processx::run(
    cabal, args = c("install", "exe:quone", "--install-method=copy",
                    paste0("--installdir=", dirname(dest)),
                    "--overwrite-policy=always"),
    wd = src_dir,
    error_on_status = FALSE,
    spinner = TRUE
  )

  if (result$status != 0) {
    stop("Build failed:\n", result$stderr, call. = FALSE)
  }

  Sys.chmod(dest, mode = "0755")
  message("Built and installed quone to: ", dest)
}

find_source_dir <- function() {
  pkg_dir <- system.file(package = "quone")
  cabal_file <- file.path(pkg_dir, "quone.cabal")
  if (file.exists(cabal_file)) return(pkg_dir)

  dev_dir <- Sys.getenv("QUONE_SOURCE", unset = "")
  if (nzchar(dev_dir) && file.exists(file.path(dev_dir, "quone.cabal"))) {
    return(dev_dir)
  }

  stop(
    "Cannot find quone source directory.\n",
    "Set QUONE_SOURCE to the path containing quone.cabal,\n",
    "or download a pre-built binary from:\n",
    "  https://github.com/armcn/quone-lang/releases",
    call. = FALSE
  )
}
