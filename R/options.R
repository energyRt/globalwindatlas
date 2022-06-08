.onLoad <- function(libname, pkgname) {
  if (file.exists("~/.globalwindatlas")) {
    source("~/.globalwindatlas")
  } else {
    warning("Global Wind Atlas data directory is not found.\n
            Use '?set_gwa_dir' for help")
  }
  # options(gwa.verbose = TRUE)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
get_gwa_dir <- function() {
  getOption("gwa.dir")
}

#' Set directory for downloaded GWA files
#'
#' @param gwa.dir
#'
#' @return
#' @export
#'
#' @examples
set_gwa_dir <- function(gwa.dir) {
  if (!dir.exists(gwa.dir)) {
    dir.create(gwa.dir, recursive = T)
    message("Creating directory '", gwa.dir, "'")
  }
  if (is.null(gwa.dir)) gwa.dir <- getOption("gwa.dir")
  options(gwa.dir = gwa.dir)
  con <- file("~/.globalwindatlas")
  writeLines(paste0("options(gwa.dir = '", gwa.dir, "')"), con)
  close(con)

}
