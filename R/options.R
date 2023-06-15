.onLoad <- function(libname, pkgname) {
  if (file.exists("~/.globalwindatlas")) {
    source("~/.globalwindatlas")
  } else {
    warning("Global Wind Atlas data directory is not found.\n
            Use '?gwa_set_dir' for help")
  }
  # options(gwa.verbose = TRUE)
}

#' Set directory for/with GWA raster-files
#'
#' @param gwa.dir path to a directory where GWA files will downloaded and stored for further use.
#'
#' @return
#' @export
#'
#' @examples
gwa_set_dir <- function(gwa.dir) {
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

#' Return current directory with/for downloaded GWA files
#'
#' @return
#' @export
#'
#' @examples
gwa_get_dir <- function() {
  getOption("gwa.dir")
}
