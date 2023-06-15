#' Propose a name for a downloaded file based on URL
#'
#' @param url API-url for download
#' @param path optional path to a directory for saving the dounloaded file
#' @param extension file extension
#'
#' @return filename with path if specified
#' @export
#'
#' @examples
get_filename <- function(url, path = get_gwa_dir(), extension = ".tif") {
  a <- strsplit(url, "/")[[1]]
  ii <- 1:length(a)
  n <- which(a == "country")[1]
  if (is.na(n)) n <- which(grepl("gis", a))[1]
  if (is.na(n)) stop("Cannot auto-create `filename`, try assigne it explicitly")
  f <- paste(a[ii > n], collapse = "_")
  f <- paste0(f, extension)
  if (!is.null(path)) f <- file.path(path, f)
  f <- gsub("-", "_", f)
  # print(f)
  return(f)
}

#' Download wind capacity factors for a country
#'
#' @param country isocode (three letters) of a country (case sensitive)
#' @param IEC wind class (1, 2, or 3) according to The International Electrotechnical Commission (IEC)
#' @param path optional path to a directory for saving the downloaded file
#' @param filename optional file-name
#' @param overwrite logical, should the file be overwritten if exists
#'
#' @return path to the downloaded or existing file
#' @export
#'
#' @examples
get_wind_capacity_factor <- function(country, IEC = 2, path = get_gwa_dir(),
                                     filename = NULL, overwrite = FALSE) {
  if (country != "global") country <- paste0("country/", country)
  url <- paste0("https://globalwindatlas.info/api/gis/", country,
                "/capacity-factor_IEC", IEC)
  if (is.null(filename)) {
    f <- get_filename(url, path = path)
  } else {
    f <- filename
  }
  if (!overwrite) {if (file.exists(f)) return(f)}
  p <- try(httr::GET(url, httr::write_disk(f, overwrite = overwrite), httr::progress()))
  print(p)
  return(f)
}

#' Download wind speed data for a particular country
#'
#' @param country 3-leter isocode of a country (case sensitive)
#' @param height wind turbine hub height: 10, 50, 100, 150, or 200 meters
#' @param path optional path to a directory for saving the dounloaded file
#' @param filename optional file-name
#' @param overwrite logical, should the file be overvriten if exists
#'
#' @return path to the downloaded or existing file
#' @export
#'
#' @examples
get_wind_speed <- function(country, height = 100, path = get_gwa_dir(),
                                     filename = NULL, overwrite = FALSE) {
  if (country != "global") country <- paste0("country/", country)
  url <- paste0("https://globalwindatlas.info/api/gis/", country,
                "/wind-speed/", height)
  if (is.null(filename)) {
    f <- get_filename(url, path = path)
  } else {
    f <- filename
  }
  # print(f); print(url)
  if (!overwrite) {if (file.exists(f)) return(f)}
  p <- try(httr::GET(url, httr::write_disk(f, overwrite = overwrite), httr::progress()))
  print(p)
  return(f)
}



