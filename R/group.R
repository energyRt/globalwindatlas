#' Group locations by intervals of values in given `tiff`-file and return geometries of the groups.
#'
#' @param gwa_tif tiff-file (terra-object)
#' @param gis_sf optional `sf` object (map) to crop `gwa-tif`
#' @param by_feature logical, should the grouping be done for each geometry (row in sf-object), `TRUE` by default (recommended for large objects), if `FALSE` the geometries will be merged into one.
#' @param ID character, identification column of the sf-object. If provided, geometries will be merged by IDs.
#' @param int numeric, intervals of values for grouping locations
#' @param simplify logical, if TRUE, geometries will be simplified.
#' @param buffer logical, if TRUE, buffers will be added to the geometries.
#' @param drop_crumps logical, if TRUE, small geometries will be dropped.
#' @param verbose logical, should the process be reported.
#' @param plot_process logical, plot the process of creation of geometries.
#'
#' @return `sf` object with geometries for each group, defined by intervals.
#'
#' @export
#'
#' @examples
gwa_group_locations <- function(
    gwa_tif,
    gis_sf = NULL,
    by_feature = TRUE,
    ID = NULL,
    # lo = 0,
    # up = seq(0.1, 1, by = .2),
    int = seq(0, 1, by = .2),
    # eq = ">=", # "<=" or "within"
    # keep_first_geom = T,
    simplify = T,
    buffer = T,
    drop_crumps = T,
    verbose = T,
    plot_process = T
  ) {
  # browser()
  obj_tr <- rast(gwa_tif)
  if (terra::crs(obj_tr) == "") stop("Please set CRS for `gwa_tif`, see ?terra::crs")
  eq = ">=" # temporary solution
  # plot(obj_tr)
  # num <- as.numeric(obj_tr)
  if (!is.null(gis_sf)) {
    # pre-process the shape:
    #   * merge geometries within the same ID
    #   * auto-fix invalid geometries
    stopifnot(inherits(gis_sf, "sf")) # check the class
    if (is.null(by_feature) || isFALSE(by_feature)) {
      # merge all regions/shapes into one
      gis_sf <- gis_sf %>% st_combine() %>% st_as_sf() %>% st_make_valid()
    } else {
      # browser()
      if (!is.null(ID)) {
        stopifnot(!is.null(gis_sf[[ID]]))
        # merge by the provided ID
        gis_sf <- gis_sf %>%
          group_by_at(ID) %>%
          summarise() %>%
          st_as_sf() %>% st_make_valid()
      } else {
        # merge by geometries
        g <- gis_sf %>%
          st_geometry() %>%
          st_union(by_feature = T, is_coverage = T) %>%
          # st_as_sf() %>%
          st_make_valid()
        # browser()
        st_geometry(gis_sf) <- g; rm(g)
      }
    }
    gis_sf <- st_transform(gis_sf, crs = st_crs(obj_tr))
    # plot(gis_sf[1])
    # crop & mask
  } else {
    # browser()
    gis_sf <- as.polygons(ext(obj_tr)) %>%
      st_as_sf() %>%
      st_set_crs(st_crs(obj_tr))
  }

  ll <- list() # stores individual shapes
  i <- 0 # a number of final shapes
  # n - a number of geometries (rows in gis_sf)
  # browser()
  N <- max(1, nrow(gis_sf))
  for (n in 1:N) { # loop over shapes
    # browser()
    if (is.null(ID)) {
      reg <- n
    } else {
      reg <- try(gis_sf[n, ID][[1]], silent = T)
      if (inherits(reg, "try-error")) reg <- n
    }
    nN <- paste0(" (", n, "/", N, ")")
    if (verbose) message("region/geometry: ", reg, nN)
    # cut the tiff-file
    obj_tr <- rast(gwa_tif) #!!!
    gis_sv <- as(gis_sf[n,], "SpatVector")
    # plot(gis_sv)
    obj_tr <- crop(obj_tr, gis_sv)
    # plot(obj_tr)
    obj_tr <- mask(obj_tr, gis_sv)
    # plot(obj_tr)
    rng <- range(values(obj_tr), na.rm = T)
    if (min(int) <= rng[1]) {
      # do not process/simplify the first shape
      first_geom <- TRUE
    } else {
      first_geom <- FALSE
    }
    # browser()
    while (TRUE) { # loop over intervals
      # if (eq == ">=" || eq == "ge") {
        i <- i + 1
        if (i > length(int)) break
        im <- paste0(">= ", int[i])
        if (verbose) cat("select values", im, "|")
        obj_tr_i <- app(obj_tr, fun = function(x) {x[x < int[i]] <- NA; return(x)})
        # plot(obj_tr_i)
        num_i <- as.numeric(obj_tr_i)
      # }

      if (is.na(summary(c(values(num_i)), na.rm = T)["Max."])) break

      try({
        if (verbose) cat(" -> polygons")
        obj_tr_i <- terra::as.polygons(obj_tr_i) %>% terra::makeValid()
        # plot(v3, col = "dodgerblue")

        if (!first_geom) {
          if (isTRUE(drop_crumps)) {drop_crumps <- units::set_units(10, "km^2")}
          if (!inherits(drop_crumps, "units") & is.numeric(drop_crumps)) {
            warning("Setting 'drop_crumps' units to 'km^2'")
            units::set_units(drop_crumps, "km^2")
          }
          if (is.numeric(drop_crumps)) { # & units - set above
            if (verbose) cat(" -> drop crumbs")
            suppressWarnings({
              obj_tr_i <- smoothr::drop_crumbs(obj_tr_i, drop_crumps)
            })
          } else {
            if (!is.null(drop_crumps) || (is.logical(drop_crumps) && !drop_crumps)) {
              warning("Ignoring unrecognized values of 'drop_crumps'")
            }
          }

          if (isTRUE(buffer)) {buffer <- units::set_units(100, "m")}
          if (!inherits(buffer, "units") & is.numeric(buffer)) {
            warning("Setting 'buffer' units to 'm'")
            units::set_units(buffer, "m")
          }
          if (is.numeric(buffer)) { # & units - set above
            if (verbose) cat(" -> buffer")
            obj_tr_i <- terra::buffer(obj_tr_i, buffer)
            # plot(v5, add = F, border = "navy", col = "grey")
            # v5 <- v4
          } else {
            if (!is.null(buffer) || (is.logical(buffer) && !buffer)) {
              warning("Ignoring unrecognized values of 'buffer'")
            }
          }

          if (isTRUE(simplify)) {
            simplify <- simplify <- units::set_units(.00001, "km")
          }
          if (!inherits(simplify, "units") & is.numeric(simplify)) {
            warning("Setting 'simplify' units to 'm'")
            units::set_units(simplify, "km")
          }
          if (is.numeric(simplify)) { # & units - set above
            if (verbose) cat(" -> simplify")
            obj_tr_i <- terra::simplifyGeom(obj_tr_i, simplify)
            # browser()
            ttl <- paste0("region: ", reg, ", val ", im)
            if (plot_process) {
              try({
                terra::plot(obj_tr_i, add = F, border = "navy", col = "grey",
                            main = ttl)
              }, silent = T)
              # browser()
            }
          } else {
            if (!is.null(simplify) || (is.logical(simplify) && !simplify)) {
              warning("Ignoring unrecognized values of 'simplify'")
            }
          }
        }
        if (verbose) cat(" -> sf\n")
        sim_sf <- sf::st_as_sf(obj_tr_i) %>% st_union() %>% st_make_valid()
        # sim_sf <- sf::st_as_sf(v6) %>% st_union()
        sim_sf <- st_set_crs(sim_sf, st_crs(gis_sf)) %>% st_as_sf()
        # browser()
        if (is.null(sim_sf$geometry)) try({sim_sf <- dplyr::rename(sim_sf, geometry = x)})
        # browser()
        if (by_feature & !is.null(ID)) {
          sim_sf <- mutate(sim_sf, region = gis_sf[n, ID][[1]], .before = 1) %>%
            dplyr::rename_with(function(x = names(.data)) {x[x == "region"] <- ID; x})
        }
        sim_sf <- cbind(sim_sf, st_drop_geometry(st_as_sf(gis_sv)))
        # browser()
        sim_sf <- sim_sf %>%
          st_make_valid() %>%
          # bind(st_drop_geometry(gis_sf[n,])) %>%
          dplyr::mutate(
            # region = gis_sf[n,]$region,
            eq = paste(">=", int[i]),
            int = int[i],
            area = st_area(sim_sf),
            area = units::set_units(area, "km^2"),
            .before = "geometry"
            # .before = max(1, min(2, ncol(sim_sf) - 1))
          )
        ll[[length(ll) + 1]] <- sim_sf
      }, silent = !verbose)
      first_geom <- FALSE
    } # end of slicing of i-th shape
    i <- 0L
    if (verbose) cat("\n")
  } # next shape

  # length(ll)
  # browser()
  dd <- ll[[1]]
  for (i in 2:length(ll)) { # combine all shapes into one `sf` object
    dd <- dplyr::bind_rows(dd, ll[[i]])
  }
  return(dd)
}
