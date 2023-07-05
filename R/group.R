#' Group locations by intervals of values in given `tiff`-file and return geometries of the groups.
#'
#' @param gwa_tif tiff-file (terra-object)
#' @param gis_sf optional `sf` object (map) to crop `gwa-tif`
#' @param by_feature logical, should the grouping be done for each geometry (row in sf-object), `TRUE` by default (recommended for large objects), if `FALSE` the geometries will be merged into one.
#' @param ID character, identification column of the sf-object. If provided, geometries will be merged by IDs.
#' @param int numeric, intervals of values for grouping locations
#' @param simplify numeric, tolerance, the minimum distance between nodes in units of the crs (i.e. degrees for long/lat, see `?terra::simplifyGeom` for details). Default value is 0.1. To skip this step set the value to 0.
#' @param buffer numeric. If positive, buffer will be added to every geometry.  Unit is meter if `gwa_tif` and `gis_sf` has a longitude/latitude CRS, or in the units of the coordinate reference system in other cases (typically also meter, see `?terra::buffer` for details). The default value is 100. Set to 0 to skip this step.
#' @param drop_crumps logical, if TRUE, small geometries will be dropped.
#' @param verbose logical, should the process be reported.
#' @param plot_process logical, plot the process of creation of geometries.
#' @param aggregate_tif (optional) integer, number of pixels in `tiff` to aggregation towards x and y directions (see `fact` parameter in `terra::aggregate` for details). The default value is `0` (no aggregation).
#' @param snap_to_grid (optional) numeric size of grid. If positive, geometries will be adjusted to the closest grid (see `grid_size` argument in `s2::s2_snap_to_grid`). The default value is `1e-5` in lon/lat coordinates.
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
    # by_feature = !is.null(gis_sf),
    ID = NULL,
    # lo = 0,
    # up = seq(0.1, 1, by = .2),
    int = seq(0, 1, by = .1),
    # eq = ">=", # "<=" or "within"
    # keep_first_geom = T
    aggregate_tif = 0,
    snap_to_grid = 1e-5,
    simplify = 0.001,
    buffer = 100,
    drop_crumps = units::set_units(10, "km^2"),
    verbose = T,
    plot_process = T
) {
  # browser()
  obj_tr <- rast(gwa_tif)
  if (aggregate_tif > 0) {
    if (verbose) cat("Aggregation of `gwa_tif`, factor:", aggregate_tif, "\n")
    # browser()
    obj_tr <- terra::aggregate(obj_tr, aggregate_tif)
  }
  if (terra::crs(obj_tr) == "") {
    stop("Please set CRS for `gwa_tif`, see ?terra::crs")
  }

  eq = ">=" # temporary solution

  # units
  if (isTRUE(drop_crumps)) {drop_crumps <- units::set_units(10, "km^2")}
  if (!inherits(drop_crumps, "units") && inherits(drop_crumps, "numeric")) {
    drop_crumps <- units::set_units(drop_crumps, "km^2")
  }

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
      if (i > length(int)) {
        if (verbose) cat("empty\n")
        break
      }
      im <- paste0(">= ", int[i])
      if (verbose) cat("values", im, "|")
      obj_tr_i <- app(obj_tr, fun = function(x) {x[x < int[i]] <- NA; return(x)})
      # plot(obj_tr_i)
      num_i <- as.numeric(obj_tr_i)
      # }

      if (is.na(summary(c(values(num_i)), na.rm = T)["Max."])) {
        if (verbose) cat("\n")
        break
      }

      try({
        # if (verbose) cat(" -> polygons")
        obj_tr_i <- terra::as.polygons(obj_tr_i) %>%
          terra::makeValid() %>% terra::aggregate()
        # obj_sf_i <- st_as_sf(obj_tr_i)
        # plot(v3, col = "dodgerblue")

        if (!first_geom) {

          if (isTRUE(simplify)) {
            simplify <- 0.01 # units::set_units(.00001, "km")
          }
          if (inherits(simplify, "numeric") && simplify > 0) {
            if (verbose) cat(" -> simplify")
            try({
              obj_tr_i <- terra::simplifyGeom(obj_tr_i, simplify) %>%
                terra::makeValid() %>% terra::aggregate()
            })
            # browser()
          }

          if (isTRUE(buffer)) {
            # buffer <- units::set_units(100, "m")
            buffer <- 1
          }

          if (inherits(buffer, "numeric") && buffer > 0) { # & units - set above
            if (verbose) cat(" -> buffer")
            try({
              obj_tr_i <- terra::buffer(obj_tr_i, buffer) %>%
                terra::makeValid() %>% terra::aggregate()
            })

          }
        }
        ttl <- paste0("region: ", reg, ", val ", im)

        # if (verbose) cat(" -> sf")
        sim_sf <- sf::st_as_sf(obj_tr_i) %>%
          gwa_union_polygons(grid_size = snap_to_grid) %>%
          st_set_crs(st_crs(gis_sf)) %>% st_as_sf() %>% st_make_valid()
          # st_union() %>% st_make_valid()
        # if ((inherits(sim_sf, "sfc_GEOMETRYCOLLECTION"))) {
        #   sim_sf <- try({
        #     st_collection_extract(sim_sf, "POLYGON", warn = F)
        #   }, silent = T)
        #   if (inherits(sim_sf, "try-error")) sim_sf <- st_polygon(list())
        # }
        # sim_sf <- sim_sf %>%
          # gwa_union_polygons()
          # st_make_valid() %>% st_union() %>%
          # st_set_crs(st_crs(gis_sf)) %>% st_as_sf() %>% st_make_valid()

         if (inherits(drop_crumps, "units") && as.numeric(drop_crumps) > 0
            && !first_geom && !is_empty(sim_sf)) {
          if (verbose) cat(" -> drop crumbs")
          try({
            sim_sf <- smoothr::drop_crumbs(sim_sf, drop_crumps)
          })
        }

        sim_sf <- sim_sf %>%
          gwa_union_polygons(grid_size = snap_to_grid) %>% st_as_sf()
          # st_union() %>% st_make_valid() %>% st_as_sf()
        # browser()
        if (is.null(sim_sf$geometry)) {
          try({sim_sf <- dplyr::rename(sim_sf, geometry = x)})
        }
        if (by_feature & !is.null(ID)) {
          sim_sf <- mutate(sim_sf, region = gis_sf[n, ID][[1]], .before = 1) %>%
            dplyr::rename_with(function(x = names(.data)) {
              x[x == "region"] <- ID; x
            })
        }
        sim_sf <- cbind(sim_sf, st_drop_geometry(st_as_sf(gis_sv)))
        # browser()
        sim_sf <- sim_sf %>%
          st_as_sf() %>%
          st_make_valid() %>%
           # bind(st_drop_geometry(gis_sf[n,])) %>%
          dplyr::mutate(
            # region = gis_sf[n,]$region,
            eq = paste(">=", int[i]),
            int = int[i],
            area = st_area(geometry),
            area = units::set_units(area, "km^2"),
            .before = "geometry"
            # .before = max(1, min(2, ncol(sim_sf) - 1))
          ) %>%
          dplyr::filter(as.numeric(area) > 0)

        # browser()
        if (plot_process) {
          try({
            terra::plot(terra::vect(sim_sf), add = F, border = "navy",
                        col = "grey", main = ttl)
            # plot(st_geometry(sim_sf), main = ttl)
          }, silent = T)
          # browser()
        }

        km2 <- st_area(st_geometry(sim_sf))
        if (is_empty(km2)) km2 <- 0
        km2 <- km2 %>% units::set_units("km^2")
        if (verbose) cat(" |", km2, "km^2")
        if ((km2 <= drop_crumps) || (length(km2) == 0)) {
          cat(" <=" , drop_crumps,"\n")
          break
        } else {
          ll[[length(ll) + 1]] <- sim_sf
          cat("\n")
        }
      }, silent = !verbose)
      first_geom <- FALSE
    } # end of slicing of i-th shape
    i <- 0L
    # if (verbose) cat("\n")
  } # next shape

  # browser()
  dd <- ll[[1]]
  if (length(ll) > 1) {
    for (i in 2:length(ll)) { # combine all shapes into one `sf` object
      dd <- dplyr::bind_rows(dd, ll[[i]])
    }
  }
  return(dd)
}

gwa_union_polygons <- function(x, grid_size = 1e-5) {
  # workaround to improve stability of aggregation with `st_union()`
  # x - geometry
  # browser()
  ii <- sapply(x, function(g) {
    inherits(g, c("sfc_GEOMETRYCOLLECTION", "GEOMETRYCOLLECTION"))
  })
  if (any(ii)) {
    x <- try({
      suppressWarnings(
        st_collection_extract(x, "POLYGON", warn = F)
      )
    }, silent = T)
    if (inherits(x, "try-error")) x <- st_polygon(list())
  }
  if (grid_size > 0) x <- x %>% s2::s2_snap_to_grid(grid_size)
  x %>%
    st_as_sf() %>%
    st_union(by_feature = F) %>%
    st_make_valid()
}
