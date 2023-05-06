
#' Title
#'
#' @param gwa_tif
#' @param gis_sf
#' @param int
#' @param eq
#' @param simplify
#' @param buffer
#' @param drop_crumps
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
group_locations <- function(gwa_tif, gis_sf = NULL,
                            int = seq(0, 1, by = .2),
                            eq = "ge", # "le" , "within"
                            simplify = T,
                            buffer = T,
                            drop_crumps = T,
                            verbose = T) {

  if (F) {
    gwa_tif <- get_wind_capacity_factor("IND", IEC = 2)
    gis_sf <- IDEEA.dev::ideea_maps$r32$land$sf
    reg <- "WB"
    gis_sf <- gis_sf[gis_sf$reg32 == reg,]
    int <- seq(0, 1, by = .05)
  }
  obj_tr <- rast(gwa_tif)
  if (!is.null(gis_sf)) {
    gis_sf <- st_transform(gis_sf, crs = st_crs(obj_tr))
    gis_sv <- as(gis_sf, "SpatVector")
    obj_tr <- crop(obj_tr, gis_sv)
    obj_tr <- mask(obj_tr, gis_sv)
    # plot(obj_tr)
  }
  # num <- as.numeric(obj_tr)

  ll <- list()
  i <- 0
  while (TRUE) {

    if (eq == ">=" || eq == "ge") {
      i <- i + 1
      if (i > length(int)) break
      cat("select values >=", int[i], "|")
      obj_tr_i <- app(obj_tr, fun = function(x) {x[x < int[i]] <- NA; return(x)})
      # plot(obj_tr_i)
      num_i <- as.numeric(obj_tr_i)
    }

    if (is.na(summary(c(values(num_i)), na.rm = T)["Max."])) break
    try({
      cat(" -> polygons")
      v3 <- as.polygons(obj_tr_i)
      # plot(v3, col = "dodgerblue")

      cat(" -> drop crumbs")
      area_thresh <- units::set_units(10, "km^2")
      suppressWarnings(
        {v4 <- smoothr::drop_crumbs(v3, area_thresh)}
      )
      # plot(v4, add = F, border = "navy", col = "grey")

      cat(" -> buffer")
      buffer_km <- units::set_units(100, "m")
      v5 <- terra::buffer(v4, buffer_km)
      # plot(v5, add = F, border = "navy", col = "grey")
      # v5 <- v4

      cat(" -> simplify")
      simpl_km <- units::set_units(.00001, "km")
      v6 <- terra::simplifyGeom(v5, simpl_km)
      plot(v6, add = F, border = "navy", col = "grey")

      cat(" -> sf\n")
      sim_sf <- sf::st_as_sf(v6) %>% st_union()
      # sim_sf <- sf::st_as_sf(v6) %>% st_union()
      sim_sf <- st_set_crs(sim_sf, st_crs(gis_sf)) %>% st_as_sf()
      # browser()
      sim_sf <- sim_sf %>%
        dplyr::mutate(
        eq = paste(">=", int[i]),
        int = int[i],
        area = st_area(sim_sf),
        area = units::set_units(area, "km^2"),
        .before = 1
      )
      ll[[length(ll) + 1]] <- sim_sf
    }, silent = !verbose)
  }
  # length(ll)
  dd <- ll[[1]]
  for (i in 2:length(ll)) {
    dd <- rbind(dd, ll[[i]])
  }
  return(dd)
}

if (F) {
  gwa_tif <- get_wind_capacity_factor("IND", IEC = 2)
  gis_sf <- IDEEA.dev::ideea_maps$r32$land$sf
  reg <- "WB"
  gis_sf <- gis_sf[gis_sf$reg32 == reg,]
  int <- seq(0, 1, by = .05)
  bb <- group_locations(gwa_tif, gis_sf, int = seq(0, 1, by = .05), verbose = F)
  plot(bb, key.width = lcm(3))
}
