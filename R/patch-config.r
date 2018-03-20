#' Calculate interpatch distances and areas
#'
#' Given a set of patches in raster or vector format, calculate the areas and
#' interpatch distances.
#'
#' If the patch network is provided as a `Raster` object, cells included in the
#' network should be 1, and background cells should be 0 or `NA`. Patches can
#' also be provided as polygons from either the `sf` or `sp` packages. Spatial
#' data must be provided in projecred coordinates with units of meters.
#'
#' @param x patch network in raster or vector format, see Details.
#' @param units character; whether distances and areas should be in meters or
#'   kilometers.
#'
#' @return A `patch_config` object with two elements: the interpatch distances
#' (`distances`) and patch areas (`areas`).
#' @export
#' @examples
#' # raster
#' r <- raster::raster(nrows = 10, ncols = 10, crs = "+proj=aea")
#' r[] <- round(runif(raster::ncell(r)) * 0.7)
#' patch_config(r, units = "m")
#'
#' # polygon
#' p_sp <- raster::rasterToPolygons(r, dissolve = TRUE)
#' p_sp <- p_sp[p_sp$layer == 1, ]
#' p_sp <- sp::disaggregate(p_sp)
#' patch_config(p_sp, units = "km")
patch_config <- function(x, units = c("m", "km")) {
  UseMethod("patch_config")
}

#' @export
patch_config.RasterLayer <- function(x, units = c("km", "m")) {
  if (raster::cellStats(x, 'sum') == 0) {
    out <- structure(list(areas = numeric(0),
                          distances = matrix(nrow = 0, ncol = 0)),
                     class = "patch_config")
    return(out)
  }
  p <- raster::clump(x, directions = 4, gaps = FALSE)
  p <- raster::rasterToPolygons(p, dissolve = TRUE)
  patch_config.SpatialPolygons(p, units = units)
}

#' @export
patch_config.SpatialPolygons <- function(x, units = c("km", "m")) {
  patch_config.sf(sf::st_as_sf(x), units = units)
}

#' @export
patch_config.sf <- function(x, units = c("km", "m")) {
  patch_config.sfc(sf::st_geometry(x), units = units)
}

#' @export
patch_config.sfc <- function(x, units = c("km", "m")) {
  stopifnot(all(sf::st_geometry_type(x) %in% c("MULTIPOLYGON", "POLYGON")))
  units <- match.arg(units)
  if (grepl("longlat", sf::st_crs(x)$proj4string)) {
    stop("Patch network must be in projected coordinates.")
  }

  # distance matrix
  if (requireNamespace("rgeos", quietly = TRUE)) {
    d <- rgeos::gDistance(methods::as(x, "Spatial"), byid = TRUE)
    dimnames(d) <- NULL
    d <- units::set_units(d, projection_units(x), mode = "standard")
  } else {
    d <- sf::st_distance(x)
  }
  # change units
  d <- units::set_units(d, units, mode = "standard")
  d <- matrix(d, dim(d)[1], dim(d)[2])

  # areas
  a <- sf::st_area(x)
  # change units
  uuu <- paste0(units, "^2")
  a <- units::set_units(a, uuu, mode = "standard")
  a <- as.numeric(a)
  structure(list(areas = a, distances = d), class = "patch_config")
}
