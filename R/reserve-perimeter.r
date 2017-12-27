#' Caculate the perimeter of a reserve network
#'
#' @inheritParams mc_reserve
#' @return A `units` object giving the reserve perimeter in the specified units.
#' @export
#' @examples
#' r <- raster::raster(nrows = 10, ncols = 10, crs = "+proj=aea",
#'                     vals = sample(0:1, 100, replace = TRUE))
#' selected <- sample(c(FALSE, TRUE), 100, replace = TRUE, prob = c(0.7, 0.3))
#' reserve_perimeter(r, selected)
reserve_perimeter <- function(pu, x, units = c("m", "km")) {
  UseMethod("reserve_perimeter")
}

#' @export
reserve_perimeter.Raster <- function(pu, x, units = c("m", "km")) {
  pu <- pu[[1]]
  pu <- raster::rasterToPolygons(pu)
  reserve_perimeter.SpatialPolygonsDataFrame(pu, x, units = units)
}

#' @export
reserve_perimeter.SpatialPolygonsDataFrame <- function(pu, x,
                                                       units = c("m", "km")) {
  reserve_perimeter.sf(sf::st_as_sf(pu), x, units = units)
}

#' @export
reserve_perimeter.sf <- function(pu, x, units = c("m", "km")) {
  stopifnot(all(sf::st_geometry_type(pu) %in% c("MULTIPOLYGON", "POLYGON")))
  stopifnot(is.logical(x))
  stopifnot(length(x) == nrow(pu))
  units <- match.arg(units)
  if (grepl("longlat", sf::st_crs(pu)$proj4string)) {
    stop("Patch network must be in projected coordinates.")
  }

  uuu <- units
  patches <- sf::st_union(pu[as.logical(x), ])
  perimeter <- sum(sf::st_length(patches))
  units::set_units(perimeter, uuu)
}
