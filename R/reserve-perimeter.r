#' Calculate the perimeter of a reserve network
#'
#' @inheritParams mc_reserve
#' @return A `units` object giving the reserve perimeter in the specified units.
#' @export
#' @examples
#' r <- raster::raster(nrows = 10, ncols = 10, crs = "+proj=aea",
#'                     vals = sample(0:1, 100, replace = TRUE))
#' selected <- sample(c(FALSE, TRUE), 100, replace = TRUE, prob = c(0.7, 0.3))
#' reserve_perimeter(r, selected)
reserve_perimeter <- function(pu, x, units = c("km", "m")) {
  UseMethod("reserve_perimeter")
}

#' @export
reserve_perimeter.Raster <- function(pu, x, units = c("km", "m")) {
  pu <- pu[[1]]
  pu <- raster::rasterToPolygons(pu)
  reserve_perimeter.SpatialPolygonsDataFrame(pu, x, units = units)
}

#' @export
reserve_perimeter.SpatialPolygonsDataFrame <- function(pu, x,
                                                       units = c("km", "m")) {
  reserve_perimeter.sf(sf::st_as_sf(pu), x, units = units)
}

#' @export
reserve_perimeter.sf <- function(pu, x, units = c("km", "m")) {
  stopifnot(all(sf::st_geometry_type(pu) %in% c("MULTIPOLYGON", "POLYGON")))
  stopifnot(is.logical(x), length(x) == nrow(pu))
  units <- match.arg(units)
  if (grepl("longlat", sf::st_crs(pu)$proj4string)) {
    stop("Patch network must be in projected coordinates.")
  }

  patches <- sf::st_cast(sf::st_union(pu[as.logical(x), ]), to = "POLYGON")
  patch_perimeters <- sf::st_cast(patches, to = "LINESTRING")
  perimeter <- sum(sf::st_length(patch_perimeters))
  units::set_units(perimeter, units, mode = "standard")
}
