#' Get projection units of an sf object
#'
#' @param x an sf object
#' @return A units object with the units of the projection.
#' @export
#' @examples
#' pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)))
#' pts_df <- sf::st_sf(a = 1:2, geom = pts)
#' sf::st_crs(pts_df) = "+proj=aea"
#' projection_units(pts_df)
projection_units <- function(x) {
  if (isTRUE(attr(class(x), "package") == "sp")) {
    x <- sf::st_as_sf(x)
  }
  units::as_units(sf::st_crs(x)$units)
}
