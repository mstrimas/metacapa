#' Get projection units of an sf object
#'
#' @param x an sf object
#' @return A units object with the units of the projection.
#' @export
#' @examples
#' sfc <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)))
#' sf <- sf::st_sf(a = 1:2, geom = sfc)
#' sf::st_crs(sf) = "+proj=aea"
#' projection_units(sfc)
projection_units <- function(x) {
  if (isTRUE(attr(class(x), "package") == "sp")) {
    x <- sf::st_as_sf(x)
  }
  units::make_unit(sf::st_crs(x)$units)
}
