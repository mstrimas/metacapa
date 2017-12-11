#' Metapopulation capacity of a candidate reserve
#'
#' For use in conservation prioritization exercises, this function computes the
#' metapopulation capacities for a suite of species within a selected set of
#' planning units.
#'
#' @param pu [raster::RasterStack-class], [sp::SpatialPolygonsDataFrame], or
#'   [sf::sf] object; planning units and representation levels of features as
#'   layers (raster inputs) or columns (vector inputs).
#' @param x logical or binary; indicator variables specifying which planning
#'   units are included.
#' @param f named list of dispersal survival functions with names matching the
#'   layer/column names in `pu`.
#' @param threshold numeric; representation level threshold required to consider
#'   a species present in a planning unit.
#' @param ... additional arguments passed on to [meta_capacity()].
#'
#' @return A numeric vector of metapopulation capacities for each species.
#' @export
#' @examples
#' # generate data
#' r <- raster::raster(nrows = 10, ncols = 10, crs = "+proj=aea",
#'                     vals = sample(0:1, 100, replace = TRUE))
#' s <- raster::stack(r, r, r)
#' s[[2]][] <- sample(0:1, 100, replace = TRUE, prob = c(0.6, 0.4))
#' s[[3]][] <- sample(0:1, 100, replace = TRUE, prob = c(0.8, 0.2))
#' names(s) <- c("a", "b", "c")
#' selected <- sample(c(FALSE, TRUE), 100, replace = TRUE, prob = c(0.7, 0.3))
#' disp_f <- list(a = dispersal_negexp(1 / 0.01),
#'                b = dispersal_negexp(1 / 0.005),
#'                c = dispersal_negexp(1 / 0.02))
#' mc_reserve(s, selected, disp_f)
#'
#' # vector data
#' features <- raster::rasterToPolygons(s)
#' features <- sf::st_as_sf(features)
#' mc_reserve(features, selected, disp_f)
mc_reserve <- function(pu, x, f, threshold = 0, ...) {
  UseMethod("mc_reserve")
}


#' @export
mc_reserve.Raster <- function(pu, x, f, threshold = 0, ...) {
  stopifnot(length(x) == raster::ncell(pu))
  stopifnot(all(names(f) %in% names(pu)))
  if (!is.logical(x)) {
    x <- as.logical(x)
  }

  # subset to selected cells
  msk <- raster::raster(pu)
  msk[x] <- 1
  pu <- raster::mask(pu, msk)
  rm(msk)

  mc <- numeric(length(f))
  names(mc) <- names(f)
  for (i in seq_along(f)) {
    r <- pu[[names(f)[i]]] > threshold
    patch <- patch_config(r)
    mc[i] <- meta_capacity(patch, f = f[[i]], ...)
  }
  return(mc)
}

#' @export
mc_reserve.SpatialPolygonsDataFrame <- function(pu, x, f, threshold = 0, ...) {
  mc_reserve.sf(sf::st_as_sf(pu), x = x, f = f, threshold = threshold)
}

#' @export
mc_reserve.sf <- function(pu, x, f, threshold = 0, ...) {
  stopifnot(length(x) == nrow(pu))
  stopifnot(all(names(f) %in% names(pu)))
  if (!is.logical(x)) {
    x <- as.logical(x)
  }

  pu <- pu[x, ]
  mc <- numeric(length(f))
  names(mc) <- names(f)
  for (i in seq_along(f)) {
    # only include planning units with the species
    patch <- pu[pu[[names(f)[i]]] > threshold, ]
    # union adjoining planning units
    patch <- sf::st_union(patch)
    # disaggregate into patches
    patch <- sf::st_cast(patch, "POLYGON")
    patch <- patch_config(patch)
    mc[i] <- meta_capacity(patch, f = f[[i]], ...)
  }
  return(mc)
}
