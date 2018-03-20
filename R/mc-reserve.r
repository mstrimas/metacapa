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
#' @param scale numeric; vales to rescale the metapopulation capacity by, e.g.
#'   to normalize them between 0-1 one can scale by the maximum,
#'   species-specific metapopulation capacity given by selecting all planning
#'   units. This should be a vector of length 1 or equal in length to the number
#'   of features.
#' @param units character; metapopulation capacity depends on the units used for
#'   the areas and distances, this argument determines whether these are
#'   measured in meters or kilometers.
#' @param parallel logical; whether to parallelize the metapopulation capacity
#'   calculations over the species. Parallelization is accomplished using
#'   [foreach::foreach()] and requires registering a parallel backend with
#'   [doParallel::registerDoParallel()] prior to calling this function.
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
#'
#' # run in parallel
#' \dontrun{
#' library(doParallel)
#' registerDoParallel(3)
#' mc_reserve(features, selected, disp_f, parallel = TRUE)
#' }
mc_reserve <- function(pu, x, f, threshold = 0, scale = 1, units = c("km", "m"),
                       parallel = FALSE, ...) {
  UseMethod("mc_reserve")
}

#' @export
mc_reserve.Raster <- function(pu, x, f, threshold = 0, scale = 1,
                              units = c("km", "m"), parallel = FALSE, ...) {
  stopifnot(length(x) == raster::ncell(pu))
  stopifnot(all(names(f) %in% names(pu)))
  stopifnot(is.logical(parallel) && length(parallel) == 1)
  units <- match.arg(units)
  if (!is.logical(x)) {
    x <- as.logical(x)
  }

  # handle parallelization
  if (parallel) {
    if (foreach::getDoParRegistered()) {
      `%mydo%` <- foreach::`%dopar%`
    } else {
      stop("A parallel backend must be registered if parallel = TRUE.")
    }
  } else {
    `%mydo%` <- foreach::`%do%`
  }

  # subset to selected cells
  msk <- raster::raster(pu)
  msk[x] <- 1
  pu <- raster::mask(pu, msk)
  rm(msk)

  i <- NULL
  mc <- foreach::foreach (i = seq_along(f), .combine = c) %mydo% {
    r <- pu[[names(f)[i]]] > threshold
    patch <- patch_config(r, units = units)
    meta_capacity(patch, f = f[[i]], patch_mc = FALSE, ...)
  }
  names(mc) <- names(f)
  return(mc / scale)
}

#' @export
mc_reserve.SpatialPolygonsDataFrame <- function(pu, x, f, threshold = 0,
                                                scale = 1, units = c("km", "m"),
                                                parallel = FALSE, ...) {
  mc_reserve.sf(sf::st_as_sf(pu), x = x, f = f, threshold = threshold)
}

#' @export
mc_reserve.sf <- function(pu, x, f, threshold = 0, scale = 1,
                          units = c("km", "m"), parallel = FALSE, ...) {
  stopifnot(length(x) == nrow(pu))
  stopifnot(all(names(f) %in% names(pu)))
  stopifnot(length(scale) %in% c(1, length(f)))
  stopifnot(is.logical(parallel) && length(parallel) == 1)
  units <- match.arg(units)
  if (!is.logical(x)) {
    x <- as.logical(x)
  }

  # handle parallelization
  if (parallel) {
    if (foreach::getDoParRegistered()) {
      `%mydo%` <- foreach::`%dopar%`
    } else {
      stop("A parallel backend must be registered if parallel = TRUE.")
    }
  } else {
    `%mydo%` <- foreach::`%do%`
  }

  pu_x <- pu[x, ]
  i <- NULL
  mc <- foreach::foreach (i = seq_along(f), .combine = c) %mydo% {
    # only include planning units with the species
    patch <- pu_x[pu_x[[names(f)[i]]] > threshold, ]
    if (nrow(patch) == 0) {
      return(0)
    }
    # union adjoining planning units
    patch <- sf::st_union(patch)
    # disaggregate into patches
    patch <- sf::st_cast(patch, "POLYGON")
    patch <- patch_config(patch, units = units)
    meta_capacity(patch, f = f[[i]], patch_mc = FALSE, ...)
  }
  # deal with an odd scenario when parallizing doesn't work
  if (parallel && is.null(mc)) {
    message("Problem with parallelization, running sequentially.")
    mc <- mc_reserve(pu = pu, x = x, f = f, threshold = threshold,
                     scale = scale, units = units, parallel = FALSE, ...)
    return(mc)
  }
  names(mc) <- names(f)
  return(mc / scale)
}
