#' Generate neighbour selection function
#'
#' Optimization by simulated annealing requires a function to select a
#' neighbouring state to test given the current state. This function generates
#' such a neighbour selection function. The resulting function will swap the
#' status of a single planning unit.
#'
#' In an attempt to more intelligently select
#' a planning unit to switch, only those planning units meeting the following
#' criteria will be considered:
#'
#' - Units touching the boundary of the existing reserve network, whether inside
#' or outside.
#' - Units within a defined distance, `buffer`, of the boundary of the existing
#' reserve network.
#'
#' This ensures that units in the interior of an existing reserve or far from an
#' existing reserve are not changed.
#'
#' @param pu [raster::RasterStack-class], [sp::SpatialPolygonsDataFrame], or
#'   [sf::sf] object; planning units.
#' @param buffer numeric; a buffer around the perimeter of the currently
#'   selected planning units in the units of the projection. Any planning units
#'   within this buffer will be considered for switching when generating a
#'   neighbouring state.
#' @param locked name of a binary column (for vector data) or layer (for raster
#'   data) that specified planning units that are locked in or out and should be
#'   left unchanged. Alternatively, a integer vector of planning unit numbers
#'   may be supplied specifying units to leave unchanged.
#' @param recalculate integer; frequency at which the set of planning units to
#'   consider for switching should be recalculated, e.g. a value of 100 would
#'   result in recalculating every 100 calls to the returned function. This
#'   process of recalculating can be computationally intensive for large sets
#'   of planning units, so choosing a larger value for `recalculate` will speed
#'   up processing time.
#'
#' @return A function to choose a neighbouring state given a vector of binary
#' decision variables. To return the list of planning units under consideration
#' for switching use `pu_list = TRUE`.
#' @export
#' @examples
#' # generate data
#' r <- raster::raster(nrows = 10, ncols = 10, crs = "+proj=aea", vals = 0)
#' # lock some units to be unchanged
#' names(r) <- "locked_out"
#' r[sample(1:100, 10)] <- 1
#' pus <- raster::rasterToPolygons(r)
#' selected <- sample(c(FALSE, TRUE), 100, replace = TRUE, prob = c(0.9, 0.1))
#'
#' # generate neighbour selection function
#' neighbour <- generate_nsf(pus, buffer = 20, locked = "locked_out")
#'
#' # neighbouring solution
#' print(devtools::session_info())
#' neighbour(selected)
#' neighbour(selected, pu_list = TRUE)
generate_nsf <- function(pu, buffer, locked, recalculate = 1L) {
  stopifnot(inherits(pu, c("sf", "SpatialPolygonsDataFrame")))
  stopifnot(is.numeric(buffer), length(buffer) == 1, buffer >= 0)
  stopifnot(is_count(recalculate))

  # handle locked in/out
  if (missing(locked)) {
    locked <- integer()
  } else if (is.character(locked)) {
    stopifnot(length(locked) == 1)
    stopifnot(locked %in% names(pu))
    locked <- which(as.logical(pu[[locked]][]))
  } else {
    if (inherits(pu, "Raster")) {
      stopifnot(inherits(locked, "RasterLayer"))
      stopifnot(raster::compareRaster(pu, locked))
      locked <- which(locked[] == 1)
    } else {
      stopifnot(is.integer(locked))
      stopifnot(locked > 0L || locked <= nrow(pu))
    }
  }

  if (inherits(pu, "SpatialPolygonsDataFrame")) {
    pu <- sf::st_as_sf(pu)
  }

  consider <- integer()
  counter <- 0
  function(x, pu_list = FALSE) {
    x <- as.logical(x)
    if ((counter %% recalculate) == 0) {
      patches <- sf::st_union(pu[x, ])
      # edge of existing reserves, both inside and outside
      borders <- sf::st_cast(patches[[1]], "MULTILINESTRING")
      border_pu <- purrr::map_lgl(sf::st_intersects(pu, borders),
                                  ~ length(.x) > 0)

      # outside reserves up to given buffer
      buff_out <- sf::st_buffer(patches, dist = buffer)
      buff_pu <- purrr::map_lgl(sf::st_intersects(pu, buff_out),
                                ~ length(.x) > 0)
      # exclude pus inside reserve
      buff_pu <- buff_pu & !x

      consider <<- setdiff(which(buff_pu | border_pu), locked)
    }
    # just return the list of planning units to consider
    if (pu_list) {
      return(consider)
    }
    counter <<- counter + 1
    i <- sample(consider, 1)
    x[i] <- !x[i]
    return(x)
  }
}
