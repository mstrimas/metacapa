#' Generate objective function
#'
#' Generate an objective function to perform the metapopulation capacity-based
#' conservation prioritization developed by Strimas-Mackey and Brodie (2018).
#' This objective function maximizes the average metapopulation capacity across
#' species for a fixed budget. There is an optional additional term, similar to
#' the boundary term in Marxan, that can be used to favour more compact
#' solutions.
#'
#' @inheritParams mc_reserve
#' @param budget numeric; the target budget for the prioritization exercise.
#' @param delta numeric; a scale factor to apply to the budget term in the
#'   objective function that determines. To use simulated annealing for the
#'   optimization, the budget constraint must be incorporated directly into the
#'   objective function. This argument determines the relative importance to be
#'   placed on maximizing the metapopulation capacity compared to meeting the
#'   budget constraint.
#' @param blm numeric; a scale factor to apply to the perimeter term in the
#'   objective function that determines the relative importance to be
#'   placed on maximizing the metapopulation capacity compared to producing more
#'   compact solutions.
#' @param benefit function; a function to apply to the scaled metapopulation
#'   capacity before averaging across species. Often a saturating function is
#'   used to ensure that less value is given to changes in metapopulation
#'   capacity for species that are already near their maximum.
#' @param cost name of the cost column or layer in the planning unit object,
#'   `pu`, a numeric vector of costs in the same order as the rows of `pu` (for
#'   vector planning units), or a `RasterLayer` of costs (for `Raster` planning
#'   units.
#'
#' @return A function that, given a logical vector specifying which planning
#'   units are selected, returns the corresponding objective function value. If
#'   the objective function is called with `components = FALSE`, then a list is
#'   returned with the three components of the object function:
#'   - `mc`: the metapoplation capacity of each species.
#'   - `cost`: the cost of the reserve network.
#'   - `boundary`: the total boundary length, in km, of the reserve network.
#' @export
#' @examples
#' # generate features
#' r <- raster::raster(nrows = 10, ncols = 10, crs = "+proj=aea",
#'                     vals = sample(0:1, 100, replace = TRUE))
#' s <- raster::stack(r, r, r)
#' s[[2]][] <- sample(0:1, 100, replace = TRUE, prob = c(0.6, 0.4))
#' s[[3]][] <- sample(0:1, 100, replace = TRUE, prob = c(0.8, 0.2))
#' names(s) <- c("a", "b", "c")
#' features <- raster::rasterToPolygons(s)
#' features <- sf::st_as_sf(features)
#' # cost
#' features$cost <- runif(nrow(features))
#' # dispersal functions
#' disp_f <- list(a = dispersal_negexp(1 / 0.01),
#'                b = dispersal_negexp(1 / 0.005),
#'                c = dispersal_negexp(1 / 0.02))
#'
#' # calculate scale factors
#' scale_mc <- mc_reserve(features, rep(TRUE, nrow(features)), disp_f)
#'
#' # set budget at 50% of total
#' budget <- 0.5 * sum(features$cost)
#'
#' # build an objective function
#' objective <- generate_objective(features, disp_f, budget, delta = 0.001,
#'                                 blm = 0.001, units = "km")
#'
#' # calculate objective
#' selected <- sample(c(FALSE, TRUE), 100, replace = TRUE, prob = c(0.7, 0.3))
#' objective(selected, components = TRUE)
#' objective(selected)
generate_objective <- function(pu, f, budget, delta, blm = 0,
                               scale = rep(1, length(f)),
                               benefit = function(y) y,
                               cost = "cost",
                               units = c("m", "km"),
                               parallel = FALSE) {
  stopifnot(inherits(pu, c("Raster", "sf", "SpatialPolygonsDataFrame")))
  stopifnot(is.numeric(budget), length(budget) == 1, budget > 0)
  stopifnot(is.numeric(delta), length(delta) == 1, delta > 0)
  stopifnot(is.numeric(blm), length(blm) == 1)
  stopifnot(is.function(benefit))
  units <- match.arg(units)

  if (is.character(cost)) {
    stopifnot(length(cost) == 1, cost %in% names(pu))
    cost <- pu[[cost]]
  } else {
    if (inherits(pu, "Raster")) {
      stopifnot(inherits(cost, "RasterLayer"), raster::compareRaster(pu, cost))
    } else {
      stopifnot(is.numeric(cost), length(cost) == nrow(pu))
    }
  }

  function(x, components = FALSE) {
    x <- as.logical(x)
    # metapopulation capacity term
    mc <- mc_reserve(pu, x, f, scale = scale, units = units,
                     parallel = parallel)
    mc_term <- sum(benefit(mc))

    # cost
    cost_total <- sum(cost[x], na.rm = TRUE)
    budget_term <- max(cost_total - budget, 0)

    # perimeter term
    if (blm > 0) {
      perimeter <- as.numeric(reserve_perimeter(pu, x, units = units))
      if (components) {
        return(list(mc = mc, cost = cost_total, boundary = perimeter))
      } else {
        return(mc_term - delta * budget_term - blm * perimeter)
      }
    } else {
      if (components) {
        return(list(mc = mc, cost = cost_total))
      } else {
        return(mc_term - delta * budget_term)
      }
    }
  }
}
