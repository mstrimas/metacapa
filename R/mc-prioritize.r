#' Metapopulation capacity-based prioritization
#'
#' This function is a thin wrapper around [optim()] that performs conservation
#' prioritization using simulated annealing.
#'
#' @param objective an objective function to optimize over, typically created by
#'   [generate_objective()].
#' @param nsf a neighbour selection function, typically created by
#'   [generate_nsf()].
#' @param x logical vector specifying which planning units are selected as a
#'   starting point. Traditional conservation prioritization methods, such as
#'   those implemented by the `prioritizr` R package, can be used to generate
#'   starting solutions.
#' @param n integer; number of simulated annealing iterations.
#' @param itemp numeric; initial temperature for the simulated annealing
#'   cooling schedule. Higher temperatures will result in bad changes being
#'   more likely to be accepted.
#' @param n_temp integer; number of objective function evaluations at each
#'   temperature in the annealing process.
#' @param maximize logical; whether the objective function is to be maximized or
#'   minimized. The standard objective function generated by
#'   [generate_objective()] is meant to be maximized.
#' @param verbose logical; whether to print the simulated annealing output as
#'   the heuristic progresses.
#'
#' @return A logical vector indicating which planning units are selected in the
#'   final solution.
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
#' # build an objective function and neighbour selection function
#' objective <- generate_objective(features, disp_f, budget, delta = 0.001,
#'                                 blm = 0.001, units = "km")
#' nsf <- generate_nsf(features, buffer = 20)
#'
#' # random starting point
#' x_start <- sample(c(FALSE, TRUE), 100, replace = TRUE, prob = c(0.9, 0.1))
#'
#' # optimize
#' mc_prioritize(objective, nsf, x_start, n = 50L)
mc_prioritize <- function(objective, nsf, x, n = 10000L, itemp = 10,
                          n_temp = 10L, maximize = TRUE, verbose = TRUE) {
  stopifnot(is.function(objective), is.function(nsf))
  stopifnot(is.logical(x) || all(x %in% c(0, 1)))
  stopifnot(is_count(n))
  stopifnot(is.numeric(itemp), length(itemp) == 1)
  stopifnot(is_count(n_temp))
  stopifnot(is.logical(maximize), length(maximize) == 1)
  stopifnot(is.logical(verbose), length(verbose) == 1)

  x_out <- stats::optim(par = x, fn = objective, gr = nsf,
                        method = "SANN",
                        control = list(fnscale = ifelse(maximize, -1, 1),
                                       temp = itemp, maxit = n, tmax = n_temp,
                                       trace = as.integer(verbose),
                                       REPORT = 100))
  return(as.logical(x_out$par))
}
