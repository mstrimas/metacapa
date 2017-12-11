#' Generate dispersal kernel functions
#'
#' Generate linear or negative exponential dispersal kernel functions, taking a
#' distance, `d`, and returning the probability density of dispersing that
#' distance.
#'
#' The available dispersal kernel functions are:
#' - `dispersal_linear`: decays linearly from 1 and `d = 0` to 0 at `d = dmax`.
#' - `dispersal_negexp`: decays exponentially with rate parameter `alpha`.
#'
#' @param dmax numeric; maximum dispersal distance.
#' @param alpha numeric; exponential decay rate parameter, equal to the inverse
#'   of the mean dispersal distance.
#'
#' @return A dispersal kernel function.
#' @name dispersal
#' @examples
#' dl <- dispersal_linear(10)
#' dne <- dispersal_negexp(1/5)
#' dl(5)
#' dne(5)
NULL

#' @rdname dispersal
#' @export
dispersal_linear <- function(dmax) {
  stopifnot(is.numeric(dmax))
  stopifnot(!is.na(dmax))
  stopifnot(length(dmax) == 1)

  function(d) {
    d <- abs(d)
    ifelse(d < dmax, 1 - d / dmax, 0)
  }
}

#' @rdname dispersal
#' @export
dispersal_negexp <- function(alpha) {
  stopifnot(is.numeric(alpha))
  stopifnot(!is.na(alpha))
  stopifnot(length(alpha) == 1)
  stopifnot(alpha > 0)

  function(d) {
    exp(-alpha * abs(d))
  }
}
