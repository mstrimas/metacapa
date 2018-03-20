#' Metapopulation capacity
#'
#' Metapopulation capacity (Hanski and Ovaskainen 2000) is a relative measure of
#' the ability of a spatially explicit landscape to support a metapopulation of
#' a species. This function implements the modification proposed by Schnell et
#' al. (2013) that includes self colonization.
#'
#' @param x interpatch distance matrix or [patch_config] object.
#' @param a numeric; patch areas in the same order as the distance matrix.
#'   Only used if `x`` is a distance matrix.
#' @param f function; dispersal kernel function taking a distance, `d`, and
#'   returning the probability density of dispersing that distance.
#' @param ex numeric; extinction-area parameter, where extinction rate is given
#'   by \eqn{E / A^ex} for patch size \eqn{A}.
#' @param self logical; whether patches can recolonize themselves, this
#'   correction was proposed by Schnell et al. (2013) because the original
#'   favours networks of small patches over large contiguous patches
#' @param patch_mc logical; whether to return the patch-level contributions to
#'   the metapopulation capacity.
#' @param ... additional parameters passed to the dispersal survival function.
#'
#' @return The metapopulation capacity or, if `patch_mc = TRUE`, a list
#'   containing the metapopulation capacity (`capacity`) and the patch
#'   contributions (`patch_mc`).
#' @references
#'
#'  - Hanski, I, & O Ovaskainen (2000). The metapopulation capacity of a
#'  fragmented landscape. Nature, 404(6779), 755.
#'  [doi:10.1038/35008063](http://doi.org/10.1038/35008063)
#'  - Schnell, JK, GM Harris, SL Pimm, & GJ Russell (2013). Estimating
#'  Extinction Risk with Metapopulation Models of Large‐Scale Fragmentation.
#'  Conservation Biology, 27(3), 520-530.
#'  [doi:10.1111/cobi.12047](http://doi.org/10.1111/cobi.12047)
#' @export
#' @examples
#' d <- matrix(c(0, 2, 4, 2, 0, 3, 4, 3, 0), nrow = 3)
#' a <- c(1, 1.5, 0.5)
#' f <- dispersal_negexp(1)
#' meta_capacity(d, a, f)
#'
#' r <- raster::raster(nrows = 10, ncols = 10, crs = "+proj=aea")
#' r[] <- round(runif(raster::ncell(r)) * 0.7)
#' pc <- patch_config(r, "m")
#' f <- dispersal_negexp(1 / 100)
#' meta_capacity(pc, f = f)
meta_capacity <- function(x, a, f, ex = 0.5, self = TRUE,
                          patch_mc = FALSE, ...) {
  UseMethod("meta_capacity")
}

#' @export
meta_capacity.patch_config <- function(x, a, f, ex = 0.5, self = TRUE,
                                       patch_mc = FALSE, ...) {
  meta_capacity.matrix(x$distances, x$areas, f = f, ex = ex, self = self,
                       patch_mc = patch_mc)
}

#' @export
meta_capacity.dist <- function(x, a, f, ex = 0.5, self = TRUE,
                               patch_mc = FALSE, ...) {
  meta_capacity.matrix(as.matrix(x), a, f = f, ex = ex, self = self,
                       patch_mc = patch_mc)
}

#' @export
meta_capacity.matrix <- function(x, a, f, ex = 0.5, self = TRUE,
                                 patch_mc = FALSE, ...) {
  stopifnot(is.numeric(a))
  stopifnot(is.function(f))
  stopifnot(is.numeric(ex), length(ex) == 1)
  stopifnot(is.logical(self), length(self) == 1)
  stopifnot(is.logical(patch_mc), length(patch_mc) == 1)

  # if there are no patches return 0 for metapopulation capacity
  if (is.null(a)) {
    return(0)
  }

  # calculate landscape matrix
  m <- as.matrix(f(x, ...))
  diag(m) <- ifelse(self, 1, 0)
  m <- outer(a ^ ex, a) * m

  # capacity = dominant eigenvalue, i.e. that with largest magnitude
  # NOTE: Perron–Frobenius Theorem => a real, positive, square matrices will
  # have a positive dominant eigenvalue and eigenvector with positive
  # components. Since eigen function sorts in descending order of eigenvalue,
  # and dominant eigenvalue is positive, the dominant eigenvalue will appear
  # first
  e <- eigen(m, only.values = !patch_mc)

  if (patch_mc) {
    cap <- list(capacity = Re(e$values[1]), patch_mc = Re(e$vectors[, 1]) ^ 2)
  } else {
    cap <- Re(e$values[1])
  }

  return(cap)
}
