% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/patch-config.r
\name{patch_config}
\alias{patch_config}
\title{Calculate interpatch distances and areas}
\usage{
patch_config(x, units = c("m", "km"))
}
\arguments{
\item{x}{patch network in raster or vector format, see Details.}

\item{units}{character; whether distances and areas should be in meters or
kilometers.}
}
\value{
A \code{patch_config} object with two elements: the interpatch distances
(\code{distances}) and patch areas (\code{areas}).
}
\description{
Given a set of patches in raster or vector format, calculate the areas and
interpatch distances.
}
\details{
If the patch network is provided as a \code{Raster} object, cells included in the
network should be 1, and background cells should be 0 or \code{NA}. Patches can
also be provided as polygons from either the \code{sf} or \code{sp} packages. Spatial
data must be provided in projected coordinates with units of meters.
}
\examples{
# raster
r <- raster::raster(nrows = 10, ncols = 10, crs = "+proj=aea")
r[] <- round(runif(raster::ncell(r)) * 0.7)
patch_config(r, units = "m")

# polygon
p_poly <- raster::rasterToPolygons(r, dissolve = TRUE)
p_poly <- sf::st_as_sf(p_poly)
p_poly <- p_poly[p_poly$layer == 1, ]
p_poly <- sf::st_cast(p_poly, "POLYGON")
patch_config(p_poly, units = "km")
}
