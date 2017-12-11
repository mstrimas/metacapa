
<!-- README.md is generated from README.Rmd. Please edit that file -->
metacapa
========

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) [![Travis-CI Build Status](https://img.shields.io/travis/mstrimas/metacapa/master.svg?label=Mac%20OSX%20%26%20Linux)](https://travis-ci.org/mstrimas/matacapa) [![AppVeyor Build Status](https://img.shields.io/appveyor/ci/mstrimas/metacapa/master.svg?label=Windows)](https://ci.appveyor.com/project/mstrimas/metacapa)

Metapopulation capacity (Hanski & Ovaskainen 2000) is a relative measure of a spatially explicit landscape's ability to support a metapopulation of a species. It is calculated as the dominant eigenvalue of a landscape matrix that encapsulates the areas and interpatch distances of the landscape, as well as the dispersal ability of the species. Since metapopulation capacity can rank landscapes by their ability to support a species in the long term, it is a useful metric for evaluating alternative scenerios in the context of conservation plannaing and prioritization.

The `metacapa` package implements the calculation of metapopulation capacity, both the original formulation and the modification proposed by Schnell et al. (2013) to account for self-colonization.

Installation
------------

You can install metacapa from github with:

``` r
# install.packages("devtools")
devtools::install_github("mstrimas/metacapa")
```

Example
-------

Given a configuration of habitat patches and a disersal kernel function of a species, calculate the metapopulation capacity.

``` r
library(raster)
#> Loading required package: sp
library(metacapa)

# generate a network of patches
r <- raster::raster(nrows = 10, ncols = 10, crs = "+proj=aea")
r[] <- round(runif(raster::ncell(r)) * 0.7)

# exponential dispersal kernel
f <- dispersal_negexp(1 / 100)

# calulate the areas and interpatch distances
pc <- patch_config(r, "m")
#> Loading required namespace: igraph
#> Loading required namespace: rgeos

# metapopulation capacity
meta_capacity(pc, f = f)
#> [1] 1417446
```

In the context of conservation prioritization, the landscape is divided into planning units, a subset of which are selected for inclusion in a candidate reserve network. Metapopulation capcity can be calcualted for a suite of species, given data on the occurrence of each species within each planning unit.

``` r
# generate data for three species
# distributions
r <- raster(nrows = 10, ncols = 10, 
            #xmn = 0, xmx = 1, ymn = 0, ymx = 1,
            crs = "+proj=aea",
            vals = sample(0:1, 100, replace = TRUE))
s <- stack(r, r, r)
s[[2]][] <- sample(0:1, 100, replace = TRUE, prob = c(0.6, 0.4))
s[[3]][] <- sample(0:1, 100, replace = TRUE, prob = c(0.8, 0.2))
names(s) <- c("a", "b", "c")
# dispersal kernel function
disp_f <- list(a = dispersal_negexp(1 / 0.01),
               b = dispersal_negexp(1 / 0.005),
               c = dispersal_negexp(1 / 0.02))

# select 30% of planning units for inclusion
selected <- sample(c(FALSE, TRUE), 100, replace = TRUE, prob = c(0.7, 0.3))

# calculate metapopulation capacity for each species
mc_reserve(s, selected, disp_f)
#>            a            b            c 
#> 1.324761e-04 8.578884e-05 4.666467e-05
```
