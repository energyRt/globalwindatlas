
<!-- README.md is generated from README.Rmd. Please edit that file -->

# globalwindatlas

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/globalwindatlas)](https://CRAN.R-project.org/package=globalwindatlas)

<!-- badges: end -->

The goal of globalwindatlas is to download high resolution GIS files
(250m grid) from …

## Installation

You can install the development version of globalwindatlas from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("energyRt/globalwindatlas")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(globalwindatlas)
## set directory for downloaded files, also will be used for quick files access
set_gwa_dir("data/gwa")
## download wind capacity factors for wind-class #1
get_wind_capacity_factor("ISL", IEC = 1)
## download wind speed data
ISL <- get_wind_speed("ISL", height = 100) # Iceland
ISL

MAR <- get_wind_speed("MAR", height = 100) # Morocco
MAR <- get_wind_speed("MAR", height = 150) # Morocco

## plot data
library(terra)
tr <- rast(MAR)
plot(tr)
plot(rast(ISL))
```

## References

<https://globalwindatlas.info/download/gis-files>
