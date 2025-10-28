
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sov

<!-- badges: start -->

<!-- badges: end -->

The R package `sov` calculates vote-specific Shapley-Owen values
(vs-SOVs) for assemblies with weighted voting, various voting
thresholds, and different numbers of dimensions.

## Description

This program calculates VS-SOVs and traditional SOVs in multidimensional
space.

- VS-SOVs utilize the “observed” normal vectors and their reflections to
  determine the proportion of times a voter pivots.  
- Traditional SOVs utilize all angles of the vote from 0 to 360 degrees
  for each dimension greater than 1.  
- The package should work for 1 to 4 dimensions, weighted voting, and
  various voting thresholds.

Separate functions are used for package-estimated inputs and
user-provided inputs.

## Installation

You can install the development version of sov from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("emmabbn/sov")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(sov)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
