
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PatricksPlants

<!-- badges: start -->

<!-- badges: end -->

The goal of PatricksPlants is to make Patrick Alexander’s plant
occurance observations more accessible for analysis.

A data frame is provided with all observations and metadata coded as
presence-only. Presence-absence data may reasonably be infered from this
and a function will be provided to do so for a specified set of species
or for the entire dataset. Beware that presence-absence can get rather
large for cross-product of sites and plants in the dataset. At the time
of this writing, the presence-only data were around 5,000 rows, while
the presence-absence data were over 10,000,000 rows.

A function is provided for reading Patrick’s xlsx file, in which he
records his obsevations from his field notebook. The function returns a
data frame of presence-only or presence-absence with metadata for sites,
surveys, and plant taxonomy. The presence-only output is identical in
format to the supplied
dataset.

## Installation

<!-- You can install the released version of PatricksPlants from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("PatricksPlants") -->

<!-- ``` -->

You ~~can~~ *will be able to* install the development version of
PatricksPlants from GitHub with:

``` r
install.packages("devtools")
devtools::install_github("gregorypenn/PatricksPlants")
```

## Examples

This is a basic example which shows you how to solve a common problem:

``` r
# library(PatricksPlants)
## basic example code
```

<!-- Don't forget to commit and push figure files, so they display on GitHub! -->
