
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blsR

<!-- badges: start -->
<!-- badges: end -->

The goal of blsR is to make it easy to request time series data from the
BLS API and turn it into usable tabular data.

## Installation

You can install the released version of blsR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("blsR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("groditi/blsR")
```

## Example

This is a basic example which shows you how to retrieve one series as a
tibble or retrieve two series as a joined tibble.

``` r
library(blsR)
get_series_table('LNS14000000')
get_n_series_table(
  list(uer.men ='LNS14000001', uer.women = 'LNS14000002'),
  'your-api-key-here',
  start_year = 2005, end_year=2006
)
```
