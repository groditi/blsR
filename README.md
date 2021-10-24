
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

## Getting Started

`blsR` provides functions for retrieving and processing data from the
BLS API. The functions are divided into 4 categories: query generators,
query requests, result processors, and the user-friendly simplified
interface. It was designed with a three-step workflow in mind:

-   Identify which data you would like to retrieve and create a query.
-   Make an http request to execute a query (\[`bls_request()`\])
-   Transform the response data to fit the user workflow

If the only data needed is periods and values, then the functions
`get_series_table`, `get_series_tables`, and `get_n_series_table` are
all that a user will need. An API key is not required to use this
package but users will be restricted in how many years of data can be
retrieved per request, how many series can be included per request, and
how many requests can be made in a day. An API Key can be obtained at:
<https://data.bls.gov/registrationEngine/> \#\# Example

This is a basic example which shows you how to retrieve one series as a
tibble or retrieve two series as a joined tibble.

## Example

To request a single series as a tibble:

``` r
library(blsR)
uer <- get_series_table('LNS14000000', NA, 2006, 2006)

# uer: 
# # A tibble: 12 x 5
#    year  period periodName value footnotes       
#    <chr> <chr>  <chr>      <chr> <list>          
#  1 2006  M12    December   4.4   <named list [0]>
#  2 2006  M11    November   4.5   <named list [0]>
#  3 2006  M10    October    4.4   <named list [0]>
#  4 2006  M09    September  4.5   <named list [0]>
#  5 2006  M08    August     4.7   <named list [0]>
#  6 2006  M07    July       4.7   <named list [0]>
#  7 2006  M06    June       4.6   <named list [0]>
#  8 2006  M05    May        4.6   <named list [0]>
#  9 2006  M04    April      4.7   <named list [0]>
# 10 2006  M03    March      4.7   <named list [0]>
# 11 2006  M02    February   4.8   <named list [0]>
# 12 2006  M01    January    4.7   <named list [0]>

tidy_periods(uer)
# # A tibble: 12 x 4
#     year month value footnotes       
#    <int> <int> <chr> <list>          
#  1  2006     1 4.7   <named list [0]>
#  2  2006     2 4.8   <named list [0]>
#  3  2006     3 4.7   <named list [0]>
#  4  2006     4 4.7   <named list [0]>
#  5  2006     5 4.6   <named list [0]>
#  6  2006     6 4.6   <named list [0]>
#  7  2006     7 4.7   <named list [0]>
#  8  2006     8 4.7   <named list [0]>
#  9  2006     9 4.5   <named list [0]>
# 10  2006    10 4.4   <named list [0]>
# 11  2006    11 4.5   <named list [0]>
```

To request multiple series as one tibble

``` r
get_n_series_table(
  list('LNS14000001', 'LNS14000002'), NA, start_year = 2005, end_year=2006
  )
# # A tibble: 24 x 4
#     year period LNS14000001 LNS14000002
#    <int> <chr>  <chr>       <chr>      
#  1  2006 M12    4.5         4.4        
#  2  2006 M11    4.5         4.5        
#  3  2006 M10    4.4         4.4        
#  4  2006 M09    4.4         4.7        
#  5  2006 M08    4.7         4.6        
#  6  2006 M07    4.7         4.7        
#  7  2006 M06    4.6         4.6        
#  8  2006 M05    4.7         4.5        
#  9  2006 M04    4.7         4.7        
# 10  2006 M03    4.7         4.7        
# # ... with 14 more rows

get_n_series_table(
  list(uer.men ='LNS14000001', uer.women = 'LNS14000002'),
  NA,
  start_year = 2005, end_year=2006, tidy=TRUE
)

# # A tibble: 24 x 4
#     year month uer.men uer.women
#    <int> <int> <chr>   <chr>    
#  1  2005     1 5.4     5.1      
#  2  2005     2 5.5     5.3      
#  3  2005     3 5.3     5.1      
#  4  2005     4 5.1     5.2      
#  5  2005     5 5.0     5.2      
#  6  2005     6 5.0     5.1      
#  7  2005     7 4.9     5.1      
#  8  2005     8 4.9     4.9      
#  9  2005     9 5.0     5.1      
# 10  2005    10 4.8     5.1      
# # ... with 14 more rows
```
