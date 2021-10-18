

#' Retrieve a single time series from BLS API and return a tibble
#'
#' @param series_id a BLS time-series ID
#' @param start_year optional numeric 4-digit year
#' @param end_year optional numeric 4-digit year
#' @param ... additional parameters to pass to [`bls_request`]
#'
#' @return a tibble of observations
#' @export
#'
#' @examples
get_series_table <- function(series_id, start_year=NA, end_year=NA, ...){
  results <- bls_request(query_series(series_id, ...))
  data_as_table(results$series$data)
}


#' Retrieve multiple time series in one API request and return a list of tibbles
#'
#' @param series_ids a list or character vector of BLS time-series IDs. If the
#' list items are named then the names will be used in the returned list
#' @param api_key a mandatory API key, available from
#'  <https://data.bls.gov/registrationEngine/>
#' @param start_year optional numeric 4-digit year
#' @param end_year optional numeric 4-digit year
#' @param ... additional parameters to pass to [`query_n_series`]
#'
#' @return a list of tables
#' @export
#'
#' @examples
get_series_tables <- function(series_ids, api_key, start_year=NA, end_year=NA, ...){
  lapply(
    get_n_series(series_ids, api_key, start_year=NA, end_year=NA, ...),
    function(x) { data_as_table(x[['data']]) }
  )
}


#' Retrieve multiple time series in one API request and return a single tibble
#'
#' @param series_ids a named list of BLS time-series IDs. If the
#' list items are named then the names will be used in the returned list
#' @param api_key a mandatory API key, available from
#'  <https://data.bls.gov/registrationEngine/>
#' @param start_year optional numeric 4-digit year
#' @param end_year optional numeric 4-digit year
#' @param ... additional parameters to pass to [`query_n_series`]
#'
#' @return
#' @export
#'
#' @examples
get_n_series_table <- function(series_ids, api_key, start_year=NA, end_year=NA, tidy=TRUE, ...){
  tidy_periods(merge_tables(
    get_series_tables(series_ids, api_key, start_year=NA, end_year=NA, ...)
  ) )
}
