

#' Retrieve a single time series from BLS API and return a tibble
#'
#' @param series_id a BLS time-series ID
#' @param api_key a mandatory API key, available from
#'  <https://data.bls.gov/registrationEngine/>
#' @param start_year optional numeric 4-digit year
#' @param end_year optional numeric 4-digit year
#' @param year_limit optional number of years to paginate request by. Defaults
#' to 20, the API request cap when using API key. Requests made without an API
#' key are capped to 10 years.
#' @param parse_values optional boolean. If set to `true` (default) it will
#' attempt to parse the contents of `value` and cast numeric strings as numeric
#' values. If set to `false` it will keep return a `value` column of strings.
#' @param ... additional arguments to pass to [`get_series`]
#'
#' @return a tibble of observations or `NA` if the request had zero results.
#'
#' @family blsR-requests
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_series_table('LNS14000001',2005,2006)
#' }
#'

get_series_table <- function(
  series_id, api_key,
  start_year=NA, end_year=NA, year_limit=20, parse_values=TRUE, ...
  ){

  if(is.na(api_key)) year_limit <- 10

  series <- get_series(series_id, start_year, end_year, year_limit, api_key=api_key, ...)
  if(length(series$data) > 0)
    return(data_as_table(series$data, parse_values))

  NA
}


#' Retrieve multiple time series in one API request and return a list of tibbles
#'
#' @param series_ids a list or character vector of BLS time-series IDs. If the
#' list items are named then the names will be used in the returned list
#' @param api_key a mandatory API key, available from
#'  <https://data.bls.gov/registrationEngine/>
#' @param start_year optional numeric 4-digit year
#' @param end_year optional numeric 4-digit year
#' @param year_limit optional number of years to paginate request by. Defaults
#' to 20, the API request cap when using API key. Requests made without an API
#' key are capped to 10 years.
#' @param parse_values optional boolean. If set to `true` (default) it will
#' attempt to parse the contents of `value` and cast numeric strings as numeric
#' values. If set to `false` it will keep return a `value` column of strings.
#' @param ... additional arguments to pass to [`get_n_series`]
#'
#' @return a list of tibbles. Series requests which return observations will be
#' a tibble. Series with no observations will be `NA`
#'
#' @family blsR-requests
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_series_tables(
#'   list(uer.men ='LNS14000001', uer.women = 'LNS14000002')),
#'   'your-api-key-here'
#' )
#' get_series_tables(
#'   list(uer.men ='LNS14000001', uer.women = 'LNS14000002'),
#'   'your-api-key-here',
#'   2005,2006
#' )
#' }
#'

get_series_tables <- function(
  series_ids, api_key,
  start_year=NA, end_year=NA, year_limit=20, parse_values=TRUE, ...
){

  series <- get_n_series(series_ids, api_key, start_year, end_year, year_limit, ...)
  series_data <- purrr::map(series, 'data')
  purrr::modify_if(
    purrr::modify_if(series_data, rlang::is_empty, ~NA),
    purrr::negate(rlang::is_na),
    data_as_table, parse_values
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
#' @param tidy optional boolean. Return will use [`tidy_periods()`] if true
#' @param parse_values optional boolean. If set to `true` (default) it will
#' attempt to parse the values of requested data series and cast numeric strings
#' as numeric values. If set to `false` it will retain them as strings.
#' @param ... additional arguments to pass to [`get_series_tables`]
#'
#' @return a tibble of multiple merged time series
#'
#' @family blsR-requests
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_n_series_table(
#'   list(uer.men ='LNS14000001', uer.women = 'LNS14000002'),
#'   'your-api-key-here',
#'   start_year = 2005, end_year=2006
#' )
#' }
#'
get_n_series_table <- function(
  series_ids, api_key,
  start_year=NA, end_year=NA, tidy=FALSE, parse_values=TRUE, ...
){
  tables <- get_series_tables(
    series_ids = series_ids,
    api_key = api_key,
    start_year = start_year,
    end_year = end_year,
    parse_values = parse_values,
    ...
  )

  if(tidy){
    return(merge_tidy_tables(lapply(tables, tidy_periods)))
  }

  return(merge_tables(tables))
}
