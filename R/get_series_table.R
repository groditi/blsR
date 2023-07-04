#' Retrieve a time series from BLS API as a tibble
#'
#' @inheritParams get_series
#' @inheritParams data_as_table
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
  series_id, api_key = bls_get_key(),
  start_year=NULL, end_year=NULL, year_limit=NULL, parse_values=TRUE, ...
  ){

  if(!rlang::is_scalar_character(api_key)) year_limit <- 10

  series <- get_series(series_id, start_year, end_year, year_limit, api_key=api_key, ...)
  if(length(series$data) > 0)
    return(data_as_table(series$data, parse_values))

  NA
}


#' Retrieve multiple time series as in one API request as tibbles
#'
#' @inheritParams get_n_series
#' @inheritParams get_series_table
#' @inheritDotParams get_n_series series_limit span
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
#'
#' blsr_set_key('your-api-key-here-xxxxxxxxxxxxxx')
#'
#' get_series_tables(
#'   list(uer.men ='LNS14000001', uer.women = 'LNS14000002')
#' )
#' get_series_tables(
#'   list(uer.men ='LNS14000001', uer.women = 'LNS14000002'),
#'   2005,2006
#' )
#' }
#'

get_series_tables <- function(
  series_ids, api_key = bls_get_key(),
  start_year=NULL, end_year=NULL, year_limit=NULL, parse_values=TRUE, ...
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
#' @inheritParams get_series_tables
#' @param tidy optional boolean. Return will use [`tidy_periods()`] if true
#' @inheritDotParams get_n_series series_limit span
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
#'   start_year = 2005, end_year=2006
#' )
#' }
#'
get_n_series_table <- function(
  series_ids, api_key = bls_get_key(),
  start_year=NULL, end_year=NULL, year_limit=NULL,
  tidy=FALSE, parse_values=TRUE, ...
){
  tables <- get_series_tables(
    series_ids = series_ids,
    api_key = api_key,
    start_year = start_year,
    end_year = end_year,
    year_limit = year_limit,
    parse_values = parse_values,
    ...
  )

  if(tidy){
    return(merge_tidy_tables(lapply(tables, tidy_periods)))
  }

  return(merge_tables(tables))
}
