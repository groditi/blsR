

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
#' @param ... additional parameters to pass to [`bls_request`]
#'
#' @return a tibble of observations
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

get_series_table <- function(series_id, api_key, start_year=NA, end_year=NA, year_limit=20, ...){
  if(is.na(api_key)) year_limit <- 10
  #auto-magically paginate results
  if(!is.na(start_year) && (end_year - start_year) >= year_limit ){
    tables <- lapply(
      seq(0, ceiling(((end_year - start_year) + 1) / year_limit) - 1) * year_limit,
      function(x) get_series_table(
        series_id = series_id,
        api_key = api_key,
        start_year = start_year+x,
        end_year = min(end_year, start_year+(x+(year_limit-1))),
        ...
      )
    )

    return(purrr::reduce(tables, dplyr::union_all))
  }
  #query_series(series_id, start_year, end_year, ...)
  data_as_table(get_series(series_id, start_year, end_year, api_key=api_key, ...)$data)
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
#' @param ... additional parameters to pass to [`query_n_series`]
#'
#' @return a list of tables
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

get_series_tables <- function(series_ids, api_key, start_year=NA, end_year=NA, year_limit=20, ...){
  if(is.na(api_key)) year_limit <- 10
  #auto-magically paginate results
  if(!is.na(start_year) && (end_year - start_year) >= year_limit ){
    pages <- lapply(
      seq(0, ceiling(((end_year - start_year) + 1) / year_limit) - 1) * year_limit,
      function(x) get_series_tables(
        series_ids = series_ids,
        api_key = api_key,
        start_year = start_year+x,
        end_year = min(end_year, start_year+(x+(year_limit-1))),
        ...
      )
    )
    #invert the indices so the map reduce is simpler
    tables <- purrr::transpose(pages)

    return( purrr::map(tables, ~purrr::reduce(.x, dplyr::union_all)) )
  }

  lapply(
    get_n_series(series_ids, api_key, start_year, end_year, ...),
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
#' @param tidy optional boolean. Return will use [`tidy_periods()`] if true
#' @param ... additional parameters to pass to [`get_series_tables`]
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
get_n_series_table <- function(series_ids, api_key, start_year=NA, end_year=NA, tidy=FALSE, ...){
  tables <- get_series_tables(series_ids, api_key, start_year, end_year, ...)
  if(tidy){
    return(merge_tidy_tables(lapply(tables, tidy_periods)))
  }

  return(merge_tables(tables))
}
