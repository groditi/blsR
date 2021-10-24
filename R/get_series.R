#' Create and execute query for a single time series
#'
#' @param series_id BLS series ID
#' @param start_year numeric 4-digit year
#' @param end_year numeric 4-digit year
#' @param ... additional parameters to pass to [bls_request]
#'
#' @return a single series result, in list form
#'
#' @family blsR-requests
#'
#' @seealso [`query_series`]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' series <- get_series('LNS14000001')
#' }
get_series <- function(series_id, start_year=NA, end_year=NA, ...){
  results <- bls_request(query_series(series_id, start_year, end_year), ...)
  return(results$series[[1]])
}

#' Create and execute a query to retrieve one or more time series and their
#' catalog data
#'
#' @param series_ids a list or character vector of BLS time-series IDs. If the
#' list items are named then the names will be used in the returned list
#' @param api_key a required API key, available from
#'  <https://data.bls.gov/registrationEngine/>
#' @param ... additional parameters to pass to [query_n_series]
#'
#' @return a list of series results (a list of lists)
#'
#' @family blsR-requests
#'
#' @seealso [`query_n_series`]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' series_ids <- list(uer.men ='LNS14000001', uer.women = 'LNS14000002')
#' uer_series <- get_n_series(series_ids, 'your-api-key-here' )
#' }
get_n_series <- function(series_ids, api_key, ...){
  bls_series_ids <- vector(mode = "character", length = length(series_ids))
  series_aliases <- vector(mode = "character", length = length(series_ids))

  if( is.list(series_ids) && is.character(names(series_ids)) ){
    bls_series_ids <- unlist(series_ids, use.names = F)
    series_aliases <- names(series_ids)
  } else {
    bls_series_ids <- series_ids
    series_aliases <- series_ids
  }

  results <- bls_request(query_n_series(bls_series_ids, ...), api_key=api_key)
  names(results$series) <- series_aliases
  return(results$series)
}


#' Create and execute a query to retrieve popular series
#'
#' @param survey_id optional survey abbreviation
#' @param ... additional parameters to pass to [bls_request]
#'
#' @return a character vector of series IDs
#'
#' @family blsR-requests
#'
#' @seealso [`query_popular_series`]
#'
#' @export
#'
get_popular_series <- function(survey_id=NA, ...){
  results <- bls_request(query_popular_series(survey_id = survey_id), ...)
  return(sapply(results$series, function(x) x$seriesID))
}

#' Create and execute a query to retrieve all surveys
#'
#' @param ... additional parameters to pass to [bls_request]
#'
#' @return a table with a survey_abbreviation and survey_name columns
#'
#' @family blsR-requests
#'
#' @seealso [`query_all_surveys`]
#'
#' @export
#'
get_all_surveys <- function(...){
  results <- bls_request(query_all_surveys(), ...)
  return(dplyr::bind_rows(results$survey))
}

#' Create and execute a query to retrieve information about a survey
#'
#' @param survey_id survey abbreviation
#' @param ... additional parameters to pass to [bls_request]
#'
#' @return a list of survey information
#'
#' @family blsR-requests
#'
#' @seealso [`query_survey_info`]
#'
#' @export
#'
get_survey_info <- function(survey_id, ...){
  results <- bls_request(query_survey_info(survey_id), ...)
  return(results$survey[[1]])
}

#' Create and execute a query to retrieve the latest observation for a series
#'
#' @param survey_id BLS series ID
#' @param ... additional parameters to pass to [bls_request]
#'
#' @return a datum in the form of a list
#'
#' @family blsR-requests
#'
#' @seealso [`query_latest_observation`]
#'
#' @export
#'
get_latest_observation <- function(survey_id, ...){
  results <- bls_request(query_latest_observation(survey_id))
  return(results$series[[1]]$data[[1]])
}
