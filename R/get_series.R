
.validate_years <- function(start_year, end_year){
  if(rlang::is_na(start_year) && rlang::is_na(end_year))
    return(TRUE)

  if(rlang::is_na(start_year) || rlang::is_na(end_year))
    rlang::abort('start_year and end_year must both be provided')
  if(!rlang::is_bare_numeric(start_year, 1))
    rlang::abort('start_year must be a scalar numeric')
  if(!rlang::is_bare_numeric(end_year, 1))
    rlang::abort('end_year must be a scalar numeric')
  if(start_year > end_year)
    rlang::abort('end_year must be greater than or equal to start_year')

  return(TRUE)
}

.series_id_names <- function(series_ids){

  if(!rlang::is_list(series_ids) && !rlang::is_character(series_ids))
    rlang::abort('series_ids must be character vector or a named list')
  if(rlang::is_empty(series_ids))
    rlang::abort('series_ids must not be empty')

  series_id_values <- unlist(series_ids, use.names = FALSE)
  if(!rlang::is_character(series_id_values))
    rlang::abort('series_ids values must be a character vector')
  if( length(series_id_values) < 1)
    rlang::abort('series_ids must have a length 1 or higher')
  if(!all(stringr::str_length(series_id_values)))
    rlang::abort('series_ids can not be empty strings')
  if( rlang::is_character(series_ids) )
    return(series_id_values)

  series_id_names <- names(series_ids)
  if(length(series_id_names) != length(series_ids))
    rlang::abort('all series_ids list elements must be named')
  if(length(series_id_names) != length(unique(series_id_names)))
    rlang::abort('series_ids names must be unique')
  if(!all(stringr::str_length(series_id_names)))
    rlang::abort('series_ids names can not be empty strings')

  return(series_id_names)
}


#' Create and execute query for a single time series
#'
#' @param series_id BLS series ID
#' @param start_year numeric 4-digit year
#' @param end_year numeric 4-digit year
#' @param year_limit optional number of years to paginate request by. Defaults
#' to 10, the API request cap when using no API key. Requests made with an API
#' key, which can be provided in `...`, are capped to 20 years.
#' @param span when set to `TRUE`, requests where the number of years between
#'   `start_year` and `end_year` exceed `year_limit` will be performed as
#'   multiple requests automatically
#' @param ... additional arguments to pass to [`bls_request()`]
#'
#' @return a single series result, in list form. The resulting list will have
#' the following items:
#' * `seriesID`: a character vector of length 1 containing the `series_id`
#' * `data`: a list of lists containing the payload data. Each item of the list
#' represents an observation. Each observation is a list with the following
#' named items `year`, `period`, `periodName`, `value`, `footnotes`.
#' Footnotes are a list. Additionally, the most recent observation will have
#' an item named `latest` which will be marked as 'true'.
#'
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
get_series <- function(
    series_id, start_year=NA, end_year=NA, year_limit=10, span=TRUE, ...
){
  .validate_years(start_year, end_year)
  query_fn <- purrr::partial(query_series, series_id)
  if(!span){
    results <- bls_request(query_fn(start_year, end_year), ...)
    return(results$series[[1]])
  }

  series <- span_series_request(start_year, end_year, year_limit, query_fn, ...)
  return(series[[1]])
}

#' Create and execute a query to retrieve one or more time series and their
#' catalog data
#'
#' @param series_ids a list or character vector of BLS time-series IDs. If the
#' list items are named then the names will be used in the returned list
#' @param api_key a required API key, available from
#'  <https://data.bls.gov/registrationEngine/>
#' @param start_year numeric 4-digit year
#' @param end_year numeric 4-digit year
#' @param year_limit optional number of years to paginate request by. Defaults
#' to 20, the API request cap when using API key. Requests made without an API
#' key are capped to 10 years.
#' @param span when set to `TRUE`, requests where the number of years between
#'   `start_year` and `end_year` exceed `year_limit` will be performed as
#'   multiple requests automatically
#' @param catalog boolean. If set to `TRUE`, element item in the list returned
#'   may include a named item `catalog`, a named list containing descriptive
#'   information about the series. Not all series have a catalog entry
#'   available.
#' @param calculations boolean. If set to `TRUE`, each element in the `data`
#'   list for each series returned may include an additional named element
#'   `calculations`, a named list containing two items, `net_changes` and
#'   `pct_changes`, each of them a named list which may include items `1`, `3`,
#'   `6`, `12` which represent 1, 3, 6, and 12 month net changes and percent
#'   changes respectively. Not all data series will have enough data points to
#'   include these calculations.
#' @param annualaverage boolean. If set to `TRUE`, each `data` list may include
#' an additional element for a an annual average of the time series, which is
#' usually presented as month 13 in monthly data. Not all data series
#' support this feature.
#' @param aspects boolean. If set to `TRUE`, each item in the `data` list
#' for each series returned may include an additional named element `aspects`,
#' which will be a named list. Not all data series support this feature.
#' @param ... additional arguments to pass to [`bls_request()`]
#'
#' @return a list of series results. Each element of the returned list is
#' a named list guaranteed to have two items, `SeriesID` and `data` and
#' optionally `catalog`. The unnamed list `data` will have 0 or more elements,
#' each one a named list representing an observation in the time series. Each
#' observation is guaranteed to include the elements `year`, `period`,
#' `periodName`, `value`, and `footnotes`. Footnotes are a list of named lists.
#' The rest are scalar values. If the the most recent observation is included,
#' that observation will have an element named `latest` which will contain the
#' text '`true`'. If `calculations` or `aspects` were requested they will be
#' present as named elements in each observation.
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
get_n_series <- function(
    series_ids, api_key,
    start_year=NA, end_year=NA, year_limit = 20, span = TRUE,
    catalog = FALSE, calculations = FALSE, annualaverage = FALSE, aspects = FALSE,
    ...
){
  if(rlang::is_na(api_key)) year_limit <- 10
  .validate_years(start_year, end_year)

  bls_series_ids <- unlist(series_ids, use.names = F)
  series_aliases <- .series_id_names(series_ids)
  query_fn <- purrr::partial(
    query_n_series,
    bls_series_ids,
    ...=,
    catalog = catalog,
    calculations = calculations,
    annualaverage = annualaverage,
    aspects = aspects
  )

  if(rlang::is_na(start_year) || rlang::is_na(end_year) || !span){
    series <- bls_request(query_fn(start_year, end_year), api_key, ...)$series
  } else{
    series <- span_series_request(
      start_year, end_year, year_limit, query_fn, api_key, ...
      )[bls_series_ids] #in theory this is necessary, in practice, why not both?
  }

  return(purrr::set_names(series, series_aliases))
}


#' Create and execute a query to retrieve popular series
#'
#' @param survey_id optional survey abbreviation
#' @param ... additional arguments to pass to [`bls_request()`]
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
#' @param ... additional arguments to pass to [`bls_request()`]
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
#' @param ... additional arguments to pass to [`bls_request()`]
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
#' @param ... additional arguments to pass to [`bls_request()`]
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
  results <- bls_request(query_latest_observation(survey_id), ...)
  return(results$series[[1]]$data[[1]])
}
