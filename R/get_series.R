
.validate_years <- function(start_year, end_year){

  is_numeric_start <- rlang::is_bare_numeric(start_year, 1)
  is_numeric_end <- rlang::is_bare_numeric(end_year, 1)

  is_empty_start <- rlang::is_null(start_year) || rlang::is_na(start_year)
  is_empty_end <- rlang::is_null(end_year) || rlang::is_na(end_year)

  if(is_empty_start && is_empty_end)
    return(TRUE)

  if(is_empty_start || is_empty_end)
    rlang::abort('start_year and end_year must both be provided')
  if(!is_numeric_start)
    rlang::abort('start_year must be a scalar numeric')
  if(!is_numeric_end)
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
  if(length(series_id_names) == 0)
    return(series_id_values)
  if(length(series_id_names) != length(series_ids))
    rlang::abort('none or all series_ids list elements must be named')
  if(length(series_id_names) != length(unique(series_id_names)))
    rlang::abort('series_ids names must be unique')
  if(!all(stringr::str_length(series_id_names)))
    rlang::abort('series_ids names can not be empty strings')

  return(series_id_names)
}

.span_series_ids <- function(series_ids, series_limit){
  series_index <- c(1:length(series_ids))
  buckets <- split(series_index, ceiling(seq_along(series_index)/series_limit))
  id_buckets <- lapply(buckets, function(x) series_ids[x])
  names(id_buckets) <- NULL
  id_buckets
}

#' Create and execute query for a single time series
#'
#' @inheritParams query_series
#' @param year_limit optional number of years to paginate request by. If not
#' explicitly set, it will be set to 10 or 20 depending on if an `api_key` is
#' available
#' @param span when set to `TRUE`, requests where the number of years between
#'   `start_year` and `end_year` exceed `year_limit` will be performed as
#'   multiple requests automatically
#' @param api_key Optional. An API key string. Defaults to the value returned by
#' [`bls_get_key()`]. The preferred way to provide an API key is to use
#' [`bls_set_key()`] or the `BLS_API_KEY` environment variable. Manually passing
#' the key will be deprecated in future releases.
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
    series_id, start_year=NULL, end_year=NULL, year_limit=NULL, span=TRUE,
    api_key = bls_get_key(), ...
){
  if(!rlang::is_bare_numeric(year_limit)){
    year_limit <- ifelse(rlang::is_null(api_key), 10, 20)
  }

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
#' items are named then the names will be used in the returned list
#' @inheritParams query_n_series
#' @inheritParams get_series
#' @param series_limit Maximum number of series to request in one API call
#' when `span` is set to `TRUE`.
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
    series_ids, api_key = bls_get_key(),
    start_year=NULL, end_year=NULL, year_limit = NULL, span = TRUE,
    catalog = FALSE, calculations = FALSE, annualaverage = FALSE, aspects = FALSE,
    series_limit = NULL,
    ...
){

  has_api_key <- !rlang::is_null(api_key)
  series_limit <- ifelse(has_api_key, 50, 25)
  if(rlang::is_null(year_limit) || rlang::is_na(year_limit)){
    year_limit <- ifelse(has_api_key, 20, 10)
  }

  .validate_years(start_year, end_year)

  series_length <- length(series_ids)
  if(series_length > series_limit){
    if(!isTRUE(span)){
      template <- paste(
        'argument series_ids length of %i exceeds maximum of %i.',
        'To request more than %i series in one call, use `span=TRUE`'
      )
      rlang::abort(sprintf(template, series_length, series_limit, series_limit))
    }

    span_buckets <- .span_series_ids(series_ids, series_limit)
    span_call <- function(spanned_ids, ...){
      get_n_series(
        series_ids = spanned_ids,
        api_key = api_key,
        start_year = start_year,
        end_year = end_year,
        year_limit = year_limit,
        span = span,
        catalog = catalog,
        calculations = calculations,
        annualaverage = annualaverage,
        aspects = aspects,
        series_limit = series_limit,
        ...
      )
    }

    spanned_results <- lapply(span_buckets, span_call, ...)
    return(purrr::reduce(spanned_results, c))
  }


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

  if(rlang::is_na(start_year) || rlang::is_null(start_year) || !isTRUE(span)){
    query <-  query_fn(start_year = start_year, end_year = end_year)
    response <- bls_request(query, api_key = api_key, ...)
    series <- response$series
  } else{
    series <- span_series_request(
      start_year, end_year, year_limit, query_fn, api_key = api_key, ...
      )[bls_series_ids] #to ensure correct order
  }

  return(purrr::set_names(series, series_aliases))
}


#' Create and execute a query to retrieve popular series
#'
#' @inheritParams query_popular_series
#' @inheritParams get_series
#'
#' @return a character vector of series IDs
#'
#' @family blsR-requests
#'
#' @seealso [`query_popular_series`]
#'
#' @export
#'
get_popular_series <- function(survey_id=NULL, ...){
  results <- bls_request(query_popular_series(survey_id = survey_id), ...)
  return(sapply(results$series, function(x) x$seriesID))
}

#' Create and execute a query to retrieve all surveys
#'
#' @inheritParams get_series
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
#' @inheritParams query_survey_info
#' @inheritParams get_series
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
#' @inheritParams query_latest_observation
#' @inheritParams get_series
#'
#' @return a datum in the form of a list
#'
#' @family blsR-requests
#'
#' @seealso [`query_latest_observation`]
#'
#' @export
#'
get_latest_observation <- function(series_id, ...){
  results <- bls_request(query_latest_observation(series_id), ...)
  return(results$series[[1]]$data[[1]])
}
