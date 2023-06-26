.api_uri_root <- function(){
  httr::parse_url('https://api.bls.gov/publicAPI/v2')
}

.build_payload <- function(
  series=c(), start_year=NULL, end_year=NULL, catalog = FALSE, calculations = FALSE,
  annualaverage = FALSE, aspects = FALSE){
  # tell the API what we want
  if(rlang::is_bare_numeric(start_year, 1) != rlang::is_bare_numeric(end_year, 1))
    rlang::abort('start_year and end_year must both be specified or both be NULL')
  if(rlang::is_bare_numeric(start_year, 1)){
    if(start_year > end_year)
      rlang::abort('start year can not be greater than end year')
    if( (end_year - start_year)+1 > 20 ){
      rlang::warn( paste(c(
        'BLS restricts timeseries requests to a maximum of 20 years of data.',
        'Timespan', start_year, 'to', end_year, 'exceeds 20 years.',
        'Split request into two or more queries and join the results.'
        )))
    }
  }


  payload <- list()
  if(length(series) > 0) payload[['seriesid']] <- series
  if(rlang::is_bare_numeric(start_year)) payload[['startyear']] <- start_year
  if(rlang::is_bare_numeric(end_year)) payload[['endyear']] <- end_year
  if(isTRUE(catalog)) payload[['catalog']] <- catalog
  if(isTRUE(calculations)) payload[['calculations']] <- calculations
  if(isTRUE(annualaverage)) payload[['annualaverage']] <- annualaverage
  if(isTRUE(aspects)) payload[['aspects']] <- aspects
  #if(!is.na(registrationkey)) payload[['registrationkey']] = registrationkey
  return(payload)
}

#' Create a query for a single time series
#'
#' @param series_id Character scalar BLS series ID
#' @param start_year,end_year numeric 4-digit years. While optional, they are
#' strongly recommended. If one is provided, the other is mandatory. `end_year`
#' must be greater than `start_year`
#'
#' @return list of query parameters
#'
#' @family blsR-queries
#'
#' @export
#'
#' @examples
#'
#' unemployment_rate_query <- query_series('LNS14000000')
#' unemployment_rate_query <- query_series('LNS14000000', 2005, 2010)
#'
query_series <- function(series_id, start_year=NULL, end_year=NULL){
  #query a singular series (easy GET from JSON URI)
  api_url <- .api_uri_root()
  url_path <- c(api_url$path, 'timeseries','data', series_id)
  payload <- .build_payload(start_year = start_year, end_year = end_year)

  list(
    is_complex = FALSE,
    url = httr::modify_url(api_url, path = url_path, query = payload)
  )
}


#' Create a query to retrieve one or more time series and their catalog data
#'
#' @inheritParams query_series
#' @param series_ids Character vector of BLS series IDs
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
#'
#' @return list of query parameters
#'
#' @family blsR-queries
#'
#' @export
#'
#' @examples
#'
#' a <- query_n_series(c('LNS14000001', 'LNS14000002'))
#' b <- query_n_series(c('LNS14000001', 'LNS14000002'), start_year = 2005, end_year=2010)
#' c <- query_n_series(c('LNS14000001', 'LNS14000002'), 2005, 2010)
#' d <- query_n_series(c('LNS14000001', 'LNS14000002'), catalog=TRUE)
#'
#'
query_n_series <- function(
  series_ids, start_year=NULL, end_year=NULL, catalog = FALSE, calculations = FALSE,
  annualaverage = FALSE, aspects = FALSE){
  #request multiple series and optional series-level information

  api_url <- .api_uri_root()
  url_path <- c(api_url$path, 'timeseries','data')
  payload <- .build_payload(
    series_ids, start_year, end_year, catalog, calculations, annualaverage, aspects
  )

  list(
    is_complex = TRUE,
    url = httr::modify_url(api_url, path = url_path),
    payload = payload
  )

}

#' Create a query to retrieve popular series
#'
#' @param survey_id BLS survey abbreviation (two letter code)
#'
#' @return list of query parameters
#'
#' @family blsR-queries
#'
#' @export
#'
#' @examples
#' popular_series_query <- query_popular_series()
#' popular_labor_force_series <- query_popular_series('LN')
#'
query_popular_series <- function(survey_id = NULL){
  #query for popular series (optional: from a specific survey)
  api_url <- .api_uri_root()
  url_path <- c(api_url$path, 'timeseries','popular')
  if(rlang::is_string(survey_id))
    api_url <- httr::modify_url(api_url, query = list(survey = survey_id))

  list(
    is_complex = FALSE,
    url = httr::modify_url(api_url, path = url_path)
  )
}

#' Create a query to retrieve all surveys
#'
#' @return list of query parameters
#'
#' @family blsR-queries
#'
#' @export

query_all_surveys <- function(){
  api_url <- .api_uri_root()
  url_path <- c(api_url$path, 'surveys')

  list(
    is_complex = FALSE,
    url = httr::modify_url(api_url, path = url_path)
  )
}

#' Create a query to retrieve information about a survey
#'
#' @param survey_id BLS survey abbreviation (two letter code)
#'
#' @return list of query parameters
#'
#' @family blsR-queries
#'
#' @export
#'
#' @examples
#' query_survey_info('LN')
#'
query_survey_info <- function(survey_id){
  #TODO: throw an error if survey_id is missing
  api_url <- .api_uri_root()
  url_path <- c(api_url$path, 'surveys',survey_id)

  list(
    is_complex = FALSE,
    url = httr::modify_url(api_url, path = url_path)
  )
}


#' Create a Query to retrieve the latest observation for a time series
#'
#' @param series_id BLS series ID
#'
#' @return list of query parameters
#'
#' @family blsR-queries
#'
#' @export
#'

query_latest_observation <- function(series_id){
  #we can re-use query_series here
  query <- query_series(series_id)
  query[['url']] <- httr::modify_url(query[['url']], query = list(latest = TRUE))

  return(query)
}
