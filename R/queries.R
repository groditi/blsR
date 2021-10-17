.api_uri_root <- function(){
  httr::parse_url('https://api.bls.gov/publicAPI/v2')
}

.build_payload <- function(
  series=NA, start_year=NA, end_year=NA, catalog = FALSE, calculations = FALSE,
  annualaverage = FALSE, aspects = FALSE){
  # tell the API what we want
  if(is.na(start_year) != is.na(end_year))
    stop("start_year and end_year must both be specified or both be NA")

  #TODO: throw an error if start year is defined and larger than end year

  payload = list()
  if(!is.na(series) && length(series) >= 1) payload[['seriesid']] = c(series)
  if(!is.na(start_year)) payload[['startyear']] = start_year
  if(!is.na(end_year)) payload[['endyear']] = end_year
  if(isTRUE(catalog)) payload[['catalog']] = catalog
  if(isTRUE(calculations)) payload[['calculations']] = calculations
  if(isTRUE(annualaverage)) payload[['annualaverage']] = annualaverage
  if(isTRUE(aspects)) payload[['aspects']] = aspects
  #if(!is.na(registrationkey)) payload[['registrationkey']] = registrationkey
  return(payload)
}

#' Create a query for a single time series
#'
#' @param series_id BLS series ID
#' @param start_year numeric 4-digit year
#' @param end_year numeric 4-digit year
#'
#' @return list of query parameters for retriever function
#'
#'
#'
#' @export
#'
#' @examples
#' unemployment_rate_query <- query_series('LNS14000000')
#' unemployment_rate_query <- query_series('LNS14000000', 2005, 2010)
query_series <- function(series_id, start_year=NA, end_year=NA){
  #query a singular series (easy GET from JSON URI)
  api_url <- .api_uri_root()
  url_path <- c(api_url$path, 'timeseries','data', series_id)
  payload <- .build_payload(start_year, end_year)

  list(
    is_complex = FALSE,
    url = httr::modify_url(api_url, path = url_path, query = payload)
  )
}


#' Create a query to retrieve one or more time series and their catalog data
#'
#' @param series vector of BLS series IDs
#' @param start_year numeric 4-digit year
#' @param end_year numeric 4-digit year
#' @param catalog boolean
#' @param calculations boolean
#' @param annualaverage boolean
#' @param aspects boolean
#'
#' @return list of query parameters for retriever function
#' @export
#'
#' @examples
query_n_series <- function(
  series, start_year=NA, end_year=NA, catalog = FALSE, calculations = FALSE,
  annualaverage = FALSE, aspects = FALSE){
  #request multiple series and optional series-level information

  api_url <- .api_uri_root()
  url_path <- c(api_url$path, 'timeseries','data')
  payload <- .build_payload(
    series, start_year, end_year, catalog, calculations, annualaverage, aspects
  )

  list(
    is_complex = TRUE,
    url = httr::modify_url(api_url, path = url_path),
    payload = payload
  )

}

#' Create a query to retrieve popular series
#'
#' @param survey_id string optional
#'
#' @return list of query parameters for retriever function.
#' retriever returns a tibble of popular series IDs
#' @export
#'
#' @examples
#' popular_series_query <- query_popular_series()
#' popular_labor_force_series <- query_popular_series('LN')
#'
query_popular_series <- function(survey_id = NA){
  #query for popular series (optional: from a specific survey)
  api_url <- .api_uri_root()
  url_path <- c(api_url$path, 'timeseries','popular')
  if(!is.na(survey_id))
    api_url <- httr::modify_url(api_url, query = list(survey = survey_id))

  list(
    is_complex = FALSE,
    url = httr::modify_url(api_url, path = url_path)
  )
}

#' Create a query to retrieve all surveys
#'
#' @return list of query parameters for retriever function.
#' retriever returns a tibble of survey abbreviations and survey names
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
#' @param survey_id
#'
#' @return list of query parameters for retriever function.
#' retriever returns a list of descriptive fields about a survey
#' @export
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
#' @param series_id
#'
#' @return list of query parameters for retriever function.
#' retriever returns a list of series with one observation
#' @export
#'

query_latest_observation <- function(series_id){
  #we can re-use query_series here
  query <- query_series(series_id)
  query[['url']] <- httr::modify_url(query[['url']], query = list(latest = TRUE))

  return(query)
}
