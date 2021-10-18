#' blsR: Retrieve Data From the U.S. Bureau Of Labor Statistics API
#'

#' `blsR` provides functions for retrieving and processing data from the BLS API.
#'  The functions are divided into 4 categories: query generators, query requests,
#'  result processors, and the user-friendly simplified interface.
#'
#'  @section Query generators:
#'
#'  The query generators return a list suitable for passing to [`bls_request()`].
#'  Most users should never need to access these functions directly but they are
#'  made available for advanced users and user-extensions.
#'
#'  * [`query_series()`] - Create a query for a single time series
#'  * [`query_n_series()`] - Create a query to retrieve one or more time series
#'   and their catalog data
#'  * [`query_popular_series()`] - Create a query to retrieve popular series
#'  * [`query_all_surveys()`] - Create a query to retrieve all surveys
#'  * [`query_survey_info()`] - Create a query to retrieve information about a
#'   survey
#'  * [`query_latest_observation()`] - Create a Query to retrieve the latest
#'  observation for a time series
#'
#'  @section Query Requests:
#'
#'  The query-requester functions will execute the query by making the API request
#'  and returning a minimally-processed response. These are likely to be the
#'  most suitable functions to use for users who want to access the raw results.
#'
#'  * [`bls_request()`] - Execute a query and return the unprocessed results
#'  * [`get_series()`] - Create and execute query for a single time series
#'  * [`get_n_series()`] - Create and execute a query to retrieve one or more
#'  time series and their catalog data
#'  * [`get_popular_series()`] - Create and execute a query to retrieve popular
#'  series
#'  * [`get_all_surveys()`] - Create and execute a query to retrieve all surveys
#'  * [`get_survey_info()`] - Create and execute a query to retrieve information
#'  about a survey
#'  * [`get_latest_observation()`] - Create and execute a query to retrieve the
#'  latest observation for a time series
#'
#'  @section Result Processors:
#'
#'  The result-processor functions will transform the raw API response data
#'  structures into data structures more likely to be suitable for modern user
#'  workflows. The functions generally take as input the values returned by the
#'  query-requester functions and make transform the data to different formats
#'  or modify the output of another result-processor function.
#'
#'  * [`data_as_table()`] - Flatten the data list into a table
#'  * [`merge_tables()`] - Merge multiple tables by period
#'  * [`tidy_periods()`] - Transform periods to a more useful format
#'  * [`data_as_tidy_table()`] - Flatten the data list and transform period data
#'  * [`merge_tidy_tables()`] - Merge multiple tables with tidy period data
#'
#'  @section Simplified Interface:
#'
#'  These functions simplify the query generation, execution, and response
#'  processing into a single function call. For most common use cases these are
#'  likely to be the only functions needed.
#'
#'  * [`get_series_table()`] - Request one series and return a data table
#'  * [`get_series_tables()`] - Request series and return list of data tables
#'  * [`get_n_series_table()`] - Request series and return one table of values
#'
#'  @docType package
#'  @name blsR
NULL
#>  NULL





#'
#' @description
#'
#' `bls_request()` constructs a function that will execute queries against the BLS API.
#' Queries are generated using one of the query-generating functions and then
#' passed on to the retriever function returned by `blsR()`. The retriever
#' function will return the requested data.
#'
#' @param query
#' @param api_key string, only necessary for retrieving multiple series in one
#' request, requesting calculations, or custom time frames and catalog data
#' @param user_agent string, optional
#'
#' @return a retriever function that handles the API's http request and response.
#' @export
#'
#' @examples
#' library(blsR)
#' retriever <- blsR()
#' unemployment_rate_query <- query_series('LNS14000000') #a monthly data series
#' results <- retriever(unemployment_rate_query) #a list of data series and metadata
#' unemployment_tibble <- flatten_observations(results)
#'
#' \dontrun{
#' retriever <- blsR('your-api-key-goes-here')
#' multiple_series_query <- query_n_series(c('LNS14000000', 'LNS14000001'))
#' results <- retriever(multiple_series_query) #a list of data series and metadata
#' multiple_series_tibble <- flatten_observations(results)
#' }

blsR <- function(api_key = NA, user_agent = 'http://github.com/groditi/blsR' ){
  ua <-  httr::user_agent(user_agent)
  #return a closure that makes the http request and processes the response.
  #API keys expire but URLs and payloads have no other temporality associated.
  #separate the part that uses API key into instance-level data so queries can
  #be safely serialized or stored without leaking private API data or storing
  #a stale key. It's not the prettiest or very R-like, but it works.

  function(query){
    #the query object should contain all it needs to make the request except
    #the BLS API key. simple requests that don't need it should be run even if
    #its missing but for complex ones with multiple series requested it should
    #inject the  key into the payload
    if(query$is_complex == FALSE){
      response <- httr::GET(query$url, ua)
      return(.process_response(response))
    }


    if('payload' %in% names(query)){
      if('series' %in% names(query$payload)){
        if(is.na(registrationkey))
          warning('api_key is required for multiple series requests.')
        query$payload[['registrationkey']] = api_key
      }
    }

    response <- httr::POST(url=query$url, ua, body=query$payload, encode="json")
    return(.process_response(response))
  }

}

.process_response <- function(response){

  #die if the format of the response content isnt json
  if(httr::http_type(response) != "application/json"){
    stop("http request did not return json", call. = FALSE)
  }

  json_response <- jsonlite::fromJSON(
    httr::content(response, 'text'), simplifyVector=FALSE
  )

  #die if request wasn't successful
  if(json_response$status != 'REQUEST_SUCCEEDED') {
    stop(paste(json_response$message, '; '), call. = FALSE)
  }
  results <- json_response$Results

  #survey type requests
  if('survey' %in% names(results)){
    if(length(results$survey) == 1) return(results$survey[[1]])
    if(length(results$survey) > 1){
      return(dplyr::bind_rows(results$survey))
    }
  }


  if('series' %in% names(results)){
    #popular series request
    if(! 'data' %in% names(results$series[[1]])){
      return(dplyr::bind_rows(results$series))
    }

    series_ids <- sapply(results$series, function(x) return(x$seriesID))
    series_data <- lapply(results$series, .process_timeseries)
    names(series_data) <- series_ids
    return(series_data)
  }
}

.process_timeseries <- function(series){
  series[['observations']] <- dplyr::bind_rows(series$data)
  return(series)
}

