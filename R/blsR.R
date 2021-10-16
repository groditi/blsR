
.api_uri_root <- function(){
  httr::parse_url('https://api.bls.gov/publicAPI/v2')
}

blsR <- function(registrationkey = NA){
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
      response <- httr::GET(query$url)
      #TODO: do something with the response and return
    }


    if('payload' %in% names(query)){
      if('series' %in% names(query$payload)){
        if(is.na(registrationkey))
          warning('registrationkey is required for multiple series requests.')
        query$payload[['registrationkey']] = registrationkey
      }
    }

    response <- httr::POST(url = query$url, body = query$payload, encode = "json")
    #TODO: do something with the response and return
  }

}

.process_response <- function(json_response){
  response <- fromJSON(response)

  #stop here if request wasn't successful
  if(response$status != 'REQUEST_SUCCEEDED') {
    stop(paste(request$message, '; '))
  }

  results <- response$Results
  if('survey' %in% names(results)){
    if(length(results$survey) == 1) rerturn(results$survey[1])
    if(length(results$survey) > 1) rerturn(results$survey)
  }

  #TODO process series responses
}

query_series <- function(series_id){
  #query a singular series (easy GET from JSON URI)
  api_url <- .api_uri_root()
  url_path <- c(api_url$path, 'timeseries','data', series_id)

  list(
    is_complex = FALSE,
    url = httr::modify_url(api_url, path = url_path)
  )
}

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

.build_payload <- function(){
  # tell the API what we want
  payload = list()
  if(!is.na(series)) payload[['seriesid']] = c(series)
  if(!is.na(start_year)) payload[['startyear']] = start_year
  if(!is.na(end_year)) payload[['endyear']] = end_year
  if(!is.na(catalog)) payload[['catalog']] = catalog
  if(!is.na(calculations)) payload[['calculations']] = calculations
  if(!is.na(annualaverage)) payload[['annualaverage']] = annualaverage
  if(!is.na(aspects)) payload[['aspects']] = aspects
  #if(!is.na(registrationkey)) payload[['registrationkey']] = registrationkey
  return(payload)
}

query_popular_series <- function(survey = NA){
  #query for popular series (optional: from a specific survey)
  api_url <- .api_uri_root()
  url_path <- c(api_url$path, 'timeseries','popular')
  if(!is.na(survey))
    api_url <- httr::modify_url(api_url, query = list(survey = survey))

  list(
    is_complex = FALSE,
    url = httr::modify_url(api_url, path = url_path)
  )
}

query_all_surveys <- function(){
  api_url <- .api_uri_root()
  url_path <- c(api_url$path, 'surveys')

  list(
    is_complex = FALSE,
    url = httr::modify_url(api_url, path = url_path)
  )
}

query_survey_info <- function(survey_id){
  #TODO: throw an error if survey_id is missing
  api_url <- .api_uri_root()
  url_path <- c(api_url$path, 'surveys',survey_id)

  list(
    is_complex = FALSE,
    url = httr::modify_url(api_url, path = url_path)
  )
}

query_latest_observation <- function(series_id){
  #we can re-use query_series here
  query <- query_series(series_id)
  query[['url']] <- httr::modify_url(query[['url']], query = list(latest = TRUE))

  return(query)
}
