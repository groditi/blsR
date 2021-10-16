
.api_uri_root <- function(){
  httr::parse_url('https://api.bls.gov/publicAPI/v2')
}

blsR <- function(registrationkey = NA){
    #return a closure that makes the http request and processes the response

}

query_series <- function(series_id){
    #query a singular series (easy GET from JSON URI)

}

query_n_series <- function(
  series, start_year=NA, end_year=NA, catalog = FALSE, calculations = FALSE,
  annualaverage = FALSE, aspects = FALSE){
  #request multiple series and optional series-level information
}

.build_payload <- function(){
  # tell the API what we want

}

query_popular_series <- function(survey = na()){

}

query_all_surveys <- function(){}

query_survey_info <- function(){}

query_latest_observation <- function(){}
