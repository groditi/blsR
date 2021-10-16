
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


  }


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
