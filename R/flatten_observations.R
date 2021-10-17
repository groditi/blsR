#' Turn a list of one or more series into a single table of time series data
#'
#' @description
#'
#' `flatten_observations()` turns a list of series as returned by the
#' [retriever function][blsR] into a single tibble
#'
#' @param series a list of series all with the same periodicity. Mixing series
#' with more than one periodity (monthly, quarterly, annual) is not supported.
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' retriever <- blsR('your-api-key-goes-here')
#' multiple_series_query <- query_n_series(c('LNS14000000', 'LNS14000001'))
#' results <- retriever(multiple_series_query) #a list of data series and metadata
#' multiple_series_tibble <- flatten_observations(results)
#' }
#'
flatten_observations <- function(series){
  #remove period and period name
  obs <- lapply(series, function(x) tidy_periods(x$observations))

  join_by <- c('year')
  if('month' %in% names(obs[[1]]))
    join_by <- c('year', 'month')
  if('quarter' %in% names(obs[[1]]))
    join_by <- c('year', 'quarter')

  #TODO: rewrite this in base R to drop dependencies
  table <- purrr::reduce(
    purrr::imap(obs, ~dplyr::select(.x, join_by, !!rlang::as_name(.y) := 'value')),
    dplyr::left_join,
    by = join_by
  )
  return(table)
}

tidy_periods <- function(observations){
  if( substr(observations$period[1], 1, 1) == 'A'){
    return(dplyr::select(observations, year,value))
  }
  if( substr(observations$period[1], 1, 1) == 'M'){
    return(
      dplyr::select(
        dplyr::mutate(observations, month = as.numeric(substr(period, 2, 3))),
        year, month, value
      )
    )
  }
  if( substr(observations$period[1], 1, 1) == 'Q'){
    return(
      dplyr::select(
        dplyr::mutate(observations, quarter = as.numeric(substr(period, 2, 3))),
        year, quarter, value
      )
    )
  }
}
