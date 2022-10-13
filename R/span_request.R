
#' Break up a long request into multiple API calls
#'
#' @param start_year numeric
#' @param end_year numeric
#' @param year_limit numeric
#' @param query_fn a function that takes two arguments, `start_year` and
#' `end_year` and returns a query (see [`purrr::partial()`])
#' @param ... additional arguments to pass to  [`bls_request()`]
#'
#' @return a list of API responses (what comes back from bls_re)
#'
#' @family blsR-requests
#' @family blsR-utils
#'
#' @export
#'

span_series_request <- function(start_year, end_year, year_limit, query_fn, ...){

  queries <- span_request_queries(start_year, end_year, year_limit, query_fn)
  responses <- purrr::map(queries, bls_request, ...)

  return(reduce_spanned_responses(responses))
}

#' Generate multiple queries that don't exceed a year limit
#'
#' @param start_year numeric
#' @param end_year numeric
#' @param year_limit numeric
#' @param query_fn a function or closure that takes two arguments, `start_year`
#' and `end_year`, and returns a query (see [`purrr::partial()`])
#'
#' @return a list of query objects in reverse chronological order
#'
#' @family blsR-queries
#' @family blsR-utils
#'
#' @export
#'

span_request_queries <- function(start_year, end_year, year_limit, query_fn){
  #paging from most to least recent makes merging easier bc BLS returns results
  #in reverse chronological order
  page_ends <- seq(end_year, start_year, year_limit * -1)
  page_cnt <- length(page_ends)
  page_starts <- c(
    page_ends[-page_cnt] - (year_limit - 1),
    max(page_ends[page_cnt] - (year_limit - 1), start_year)
  )

  if(page_cnt > 1)
    rlang::inform( sprintf(
      'Year %i to %i is longer than %i year API limit. Performing %i requests.',
      start_year, end_year, year_limit, page_cnt
    ) )

  return(purrr::map2(page_starts, page_ends, query_fn))
}

#' Reduce the multiple spanned responses into a list of series
#'
#' @param responses a list of API responses as returned by [`bls_request()`]
#'
#' @return series list
#'
#' @family blsR-requests
#' @family blsR-utils
#'
#' @export
#'

reduce_spanned_responses <- function(responses){
  series <- purrr::map(responses, 'series')
  series_ids <- purrr::map(series[[1]], 'seriesID')
  pages <- purrr::map(series, purrr::set_names, series_ids)

  if(length(pages) == 1) return(pages[[1]])

  series_data <- purrr::map(pages, purrr::map, 'data')
  reduced <- purrr::map(purrr::transpose(series_data), purrr::reduce, append)

  return(
    purrr::imap(
      pages[[1]],
      ~purrr::modify_at(.x, 'data', ~.y, reduced[[.y]])
    )
  )
}
