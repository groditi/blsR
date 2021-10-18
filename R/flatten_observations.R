#' Turn a list of one or more series into a single table of time series data
#'
#' @description
#'
#' `merge_tidy_tables()` turns a list of series as returned by
#' [`data_as_tidy_table()`] into a single tibble
#'
#' @param tidy_tables a named list of tables with matching periodicity. Mixing
#' data with different (monthly, quarterly, annual) periodicity is unsupported.
#' The list names will be used as column names in the output.
#'
#' @return tibble
#' @export
#'
merge_tidy_tables <- function(tidy_tables){
  join_by <- c('year')
  if('month' %in% names(tidy_tables[[1]]))
    join_by <- c('year', 'month')
  if('quarter' %in% names(tidy_tables[[1]]))
    join_by <- c('year', 'quarter')

  merge_tables(tidy_tables, join_by)
}


#' Turn a list of one or more series into a single table of time series data
#'
#' @description
#'
#' `merge_tables()` turns a list of series as returned by
#' [`data_as_table()`] into a single tibble
#'
#' @param tables a named list of tables with matching periodicity. Mixing
#' data with different (monthly, quarterly, annual) periodicity is unsupported.
#' The list names will be used as column names in the output.
#' @param join_by an optional character vector of columns to use to join tables.
#'
#' @return tibble
#' @export
#'
merge_tables <- function(tables, join_by = c('period')){
  if( !is.list(tables) || !is.character(names(tables)) )
    stop('merge_tables requires a named list as input "tables".')

  purrr::reduce(
    purrr::imap(tidy_tables, ~dplyr::select(.x, join_by, !!rlang::as_name(.y) := 'value')),
    dplyr::left_join,
    by = join_by
  )
}



#' Clean the period information returned by BLS
#'
#' @details
#' `tidy_periods` will return a tibble where the period and periodName columns
#'  have been deleted and replaced. Monthly periodicity data will have a new
#'  column `month` and quarterly data will have a new column `quarter`.
#'
#' @param observations a tibble or data.frame of the `data` slot in a series
#'
#' @return a tibble containing the period and the value
#' @export
#'

tidy_periods <- function(observations){
  if( substr(observations$period[1], 1, 1) == 'A' ){
    return(dplyr::select(observations, year, value))
  }
  if( substr(observations$period[1], 1, 1) == 'M' ){
    return(
      dplyr::select(
        dplyr::mutate(observations, month = as.numeric(substr(period, 2, 3))),
        -period, -PeriodName
      )
    )
  }
  if( substr(observations$period[1], 1, 1) == 'Q' ){
    return(
      dplyr::select(
        dplyr::mutate(observations, quarter = as.numeric(substr(period, 2, 3))),
        -period, -PeriodName
      )
    )
  }
}



#' Convert a list of data entries as returned by BLS API to a table
#'
#' @param data a list of individual datum entries as returned the API
#'
#' @details currently `data_as_table` is just an alias for [dplyr::bind_rows()]
#'
#' @return tibble flattening `data` into rows for entries and columns for fields
#' @export
#'

data_as_table <- function(data){
  dplyr::bind_rows(data)
}

#' Convert a list of data entries as returned by BLS API to a table
#'
#' @param data a list of individual datum entries as returned the API
#'
#' @details An extension of [`data_as_table`] that replaces the BLS period
#' format by removing columns `period` and `periodName` and adding `month` or
#' `quarter` where appropriate.
#'
#' @return tibble flattening `data` into rows for entries and columns for fields
#' @export
#'

data_as_tidy_table <- function(data){
  tidy_periods( dplyr::bind_rows(data) )
}
