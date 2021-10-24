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
#'
#' @family blsR-utils
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
#'
#' @family blsR-utils
#' @export
#'
#' @examples
#' \dontrun{
#' series_ids <- list(uer.men ='LNS14000001', uer.women = 'LNS14000002')
#' uer_series <- get_n_series(series_ids, 'your-api-key-here' )
#' uer_tables <- lapply(uer_series, function(x) data_to_table(x$data))
#' big_table <- merge_tables(uer_tables)
#' }
#'
merge_tables <- function(tables, join_by = c('year', 'period')){
  if( !is.list(tables) || !is.character(names(tables)) )
    stop('merge_tables requires a named list as input "tables".')

  purrr::reduce(
    purrr::imap(tables, ~dplyr::select(.x, join_by, !!rlang::as_name(.y) := 'value')),
    dplyr::left_join,
    by = join_by
  )
}



#' Clean the period information returned by BLS
#'
#' @details
#' `tidy_periods` will return a tibble where the period and periodName columns
#'  have been deleted and replaced. Monthly periodicity data will have a new
#'  column `month` and quarterly data will have a new column `quarter`. Rows will
#'  be sorted from oldest to newest.
#'
#' @param table a tibble of the `data` slot in a series
#'
#' @return a sorted tibble containing the period and the value
#'
#' @family blsR-utils
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' series <- get_series('LNS14000001')
#' table <- data_as_table(series$data)
#' tidy_table <- tidy_periods(table)
#' }

tidy_periods <- function(table){
  if( substr(table$period[1], 1, 1) == 'A' ){
    return(dplyr::arrange(dplyr::select(table, .data$year, .data$value), .data$year))
  }
  if( substr(table$period[1], 1, 1) == 'M' ){
    return(
      dplyr::arrange(
        dplyr::relocate(
          dplyr::select(
            dplyr::mutate(table, month = as.integer(substr(.data$period, 2, 3))),
            -'period', -'periodName'
          ),
          .data$month, .after='year'
        ),
        .data$year, .data$month
      )
    )
  }
  if( substr(table$period[1], 1, 1) == 'Q' ){
    return(
      dplyr::arrange(
        dplyr::relocate(
          dplyr::select(
            dplyr::mutate(table, quarter = as.integer(substr(.data$period, 2, 3))),
            -'period', -'periodName'
          ),
          .data$quarter, .after='year'
        ),
        .data$year, .data$quarter
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
#'
#' @family blsR-utils
#' @export
#'
#' @examples
#' \dontrun{
#' series <- get_series('LNS14000001')
#' table <- data_as_table(series$data)
#' }

data_as_table <- function(data){
  dplyr::mutate(dplyr::bind_rows(data), year = as.integer(.data$year))
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
#'
#' @family blsR-utils
#' @export
#'
#' @examples
#' \dontrun{
#' series <- get_series('LNS14000001')
#' table <- data_as_tidy_table(series$data)
#' }

data_as_tidy_table <- function(data){
  tidy_periods( dplyr::bind_rows(data) )
}
