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
#' The result will be sorted in ascending order using these columns.
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
  if( !is.list(tables) | !is.character(names(tables)) | any(is.na(names(tables))) )
    stop('merge_tables requires a named list as input "tables".')
  keys <- dplyr::arrange(
    unique(
      purrr::reduce(
        purrr::map(tables, ~dplyr::select(.x, dplyr::all_of(join_by))),
        dplyr::union
      )
    ),
    !!!rlang::syms(join_by)
  )

  purrr::reduce(
    purrr::prepend(
      purrr::imap(
        tables,
        ~dplyr::select(.x, dplyr::all_of(join_by), !!rlang::as_name(.y) := 'value')
      ),
      list(keys=keys)
    ),
    dplyr::left_join,
    by = dplyr::all_of(join_by)
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
#' @param data a list of individual datum entries as returned by the API
#' @param parse_values optional boolean. If set to `true` (default) it will
#' attempt to parse the contents of `value` and cast numeric strings as numeric
#' values. If set to `false` it will retain `value` as a column of strings.
#'
#' @details currently `data_as_table` is very similar to [dplyr::bind_rows()]
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

data_as_table <- function(data, parse_values=TRUE){
  if(!purrr::is_list(data) || length(data) < 1)
    rlang::abort('data_as_table requires argument "data" must be a list with one or more elements')
  table <- dplyr::mutate(dplyr::bind_rows(data), year = as.integer(.data$year))
  if(parse_values){
    return(dplyr::mutate(table, value = readr::parse_guess(.data$value)))
  }

  return(table)
}

#' Convert a list of data entries as returned by BLS API to a table
#'
#' @param data a list of individual datum entries as returned the API
#' @param parse_values optional boolean. If set to `true` (default) it will
#' attempt to parse the contents of `value` and cast numeric strings as numeric
#' values. If set to `false` it will retain `value` as a column of strings.
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

data_as_tidy_table <- function(data, parse_values=TRUE){
  tidy_periods( data_as_table(data, parse_values = parse_values) )
}

.zoo_index_function <- function(table){
  if('month' %in% names(table)){
    index <- zoo::as.yearmon(paste(table$year, table$month), "%Y %m")
  } else {
    if('quarter' %in% names(table)){
      index <- zoo::as.yearqtr(paste(table$year, table$quarter), "%Y %q")
    } else {
      index <- zoo::as.yearqtr(paste(table$year, 4), "%Y %q")
    }
  }
  index
}

#' Convert a single series or n series tables into a zoo object
#'
#' @param table a table of results
#' @param index_function optional closure. The closure parameter is the `table`
#' and it should return a vector of values compatible with a `zoo` index. The
#' default function will return a vector of [`zoo::yearmon`] for monthly series and
#' [`zoo::yearqtr`] for quarterly or annual series.
#'
#' @details A utility function to easily convert retrieved BLS series into
#' `zoo` or `xts` objects.
#'
#' @return a `zoo`object
#'
#' @family blsR-utils
#' @export
#'
#' @examples
#' \dontrun{
#' series <- get_series('LNS14000001')
#' table <- data_as_tidy_table(series$data)
#' zoo_obj <- tidy_table_as_zoo(table)
#' }
tidy_table_as_zoo <- function(table, index_function = .zoo_index_function){
  zoo::zoo(
    dplyr::select(
      table,
      !dplyr::any_of(c('year','month','quarter'))
    ),
    order.by = index_function(table)
  )
}
