# blsR 0.5.0

  ## New functionality
   * Managing the API key can now be done via the `BLS_API_KEY` environment
   variable. This can be done using a `.Renviron` file or manually by setting
   the key with the new `bls_set_key` function. Functions which previously
   required the argument `api_key` now default to the result of `bls_set_key`
   * `get_n_series` and all functions downstream of it now support automatic
   spanning of lists of `series_ids` longer than the API limit by breaking up
   the query into multiple API calls.
   
  ## Other Changes
   * Function arguments that previously defaulted to `NA` now default to `NULL`
   * Major refactor of documentation
  
  ## Bug Fixes
   * `get_latest_observation`'s argument was incorrectly named `survey_id`
   instead of `series_id`

# blsR 0.4.0

  * Previously, the `get_n_series` documentation state that `...` was passed to
  `query_n_series`. This was both inaccurate and inconsistent with the behavior 
  of the other `get_*` functions. `get_n_series` arguments were updated to 
  capture all of the `query_n_series` arguments and pass `...` to `bls_request`
  * Previously, calling `get_n_series` for only 1 series would fail due to a
  json encoding issue stemming from a length 1 vector being coerced to a scalar
  within `httr::POST` in `bls_request`.
  * Previously, `data_as_table` would return a variable number of columns based
  on the shape of the input. This led to unpredictable output and caused issues
  when merging multiple requests for different time spans. Going forward
  `data_as_table` will return a tibble with only 4 columns (`year`, `period`, 
  `periodName`, and `value`). Users that wish to access aspects, footnotes,
  calculations, or the 'latest' flag should use `get_n_series`.
  * As a result of the changes to `data_as_table`, `get_series_table` and 
  `get_n_series_table` no longer include the `latest` or `footnotes` columns.
  This fixes a condition where requesting aspects for a series without aspects
  would cause the table to have zero rows and a condition where the `latest`
  column could disappear between two identical calls if the data was updated.
  * Previously, pagination for periods exceeding the API limits was done 
  automatically in both `get_series_table` and `get_series_tables`. Pagination 
  behavior has been renamed to 'spanning', removed from the previous points of
  implementation and implemented as three spanning functions following a 
  map-reduce pattern:
    * `span_series_request` - encapsulates all of the spanning behavior
    * `span_request_queries` - creates multiple conforming queries
    * `reduce_spanned_responses` - merges the results of multiple queries
  The spanning behavior is now implemented at `get_series` and `get_n_series`,
  which removes the burden of merging multiple requests from the higher level
  `get_series_table` and `get_n_series_table` functions and allows users to
  create their own high-level functions to alter the presentation without having
  to be aware of the multiple requests. Spanning behavior may be turned off by 
  setting the `span` argument to `FALSE`.

# blsR 0.3.2

  * Previously, if `get_series_table` or `get_series_tables` were called for a 
  date range and series combination with no observations available, an error was 
  triggered by `data_as_table` failing. Series and period combinations with no
  results now return `NA`.
  * `get_series_table` and `get_series_tables` now notify if the request is 
  broken up into multiple API calls.
  * For requests broken up into multiple API calls, `get_series_tables` now
  merges the results descending order to match the order of the returned data.
  * `get_series` and `get_n_series` now notify if request returns 0 observations
  * `data_as_table` throws an error if `data` argument is empty or not a list.

# blsR 0.3.1

  * Previously, when `merge_tables` was used applied to tables with different
  periods, results were dependent on the order in which tables were arranged-in
  on the arguments list due to the use of 'left_join' logic. The updated
  implementation will return a row for every period in any of the tables,
  making it insensitive to the order of the tables in the input. As a 
  side-effect of the implementation the resulting table will return the merged
  table ordered-by the column(s) in the `join_by` argument in ascending order.
  
# blsR 0.3.0

  * `data_as_table`, `get_series_table`, `get_series_tables`, and 
  `get_n_series_table` all have new optional parameter "parse_values" which
  will cast numeric strings into numeric values
  * `bls_request` has new optional `process_response` argument which gives
  users access to the raw API response
  * `get series` documentation improved
  * Add `tidy_table_as_zoo` for users of `zoo` and `xts`

# blsR 0.2.1

  * edit DESCRIPTION to make check_rhub() happy

# blsR 0.2.0
 
  * Add warning if attempting to request more than 20 years
  * Allow `get_series_table` and `get_series_tables` to request more than 20
  years of data by making multiple requests
  * Introduce live tests if `BLS_API_TEST_KEY` env variable is set
  * Expand documentation
  * `tidy_periods` now puts month and quarter column after year
  * Improve README

# blsR 0.1.0
 
 Initial release
