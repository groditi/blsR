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
