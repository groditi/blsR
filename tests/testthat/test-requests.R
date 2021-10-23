

check_test_key <- function(){
  test_key <- Sys.getenv('BLS_API_TEST_KEY')
  skip_if(identical(test_key, ""), "'BLS_API_TEST_KEY' not set.")
}

test_that('live get_series_table request tests', {
  check_test_key()
  api_key <- Sys.getenv('BLS_API_TEST_KEY')
  t1 <- get_series_table('LNS14000002Q', api_key, 1967, 1987)
  years <- as.numeric(dplyr::pull(t1, 'year'))
  expect_equal(min(years), 1967)
  expect_equal(max(years), 1987)
  expect_equal(length(years), 84)
}
)


test_that('live get_series_tables request tests', {
  check_test_key()
  api_key <- Sys.getenv('BLS_API_TEST_KEY')
  series_ids <- c('LNS14000001','LNS14000002')
  t1 <- get_series_tables(series_ids, api_key, 1968, 1988)
  expect_setequal(names(t1), series_ids)
  years <- as.numeric(dplyr::pull(t1[[1]], 'year'))
  expect_equal(min(years), 1968)
  expect_equal(max(years), 1988)
  expect_equal(length(years), 252)
}
)
