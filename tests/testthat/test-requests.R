

check_test_key <- function(){
  test_key <- Sys.getenv('BLS_API_TEST_KEY')
  skip_if(identical(test_key, ""), "'BLS_API_TEST_KEY' not set.")
}

test_that('validate_years works',{

  expect_silent(.validate_years(NA, NA))
  expect_error(.validate_years(2014, NA), 'both')
  expect_error(.validate_years(NA, 2014), 'both')
  expect_error(.validate_years('2014','2015'), 'numeric')
  expect_error(.validate_years(c(2014,2015),c(2016,2017)), 'scalar')
  expect_error(.validate_years(2016,2015), 'greater')
  expect_silent(.validate_years(2015,2016))
  expect_silent(.validate_years(2015L,2016L))
})

 test_that('series_id_names works',{

   expect_error(.series_id_names(c(1:5)), 'character vector')
   expect_error(.series_id_names(c('foo')[-1]), 'empty')
   expect_error(.series_id_names(c('a','b','')), 'empty')
   expect_equal(.series_id_names(c('a','b','c')), c('a','b','c'))

   expect_error(.series_id_names(list(1:3)), 'character vector')
   expect_error(.series_id_names(list()), 'empty')
   expect_error(.series_id_names(list(a='a', b='b',c='')), 'empty')
   expect_equal(.series_id_names(list(a='d', b='e',c='f')), c('a','b','c'))
#
#   expect_error(.series_id_names(list(a='d', b='e','f')), 'named')
#   expect_error(.series_id_names(list(a='d', a='e')), 'unique')
#   expect_error(.series_id_names(list(a='d', a='e', ''='f')), 'empty')
#
 })

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
