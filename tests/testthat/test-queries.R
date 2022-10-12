

library(httr)

test_that('payload tests',{
  expect_equal(typeof(query_n_series(c(1,2))), 'list')
  expect_error(query_n_series('test', 2005))
  expect_error(query_n_series('test', 2005, 2004))
  expect_warning(query_n_series('test', 1984, 2004), 'exceeds 20 years')
  expect_mapequal(
    query_n_series('test', 1985, 2004, TRUE, TRUE, TRUE, TRUE)$payload,
    list(
      seriesid='test', startyear=1985, endyear=2004, catalog=TRUE,
      calculations=TRUE, annualaverage=TRUE, aspects=TRUE
      )
  )
})

test_that("urls", {
  query <- query_series('test')
  expect_equal(typeof(query), 'list')
  expect_equal(parse_url(query$url)$path, 'publicAPI/v2/timeseries/data/test')
  query2 <- query_series('test', 2004, 2006)
  expect_mapequal(parse_url(query2$url)$query, list(startyear='2004', endyear='2006'))

  query3 <- query_n_series(c(1,2), calculations = TRUE)
  expect_equal(parse_url(query3$url)$path, 'publicAPI/v2/timeseries/data')
  expect_null(parse_url(query3$url)$query)
  expect_mapequal(query3$payload, list(seriesid=c(1,2), calculations=TRUE))

  query4 <- query_popular_series()
  expect_equal(parse_url(query4$url)$path, 'publicAPI/v2/timeseries/popular')
  expect_null(parse_url(query4$url)$query)
  query5 <- query_popular_series('LN')
  expect_mapequal(parse_url(query5$url)$query, list(survey='LN'))

 query6 <- query_all_surveys()
 expect_equal(parse_url(query6$url)$path, 'publicAPI/v2/surveys')

 query7 <- query_survey_info('LN')
 expect_equal(parse_url(query7$url)$path, 'publicAPI/v2/surveys/LN')

 query8 <- query_latest_observation('test')
 expect_equal(parse_url(query8$url)$path, 'publicAPI/v2/timeseries/data/test')
 expect_mapequal(parse_url(query8$url)$query, list(latest='TRUE'))

})
