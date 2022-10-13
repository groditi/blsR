sample_json_response <- '
{"series":[
  {
    "seriesID":"LNS14000002",
    "data":[
      {"year":"2011","period":"M12","periodName":"December","value":"8.3","latest":"true","footnotes":[{}]},
      {"year":"2011","period":"M11","periodName":"November","value":"8.3","footnotes":[{}]},
      {"year":"2011","period":"M10","periodName":"October","value":"8.4","footnotes":[{}]},
      {"year":"2011","period":"M09","periodName":"September","value":"8.7","footnotes":[{}]},
      {"year":"2011","period":"M08","periodName":"August","value":"8.5","footnotes":[{}]},
      {"year":"2011","period":"M07","periodName":"July","value":"8.4","footnotes":[{}]},
      {"year":"2011","period":"M06","periodName":"June","value":"8.5","footnotes":[{}]},
      {"year":"2011","period":"M05","periodName":"May","value":"8.5","footnotes":[{}]},
      {"year":"2011","period":"M04","periodName":"April","value":"8.5","footnotes":[{}]},
      {"year":"2011","period":"M03","periodName":"March","value":"8.4","footnotes":[{}]},
      {"year":"2011","period":"M02","periodName":"February","value":"8.5","footnotes":[{}]},
      {"year":"2011","period":"M01","periodName":"January","value":"8.5","footnotes":[{}]}
    ]
  },
  {
    "seriesID":"LNS14000001",
    "data":[
      {"year":"2011","period":"M12","periodName":"December","value":"8.7","latest":"true","footnotes":[{}]},
      {"year":"2011","period":"M11","periodName":"November","value":"8.9","footnotes":[{}]},
      {"year":"2011","period":"M10","periodName":"October","value":"9.2","footnotes":[{}]},
      {"year":"2011","period":"M09","periodName":"September","value":"9.3","footnotes":[{}]},
      {"year":"2011","period":"M08","periodName":"August","value":"9.4","footnotes":[{}]},
      {"year":"2011","period":"M07","periodName":"July","value":"9.5","footnotes":[{}]},
      {"year":"2011","period":"M06","periodName":"June","value":"9.6","footnotes":[{}]},
      {"year":"2011","period":"M05","periodName":"May","value":"9.5","footnotes":[{}]},
      {"year":"2011","period":"M04","periodName":"April","value":"9.6","footnotes":[{}]},
      {"year":"2011","period":"M03","periodName":"March","value":"9.5","footnotes":[{}]},
      {"year":"2011","period":"M02","periodName":"February","value":"9.5","footnotes":[{}]},
      {"year":"2011","period":"M01","periodName":"January","value":"9.7","footnotes":[{}]}
    ]
  }
]}'

sample_json_response2 <- '
{"series":[
  {
    "seriesID":"LNS14000002",
    "data":[
      {"year":"2010","period":"M12","periodName":"December","value":"8.3","footnotes":[{}]},
      {"year":"2010","period":"M11","periodName":"November","value":"8.3","footnotes":[{}]},
      {"year":"2010","period":"M10","periodName":"October","value":"8.4","footnotes":[{}]},
      {"year":"2010","period":"M09","periodName":"September","value":"8.7","footnotes":[{}]},
      {"year":"2010","period":"M08","periodName":"August","value":"8.5","footnotes":[{}]},
      {"year":"2010","period":"M07","periodName":"July","value":"8.4","footnotes":[{}]},
      {"year":"2010","period":"M06","periodName":"June","value":"8.5","footnotes":[{}]},
      {"year":"2010","period":"M05","periodName":"May","value":"8.5","footnotes":[{}]},
      {"year":"2010","period":"M04","periodName":"April","value":"8.5","footnotes":[{}]},
      {"year":"2010","period":"M03","periodName":"March","value":"8.4","footnotes":[{}]},
      {"year":"2010","period":"M02","periodName":"February","value":"8.5","footnotes":[{}]},
      {"year":"2010","period":"M01","periodName":"January","value":"8.5","footnotes":[{}]}
    ]
  },
  {
    "seriesID":"LNS14000001",
    "data":[
      {"year":"2010","period":"M12","periodName":"December","value":"8.7","footnotes":[{}]},
      {"year":"2010","period":"M11","periodName":"November","value":"8.9","footnotes":[{}]},
      {"year":"2010","period":"M10","periodName":"October","value":"9.2","footnotes":[{}]},
      {"year":"2010","period":"M09","periodName":"September","value":"9.3","footnotes":[{}]},
      {"year":"2010","period":"M08","periodName":"August","value":"9.4","footnotes":[{}]},
      {"year":"2010","period":"M07","periodName":"July","value":"9.5","footnotes":[{}]},
      {"year":"2010","period":"M06","periodName":"June","value":"9.6","footnotes":[{}]},
      {"year":"2010","period":"M05","periodName":"May","value":"9.5","footnotes":[{}]},
      {"year":"2010","period":"M04","periodName":"April","value":"9.6","footnotes":[{}]},
      {"year":"2010","period":"M03","periodName":"March","value":"9.5","footnotes":[{}]},
      {"year":"2010","period":"M02","periodName":"February","value":"9.5","footnotes":[{}]},
      {"year":"2010","period":"M01","periodName":"January","value":"9.7","footnotes":[{}]}
    ]
  }
]}'

sample_response <- jsonlite::fromJSON(sample_json_response, simplifyVector = FALSE)
sample_response2 <- jsonlite::fromJSON(sample_json_response2, simplifyVector = FALSE)

test_that("spanning queries works", {
  series_ids <- c('LNS14000001','LNS14000002')
  query_fn <- purrr::partial(query_n_series, series_ids, ...=, catalog=TRUE)
  expect_silent(queries <- span_request_queries(2013, 2015, 5, query_fn))
  expect_message(
    queries <- span_request_queries(2013, 2015, 2, query_fn),
    'longer than'
  )

  expect_equal(length(queries), 2)
  expect_equal(queries[[1]]$payload$seriesid, series_ids)
  expect_equal(queries[[1]]$payload$seriesid, queries[[2]]$payload$seriesid)
  expect_equal(queries[[1]]$payload$startyear, 2014)
  expect_equal(queries[[1]]$payload$endyear, 2015)
  expect_equal(queries[[2]]$payload$startyear, 2013)
  expect_equal(queries[[2]]$payload$endyear, 2013)

})

test_that("reducing responses works", {
  responses <- list(sample_response, sample_response2)
  series <- reduce_spanned_responses(responses)
  expect_equal(names(series), c('LNS14000002','LNS14000001'))
  expect_equal(unname(lengths(series)), c(2,2))
  expect_equal(series[[1]]$seriesID[1], names(series[1]))
  expect_equal(length(series[[1]]$data), 24)
  expect_equal(length(series[[1]]$data), length(series[[2]]$data))
  expect_equal(length(series[[1]]$data), length(series[[2]]$data))
})
