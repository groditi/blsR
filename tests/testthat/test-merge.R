sample_json_response <- '{
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
    ]}'
sample_json_response2 <- '{
  "seriesID":"LNS14000001",
  "data":[
{"year":"2011","period":"M12","periodName":"December","value":"8.7","footnotes":[{}]},
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
    ]}'

sample_response <- jsonlite::fromJSON(sample_json_response, simplifyVector = FALSE)
sample_response2 <- jsonlite::fromJSON(sample_json_response2, simplifyVector = FALSE)


test_that("data_as_table parse_data works", {
  table <- data_as_table(sample_response$data)
  expect_equal(nrow(table), 12)
  expect_equal(ncol(table), 4)
  expect_equal(table$value[1], 8.3)
  expect_equal(table$year[1], 2011)
  expect_equal(table$period[1], 'M12')

  tidy_table <- data_as_tidy_table(sample_response$data)
  expect_equal(nrow(tidy_table), 12)
  expect_equal(ncol(tidy_table), 3)
  expect_equal(tidy_table$value[1], 8.5)
  expect_equal(tidy_table$year[1], 2011)
  expect_equal(tidy_table$month[1], 1)
})

test_that("data_as_table parse_values false works", {
  table <- data_as_table(sample_response$data, parse_values = FALSE)
  expect_equal(nrow(table), 12)
  expect_equal(ncol(table), 4)
  expect_equal(table$value[1], '8.3')
  expect_equal(table$year[1], 2011)

  tidy_table <- data_as_tidy_table(sample_response$data, parse_values = FALSE)
  expect_equal(nrow(tidy_table), 12)
  expect_equal(ncol(tidy_table), 3)
  expect_equal(tidy_table$month[1], 1)
  expect_equal(tidy_table$value[1], '8.5')
})

test_that("merges work", {
  table1 <- data_as_table(sample_response$data)
  table2 <- data_as_table(sample_response2$data)
  merged_table <- merge_tables(list(uer1 = table1, uer2 = table2))
  expect_equal(nrow(merged_table), 12)
  expect_equal(ncol(merged_table), 4)
  expect_named(merged_table, c('year','period','uer1','uer2'))


  tidy_table1 <- data_as_tidy_table(sample_response$data)
  tidy_table2 <- data_as_tidy_table(sample_response2$data)
  merged_tidy_table <- merge_tidy_tables(list(uer1 = tidy_table1, uer2 = tidy_table2))
  expect_equal(nrow(merged_tidy_table), 12)
  expect_equal(ncol(merged_tidy_table), 4)
  expect_named(merged_tidy_table, c('year','month','uer1','uer2'))
})

test_that("merge join logic works", {
  table1 <- data_as_table(sample_response$data)
  table2 <- data_as_table(sample_response2$data)[-1,]
  merged_table <- merge_tables(list(uer1 = table1, uer2 = table2))
  expect_equal(nrow(merged_table), 12)
  expect_equal(ncol(merged_table), 4)
  expect_named(merged_table, c('year','period','uer1','uer2'))


  tidy_table1 <- data_as_tidy_table(sample_response$data)
  tidy_table2 <- data_as_tidy_table(sample_response2$data)[-1,]
  merged_tidy_table <- merge_tidy_tables(list(uer1 = tidy_table1, uer2 = tidy_table2))
  expect_equal(nrow(merged_tidy_table), 12)
  expect_equal(ncol(merged_tidy_table), 4)
  expect_named(merged_tidy_table, c('year','month','uer1','uer2'))
})

test_that("as_zoo work", {
  tidy_table <- data_as_tidy_table(sample_response$data, parse_values = FALSE)
  zoo_table <- tidy_table_as_zoo(tidy_table)
  expect_equal(nrow(zoo_table), 12)
  expect_equal(ncol(zoo_table), 1)
  expect_equal(zoo::index(zoo_table), zoo::as.yearmon(paste(2011, 1:12), "%Y %m"))


  tidy_table1 <- data_as_tidy_table(sample_response$data)
  tidy_table2 <- data_as_tidy_table(sample_response2$data)
  merged_tidy_table <- merge_tidy_tables(list(uer1 = tidy_table1, uer2 = tidy_table2))
  merged_zoo <- tidy_table_as_zoo(merged_tidy_table)
  expect_equal(nrow(merged_zoo), 12)
  expect_equal(ncol(merged_zoo), 2)
  expect_named(merged_zoo, c('uer1','uer2'))
})
