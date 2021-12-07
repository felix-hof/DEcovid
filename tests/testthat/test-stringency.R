test_that("No NAs in stringency data", {
  expect_true(all(!is.na(get_stringency()$value)))
})

test_that("Date is a sequence from some start to some end by increment 1 day", {
  dataset_date <- get_stringency()$date
  created_date <- seq(min(dataset_date), max(dataset_date), by = 1)
  expect_true(all(created_date %in% dataset_date))
  expect_true(all(dataset_date %in% created_date))
})
