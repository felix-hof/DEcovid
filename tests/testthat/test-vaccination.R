dat <- get_vaccination()

test_that("All dates included", {
  data_date <- dat$date
  constructed_date <- seq(min(dat$date), max(dat$date), by = 1)

  expect_true(all(data_date %in% constructed_date))
  expect_true(all(constructed_date %in% data_date))
})

test_that("Log-proportion of unvaccinated people is decreasing", {
  expect_equal(order(dat$value, decreasing = TRUE), seq_len(nrow(dat)))
})
