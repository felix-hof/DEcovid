dat <- get_vaccination()

test_that("All dates included", {
  data_date <- dat$date
  constructed_date <- seq(min(dat$date), max(dat$date), by = 1)

  expect_true(all(data_date %in% constructed_date))
  expect_true(all(constructed_date %in% data_date))
})

test_that("Proportion of unvaccinated people is decreasing", {
  grid <- expand.grid(region = unique(dat$region), age = unique(dat$age))
  grid$decreasing <- vapply(seq_len(nrow(grid)), function(x){
    idx <- with(dat, region == grid$region[x] & age == grid$age[x])
    data <- dat[idx, ]
    all(order(data$value, decreasing = TRUE) == seq(1L, nrow(data), 1L))
  }, logical(1L))
  expect_true(all(grid$decreasing))
})
