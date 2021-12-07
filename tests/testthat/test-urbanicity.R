dat <- get_urbanicity()
nuts_table <- nuts_table()

test_that("there are no NAs", {
  expect_true(!any(is.na(dat$lvl3)))
})

test_that("All values are finite", {
  expect_true(all(is.finite(dat$value)))
})

test_that("All regions are included", {
  expect_true(all(nuts_table$lvl3 %in% dat$lvl3))
})
