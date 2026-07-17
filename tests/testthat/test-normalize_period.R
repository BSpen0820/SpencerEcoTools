test_that(".normalize_period floors to first-of-month and builds the period label", {
  p <- .normalize_period(as.Date("2020-01-15"), as.Date("2020-03-20"))
  expect_equal(p$start_date, as.Date("2020-01-01"))
  expect_equal(p$end_date, as.Date("2020-03-01"))
  expect_equal(p$period_label, "20200101_to_20200301")
})

test_that(".normalize_period handles same-month start and end", {
  p <- .normalize_period(as.Date("2020-06-01"), as.Date("2020-06-28"))
  expect_equal(p$start_date, as.Date("2020-06-01"))
  expect_equal(p$end_date, as.Date("2020-06-01"))
  expect_equal(p$period_label, "20200601_to_20200601")
})
