test_that(".mtc_resolve_dates expands a length-2 range and dedupes", {
  out <- .mtc_resolve_dates(as.Date(c("2020-07-02", "2020-07-04")), tz = "America/Denver")
  expect_equal(out$date, as.Date(c("2020-07-02", "2020-07-03", "2020-07-04")))
  expect_equal(out$doy, lubridate::yday(out$date))
  expect_true(all(out$utc_end - out$utc_start == 24))
})

test_that(".mtc_resolve_dates accepts a vector of specific non-contiguous dates", {
  out <- .mtc_resolve_dates(as.Date(c("2020-01-15", "2020-06-15", "2020-01-15")),
                            tz = "America/Denver")
  expect_equal(out$date, as.Date(c("2020-01-15", "2020-06-15")))
})

test_that(".mtc_resolve_dates stops when unique days exceed 52", {
  many_dates <- as.Date("2020-01-01") + 0:52
  expect_error(.mtc_resolve_dates(many_dates, tz = "America/Denver"),
               "52")
})

test_that(".mtc_resolve_dates stops on non-Date input", {
  expect_error(.mtc_resolve_dates("2020-07-02", tz = "America/Denver"),
               "Date")
})

test_that(".mtc_resolve_dates local midnight bounds account for tz offset", {
  out <- .mtc_resolve_dates(as.Date("2020-07-02"), tz = "America/Denver")
  # Denver is UTC-6 in July (MDT): local midnight 2020-07-02 == 06:00 UTC
  expect_equal(out$utc_start, as.POSIXct("2020-07-02 06:00:00", tz = "UTC"))
})

test_that(".mtc_match_time_index returns 24 rows per fully-covered day", {
  time_utc <- seq(as.POSIXct("2020-07-01 00:00:00", tz = "UTC"), by = "hour",
                  length.out = 120)
  date_bounds <- .mtc_resolve_dates(as.Date(c("2020-07-02", "2020-07-03")),
                                    tz = "America/Denver")
  out <- .mtc_match_time_index(time_utc, date_bounds)
  expect_equal(nrow(out), 48)
  expect_equal(unique(out$hour_offset), 0:23)
  expect_equal(out$utc_idx[out$date == as.Date("2020-07-02") & out$hour_offset == 0],
              which(time_utc == date_bounds$utc_start[1]))
})

test_that(".mtc_match_time_index warns and skips a day with partial coverage", {
  time_utc <- seq(as.POSIXct("2020-07-02 06:00:00", tz = "UTC"), by = "hour",
                  length.out = 10)  # only 10 hours available, not a full local day
  date_bounds <- .mtc_resolve_dates(as.Date("2020-07-02"), tz = "America/Denver")
  expect_warning(out <- .mtc_match_time_index(time_utc, date_bounds), "10/24")
  expect_equal(nrow(out), 0)
})

test_that(".mtc_match_time_index stops when no requested day has full coverage", {
  time_utc <- as.POSIXct("2019-01-01 00:00:00", tz = "UTC")  # unrelated single timestamp
  date_bounds <- .mtc_resolve_dates(as.Date("2020-07-02"), tz = "America/Denver")
  expect_error(.mtc_match_time_index(time_utc, date_bounds), "No requested dates")
})
