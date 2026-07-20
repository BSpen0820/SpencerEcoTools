test_that(".mtc_resolve_dates expands a length-2 range and dedupes", {
  out <- .mtc_resolve_dates(as.Date(c("2020-07-02", "2020-07-04")), tz = "America/Denver")
  expect_equal(out$date, as.Date(c("2020-07-02", "2020-07-03", "2020-07-04")))
  expect_equal(out$doy, lubridate::yday(out$date))
  # POSIXct subtraction auto-picks units (e.g. "days" for exactly 24h), so
  # force hours explicitly rather than comparing a difftime to a bare 24.
  expect_true(all(as.numeric(out$utc_end - out$utc_start, units = "hours") == 24))
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

test_that(".mtc_match_time_index warns and skips a partial day while keeping a complete one", {
  # 2020-07-02 has full 24h local-day coverage; 2020-07-03 only has 10 hours
  # available. A day that's *skipped* still leaves the call successful as
  # long as at least one other requested day is complete (only zero
  # complete days overall triggers stop() -- see the next test).
  time_utc <- c(
    seq(as.POSIXct("2020-07-02 06:00:00", tz = "UTC"), by = "hour", length.out = 24),
    seq(as.POSIXct("2020-07-03 06:00:00", tz = "UTC"), by = "hour", length.out = 10)
  )
  date_bounds <- .mtc_resolve_dates(as.Date(c("2020-07-02", "2020-07-03")),
                                    tz = "America/Denver")
  expect_warning(out <- .mtc_match_time_index(time_utc, date_bounds), "10/24")
  expect_equal(nrow(out), 24)
  expect_true(all(out$date == as.Date("2020-07-02")))
})

test_that(".mtc_match_time_index stops when no requested day has full coverage", {
  time_utc <- as.POSIXct("2019-01-01 00:00:00", tz = "UTC")  # unrelated single timestamp
  date_bounds <- .mtc_resolve_dates(as.Date("2020-07-02"), tz = "America/Denver")
  expect_error(.mtc_match_time_index(time_utc, date_bounds), "No requested dates")
})

test_that(".mtc_compute_zen peaks near solar noon and clamps night values to 90", {
  utc_time <- seq(as.POSIXct("2017-07-15 06:00:00", tz = "UTC"), by = "hour",
                  length.out = 24)
  zen <- .mtc_compute_zen(utc_time, lon = -110.7, lat = 43.9, tz = "America/Denver")

  expect_length(zen, 24)
  expect_true(all(zen <= 90))
  local_hour <- as.integer(format(lubridate::with_tz(utc_time, "America/Denver"), "%H"))
  expect_equal(zen[local_hour == 0], 90)                 # midnight: below horizon, clamped
  expect_true(zen[which.min(abs(local_hour - 13))] < 30) # near solar noon: sun high
  expect_true(min(zen) == zen[which(local_hour %in% 12:14)][which.min(zen[local_hour %in% 12:14])])
})

test_that(".mtc_compute_zen restores the system TZ after running", {
  old_tz <- Sys.getenv("TZ")
  utc_time <- as.POSIXct("2017-07-15 12:00:00", tz = "UTC")
  .mtc_compute_zen(utc_time, lon = -110.7, lat = 43.9, tz = "America/Denver")
  expect_equal(Sys.getenv("TZ"), old_tz)
})
