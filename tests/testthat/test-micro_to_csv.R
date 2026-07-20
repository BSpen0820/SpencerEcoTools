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

.mtc_test_grid <- function(nrow_ = 3, ncol_ = 2, xmin = 500000, ymin = 4800000,
                           res = 30, crs_wkt = "EPSG:32612") {
  list(nrow = nrow_, ncol = ncol_,
       xmin = xmin, xmax = xmin + ncol_ * res,
       ymin = ymin, ymax = ymin + nrow_ * res,
       crs_wkt = terra::crs(terra::rast(crs = crs_wkt), proj = FALSE))
}

test_that(".mtc_grid_template builds a matching empty SpatRaster", {
  g <- .mtc_test_grid()
  r <- .mtc_grid_template(g)
  expect_equal(terra::nrow(r), 3L)
  expect_equal(terra::ncol(r), 2L)
  expect_equal(terra::res(r), c(30, 30))
})

test_that(".mtc_resolve_cell with cell_input_type = 'index' resolves directly", {
  g <- .mtc_test_grid()
  pos <- .mtc_resolve_cell(g, g, cell = c(2, 1), cell_input_type = "index")
  expect_equal(pos$abv_x_idx, 2L)
  expect_equal(pos$abv_y_idx, 1L)
  expect_equal(pos$blw_x_idx, 2L)
  expect_equal(pos$blw_y_idx, 1L)
})

test_that(".mtc_resolve_cell with cell_input_type = 'cellnumber' resolves row-major", {
  g <- .mtc_test_grid(nrow_ = 3, ncol_ = 2)
  pos <- .mtc_resolve_cell(g, g, cell = 3, cell_input_type = "cellnumber")
  expect_equal(pos$abv_x_idx, 1L)  # cell 3 = row 2, col 1 (row-major, 2 cols/row)
  expect_equal(pos$abv_y_idx, 2L)
})

test_that(".mtc_resolve_cell stops on grid mismatch for index-based selection", {
  g1 <- .mtc_test_grid()
  g2 <- .mtc_test_grid(res = 60)
  expect_error(.mtc_resolve_cell(g1, g2, cell = c(1, 1), cell_input_type = "index"),
               "do not share the same grid")
})

test_that(".mtc_resolve_cell stops on out-of-bounds index", {
  g <- .mtc_test_grid()
  expect_error(.mtc_resolve_cell(g, g, cell = c(99, 1), cell_input_type = "index"),
               "outside the grid")
})

test_that(".mtc_resolve_cell with cell_input_type = 'lonlat' reprojects and resolves", {
  g <- .mtc_test_grid()
  tmpl <- .mtc_grid_template(g)
  # centre of cell (1,1) in native coords, reprojected to lon/lat
  native_pt <- terra::vect(matrix(c(terra::xFromCol(tmpl, 1), terra::yFromRow(tmpl, 1)),
                                  nrow = 1), crs = terra::crs(tmpl))
  ll <- terra::crds(terra::project(native_pt, "EPSG:4326"))
  pos <- .mtc_resolve_cell(g, g, cell = c(ll[1, 1], ll[1, 2]), cell_input_type = "lonlat")
  expect_equal(pos$abv_x_idx, 1L)
  expect_equal(pos$abv_y_idx, 1L)
  expect_true(is.numeric(pos$lon) && is.numeric(pos$lat))
})

test_that(".mtc_resolve_cell stops when lonlat falls outside the grid", {
  g <- .mtc_test_grid()
  expect_error(.mtc_resolve_cell(g, g, cell = c(0, 0), cell_input_type = "lonlat"),
               "outside")
})

test_that(".mtc_resolve_cell stops on bad cell_input_type or cell length", {
  g <- .mtc_test_grid()
  expect_error(.mtc_resolve_cell(g, g, cell = c(1, 1), cell_input_type = "bogus"))
  expect_error(.mtc_resolve_cell(g, g, cell = 1, cell_input_type = "index"),
               "length 2")
})
