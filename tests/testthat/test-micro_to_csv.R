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

test_that(".mtc_resolve_cell stops when grids have matching res/origin/CRS but different dimensions", {
  g1 <- .mtc_test_grid(nrow_ = 3, ncol_ = 2)
  g2 <- .mtc_test_grid(nrow_ = 5, ncol_ = 2)  # same origin/res/crs, different row count
  expect_error(.mtc_resolve_cell(g1, g2, cell = c(1, 1), cell_input_type = "index"),
               "dimensions")
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

test_that(".mtc_open_nc reads correct grid metadata and time axis", {
  skip_if_not_installed("ncdf4")
  fix <- .mtc_write_fixture_pair("nc")
  h <- .mtc_open_nc(fix$abv_path)

  expect_equal(h$kind, "nc")
  expect_equal(h$nrow, 3L); expect_equal(h$ncol, 2L)
  expect_equal(h$res_x, 30); expect_equal(h$res_y, 30)
  expect_equal(length(h$time_utc), 120L)
  expect_equal(h$time_utc[1], fix$fx$tme[1])
  expect_true(all(c("Tz", "relhum", "windspeed", "Rdirdown", "Rdifdown", "Rlwdown") %in% h$vars))
  expect_true(!is.na(h$crs_wkt) && nzchar(h$crs_wkt))
})

test_that(".mtc_read_nc reads exact values at a specific cell and contiguous time range", {
  skip_if_not_installed("ncdf4")
  fix <- .mtc_write_fixture_pair("nc")
  h <- .mtc_open_nc(fix$abv_path)

  out <- .mtc_read_nc(h, c("Tz", "relhum"), x_idx = 2, y_idx = 1, time_idx = 5:10)
  expect_equal(out$Tz,     fix$fx$mout$Tz[1, 2, 5:10])
  expect_equal(out$relhum, fix$fx$mout$relhum[1, 2, 5:10])
})

test_that(".mtc_read_nc handles a non-contiguous (gapped) time_idx", {
  skip_if_not_installed("ncdf4")
  fix <- .mtc_write_fixture_pair("nc")
  h <- .mtc_open_nc(fix$abv_path)

  gapped <- c(3, 4, 5, 50, 51, 52)
  out <- .mtc_read_nc(h, "Tz", x_idx = 1, y_idx = 3, time_idx = gapped)
  expect_equal(out$Tz, fix$fx$mout$Tz[3, 1, gapped])
})

test_that(".mtc_read_nc reads below-ground Tz_BlwGrd_* variables", {
  skip_if_not_installed("ncdf4")
  fix <- .mtc_write_fixture_pair("nc")
  h <- .mtc_open_nc(fix$blw_path)

  expect_true("Tz_BlwGrd_0015" %in% h$vars)
  out <- .mtc_read_nc(h, "Tz_BlwGrd_0015", x_idx = 2, y_idx = 2, time_idx = 1:3)
  expect_equal(out$Tz_BlwGrd_0015, fix$blw_arrs$BlwGrd_0015[2, 2, 1:3])
})

test_that(".mtc_open_h5 reads correct grid metadata, time axis, and below-ground vars", {
  skip_if_not_installed("rhdf5")
  fix <- .mtc_write_fixture_pair("h5")
  h <- .mtc_open_h5(fix$abv_path)

  expect_equal(h$kind, "h5")
  expect_equal(h$nrow, 3L); expect_equal(h$ncol, 2L)
  expect_equal(h$res_x, 30); expect_equal(h$res_y, 30)
  expect_equal(length(h$time_utc), 120L)
  expect_equal(h$time_utc[1], fix$fx$tme[1])
  expect_true(all(c("Tz", "relhum", "windspeed", "Rdirdown", "Rdifdown", "Rlwdown") %in% h$vars))

  hb <- .mtc_open_h5(fix$blw_path)
  expect_true("Tz_BlwGrd_0015" %in% hb$vars)
})

test_that(".mtc_read_h5 reads exact values at a specific cell, including a gapped time_idx", {
  skip_if_not_installed("rhdf5")
  fix <- .mtc_write_fixture_pair("h5")
  h <- .mtc_open_h5(fix$abv_path)

  out <- .mtc_read_h5(h, c("Tz", "relhum"), x_idx = 2, y_idx = 1, time_idx = 5:10)
  expect_equal(out$Tz,     fix$fx$mout$Tz[1, 2, 5:10])
  expect_equal(out$relhum, fix$fx$mout$relhum[1, 2, 5:10])

  gapped <- c(3, 4, 5, 50, 51, 52)
  out2 <- .mtc_read_h5(h, "Tz", x_idx = 1, y_idx = 3, time_idx = gapped)
  expect_equal(out2$Tz, fix$fx$mout$Tz[3, 1, gapped])
})

test_that(".mtc_read_h5 reads below-ground Tz_BlwGrd_* group datasets", {
  skip_if_not_installed("rhdf5")
  fix <- .mtc_write_fixture_pair("h5")
  h <- .mtc_open_h5(fix$blw_path)

  out <- .mtc_read_h5(h, "Tz_BlwGrd_0015", x_idx = 2, y_idx = 2, time_idx = 1:3)
  expect_equal(out$Tz_BlwGrd_0015, fix$blw_arrs$BlwGrd_0015[2, 2, 1:3])
})

.mtc_write_vrt_fixture <- function(nrow_ = 3, ncol_ = 2, ntime_ = 120,
                                   start = as.POSIXct("2020-07-01 00:00:00", tz = "UTC")) {
  skip_if_not_installed("ncdf4")
  fx <- .mtc_test_fixture_data(nrow_, ncol_, ntime_, start)
  tile_dir <- tempfile("vrt_tiles_")
  dir.create(tile_dir)
  tile_path <- file.path(tile_dir, "Tile_001_test_MicroclimModel_period.nc")
  write_tile(fx$mout, tile_path, dtm = fx$dtm, tme = fx$tme, file_fmt = "nc")

  stem <- tempfile("vrt_stem_")
  stitch_tiles(tile_dir, stem, data_type = "mout", file_fmt = "vrt", dtm = fx$dtm)

  list(stem = stem, fx = fx)
}

test_that(".mtc_open_vrt reads correct grid metadata and time axis from per-variable VRTs", {
  skip_if_not_installed("ncdf4")
  fix <- .mtc_write_vrt_fixture()
  h <- .mtc_open_vrt(fix$stem)

  expect_equal(h$kind, "vrt")
  expect_equal(h$nrow, 3L); expect_equal(h$ncol, 2L)
  expect_equal(length(h$time_utc), 120L)
  expect_true(all(c("Tz", "relhum", "windspeed", "Rdirdown", "Rdifdown", "Rlwdown") %in% h$vars))
})

test_that(".mtc_read_vrt reads exact values at a specific cell and time range", {
  skip_if_not_installed("ncdf4")
  fix <- .mtc_write_vrt_fixture()
  h <- .mtc_open_vrt(fix$stem)

  out <- .mtc_read_vrt(h, c("Tz", "relhum"), x_idx = 2, y_idx = 1, time_idx = 5:10)
  expect_equal(out$Tz,     fix$fx$mout$Tz[1, 2, 5:10], tolerance = 1e-6)
  expect_equal(out$relhum, fix$fx$mout$relhum[1, 2, 5:10], tolerance = 1e-6)
})

test_that(".mtc_open_vrt stops when no matching VRT files exist for the stem", {
  expect_error(.mtc_open_vrt(file.path(tempdir(), "no_such_stem_xyz")), "No .vrt files")
})

test_that(".mtc_open_spat reads grid metadata from an already-open multi-variable SpatRaster", {
  skip_if_not_installed("ncdf4")
  fix <- .mtc_write_fixture_pair("nc")
  r <- terra::rast(fix$abv_path)  # no subds -> stacks all vars as "{var}_{time_idx}"
  h <- .mtc_open_spat(r)

  expect_equal(h$kind, "spat")
  expect_equal(h$nrow, 3L); expect_equal(h$ncol, 2L)
  expect_equal(length(h$time_utc), 120L)
  expect_true(all(c("Tz", "relhum", "windspeed", "Rdirdown", "Rdifdown", "Rlwdown") %in% h$vars))
})

test_that(".mtc_read_spat reads exact values at a specific cell and time range", {
  skip_if_not_installed("ncdf4")
  fix <- .mtc_write_fixture_pair("nc")
  r <- terra::rast(fix$abv_path)
  h <- .mtc_open_spat(r)

  out <- .mtc_read_spat(h, c("Tz", "relhum"), x_idx = 2, y_idx = 1, time_idx = 5:10)
  expect_equal(out$Tz,     fix$fx$mout$Tz[1, 2, 5:10], tolerance = 1e-6)
  expect_equal(out$relhum, fix$fx$mout$relhum[1, 2, 5:10], tolerance = 1e-6)
})

test_that(".mtc_read_spat reads below-ground Tz_BlwGrd_* layers", {
  skip_if_not_installed("ncdf4")
  fix <- .mtc_write_fixture_pair("nc")
  r <- terra::rast(fix$blw_path)
  h <- .mtc_open_spat(r)

  expect_true("Tz_BlwGrd_0015" %in% h$vars)
  out <- .mtc_read_spat(h, "Tz_BlwGrd_0015", x_idx = 2, y_idx = 2, time_idx = 1:3)
  expect_equal(out$Tz_BlwGrd_0015, fix$blw_arrs$BlwGrd_0015[2, 2, 1:3], tolerance = 1e-6)
})

test_that(".mtc_open_spat stops when the SpatRaster has no time metadata", {
  r <- terra::rast(nrows = 2, ncols = 2)
  names(r) <- "Tz_1"
  terra::values(r) <- 1:4
  expect_error(.mtc_open_spat(r), "time metadata")
})

test_that(".mtc_open dispatches correctly across all 4 input kinds", {
  skip_if_not_installed("ncdf4")
  skip_if_not_installed("rhdf5")

  nc_fix  <- .mtc_write_fixture_pair("nc")
  h5_fix  <- .mtc_write_fixture_pair("h5")
  vrt_fix <- .mtc_write_vrt_fixture()

  expect_equal(.mtc_open(nc_fix$abv_path)$kind, "nc")
  expect_equal(.mtc_open(h5_fix$abv_path)$kind, "h5")
  expect_equal(.mtc_open(vrt_fix$stem)$kind, "vrt")
  expect_equal(.mtc_open(terra::rast(nc_fix$abv_path))$kind, "spat")
})

test_that(".mtc_open stops on a missing file or unrecognized extension", {
  expect_error(.mtc_open("does_not_exist.nc"), "not found")
  expect_error(.mtc_open("does_not_exist.tif"), "Unrecognized")
})

test_that(".mtc_read dispatches to the right backend and returns matching values", {
  skip_if_not_installed("ncdf4")
  skip_if_not_installed("rhdf5")

  nc_fix <- .mtc_write_fixture_pair("nc")
  h5_fix <- .mtc_write_fixture_pair("h5")

  h_nc <- .mtc_open(nc_fix$abv_path)
  h_h5 <- .mtc_open(h5_fix$abv_path)

  out_nc <- .mtc_read(h_nc, "Tz", x_idx = 1, y_idx = 2, time_idx = 1:5)
  out_h5 <- .mtc_read(h_h5, "Tz", x_idx = 1, y_idx = 2, time_idx = 1:5)

  expect_equal(out_nc$Tz, nc_fix$fx$mout$Tz[2, 1, 1:5])
  expect_equal(out_h5$Tz, h5_fix$fx$mout$Tz[2, 1, 1:5])
})

test_that(".mtc_resolve_elev returns a direct numeric value as-is", {
  expect_equal(.mtc_resolve_elev(1850, x_coord = 0, y_coord = 0, crs_wkt = "EPSG:4326"), 1850)
})

test_that(".mtc_resolve_elev stops on a non-scalar or NA numeric", {
  expect_error(.mtc_resolve_elev(c(1, 2), 0, 0, "EPSG:4326"), "single")
  expect_error(.mtc_resolve_elev(NA_real_, 0, 0, "EPSG:4326"), "single")
})

test_that(".mtc_resolve_elev samples a SpatRaster DEM at the cell's coordinates", {
  dem <- terra::rast(nrows = 3, ncols = 3, xmin = 0, xmax = 3, ymin = 0, ymax = 3,
                     crs = "EPSG:4326")
  terra::values(dem) <- matrix(c(100, 200, 300, 400, 500, 600, 700, 800, 900),
                               nrow = 3, byrow = TRUE)
  val <- .mtc_resolve_elev(dem, x_coord = 1.5, y_coord = 1.5, crs_wkt = "EPSG:4326")
  expect_equal(val, 500)
})

test_that(".mtc_resolve_elev stops when the cell falls outside the DEM extent", {
  dem <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2,
                     crs = "EPSG:4326")
  terra::values(dem) <- 1:4
  expect_error(.mtc_resolve_elev(dem, x_coord = 100, y_coord = 100, crs_wkt = "EPSG:4326"),
              "NA")
})

test_that(".mtc_resolve_elev stops on an invalid elev type", {
  expect_error(.mtc_resolve_elev(TRUE, 0, 0, "EPSG:4326"), "numeric")
})

test_that(".mtc_resolve_tannul returns a direct numeric value as-is", {
  skip_if_not_installed("ncdf4")
  fix <- .mtc_write_fixture_pair("nc")
  h <- .mtc_open(fix$abv_path)
  expect_equal(.mtc_resolve_tannul(12.3, h, x_idx = 1, y_idx = 1), 12.3)
})

test_that(".mtc_resolve_tannul stops on a non-scalar tannul", {
  skip_if_not_installed("ncdf4")
  fix <- .mtc_write_fixture_pair("nc")
  h <- .mtc_open(fix$abv_path)
  expect_error(.mtc_resolve_tannul(c(1, 2), h, x_idx = 1, y_idx = 1), "single")
})

test_that(".mtc_resolve_tannul computes the full-time-axis mean when NULL, with a short-span warning", {
  skip_if_not_installed("ncdf4")
  fix <- .mtc_write_fixture_pair("nc", ntime_ = 120)  # 5 days, well under 330
  h <- .mtc_open(fix$abv_path)
  expect_warning(val <- .mtc_resolve_tannul(NULL, h, x_idx = 2, y_idx = 1), "annual cycle")
  expect_equal(val, mean(fix$fx$mout$Tz[1, 2, ]))
})
