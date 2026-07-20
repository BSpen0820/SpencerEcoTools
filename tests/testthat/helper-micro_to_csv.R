# Shared fixtures for test-micro_to_csv.R. testthat auto-sources helper-*.R
# files before running tests in this package.

.mtc_test_fixture_data <- function(nrow_ = 3, ncol_ = 2, ntime_ = 120,
                                   start = as.POSIXct("2020-07-01 00:00:00", tz = "UTC"),
                                   seed = 42) {
  set.seed(seed)
  mk <- function() array(round(stats::runif(nrow_ * ncol_ * ntime_, 0, 30), 4),
                        dim = c(nrow_, ncol_, ntime_))
  tme <- seq(start, by = "hour", length.out = ntime_)
  list(
    mout = list(Tz = mk(), tleaf = mk(), relhum = mk(), soilm = mk(), windspeed = mk(),
               Rdirdown = mk(), Rdifdown = mk(), Rlwdown = mk(), Rswup = mk(), Rlwup = mk(),
               tme = tme),
    tme = tme,
    dtm = terra::rast(nrows = nrow_, ncols = ncol_,
                      xmin = 500000, xmax = 500000 + ncol_ * 30,
                      ymin = 4800000, ymax = 4800000 + nrow_ * 30,
                      crs = "EPSG:32612")
  )
}

.mtc_test_fixture_blw_data <- function(fx, depths_mm = c(0, 15, 50), seed = 43) {
  set.seed(seed)
  nrow_ <- terra::nrow(fx$dtm); ncol_ <- terra::ncol(fx$dtm); ntime_ <- length(fx$tme)
  arrs <- lapply(depths_mm, function(d) {
    array(round(stats::runif(nrow_ * ncol_ * ntime_, 0, 20), 4), dim = c(nrow_, ncol_, ntime_))
  })
  names(arrs) <- sprintf("BlwGrd_%04d", depths_mm)
  arrs
}

.mtc_write_fixture_pair <- function(file_fmt = c("nc", "h5"),
                                    nrow_ = 3, ncol_ = 2, ntime_ = 120,
                                    start = as.POSIXct("2020-07-01 00:00:00", tz = "UTC"),
                                    depths_mm = c(0, 15, 50)) {
  file_fmt <- match.arg(file_fmt)
  fx <- .mtc_test_fixture_data(nrow_, ncol_, ntime_, start)
  abv_path <- tempfile(fileext = paste0(".", file_fmt))
  blw_path <- tempfile(fileext = paste0(".", file_fmt))

  write_tile(fx$mout, abv_path, dtm = fx$dtm, tme = fx$tme, file_fmt = file_fmt)

  blw_arrs <- .mtc_test_fixture_blw_data(fx, depths_mm)
  for (dl in names(blw_arrs)) {
    write_tile(list(Tz = blw_arrs[[dl]], tme = fx$tme), blw_path,
              dtm = fx$dtm, tme = fx$tme, file_fmt = file_fmt, depth_label = dl)
  }

  list(abv_path = abv_path, blw_path = blw_path, fx = fx, blw_arrs = blw_arrs)
}
