test_that("run_micro_big_nichemap rejects a negative reqhgt", {
  expect_error(
    run_micro_big_nichemap(
      tiles = NULL, clim = "does/not/exist", dates = NULL,
      dtm_fine = NULL, dtm_coarse = NULL,
      vegp = "does/not/exist", soilc = "does/not/exist",
      output_dir = tempdir(), reqhgt = -1
    ),
    "reqhgt must be a single positive numeric value"
  )
})

test_that("run_micro_big_nichemap rejects NA reqhgt", {
  expect_error(
    run_micro_big_nichemap(
      tiles = NULL, clim = "x", dates = NULL, dtm_fine = NULL, dtm_coarse = NULL,
      vegp = "x", soilc = "x", output_dir = tempdir(), reqhgt = NA
    ),
    "reqhgt must be a single positive numeric value"
  )
})

test_that("run_micro_big_nichemap rejects non-numeric reqhgt", {
  expect_error(
    run_micro_big_nichemap(
      tiles = NULL, clim = "x", dates = NULL, dtm_fine = NULL, dtm_coarse = NULL,
      vegp = "x", soilc = "x", output_dir = tempdir(), reqhgt = "2"
    ),
    "reqhgt must be a single positive numeric value"
  )
})

test_that("run_micro_big_nichemap rejects a length-2 reqhgt", {
  expect_error(
    run_micro_big_nichemap(
      tiles = NULL, clim = "x", dates = NULL, dtm_fine = NULL, dtm_coarse = NULL,
      vegp = "x", soilc = "x", output_dir = tempdir(), reqhgt = c(1, 2)
    ),
    "reqhgt must be a single positive numeric value"
  )
})

.build_grid_test_fixture <- function(clim_res, vegp_res, soilc_res) {
  work_dir <- tempfile("rmbn_test_")
  dir.create(work_dir)

  clim_dir  <- file.path(work_dir, "clim");  dir.create(clim_dir)
  vegp_dir  <- file.path(work_dir, "vegp");  dir.create(vegp_dir)
  soilc_dir <- file.path(work_dir, "soilc"); dir.create(soilc_dir)

  dtm_coarse <- terra::rast(resolution = 0.05, crs = "EPSG:4326",
                             xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5)
  dtm_fine   <- terra::rast(resolution = 0.005, crs = "EPSG:4326",
                             xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5)
  terra::values(dtm_coarse) <- 1
  terra::values(dtm_fine)   <- 1

  period_label <- "20200101_to_20200101"

  mk <- function(res) {
    r <- terra::rast(resolution = res, crs = "EPSG:4326",
                      xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5)
    terra::values(r) <- 1
    r
  }

  readr::write_rds(list(TMP = terra::wrap(mk(clim_res))),
                    file.path(clim_dir, sprintf("Climate_%s.RDS", period_label)))
  readr::write_rds(list(pai = terra::wrap(mk(vegp_res))),
                    file.path(vegp_dir, sprintf("VegPara_%s.RDS", period_label)))
  readr::write_rds(list(Bulk = terra::wrap(mk(soilc_res))),
                    file.path(soilc_dir, sprintf("SoilPara_%s.RDS", period_label)))

  list(work_dir = work_dir, clim_dir = clim_dir, vegp_dir = vegp_dir,
       soilc_dir = soilc_dir, dtm_coarse = dtm_coarse, dtm_fine = dtm_fine)
}

test_that("run_micro_big_nichemap stops when clim resolution does not match dtm_coarse", {
  f <- .build_grid_test_fixture(clim_res = 0.1, vegp_res = 0.005, soilc_res = 0.005)
  on.exit(unlink(f$work_dir, recursive = TRUE), add = TRUE)

  expect_error(
    run_micro_big_nichemap(
      tiles = list(), clim = f$clim_dir,
      dates = as.Date(c("2020-01-01", "2020-01-31")),
      dtm_fine = f$dtm_fine, dtm_coarse = f$dtm_coarse,
      vegp = f$vegp_dir, soilc = f$soilc_dir,
      output_dir = file.path(f$work_dir, "out"), reqhgt = 2
    ),
    "dtm_coarse.*clim.*do not share the same grid"
  )
})

test_that("run_micro_big_nichemap stops when vegp resolution does not match dtm_fine", {
  f <- .build_grid_test_fixture(clim_res = 0.05, vegp_res = 0.02, soilc_res = 0.005)
  on.exit(unlink(f$work_dir, recursive = TRUE), add = TRUE)

  expect_error(
    run_micro_big_nichemap(
      tiles = list(), clim = f$clim_dir,
      dates = as.Date(c("2020-01-01", "2020-01-31")),
      dtm_fine = f$dtm_fine, dtm_coarse = f$dtm_coarse,
      vegp = f$vegp_dir, soilc = f$soilc_dir,
      output_dir = file.path(f$work_dir, "out"), reqhgt = 2
    ),
    "dtm_fine.*vegp.*do not share the same grid"
  )
})

test_that("run_micro_big_nichemap stops when soilc resolution does not match dtm_fine", {
  f <- .build_grid_test_fixture(clim_res = 0.05, vegp_res = 0.005, soilc_res = 0.02)
  on.exit(unlink(f$work_dir, recursive = TRUE), add = TRUE)

  expect_error(
    run_micro_big_nichemap(
      tiles = list(), clim = f$clim_dir,
      dates = as.Date(c("2020-01-01", "2020-01-31")),
      dtm_fine = f$dtm_fine, dtm_coarse = f$dtm_coarse,
      vegp = f$vegp_dir, soilc = f$soilc_dir,
      output_dir = file.path(f$work_dir, "out"), reqhgt = 2
    ),
    "dtm_fine.*soilc.*do not share the same grid"
  )
})
