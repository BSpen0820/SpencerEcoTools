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
