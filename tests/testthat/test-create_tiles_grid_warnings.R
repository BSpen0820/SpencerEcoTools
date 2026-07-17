test_that("create_tiles warns when coarse_dem and fine_dem have different CRS", {
  coarse <- terra::rast(nrows = 4, ncols = 4, crs = "EPSG:4326",
                         xmin = 0, xmax = 4, ymin = 0, ymax = 4)
  fine   <- terra::rast(nrows = 40, ncols = 40, crs = "EPSG:3857",
                         xmin = 0, xmax = 4, ymin = 0, ymax = 4)
  terra::values(coarse) <- 1
  terra::values(fine) <- 1

  expect_warning(
    create_tiles(coarse, fine, dates = as.Date(c("2020-01-01", "2020-01-02"))),
    "different CRS"
  )
})

test_that("create_tiles warns when fine/coarse resolution ratio is not a whole number", {
  coarse <- terra::rast(nrows = 4, ncols = 4, crs = "EPSG:4326",
                         xmin = 0, xmax = 4, ymin = 0, ymax = 4)
  fine   <- terra::rast(nrows = 15, ncols = 15, crs = "EPSG:4326",
                         xmin = 0, xmax = 4, ymin = 0, ymax = 4)
  terra::values(coarse) <- 1
  terra::values(fine) <- 1

  expect_warning(
    create_tiles(coarse, fine, dates = as.Date(c("2020-01-01", "2020-01-02"))),
    "not a whole-number multiple"
  )
})

test_that("create_tiles does not warn when CRS matches and ratio is a whole number", {
  coarse <- terra::rast(nrows = 4, ncols = 4, crs = "EPSG:4326",
                         xmin = 0, xmax = 4, ymin = 0, ymax = 4)
  fine   <- terra::rast(nrows = 40, ncols = 40, crs = "EPSG:4326",
                         xmin = 0, xmax = 4, ymin = 0, ymax = 4)
  terra::values(coarse) <- 1
  terra::values(fine) <- 1

  expect_warning(
    create_tiles(coarse, fine, dates = as.Date(c("2020-01-01", "2020-01-02"))),
    regexp = NA
  )
})
