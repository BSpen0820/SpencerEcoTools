test_that(".endo_absorp_lookup extracts 1 - Gref once per distinct year-month, indexed per cell", {
  refl_dir <- tempfile("refl_")
  gref_dir <- file.path(refl_dir, "Gref")
  dir.create(gref_dir, recursive = TRUE)

  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:4326")
  terra::values(r) <- c(0.1, 0.2, 0.3, 0.4)
  terra::writeRaster(r, file.path(gref_dir, "GF_Refl_TestArea_2022_07.tif"))
  r2 <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:4326")
  terra::values(r2) <- c(0.5, 0.5, 0.5, 0.5)
  terra::writeRaster(r2, file.path(gref_dir, "GF_Refl_TestArea_2022_08.tif"))

  sim_dates <- as.Date(c("2022-07-15", "2022-07-20", "2022-08-01"))
  cell_xy <- terra::xyFromCell(r, c(1, 4))  # two cells: top-left, bottom-right

  lookup <- SpencerEcoTools:::.endo_absorp_lookup(refl_dir, "TestArea", sim_dates, cell_xy)

  expect_setequal(lookup$year_month, c("2022_07", "2022_08"))
  expect_equal(lookup$values[1, "2022_07"], 1 - 0.1, tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(lookup$values[2, "2022_07"], 1 - 0.4, tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(lookup$values[1, "2022_08"], 1 - 0.5, tolerance = 1e-6, ignore_attr = TRUE)
})
