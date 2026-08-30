test_that(".endo_create_raster_nc + .endo_write_cell_to_nc place cell values at the correct spatial location", {
  skip_if_not_installed("ncdf4")
  out_path <- tempfile(fileext = ".nc")
  on.exit(unlink(out_path))

  # 3x3 projected raster, distinct cell values so a transposition is
  # unambiguously detectable
  tile_map_r <- terra::rast(nrows = 3, ncols = 3, xmin = 0, xmax = 3, ymin = 0, ymax = 3,
                            crs = "EPSG:32612")
  time_axis <- seq(as.POSIXct("2022-12-09 00:00:00", tz = "UTC"), by = "hour", length.out = 4)
  variable_meta <- SpencerEcoTools:::.endo_variable_column("metabolic_rate")

  nc <- SpencerEcoTools:::.endo_create_raster_nc(out_path, tile_map_r, time_axis,
                                                 "metabolic_rate", variable_meta, compression = 4L)

  # Place a distinct, easily-identified series at row 1 (top row), col 3
  # (rightmost column) - if row/col are ever swapped, this ends up at the
  # bottom-left instead, and the assertions below will fail.
  SpencerEcoTools:::.endo_write_cell_to_nc(nc, "metabolic_rate", row = 1, col = 3,
                                           values = c(10, 20, 30, 40))
  # And a second, different cell at row 3 (bottom row), col 1 (leftmost)
  SpencerEcoTools:::.endo_write_cell_to_nc(nc, "metabolic_rate", row = 3, col = 1,
                                           values = c(-10, -20, -30, -40))
  ncdf4::nc_close(nc)

  r <- terra::rast(out_path, subds = "metabolic_rate")
  expect_equal(terra::nlyr(r), 4)

  xy_topright  <- terra::xyFromCell(tile_map_r, terra::cellFromRowCol(tile_map_r, 1, 3))
  xy_botleft   <- terra::xyFromCell(tile_map_r, terra::cellFromRowCol(tile_map_r, 3, 1))

  vals_topright <- as.numeric(terra::extract(r, xy_topright))
  vals_botleft  <- as.numeric(terra::extract(r, xy_botleft))

  expect_equal(vals_topright, c(10, 20, 30, 40))
  expect_equal(vals_botleft, c(-10, -20, -30, -40))

  # Every other cell was never written - must be NA, not the fill value leaking through
  xy_untouched <- terra::xyFromCell(tile_map_r, terra::cellFromRowCol(tile_map_r, 2, 2))
  expect_true(all(is.na(as.numeric(terra::extract(r, xy_untouched)))))
})
