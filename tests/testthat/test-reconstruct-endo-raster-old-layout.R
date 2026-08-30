.write_old_hourplot_for_e2e <- function(path, n_days, value_start) {
  rows <- list()
  for (day in seq_len(n_days)) {
    for (hr in 0:24) {
      val <- if (hr == 24) -999 else value_start + (day - 1) * 24 + hr
      rows[[length(rows) + 1]] <- data.frame(HR = hr, MO = day, DEP = 0, MASS.KG. = 56.6,
                                             MET.W. = val, EVP.G.S. = val / 10,
                                             LUNG.L.S. = 0.1, CO2MOL.S = 0.0001)
    }
  }
  hp <- do.call(rbind, rows)
  writeLines(" Animal species = Test", path)
  suppressWarnings(utils::write.table(hp, path, sep = ",", row.names = FALSE, append = TRUE))
}

test_that("reconstruct_endo_raster() handles the OLD hand-run layout across multiple cells and periods (no exe, cross-platform)", {
  root <- tempfile("old_layout_root_")
  dir.create(file.path(root, "year_specific", "Tile_001", "Cell_100"), recursive = TRUE)
  dir.create(file.path(root, "year_specific", "Tile_001", "Cell_101"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE))

  # Two periods, each 2 real days (48 real hours after dropping HR==24)
  .write_old_hourplot_for_e2e(
    file.path(root, "year_specific", "Tile_001", "Cell_100", "HOURPLOT_chunk1_20220101_20220102.csv"),
    n_days = 2, value_start = 0
  )
  .write_old_hourplot_for_e2e(
    file.path(root, "year_specific", "Tile_001", "Cell_101", "HOURPLOT_chunk1_20220101_20220102.csv"),
    n_days = 2, value_start = 1000
  )
  .write_old_hourplot_for_e2e(
    file.path(root, "year_specific", "Tile_001", "Cell_100", "HOURPLOT_chunk1_20220201_20220202.csv"),
    n_days = 2, value_start = 500
  )
  .write_old_hourplot_for_e2e(
    file.path(root, "year_specific", "Tile_001", "Cell_101", "HOURPLOT_chunk1_20220201_20220202.csv"),
    n_days = 2, value_start = 1500
  )

  # 2x2 projected raster: cell_id 100 -> row1/col1 (top-left), cell_id 101 -> row2/col2 (bottom-right)
  tile_map_r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:32612")
  xy_100 <- terra::xyFromCell(tile_map_r, terra::cellFromRowCol(tile_map_r, 1, 1))
  xy_101 <- terra::xyFromCell(tile_map_r, terra::cellFromRowCol(tile_map_r, 2, 2))

  manifest_1 <- data.frame(x_idx = c(xy_100[1, 1], xy_101[1, 1]),
                           y_idx = c(xy_100[1, 2], xy_101[1, 2]),
                           cell_id = c(100, 101))
  utils::write.csv(manifest_1, file.path(root, "year_specific", "Tile_001_TestArea_20220101_to_20220102_manifest.csv"),
                   row.names = FALSE)
  utils::write.csv(manifest_1, file.path(root, "year_specific", "Tile_001_TestArea_20220201_to_20220202_manifest.csv"),
                   row.names = FALSE)

  out_dir <- tempfile("old_layout_out_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  dates_df <- data.frame(
    Start_Dates = as.Date(c("2022-01-01", "2022-02-01")),
    End_Dates   = as.Date(c("2022-01-02", "2022-02-02"))
  )

  log_df <- reconstruct_endo_raster(
    root_dir = root, tile_map = tile_map_r, dates = dates_df,
    variable = "metabolic_rate", output_dir = out_dir, study_area = "TestArea"
  )

  expect_equal(nrow(log_df), 2)
  expect_true(all(file.exists(log_df$output_path)))
  expect_equal(log_df$cells_placed, c(2, 2))
  expect_equal(log_df$cells_attempted, c(2, 2))

  # Period 1: confirm both cells' values match the raw CSVs directly, computed
  # independently of .endo_read_hourplot_chunk() (plain read.csv(skip=1) +
  # manual HR!=24 filter).
  r1 <- terra::rast(log_df$output_path[1], subds = "metabolic_rate")
  vals_100_p1 <- as.numeric(terra::extract(r1, xy_100))
  vals_101_p1 <- as.numeric(terra::extract(r1, xy_101))

  raw_100_p1 <- utils::read.csv(
    file.path(root, "year_specific", "Tile_001", "Cell_100", "HOURPLOT_chunk1_20220101_20220102.csv"),
    skip = 1
  )
  raw_100_p1 <- raw_100_p1[raw_100_p1$HR != 24, , drop = FALSE]
  expect_equal(vals_100_p1, raw_100_p1$MET.W., tolerance = 1e-6)

  raw_101_p1 <- utils::read.csv(
    file.path(root, "year_specific", "Tile_001", "Cell_101", "HOURPLOT_chunk1_20220101_20220102.csv"),
    skip = 1
  )
  raw_101_p1 <- raw_101_p1[raw_101_p1$HR != 24, , drop = FALSE]
  expect_equal(vals_101_p1, raw_101_p1$MET.W., tolerance = 1e-6)

  # Period 2, cell 100 - confirms multi-period doesn't cross-contaminate values
  r2 <- terra::rast(log_df$output_path[2], subds = "metabolic_rate")
  vals_100_p2 <- as.numeric(terra::extract(r2, xy_100))
  raw_100_p2 <- utils::read.csv(
    file.path(root, "year_specific", "Tile_001", "Cell_100", "HOURPLOT_chunk1_20220201_20220202.csv"),
    skip = 1
  )
  raw_100_p2 <- raw_100_p2[raw_100_p2$HR != 24, , drop = FALSE]
  expect_equal(vals_100_p2, raw_100_p2$MET.W., tolerance = 1e-6)
  expect_false(isTRUE(all.equal(vals_100_p1, vals_100_p2)))  # different periods must not collide

  expect_false(all(is.na(vals_100_p1)))
  expect_false(all(is.na(vals_101_p1)))
})
