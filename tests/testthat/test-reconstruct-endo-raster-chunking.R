# Regression coverage for the balanced-chunking / spatial-batch rewrite of
# reconstruct_endo_raster()'s NetCDF writer (see HANDOFF_endo-raster-chunking.md
# for the background and how to run this file).

.write_hourplot_for_chunking_test <- function(path, n_days, value_start) {
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

test_that(".endo_balanced_chunk_shape() never produces the old pixel-per-chunk shape and always stays in-bounds", {
  # Production-scale, leap-year worst case (8784 = 366 * 24)
  s1 <- SpencerEcoTools:::.endo_balanced_chunk_shape(2166, 1308, 8784)
  expect_true(s1$x >= 1 && s1$x <= 2166)
  expect_true(s1$y >= 1 && s1$y <= 1308)
  expect_true(s1$t >= 1 && s1$t <= 8784)
  # The whole point of the rewrite: more than one pixel per chunk, and not
  # the full time axis in a single chunk either.
  expect_false(s1$x == 1 && s1$y == 1)
  expect_true(s1$t < 8784)

  # Non-leap year, same grid - should run cleanly and stay in-bounds too
  s2 <- SpencerEcoTools:::.endo_balanced_chunk_shape(2166, 1308, 8760)
  expect_true(s2$t >= 1 && s2$t <= 8760)

  # Tiny grid: the formula can suggest chunk dims bigger than the axis
  # itself - must clamp, never error, never exceed the axis length.
  s3 <- SpencerEcoTools:::.endo_balanced_chunk_shape(3, 3, 4)
  expect_true(s3$x >= 1 && s3$x <= 3)
  expect_true(s3$y >= 1 && s3$y <= 3)
  expect_true(s3$t >= 1 && s3$t <= 4)
})

test_that(".endo_batch_dims()/.endo_spatial_batches() cover every cell exactly once and stay chunk-aligned", {
  nx <- 50; ny <- 40; nt <- 1000
  chunk_shape <- SpencerEcoTools:::.endo_balanced_chunk_shape(nx, ny, nt)
  # Tiny budget forces multiple batches even for this small grid
  batch_dims <- SpencerEcoTools:::.endo_batch_dims(nx, ny, nt, chunk_shape, target_batch_mb = 1)
  batches <- SpencerEcoTools:::.endo_spatial_batches(nx, ny, batch_dims)

  expect_true(length(batches) > 1)  # otherwise this test isn't exercising batching at all
  expect_equal(batch_dims$x %% chunk_shape$x, 0)
  expect_equal(batch_dims$y %% chunk_shape$y, 0)

  coverage <- matrix(0L, nrow = nx, ncol = ny)
  for (b in batches) {
    coverage[b$x0:b$x1, b$y0:b$y1] <- coverage[b$x0:b$x1, b$y0:b$y1] + 1L
  }
  expect_true(all(coverage == 1L))  # no gaps, no overlaps
})

test_that("reconstruct_endo_raster() produces identical values whether forced into many tiny batches or one big batch", {
  root <- tempfile("chunking_root_")
  dir.create(file.path(root, "year_specific", "Tile_001", "Cell_100"), recursive = TRUE)
  dir.create(file.path(root, "year_specific", "Tile_001", "Cell_101"), recursive = TRUE)
  dir.create(file.path(root, "year_specific", "Tile_001", "Cell_102"), recursive = TRUE)
  dir.create(file.path(root, "year_specific", "Tile_001", "Cell_103"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE))

  # 2 real days -> 48 real hours after dropping HR == 24
  .write_hourplot_for_chunking_test(
    file.path(root, "year_specific", "Tile_001", "Cell_100", "HOURPLOT_chunk1_20220101_20220102.csv"),
    n_days = 2, value_start = 0
  )
  .write_hourplot_for_chunking_test(
    file.path(root, "year_specific", "Tile_001", "Cell_101", "HOURPLOT_chunk1_20220101_20220102.csv"),
    n_days = 2, value_start = 1000
  )
  .write_hourplot_for_chunking_test(
    file.path(root, "year_specific", "Tile_001", "Cell_102", "HOURPLOT_chunk1_20220101_20220102.csv"),
    n_days = 2, value_start = 2000
  )
  .write_hourplot_for_chunking_test(
    file.path(root, "year_specific", "Tile_001", "Cell_103", "HOURPLOT_chunk1_20220101_20220102.csv"),
    n_days = 2, value_start = 3000
  )

  # 4x4 raster; cells placed at all four corners so a tiny batch budget
  # forces them into separate spatial batches. The remaining 12 cells have
  # no manifest entry at all and must come back NA.
  tile_map_r <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4, crs = "EPSG:32612")
  xy_tl <- terra::xyFromCell(tile_map_r, terra::cellFromRowCol(tile_map_r, 1, 1))  # top-left
  xy_tr <- terra::xyFromCell(tile_map_r, terra::cellFromRowCol(tile_map_r, 1, 4))  # top-right
  xy_bl <- terra::xyFromCell(tile_map_r, terra::cellFromRowCol(tile_map_r, 4, 1))  # bottom-left
  xy_br <- terra::xyFromCell(tile_map_r, terra::cellFromRowCol(tile_map_r, 4, 4))  # bottom-right
  xy_untouched <- terra::xyFromCell(tile_map_r, terra::cellFromRowCol(tile_map_r, 2, 2))

  manifest <- data.frame(
    x_idx = c(xy_tl[1, 1], xy_tr[1, 1], xy_bl[1, 1], xy_br[1, 1]),
    y_idx = c(xy_tl[1, 2], xy_tr[1, 2], xy_bl[1, 2], xy_br[1, 2]),
    cell_id = c(100, 101, 102, 103)
  )
  utils::write.csv(manifest, file.path(root, "year_specific", "Tile_001_TestArea_20220101_to_20220102_manifest.csv"),
                   row.names = FALSE)

  dates_df <- data.frame(Start_Dates = as.Date("2022-01-01"), End_Dates = as.Date("2022-01-02"))

  out_many <- tempfile("chunking_out_many_"); dir.create(out_many)
  out_one  <- tempfile("chunking_out_one_");  dir.create(out_one)
  on.exit({ unlink(out_many, recursive = TRUE); unlink(out_one, recursive = TRUE) }, add = TRUE)

  # Tiny targets -> many small batches (one cell per batch, in the extreme)
  log_many <- reconstruct_endo_raster(
    root_dir = root, tile_map = tile_map_r, dates = dates_df,
    variable = "metabolic_rate", output_dir = out_many, study_area = "TestArea",
    target_chunk_mb = 0.0001, target_batch_mb = 0.0001
  )
  # Defaults -> the whole 4x4 grid fits in a single batch
  log_one <- reconstruct_endo_raster(
    root_dir = root, tile_map = tile_map_r, dates = dates_df,
    variable = "metabolic_rate", output_dir = out_one, study_area = "TestArea"
  )

  expect_equal(log_many$cells_placed, 4)
  expect_equal(log_one$cells_placed, 4)

  r_many <- terra::rast(log_many$output_path[1], subds = "metabolic_rate")
  r_one  <- terra::rast(log_one$output_path[1],  subds = "metabolic_rate")

  for (xy in list(xy_tl, xy_tr, xy_bl, xy_br)) {
    v_many <- as.numeric(terra::extract(r_many, xy))
    v_one  <- as.numeric(terra::extract(r_one, xy))
    expect_equal(v_many, v_one)  # batching must not change the values placed
  }

  # Spot-check one corner's values against the raw CSV directly, independent
  # of .endo_read_hourplot_chunk()/.endo_assemble_cell_series().
  raw_tl <- utils::read.csv(
    file.path(root, "year_specific", "Tile_001", "Cell_100", "HOURPLOT_chunk1_20220101_20220102.csv"),
    skip = 1
  )
  raw_tl <- raw_tl[raw_tl$HR != 24, , drop = FALSE]
  expect_equal(as.numeric(terra::extract(r_many, xy_tl)), raw_tl$MET.W., tolerance = 1e-6)

  # Untouched cells (no manifest entry at all) must be NA under both batch
  # configurations - not the -9999 fill value leaking through.
  expect_true(all(is.na(as.numeric(terra::extract(r_many, xy_untouched)))))
  expect_true(all(is.na(as.numeric(terra::extract(r_one, xy_untouched)))))

  # Confirm the fix actually changed on-disk chunking: not the old
  # one-pixel-per-chunk shape.
  nc <- ncdf4::nc_open(log_one$output_path[1])
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  chunks <- nc$var[["metabolic_rate"]]$chunksizes
  expect_false(chunks[1] == 1 && chunks[2] == 1)
})

test_that("both a per-pixel time series read and a sum-across-layers read stay fast on a moderate synthetic grid", {
  skip_on_cran()
  skip_if_not_installed("ncdf4")

  # 30x30 grid, ~3 weeks hourly (528 hours) with every cell populated - big
  # enough to be a meaningful timing signal, small enough to build quickly.
  # The old c(1, 1, ntime) chunking's own benchmark (in this file's git
  # history) was ~415s for a 120x120/3400-hour/300-cell case doing the
  # cross-pixel read this test does at smaller scale - these thresholds are
  # deliberately generous (not a tight perf assertion) so this stays a
  # regression guard against re-introducing that pathological shape, not a
  # flaky benchmark. Tune TIME_LIMIT_SEC down once you've seen real timings
  # on your machine, if you want a tighter guard.
  TIME_LIMIT_SEC <- 15

  n <- 30
  n_days <- 22  # 2022-01-01..2022-01-22 inclusive = 22 days = 528 real hours
  root <- tempfile("chunking_perf_root_")
  dir.create(file.path(root, "year_specific", "Tile_001"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE))

  tile_map_r <- terra::rast(nrows = n, ncols = n, xmin = 0, xmax = n, ymin = 0, ymax = n, crs = "EPSG:32612")
  manifest_rows <- list()
  for (cell_id in seq_len(n * n)) {
    row <- ((cell_id - 1) %/% n) + 1
    col <- ((cell_id - 1) %% n) + 1
    cell_dir <- file.path(root, "year_specific", "Tile_001", sprintf("Cell_%d", cell_id))
    dir.create(cell_dir)
    .write_hourplot_for_chunking_test(
      file.path(cell_dir, "HOURPLOT_chunk1_20220101_20220122.csv"),
      n_days = n_days, value_start = cell_id
    )
    xy <- terra::xyFromCell(tile_map_r, terra::cellFromRowCol(tile_map_r, row, col))
    manifest_rows[[cell_id]] <- data.frame(x_idx = xy[1, 1], y_idx = xy[1, 2], cell_id = cell_id)
  }
  manifest <- do.call(rbind, manifest_rows)
  utils::write.csv(manifest, file.path(root, "year_specific", "Tile_001_TestArea_20220101_to_20220122_manifest.csv"),
                   row.names = FALSE)

  out_dir <- tempfile("chunking_perf_out_"); dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  dates_df <- data.frame(Start_Dates = as.Date("2022-01-01"), End_Dates = as.Date("2022-01-22"))
  log_df <- reconstruct_endo_raster(
    root_dir = root, tile_map = tile_map_r, dates = dates_df,
    variable = "metabolic_rate", output_dir = out_dir, study_area = "TestArea"
  )
  expect_equal(log_df$cells_placed, n * n)

  nc <- ncdf4::nc_open(log_df$output_path[1])
  nt <- nc$dim$time$len

  t_pixel <- system.time(
    ncdf4::ncvar_get(nc, "metabolic_rate", start = c(1, 1, 1), count = c(1, 1, nt))
  )[["elapsed"]]

  t_slice <- system.time(
    ncdf4::ncvar_get(nc, "metabolic_rate", start = c(1, 1, 1), count = c(n, n, nt))
  )[["elapsed"]]
  ncdf4::nc_close(nc)

  expect_lt(t_pixel, TIME_LIMIT_SEC)
  expect_lt(t_slice, TIME_LIMIT_SEC)
})
