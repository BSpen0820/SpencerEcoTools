test_that("run_endo_big_nichemap runs one tile/one cell end-to-end, producing chunked, trimmed HOURPLOT output", {
  skip_on_os(c("linux", "mac"))
  fixtures_dir <- testthat::test_path("fixtures")
  fx <- .rebn_build_fixture(n_days = 60)  # forces 2 chunks at chunk_size = 52
  on.exit(unlink(fx$root, recursive = TRUE))

  output_dir <- tempfile("rebn_out_")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  log_df <- run_endo_big_nichemap(
    tile_map = fx$tile_map, valid_cells_mask = fx$valid_cells_mask, dates = fx$dates,
    microclim_dir = fx$microclim_dir, dem = fx$dem, refl_dir = fx$refl_dir,
    exe_path = file.path(fixtures_dir, "Endo2022a.exe"),
    output_dir = output_dir, wineprefix = tempdir(), study_area = "TestArea",
    file_fmt = "nc", chunk_size = 52
  )

  expect_equal(nrow(log_df), 2)  # 2 chunks for one cell
  expect_true(all(log_df$status == "success"))

  period_dir <- file.path(output_dir, "TestArea", "20220701_to_20230630")
  expect_true(file.exists(file.path(period_dir, "Tile_001_manifest.csv")))
  expect_true(file.exists(file.path(period_dir, "Tile_001_log.csv")))

  cell_files <- list.files(file.path(period_dir, "Tile_001"), recursive = TRUE, full.names = TRUE)
  expect_length(cell_files, 2)

  # Exact naming patterns from the spec: Cell_{cell_id, 6-digit zero-padded}/
  # HOURPLOT_chunk{k}_{start:%Y%m%d}_{end:%Y%m%d}.csv - a regression to an
  # unpadded Cell_%d directory or a changed HOURPLOT filename shape should
  # fail these, not just the file count above.
  cell_dir_names <- basename(dirname(cell_files))
  expect_true(all(grepl("^Cell_[0-9]{6}$", cell_dir_names)))
  expect_true(all(grepl("^HOURPLOT_chunk[0-9]+_[0-9]{8}_[0-9]{8}\\.csv$", basename(cell_files))))

  hp <- read.csv(cell_files[1])
  expect_gt(nrow(hp), 0)
  expect_equal(ncol(hp), 8)
  expect_equal(names(hp)[1:3], c("HR", "MO", "DEP"))
  expect_true("CO2MOL.S" %in% names(hp) || "CO2MOL/S" %in% names(hp))
})

test_that("run_endo_big_nichemap with snow = TRUE runs the SWE-driven surfwet branch end-to-end", {
  skip_on_os(c("linux", "mac"))
  fixtures_dir <- testthat::test_path("fixtures")
  fx <- .rebn_build_fixture(n_days = 60)
  on.exit(unlink(fx$root, recursive = TRUE))

  output_dir <- tempfile("rebn_out_snow_")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  log_df <- run_endo_big_nichemap(
    tile_map = fx$tile_map, valid_cells_mask = fx$valid_cells_mask, dates = fx$dates,
    microclim_dir = fx$microclim_dir, dem = fx$dem, refl_dir = fx$refl_dir,
    exe_path = file.path(fixtures_dir, "Endo2022a.exe"),
    output_dir = output_dir, wineprefix = tempdir(), study_area = "TestArea",
    file_fmt = "nc", chunk_size = 52, snow = TRUE
  )

  expect_equal(nrow(log_df), 2)
  expect_true(all(log_df$status == "success"))

  period_dir <- file.path(output_dir, "TestArea", "20220701_to_20230630")
  cell_files <- list.files(file.path(period_dir, "Tile_001"), recursive = TRUE, full.names = TRUE)
  expect_length(cell_files, 2)

  hp <- read.csv(cell_files[1])
  expect_gt(nrow(hp), 0)
  expect_equal(ncol(hp), 8)
})
