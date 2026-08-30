test_that("reconstruct_endo_raster() builds a correct raster from real run_endo_big_nichemap() output", {
  skip_on_os(c("linux", "mac"))
  fixtures_dir <- testthat::test_path("fixtures")
  fx <- .rebn_build_fixture(n_days = 60)
  on.exit(unlink(fx$root, recursive = TRUE))

  endo_output_dir <- tempfile("rebn_endo_out_")
  dir.create(endo_output_dir)
  on.exit(unlink(endo_output_dir, recursive = TRUE), add = TRUE)

  run_endo_big_nichemap(
    tile_map = fx$tile_map, valid_cells_mask = fx$valid_cells_mask, dates = fx$dates,
    microclim_dir = fx$microclim_dir, dem = fx$dem, refl_dir = fx$refl_dir,
    exe_path = file.path(fixtures_dir, "Endo2022a.exe"),
    output_dir = endo_output_dir, wineprefix = tempdir(), study_area = "TestArea",
    file_fmt = "nc", chunk_size = 52
  )

  reconstruct_output_dir <- tempfile("rebn_reconstruct_out_")
  dir.create(reconstruct_output_dir)
  on.exit(unlink(reconstruct_output_dir, recursive = TRUE), add = TRUE)

  log_df <- reconstruct_endo_raster(
    root_dir = endo_output_dir, tile_map = fx$tile_map, dates = fx$dates,
    variable = "metabolic_rate", output_dir = reconstruct_output_dir,
    study_area = "TestArea"
  )

  expect_equal(nrow(log_df), 1)
  expect_true(file.exists(log_df$output_path[1]))
  expect_equal(log_df$cells_placed[1], 1)  # the fixture has exactly one valid cell

  # Read back the reconstructed raster and confirm its values match the
  # real per-chunk HOURPLOT CSVs directly, at the fixture's one known cell -
  # not just "the function ran without error".
  period_dir <- file.path(endo_output_dir, "TestArea", "20220701_to_20230630")
  cell_csvs <- list.files(file.path(period_dir, "Tile_001"), pattern = "HOURPLOT_chunk.*\\.csv$",
                         recursive = TRUE, full.names = TRUE)
  expect_true(length(cell_csvs) >= 1)

  manifest <- utils::read.csv(list.files(period_dir, pattern = "manifest\\.csv$", full.names = TRUE))
  cell_xy <- c(manifest$x_idx[1], manifest$y_idx[1])

  r <- terra::rast(log_df$output_path[1], subds = "metabolic_rate")
  reconstructed_vals <- as.numeric(terra::extract(r, rbind(cell_xy)))

  # The expectation is computed independently of .endo_read_hourplot_chunk()
  # (plain read.csv() + a manual HR==24 filter, not a call to the function
  # under test) - reusing that function to build its own expected output
  # cannot catch a bug in that function. run_endo_big_nichemap() always
  # produces the "new" file format (header on line 1, no metadata line,
  # already trimmed to 8 columns), so no skip= is needed here.
  first_chunk <- sort(cell_csvs)[1]
  raw <- utils::read.csv(first_chunk)
  raw <- raw[raw$HR != 24, , drop = FALSE]
  expect_equal(reconstructed_vals[1:24], raw$MET.W.[1:24], tolerance = 1e-6)

  # A 100%-NA reconstruction must never pass silently - this is exactly the
  # failure mode a file-format or row-mapping bug produces.
  expect_false(all(is.na(reconstructed_vals)))
})
