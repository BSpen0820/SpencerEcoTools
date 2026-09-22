.touch <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines("x", path)
}

test_that(".endo_discover_hourplot_files finds chunk files under the OLD (pre-existing) layout", {
  root <- tempfile("old_layout_")
  .touch(file.path(root, "year_specific", "Tile_002", "Cell_10015", "HOURPLOT_chunk1_20171209_20180119.csv"))
  .touch(file.path(root, "year_specific", "Tile_002", "Cell_10015", "HOURPLOT_chunk1_20211209_20220119.csv"))
  .touch(file.path(root, "year_specific", "Tile_002", "Cell_10016", "HOURPLOT_chunk2_20180120_20180303.csv"))
  on.exit(unlink(root, recursive = TRUE))

  found <- ThermalScapeR:::.endo_discover_hourplot_files(root)

  expect_equal(nrow(found), 3)
  expect_setequal(found$tile_id, 2)
  expect_setequal(found$cell_id, c(10015, 10016))
  row1 <- found[found$chunk_start == as.Date("2017-12-09"), ]
  expect_equal(row1$chunk_end, as.Date("2018-01-19"))
  expect_equal(row1$cell_id, 10015)
})

test_that(".endo_discover_hourplot_files finds chunk files under the NEW (run_endo_big_nichemap) layout", {
  root <- tempfile("new_layout_")
  .touch(file.path(root, "TetonsYearSpecific", "20220701_to_20230601", "Tile_002",
                   "Cell_010015", "HOURPLOT_chunk1_20221209_20230119.csv"))
  on.exit(unlink(root, recursive = TRUE))

  found <- ThermalScapeR:::.endo_discover_hourplot_files(root)

  expect_equal(nrow(found), 1)
  expect_equal(found$tile_id, 2)
  # Cell_010015 (zero-padded) must resolve to the same integer cell_id as
  # the old layout's unpadded Cell_10015
  expect_equal(found$cell_id, 10015)
  expect_equal(found$chunk_start, as.Date("2022-12-09"))
  expect_equal(found$chunk_end, as.Date("2023-01-19"))
})

test_that(".endo_discover_manifests matches OLD layout by filename-embedded period, NEW layout by directory", {
  root <- tempfile("mixed_layout_")
  .touch(file.path(root, "year_specific",
                   "Tile_002_TetonsYearSpecific_20170701_to_20180601_manifest.csv"))
  .touch(file.path(root, "year_specific",
                   "Tile_002_TetonsYearSpecific_20210701_to_20220601_manifest.csv"))
  .touch(file.path(root, "TetonsClimatology", "20240701_to_20250601", "Tile_003_manifest.csv"))
  on.exit(unlink(root, recursive = TRUE))

  found_old <- ThermalScapeR:::.endo_discover_manifests(root, "20170701_to_20180601")
  expect_equal(nrow(found_old), 1)
  expect_equal(found_old$tile_id, 2)

  found_new <- ThermalScapeR:::.endo_discover_manifests(root, "20240701_to_20250601")
  expect_equal(nrow(found_new), 1)
  expect_equal(found_new$tile_id, 3)

  # A period with nothing matching returns a 0-row data.frame, not an error
  found_none <- ThermalScapeR:::.endo_discover_manifests(root, "19990101_to_19990201")
  expect_equal(nrow(found_none), 0)
})

test_that(".endo_check_duplicate_chunks passes on unique (tile_id, cell_id, chunk_start) triples", {
  hourplot_files <- data.frame(
    tile_id = c(2, 2, 3), cell_id = c(7506, 7507, 7506),
    chunk_start = as.Date(c("2022-12-09", "2022-12-09", "2022-12-09")),
    chunk_end = as.Date(c("2023-01-19", "2023-01-19", "2023-01-19")),
    path = c("a", "b", "c"), stringsAsFactors = FALSE
  )
  expect_true(ThermalScapeR:::.endo_check_duplicate_chunks(hourplot_files))
})

test_that(".endo_scoped_search_dir narrows to a direct child period directory", {
  root <- tempfile("scoped_direct_")
  .touch(file.path(root, "20240701_to_20250601", "Tile_002", "marker.csv"))
  on.exit(unlink(root, recursive = TRUE))

  found <- ThermalScapeR:::.endo_scoped_search_dir(root, "20240701_to_20250601")
  expect_equal(normalizePath(found), normalizePath(file.path(root, "20240701_to_20250601")))
})

test_that(".endo_scoped_search_dir narrows to a nested period directory (one level under root_dir)", {
  root <- tempfile("scoped_nested_")
  .touch(file.path(root, "TetonsYearSpecific", "20220701_to_20230601", "Tile_002", "marker.csv"))
  on.exit(unlink(root, recursive = TRUE))

  found <- ThermalScapeR:::.endo_scoped_search_dir(root, "20220701_to_20230601")
  expect_equal(normalizePath(found),
              normalizePath(file.path(root, "TetonsYearSpecific", "20220701_to_20230601")))
})

test_that(".endo_scoped_search_dir falls back to root_dir when no period directory exists (old flat layout)", {
  root <- tempfile("scoped_flat_")
  .touch(file.path(root, "year_specific", "Tile_002", "Cell_10015",
                   "HOURPLOT_chunk1_20171209_20180119.csv"))
  on.exit(unlink(root, recursive = TRUE))

  found <- ThermalScapeR:::.endo_scoped_search_dir(root, "20170701_to_20180601")
  expect_equal(normalizePath(found), normalizePath(root))
})

test_that(".endo_scoped_search_dir falls back to root_dir when the period directory is ambiguous", {
  root <- tempfile("scoped_ambiguous_")
  # Direct child AND a nested match for the same period_label both exist -
  # narrowing to either would silently hide the other scenario tree from
  # .endo_check_duplicate_chunks(), so this must fall through to root_dir.
  .touch(file.path(root, "20220701_to_20230601", "Tile_002", "marker.csv"))
  .touch(file.path(root, "TetonsYearSpecific", "20220701_to_20230601", "Tile_003", "marker.csv"))
  on.exit(unlink(root, recursive = TRUE))

  found <- ThermalScapeR:::.endo_scoped_search_dir(root, "20220701_to_20230601")
  expect_equal(normalizePath(found), normalizePath(root))
})

test_that(".endo_scoped_search_dir falls back to root_dir when multiple nested matches exist", {
  root <- tempfile("scoped_multi_nested_")
  .touch(file.path(root, "ScenarioA", "20220701_to_20230601", "Tile_002", "marker.csv"))
  .touch(file.path(root, "ScenarioB", "20220701_to_20230601", "Tile_003", "marker.csv"))
  on.exit(unlink(root, recursive = TRUE))

  found <- ThermalScapeR:::.endo_scoped_search_dir(root, "20220701_to_20230601")
  expect_equal(normalizePath(found), normalizePath(root))
})

test_that(".endo_check_duplicate_chunks stops when two scenario trees collide on the same tile/cell/chunk", {
  # Reproduces the real collision: two sibling scenario folders (e.g.
  # year_specific/ and climatology/) both reachable under one root_dir,
  # sharing tile 2 / cell 7506 / the same chunk start date.
  hourplot_files <- data.frame(
    tile_id = c(2, 2), cell_id = c(7506, 7506),
    chunk_start = as.Date(c("2022-12-09", "2022-12-09")),
    chunk_end = as.Date(c("2023-01-19", "2023-01-19")),
    path = c("year_specific/Tile_002/Cell_7506/HOURPLOT_chunk1_20221209_20230119.csv",
            "climatology/Tile_002/Cell_7506/HOURPLOT_chunk1_20221209_20230119.csv"),
    stringsAsFactors = FALSE
  )
  expect_error(ThermalScapeR:::.endo_check_duplicate_chunks(hourplot_files), "duplicate|scenario")
})
