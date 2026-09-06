# Tests for run_endo_big_nichemap()'s chunk-level retry behavior.
#
# These mock micro_to_csv() (to avoid depending on solaR's real solar-geometry
# calculations, irrelevant to retry bookkeeping) and run_endotherm_model()/
# init_wine_prefix() (to avoid needing Wine or the real exe) so they can run
# on any platform - unlike the real end-to-end tests in
# test-run_endo_big_nichemap.R, which stay Windows-only (skip_on_os) because
# they exercise the actual exe.

.rebn_fake_micro_to_csv <- function(...) {
  list(metout = data.frame(x = 1), shadmet = data.frame(x = 1),
       soil = data.frame(x = 1), shadsoil = data.frame(x = 1))
}

.rebn_write_fake_hourplot <- function(ws) {
  lines <- c(
    "metadata line",
    paste(c("HR", "MO", "DEP", "TC", "TSKIN", "TLUNG", "O2MOL.S", "CO2MOL.S"), collapse = ","),
    "1,1,1,10,10,10,0.1,0.1"
  )
  writeLines(lines, file.path(ws, "HOURPLOT.csv"))
}

test_that("a chunk that fails once and succeeds on retry ends up 'success' with n_attempts = 2, no debug dump", {
  fx <- .rebn_build_fixture(n_days = 60)  # forces 2 chunks for the one valid cell
  on.exit(unlink(fx$root, recursive = TRUE))
  output_dir <- tempfile("rebn_retry_ok_")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  exe_path <- tempfile("fake_exe_"); file.create(exe_path)

  call_n <- 0L
  testthat::local_mocked_bindings(
    micro_to_csv = .rebn_fake_micro_to_csv,
    init_wine_prefix = function(...) invisible(NULL),
    run_endotherm_model = function(workspace_dir, ...) {
      call_n <<- call_n + 1L
      if (call_n == 1L) return(list(success = FALSE, message = "simulated transient failure"))
      .rebn_write_fake_hourplot(workspace_dir)
      list(success = TRUE, message = "exit status 0 | Calculations completed.")
    }
  )

  log_df <- run_endo_big_nichemap(
    tile_map = fx$tile_map, valid_cells_mask = fx$valid_cells_mask, dates = fx$dates,
    microclim_dir = fx$microclim_dir, dem = fx$dem, refl_dir = fx$refl_dir,
    exe_path = exe_path, output_dir = output_dir, wineprefix = tempdir(),
    study_area = "TestArea", file_fmt = "nc", chunk_size = 52
  )

  expect_equal(nrow(log_df), 2)
  expect_true(all(log_df$status == "success"))
  expect_equal(sort(log_df$n_attempts), c(1, 2))
  expect_equal(call_n, 3L)  # chunk1: attempt1 (fail) + attempt2 (ok); chunk2: attempt1 (ok)
  expect_false(dir.exists(file.path(output_dir, "Debug_CSVs")))
  expect_true(all(file.exists(log_df$output_path)))
})

test_that("a chunk that fails every attempt is logged as error with n_attempts = max_attempts and exactly one debug dump", {
  fx <- .rebn_build_fixture(n_days = 60)
  on.exit(unlink(fx$root, recursive = TRUE))
  output_dir <- tempfile("rebn_retry_fail_")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  exe_path <- tempfile("fake_exe_"); file.create(exe_path)

  testthat::local_mocked_bindings(
    micro_to_csv = .rebn_fake_micro_to_csv,
    init_wine_prefix = function(...) invisible(NULL),
    run_endotherm_model = function(workspace_dir, ...) {
      list(success = FALSE, message = "simulated persistent failure")
    }
  )

  log_df <- run_endo_big_nichemap(
    tile_map = fx$tile_map, valid_cells_mask = fx$valid_cells_mask, dates = fx$dates,
    microclim_dir = fx$microclim_dir, dem = fx$dem, refl_dir = fx$refl_dir,
    exe_path = exe_path, output_dir = output_dir, wineprefix = tempdir(),
    study_area = "TestArea", file_fmt = "nc", chunk_size = 52
  )

  expect_equal(nrow(log_df), 2)
  expect_true(all(log_df$status == "error"))
  expect_true(all(log_df$n_attempts == 2))

  debug_dirs <- list.dirs(file.path(output_dir, "Debug_CSVs"), recursive = FALSE)
  expect_length(debug_dirs, 2)  # one per chunk - only the final failed attempt each
})

test_that("max_attempts = 1 preserves the original single-attempt behavior (immediate debug dump, no retry)", {
  fx <- .rebn_build_fixture(n_days = 60)
  on.exit(unlink(fx$root, recursive = TRUE))
  output_dir <- tempfile("rebn_retry_off_")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  exe_path <- tempfile("fake_exe_"); file.create(exe_path)

  call_n <- 0L
  testthat::local_mocked_bindings(
    micro_to_csv = .rebn_fake_micro_to_csv,
    init_wine_prefix = function(...) invisible(NULL),
    run_endotherm_model = function(workspace_dir, ...) {
      call_n <<- call_n + 1L
      list(success = FALSE, message = "simulated persistent failure")
    }
  )

  log_df <- run_endo_big_nichemap(
    tile_map = fx$tile_map, valid_cells_mask = fx$valid_cells_mask, dates = fx$dates,
    microclim_dir = fx$microclim_dir, dem = fx$dem, refl_dir = fx$refl_dir,
    exe_path = exe_path, output_dir = output_dir, wineprefix = tempdir(),
    study_area = "TestArea", file_fmt = "nc", chunk_size = 52, max_attempts = 1
  )

  expect_equal(nrow(log_df), 2)
  expect_true(all(log_df$status == "error"))
  expect_true(all(log_df$n_attempts == 1))
  expect_equal(call_n, 2L)  # one call per chunk, no retries

  debug_dirs <- list.dirs(file.path(output_dir, "Debug_CSVs"), recursive = FALSE)
  expect_length(debug_dirs, 2)
})

test_that("run_endo_big_nichemap rejects a non-positive-integer max_attempts", {
  expect_error(
    run_endo_big_nichemap(
      tile_map = "x", valid_cells_mask = "x",
      dates = as.Date(c("2022-07-01", "2022-07-10")), microclim_dir = "x", dem = "x",
      refl_dir = "x", exe_path = "x", output_dir = tempdir(), wineprefix = tempdir(),
      max_attempts = 0
    ),
    "max_attempts"
  )
  expect_error(
    run_endo_big_nichemap(
      tile_map = "x", valid_cells_mask = "x",
      dates = as.Date(c("2022-07-01", "2022-07-10")), microclim_dir = "x", dem = "x",
      refl_dir = "x", exe_path = "x", output_dir = tempdir(), wineprefix = tempdir(),
      max_attempts = 1.5
    ),
    "max_attempts"
  )
})

test_that("retries still work correctly when parallel = TRUE (first pass parallel, retry pass serial)", {
  # NOTE: testthat::local_mocked_bindings() only rebinds run_endotherm_model()
  # in *this* process. future::multisession workers are separate R processes
  # that load their own copy of the package, so a chunk processed by a worker
  # actually calls the real run_endotherm_model() (which fails here since
  # there's no real wine/exe) - it only succeeds once the serial retry pass
  # (back in this process, where the mock applies) picks it up. That's not a
  # test bug to paper over: it's the exact cross-worker-environment failure
  # mode this retry design exists to recover from, so the useful assertion is
  # "every chunk still ends in success," not "every chunk succeeded in one try."
  fx <- .rebn_build_fixture(n_days = 60)
  on.exit(unlink(fx$root, recursive = TRUE))
  output_dir <- tempfile("rebn_retry_parallel_")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  exe_path <- tempfile("fake_exe_"); file.create(exe_path)

  testthat::local_mocked_bindings(
    micro_to_csv = .rebn_fake_micro_to_csv,
    init_wine_prefix = function(...) invisible(NULL),
    run_endotherm_model = function(workspace_dir, ...) {
      .rebn_write_fake_hourplot(workspace_dir)
      list(success = TRUE, message = "exit status 0 | Calculations completed.")
    }
  )

  log_df <- run_endo_big_nichemap(
    tile_map = fx$tile_map, valid_cells_mask = fx$valid_cells_mask, dates = fx$dates,
    microclim_dir = fx$microclim_dir, dem = fx$dem, refl_dir = fx$refl_dir,
    exe_path = exe_path, output_dir = output_dir, wineprefix = tempdir(),
    study_area = "TestArea", file_fmt = "nc", chunk_size = 52,
    parallel = TRUE, ncores = 2
  )

  expect_equal(nrow(log_df), 2)
  expect_true(all(log_df$status == "success"))
  expect_true(all(log_df$n_attempts >= 1 & log_df$n_attempts <= 2))
})
