test_that("run_endo_big_nichemap rejects a dates data.frame without required columns", {
  expect_error(
    run_endo_big_nichemap(
      tile_map = "x", valid_cells_mask = "x",
      dates = data.frame(foo = 1), microclim_dir = "x", dem = "x", refl_dir = "x",
      exe_path = "x", output_dir = tempdir(), wineprefix = tempdir()
    ),
    "Start_Dates"
  )
})

test_that("run_endo_big_nichemap rejects a chunk_size above 52", {
  expect_error(
    run_endo_big_nichemap(
      tile_map = "x", valid_cells_mask = "x",
      dates = as.Date(c("2022-07-01", "2022-07-10")), microclim_dir = "x", dem = "x",
      refl_dir = "x", exe_path = "x", output_dir = tempdir(), wineprefix = tempdir(),
      chunk_size = 53
    ),
    "chunk_size"
  )
})

test_that("run_endo_big_nichemap rejects clust_array_arg out of range", {
  expect_error(
    run_endo_big_nichemap(
      tile_map = "x", valid_cells_mask = "x",
      dates = as.Date(c("2022-07-01", "2022-07-10")), microclim_dir = "x", dem = "x",
      refl_dir = "x", exe_path = "x", output_dir = tempdir(), wineprefix = tempdir(),
      clust_array_arg = 5, clust_array_size = 2
    ),
    "clust_array_arg"
  )
})

test_that(".normalize_dates_with_sim_window defaults Sim_Start/Sim_End to the period when absent", {
  d <- as.Date(c("2022-07-01", "2023-06-30"))
  result <- SpencerEcoTools:::.normalize_dates_with_sim_window(d)
  expect_equal(result$Start_Dates, as.Date("2022-07-01"))
  expect_equal(result$End_Dates, as.Date("2023-06-30"))
  expect_equal(result$Sim_Start, as.Date("2022-07-01"))
  expect_equal(result$Sim_End, as.Date("2023-06-30"))
})

test_that(".normalize_dates_with_sim_window keeps caller-supplied Sim_Start/Sim_End", {
  d <- data.frame(
    Start_Dates = as.Date("2022-07-01"), End_Dates = as.Date("2023-06-30"),
    Sim_Start = as.Date("2022-12-09"), Sim_End = as.Date("2023-04-15")
  )
  result <- SpencerEcoTools:::.normalize_dates_with_sim_window(d)
  expect_equal(result$Sim_Start, as.Date("2022-12-09"))
  expect_equal(result$Sim_End, as.Date("2023-04-15"))
})

test_that(".endo_chunk_bounds splits an uneven day count into <= chunk_size chunks covering every day once", {
  bounds <- SpencerEcoTools:::.endo_chunk_bounds(n_days = 128, chunk_size = 52)
  expect_length(bounds, 3)
  expect_equal(unlist(bounds), 1:128)
  expect_true(all(vapply(bounds, length, integer(1)) <= 52))
})
