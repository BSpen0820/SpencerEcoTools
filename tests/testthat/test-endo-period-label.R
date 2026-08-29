test_that(".endo_period_label matches run_endo_big_nichemap()'s existing convention exactly", {
  expect_equal(
    SpencerEcoTools:::.endo_period_label(as.Date("2022-07-01"), as.Date("2023-06-01")),
    "20220701_to_20230601"
  )
  expect_equal(
    SpencerEcoTools:::.endo_period_label(as.Date("2024-07-01"), as.Date("2025-06-01")),
    "20240701_to_20250601"
  )
})

test_that(".endo_variable_column maps friendly names to the trimmed HOURPLOT's real column names", {
  mr <- SpencerEcoTools:::.endo_variable_column("metabolic_rate")
  expect_equal(mr$column, "MET.W.")
  expect_equal(mr$units, "W")

  wl <- SpencerEcoTools:::.endo_variable_column("water_loss")
  expect_equal(wl$column, "EVP.G.S.")
  expect_equal(wl$units, "g s-1")

  expect_error(SpencerEcoTools:::.endo_variable_column("bogus"), "should be one of")
})
