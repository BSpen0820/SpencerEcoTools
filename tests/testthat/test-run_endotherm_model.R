test_that("select native invocation on Windows via sysname works as expected", {
  # run_endotherm_model's native/Wine branch selection is exercised indirectly
  # by the integration test below (which runs on Windows); this test just
  # locks in the failure path, which is OS-independent.
  workspace <- tempfile("endo_ws_missing_")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE))

  result <- run_endotherm_model(workspace, sysname = "Windows")

  expect_false(result$success)
  expect_match(result$message, "exe not found")
})

test_that("write_endotherm_inputs + write_juldays_dat + run_endotherm_model work end-to-end against the real exe", {
  skip_on_os(c("linux", "mac")) # this integration test only runs natively where the exe is a native binary
  fixtures_dir <- testthat::test_path("fixtures")
  workspace <- tempfile("endo_ws_")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE))

  file.copy(
    file.path(fixtures_dir, c("Endo2022a.exe", "metout.csv", "shadmet.csv", "soil.csv", "shadsoil.csv")),
    workspace
  )

  write_endotherm_inputs(output_dir = workspace)
  write_juldays_dat(output_dir = workspace)

  result <- run_endotherm_model(workspace, exe_name = "Endo2022a.exe", sysname = "Windows")

  expect_true(result$success)
  expect_match(result$message, "Calculations completed")
  expect_true(file.exists(file.path(workspace, "HOURPLOT.csv")))

  hourplot <- read.csv(file.path(workspace, "HOURPLOT.csv"), skip = 1)
  # MASS(KG) is an echo of the input mass (56.6, the write_endotherm_inputs default),
  # not a computed output - confirmed empirically against the real exe.
  expect_equal(hourplot[1, "MASS.KG."], 56.6, tolerance = 0.01)
})

test_that("a chunked sequence (varying juldays per chunk) runs successfully for two consecutive chunks", {
  skip_on_os(c("linux", "mac"))
  fixtures_dir <- testthat::test_path("fixtures")

  chunks <- chunk_days(total_days = 24, chunk_size = 12)
  expect_length(chunks, 2)

  for (chunk in chunks) {
    workspace <- tempfile("endo_chunk_ws_")
    dir.create(workspace)
    on.exit(unlink(workspace, recursive = TRUE), add = TRUE)

    file.copy(
      file.path(fixtures_dir, c("Endo2022a.exe", "metout.csv", "shadmet.csv", "soil.csv", "shadsoil.csv")),
      workspace
    )

    write_endotherm_inputs(output_dir = workspace, model_settings = list(julnum = 12, juldays = chunk))
    write_juldays_dat(output_dir = workspace, model_settings = list(julnum = 12, juldays = chunk))

    result <- run_endotherm_model(workspace, exe_name = "Endo2022a.exe", sysname = "Windows")
    expect_true(result$success)
  }
})
