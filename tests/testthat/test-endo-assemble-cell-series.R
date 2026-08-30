.write_real_hourplot <- function(path, chunk_start_date, n_days, value_start) {
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

test_that(".endo_assemble_cell_series concatenates two chunks in date order into one continuous series", {
  dir <- tempfile("assemble_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  chunk1 <- file.path(dir, "chunk1.csv")
  chunk2 <- file.path(dir, "chunk2.csv")
  .write_real_hourplot(chunk1, as.Date("2022-12-09"), n_days = 2, value_start = 0)
  .write_real_hourplot(chunk2, as.Date("2022-12-11"), n_days = 1, value_start = 1000)

  chunk_files_df <- data.frame(
    chunk_start = as.Date(c("2022-12-09", "2022-12-11")),
    chunk_end   = as.Date(c("2022-12-10", "2022-12-11")),
    path        = c(chunk1, chunk2),
    stringsAsFactors = FALSE
  )

  result <- SpencerEcoTools:::.endo_assemble_cell_series(
    chunk_files_df, sim_start = as.Date("2022-12-09"), sim_end = as.Date("2022-12-11"),
    variable_col = "MET.W."
  )

  expect_equal(length(result$timestamps), 72)  # 3 days * 24 real hours
  expect_equal(length(result$values), 72)
  expect_false(anyNA(result$values))
  expect_true(result$has_data)
  expect_equal(result$values[1], 0)     # day1 hour0 of chunk1
  expect_equal(result$values[48], 47)   # day2 hour23 of chunk1 (2*24-1)
  expect_equal(result$values[49], 1000) # day1 hour0 of chunk2
})

test_that(".endo_assemble_cell_series NA-fills hours with no covering chunk, without failing, and reports has_data = FALSE", {
  chunk_files_df <- data.frame(chunk_start = as.Date(character(0)), chunk_end = as.Date(character(0)),
                               path = character(0), stringsAsFactors = FALSE)

  result <- SpencerEcoTools:::.endo_assemble_cell_series(
    chunk_files_df, sim_start = as.Date("2022-12-09"), sim_end = as.Date("2022-12-09"),
    variable_col = "MET.W."
  )

  expect_equal(length(result$values), 24)
  expect_true(all(is.na(result$values)))
  expect_false(result$has_data)
})

test_that(".endo_assemble_cell_series NA-fills only the missing window when one chunk file can't be read, keeps the rest", {
  dir <- tempfile("assemble_partial_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  chunk1 <- file.path(dir, "chunk1.csv")
  .write_real_hourplot(chunk1, as.Date("2022-12-09"), n_days = 1, value_start = 0)
  missing_chunk <- file.path(dir, "does_not_exist.csv")

  chunk_files_df <- data.frame(
    chunk_start = as.Date(c("2022-12-09", "2022-12-10")),
    chunk_end   = as.Date(c("2022-12-09", "2022-12-10")),
    path        = c(chunk1, missing_chunk),
    stringsAsFactors = FALSE
  )

  result <- SpencerEcoTools:::.endo_assemble_cell_series(
    chunk_files_df, sim_start = as.Date("2022-12-09"), sim_end = as.Date("2022-12-10"),
    variable_col = "MET.W."
  )

  expect_equal(length(result$values), 48)
  expect_false(anyNA(result$values[1:24]))   # day 1: real data
  expect_true(all(is.na(result$values[25:48]))) # day 2: missing file -> NA, not an error
  expect_true(result$has_data)  # day 1's real data is enough for has_data = TRUE
})
