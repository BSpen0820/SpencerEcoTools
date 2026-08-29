.hourplot_rows <- function(n_days) {
  # Shared row-generation logic for both file-format fixtures below. HR=24
  # duplicates that day's own HR=0 row's value in a way we can detect (a
  # distinct sentinel), matching the empirically-verified real exe behavior
  # in the design spec (same climate inputs, different MET(W) value).
  rows <- list()
  for (day in seq_len(n_days)) {
    for (hr in 0:24) {
      val <- if (hr == 24) 999 + day else (day - 1) * 100 + hr
      rows[[length(rows) + 1]] <- data.frame(
        HR = hr, MO = day, DEP = 0, MASS.KG. = 56.6,
        MET.W. = val, EVP.G.S. = val / 10, LUNG.L.S. = 0.1, CO2MOL.S = 0.0001
      )
    }
  }
  do.call(rbind, rows)
}

.build_old_format_hourplot <- function(path, n_days) {
  # OLD layout: raw copy of the exe's actual output - metadata line 1,
  # header line 2. .endo_read_hourplot_chunk() must skip=1 for this format.
  writeLines(" Animal species = Test", path)
  suppressWarnings(utils::write.table(.hourplot_rows(n_days), path, sep = ",",
                                      row.names = FALSE, append = TRUE))
}

.build_new_format_hourplot <- function(path, n_days) {
  # NEW layout: run_endo_big_nichemap() already read+trimmed+rewrote this -
  # header is on line 1, no metadata line. .endo_read_hourplot_chunk() must
  # skip=0 for this format, or it silently drops every row (the real bug
  # this test exists to catch).
  utils::write.csv(.hourplot_rows(n_days), path, row.names = FALSE)
}

test_that(".endo_read_hourplot_chunk drops HR==24 rows and timestamps by position - OLD (metadata-line) format", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  .build_old_format_hourplot(path, n_days = 3)

  chunk_start <- as.Date("2022-12-09")
  chunk_end   <- chunk_start + 2
  result <- SpencerEcoTools:::.endo_read_hourplot_chunk(path, chunk_start, chunk_end, "MET.W.")

  expect_equal(nrow(result), 72)  # 3 days * 24 real hours, not 75
  expect_equal(as.Date(result$timestamp[1]), chunk_start)
  expect_equal(as.numeric(format(result$timestamp[1], "%H")), 0)
  expect_equal(result$value[1], 0)
  expect_equal(as.Date(result$timestamp[24]), chunk_start)
  expect_equal(as.numeric(format(result$timestamp[24], "%H")), 23)
  expect_equal(result$value[24], 23)
  # Row 25: day 2 hour 0 - the HR==24 row for day 1 must be dropped, not appear here
  expect_equal(as.Date(result$timestamp[25]), chunk_start + 1)
  expect_equal(as.numeric(format(result$timestamp[25], "%H")), 0)
  expect_equal(result$value[25], 100)
  expect_false(any(result$value %in% c(1000, 1001, 1002)))  # dropped HR==24 sentinels
})

test_that(".endo_read_hourplot_chunk drops HR==24 rows and timestamps by position - NEW (header-only) format", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  .build_new_format_hourplot(path, n_days = 3)

  chunk_start <- as.Date("2022-12-09")
  chunk_end   <- chunk_start + 2
  result <- SpencerEcoTools:::.endo_read_hourplot_chunk(path, chunk_start, chunk_end, "MET.W.")

  # This is the exact case a hardcoded skip=1 gets wrong: applying skip=1 to
  # this format would treat the real header row as data and misparse
  # everything after it (or, in the read.csv(skip=1) case used elsewhere in
  # this package, would skip a REAL DATA row - column names would become
  # X0/X1/etc. and hp$HR would be NULL). This must produce the same 72,
  # correctly-valued rows as the old-format test above.
  expect_equal(nrow(result), 72)
  expect_equal(result$value[1], 0)
  expect_equal(result$value[24], 23)
  expect_equal(result$value[25], 100)
  expect_false(any(is.na(result$value)))
})

test_that(".endo_read_hourplot_chunk treats a row-count mismatch as a read failure, not silent misalignment", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  .build_new_format_hourplot(path, n_days = 3)  # 75 raw rows -> 72 real hours

  # Claim a 2-day range (48 expected hours) against a file that actually has
  # 3 days (72 real hours) - a real-world case of a chunk filename/content
  # mismatch. Must return 0 rows (a loud "something's wrong" signal via the
  # caller's NA-fill), never 72 misaligned rows.
  chunk_start <- as.Date("2022-12-09")
  chunk_end   <- chunk_start + 1  # claims 2 days, file actually has 3
  expect_warning(
    result <- SpencerEcoTools:::.endo_read_hourplot_chunk(path, chunk_start, chunk_end, "MET.W."),
    "row count"
  )
  expect_equal(nrow(result), 0)
})
