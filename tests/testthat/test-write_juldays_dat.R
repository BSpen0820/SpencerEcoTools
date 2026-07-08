test_that("write_juldays_dat writes a file with the expected day count and day list", {
  tmp_dir <- tempfile("juldays_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  log <- write_juldays_dat(tmp_dir)
  expect_true(file.exists(file.path(tmp_dir, "JULDAYS.DAT")))
  expect_identical(log$status, "success")

  lines <- readLines(file.path(tmp_dir, "JULDAYS.DAT"))
  expect_identical(trimws(lines[3]), "12 1 365")
  expect_identical(trimws(lines[7]), "15 45 74 105 135 166 196 227 258 288 319 349")
})

test_that("write_juldays_dat's day list always matches model_settings$juldays, keeping it in sync with endo.dat", {
  tmp_dir <- tempfile("juldays_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  custom_days <- 53:64 # 12 consecutive days, a chunk from a longer sequential run
  write_juldays_dat(tmp_dir, model_settings = list(julnum = 12, juldays = custom_days))

  lines <- readLines(file.path(tmp_dir, "JULDAYS.DAT"))
  expect_identical(trimws(lines[7]), paste(custom_days, collapse = " "))
})

test_that("write_juldays_dat errors when juldays length doesn't match julnum", {
  tmp_dir <- tempfile("juldays_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  expect_error(
    write_juldays_dat(tmp_dir, model_settings = list(julnum = 5, juldays = 1:12)),
    "must have length"
  )
})

test_that("write_juldays_dat errors when a habitat_settings vector doesn't match julnum", {
  tmp_dir <- tempfile("juldays_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  expect_error(
    write_juldays_dat(tmp_dir, habitat_settings = list(absorp = c(0.8, 0.8))),
    "must have length"
  )
})
