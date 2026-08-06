test_that("get_endotherm_defaults returns the nine groups write_endotherm_inputs expects", {
  defaults <- get_endotherm_defaults()

  expect_named(
    defaults,
    c("model_settings", "animal", "fur", "physiology", "diet",
      "thermoreg", "flying_digging", "nest_shelter", "allometry")
  )
})

test_that("get_endotherm_defaults's model_settings matches the julnum/juldays arguments by default", {
  defaults <- get_endotherm_defaults()

  expect_identical(defaults$model_settings$julnum, 12)
  expect_identical(
    defaults$model_settings$juldays,
    c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
  )
  expect_length(defaults$animal$mass2, 12)
  expect_length(defaults$diet$digef, 12)
})

test_that("get_endotherm_defaults resizes julnum-dependent vectors for a custom julnum", {
  custom_days <- c(15, 45, 74, 105, 135, 166)
  defaults <- get_endotherm_defaults(julnum = 6, juldays = custom_days)

  expect_identical(defaults$model_settings$julnum, 6)
  expect_identical(defaults$model_settings$juldays, custom_days)
  expect_length(defaults$animal$mass2, 6)
  expect_length(defaults$fur$torlend, 6)
  expect_length(defaults$physiology$tcreg2, 6)
  expect_length(defaults$diet$digef, 6)
})

test_that("get_endotherm_defaults errors when juldays length doesn't match julnum", {
  expect_error(
    get_endotherm_defaults(julnum = 6, juldays = 1:12),
    "must have length"
  )
})

test_that("get_endotherm_defaults's output round-trips through write_endotherm_inputs via do.call", {
  tmp_dir <- tempfile("endo_defaults_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  defaults <- get_endotherm_defaults()
  log <- do.call(write_endotherm_inputs, c(list(output_dir = tmp_dir), defaults))

  expect_true(file.exists(file.path(tmp_dir, "endo.dat")))
  expect_true(file.exists(file.path(tmp_dir, "alomvars.dat")))
  expect_true(all(log$status == "success"))
})
