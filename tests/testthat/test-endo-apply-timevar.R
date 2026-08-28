test_that(".endo_validate_timevar_lengths stops when a supplied vector has the wrong length", {
  tv <- endo_timevar_template()
  tv$mass2 <- 1:5
  expect_error(SpencerEcoTools:::.endo_validate_timevar_lengths(tv, n_days = 10), "mass2")
})

test_that(".endo_validate_timevar_lengths passes when lengths match or fields are NULL", {
  tv <- endo_timevar_template()
  tv$mass2 <- rep(50, 10)
  expect_true(SpencerEcoTools:::.endo_validate_timevar_lengths(tv, n_days = 10))
})

test_that(".endo_apply_timevar leaves endo_inputs untouched when time_varying is all NULL", {
  endo_inputs <- get_endotherm_defaults(julnum = 4, juldays = 1:4)
  tv <- endo_timevar_template()
  result <- SpencerEcoTools:::.endo_apply_timevar(endo_inputs, tv, chunk_idx = 1:4)
  expect_identical(result, endo_inputs)
})

test_that(".endo_apply_timevar sets mass2/timdepmass from a sliced vector", {
  endo_inputs <- get_endotherm_defaults(julnum = 4, juldays = 1:4)
  tv <- endo_timevar_template()
  tv$mass2 <- c(56, 55, 54, 53, 52, 51, 50, 49, 47, 45)  # 10-day full window
  result <- SpencerEcoTools:::.endo_apply_timevar(endo_inputs, tv, chunk_idx = 3:6)
  expect_equal(result$animal$timdepmass, 1)
  expect_equal(result$animal$mass2, c(54, 53, 52, 51))
})

test_that(".endo_apply_timevar backfills the other three torso-fur fields and sets tmdptorfur when only one is supplied", {
  endo_inputs <- get_endotherm_defaults(julnum = 3, juldays = 1:3)
  static_lend <- endo_inputs$fur$torlend[1]
  static_lenv <- endo_inputs$fur$torlenv[1]
  static_depv <- endo_inputs$fur$tordepv[1]
  tv <- endo_timevar_template()
  tv$tordepd <- c(7, 6, 5, 4, 3, 2)  # 6-day full window
  result <- SpencerEcoTools:::.endo_apply_timevar(endo_inputs, tv, chunk_idx = 2:4)

  expect_equal(result$fur$tmdptorfur, 1)
  expect_equal(result$fur$tordepd, c(6, 5, 4))
  expect_equal(result$fur$torlend, rep(static_lend, 3))
  expect_equal(result$fur$torlenv, rep(static_lenv, 3))
  expect_equal(result$fur$tordepv, rep(static_depv, 3))
})

test_that(".endo_apply_timevar backfills when two of four torso-fur fields are supplied", {
  endo_inputs <- get_endotherm_defaults(julnum = 4, juldays = 1:4)
  static_lenv <- endo_inputs$fur$torlenv[1]
  static_depv <- endo_inputs$fur$tordepv[1]
  tv <- endo_timevar_template()
  tv$tordepd <- c(8, 7, 6, 5, 4, 3, 2, 1)  # 8-day full window
  tv$torlend <- c(2.5, 2.4, 2.3, 2.2, 2.1, 2.0, 1.9, 1.8)
  result <- SpencerEcoTools:::.endo_apply_timevar(endo_inputs, tv, chunk_idx = 3:6)

  expect_equal(result$fur$tmdptorfur, 1)
  expect_equal(result$fur$tordepd, c(6, 5, 4, 3))
  expect_equal(result$fur$torlend, c(2.3, 2.2, 2.1, 2.0))
  expect_equal(result$fur$torlenv, rep(static_lenv, 4))
  expect_equal(result$fur$tordepv, rep(static_depv, 4))
})

test_that(".endo_apply_timevar overwrites an always-vector diet field directly, no flag involved", {
  endo_inputs <- get_endotherm_defaults(julnum = 2, juldays = 1:2)
  tv <- endo_timevar_template()
  tv$act <- c(1.0, 1.2, 1.4, 1.6)  # 4-day full window
  result <- SpencerEcoTools:::.endo_apply_timevar(endo_inputs, tv, chunk_idx = 2:3)
  expect_equal(result$diet$act, c(1.2, 1.4))
})
