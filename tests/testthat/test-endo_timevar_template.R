test_that("endo_timevar_template returns the expected field names, all NULL", {
  tv <- endo_timevar_template()
  expect_named(tv, c(
    "mass2", "fatpct2", "tcreg2", "torlend", "torlenv", "tordepd", "tordepv",
    "digef", "act", "repro", "prtn", "fat", "carb", "dry",
    "diurn", "noct", "crep", "hibrn", "hibfrac", "land", "land2"
  ))
  expect_true(all(vapply(tv, is.null, logical(1))))
})
