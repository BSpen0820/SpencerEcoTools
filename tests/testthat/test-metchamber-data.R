test_that("metchamber_metout has the expected ramp structure", {
  expect_equal(nrow(metchamber_metout), 288)
  expect_setequal(
    unique(metchamber_metout$JULDAY),
    c(15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
  )
  expect_equal(sum(metchamber_metout$JULDAY == 15), 24)
  expect_true(all(c("TALOC", "TALOC.1", "TALOC.2") %in% names(metchamber_metout)))
})

test_that("metchamber_soil has the expected shape and aligns row-for-row with metchamber_metout", {
  expect_equal(nrow(metchamber_soil), 288)
  expect_true(all(c("TIME", "D0cm", "D200cm") %in% names(metchamber_soil)))
  expect_identical(metchamber_soil$TIME, metchamber_metout$TIME)
})
