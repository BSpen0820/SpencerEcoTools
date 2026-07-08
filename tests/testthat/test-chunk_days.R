test_that("chunk_days splits into consecutive equal-size blocks", {
  result <- chunk_days(total_days = 24, chunk_size = 12)
  expect_length(result, 2)
  expect_identical(result[[1]], 1:12)
  expect_identical(result[[2]], 13:24)
})

test_that("chunk_days handles a single chunk covering everything", {
  result <- chunk_days(total_days = 12, chunk_size = 12)
  expect_length(result, 1)
  expect_identical(result[[1]], 1:12)
})

test_that("chunk_days errors when total_days is not a multiple of chunk_size", {
  expect_error(chunk_days(total_days = 25, chunk_size = 12), "multiple")
})

test_that("chunk_days errors on non-positive inputs", {
  expect_error(chunk_days(total_days = 0, chunk_size = 12), "positive")
  expect_error(chunk_days(total_days = 12, chunk_size = 0), "positive")
})
