test_that("read_valid_cell_indices returns cells where the mask is 1", {
  skip_if_not_installed("terra")
  r <- terra::rast(nrows = 2, ncols = 3, vals = c(1, 0, 1, 0, 1, 1))
  tmp <- tempfile(fileext = ".tif")
  on.exit(unlink(tmp))
  terra::writeRaster(r, tmp)

  expect_identical(read_valid_cell_indices(tmp), c(1L, 3L, 5L, 6L))
})

test_that("cells_for_array_task returns everything when no array args are given", {
  expect_identical(cells_for_array_task(101:110), 101:110)
})

test_that("cells_for_array_task distributes round-robin across array tasks", {
  valid <- 101:110 # 10 cells
  # 3 array tasks, round-robin: node = 1,2,3,1,2,3,1,2,3,1
  expect_identical(cells_for_array_task(valid, clust_array_arg = 1, clust_array_size = 3),
                    c(101L, 104L, 107L, 110L))
  expect_identical(cells_for_array_task(valid, clust_array_arg = 2, clust_array_size = 3),
                    c(102L, 105L, 108L))
  expect_identical(cells_for_array_task(valid, clust_array_arg = 3, clust_array_size = 3),
                    c(103L, 106L, 109L))
})

test_that("cells_for_array_task validates its arguments like run_micro_big_nichemap does", {
  expect_error(cells_for_array_task(1:10, clust_array_arg = 1), "clust_array_size must be provided")
  expect_error(cells_for_array_task(1:10, clust_array_arg = 5, clust_array_size = 3),
               "clust_array_arg must be between 1 and clust_array_size")
})
