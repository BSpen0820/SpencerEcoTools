test_that(".check_grid_match passes silently when resolution and CRS match", {
  r1 <- terra::rast(resolution = 0.01, crs = "EPSG:4326",
                     xmin = 0, xmax = 0.05, ymin = 0, ymax = 0.05)
  r2 <- terra::rast(resolution = 0.01, crs = "EPSG:4326",
                     xmin = 0, xmax = 0.05, ymin = 0, ymax = 0.05)
  expect_silent(.check_grid_match(r1, r2, "ref", "target"))
})

test_that(".check_grid_match stops on resolution mismatch when action = 'stop'", {
  r1 <- terra::rast(resolution = 0.01, crs = "EPSG:4326",
                     xmin = 0, xmax = 0.05, ymin = 0, ymax = 0.05)
  r2 <- terra::rast(resolution = 0.02, crs = "EPSG:4326",
                     xmin = 0, xmax = 0.1, ymin = 0, ymax = 0.1)
  expect_error(.check_grid_match(r1, r2, "ref", "target", action = "stop"),
               "do not share the same grid")
})

test_that(".check_grid_match warns on resolution mismatch when action = 'warn'", {
  r1 <- terra::rast(resolution = 0.01, crs = "EPSG:4326",
                     xmin = 0, xmax = 0.05, ymin = 0, ymax = 0.05)
  r2 <- terra::rast(resolution = 0.02, crs = "EPSG:4326",
                     xmin = 0, xmax = 0.1, ymin = 0, ymax = 0.1)
  expect_warning(.check_grid_match(r1, r2, "ref", "target", action = "warn"),
                 "do not share the same grid")
})

test_that(".check_grid_match stops on CRS mismatch even with matching resolution", {
  r1 <- terra::rast(resolution = 0.01, crs = "EPSG:4326",
                     xmin = 0, xmax = 0.05, ymin = 0, ymax = 0.05)
  r2 <- terra::rast(resolution = 0.01, crs = "EPSG:3857",
                     xmin = 0, xmax = 0.05, ymin = 0, ymax = 0.05)
  expect_error(.check_grid_match(r1, r2, "ref", "target", action = "stop"),
               "do not share the same grid")
})

test_that(".check_grid_match tolerates tiny floating-point resolution differences", {
  r1 <- terra::rast(resolution = 0.01, crs = "EPSG:4326",
                     xmin = 0, xmax = 0.05, ymin = 0, ymax = 0.05)
  r2 <- terra::rast(resolution = 0.01 + 1e-10, crs = "EPSG:4326",
                     xmin = 0, xmax = 0.05, ymin = 0, ymax = 0.05)
  expect_silent(.check_grid_match(r1, r2, "ref", "target"))
})

test_that(".first_spatraster unwraps a plain SpatRaster", {
  r <- terra::rast(nrows = 2, ncols = 2)
  expect_true(inherits(.first_spatraster(r), "SpatRaster"))
})

test_that(".first_spatraster unwraps a PackedSpatRaster", {
  r <- terra::rast(nrows = 2, ncols = 2)
  terra::values(r) <- 1:4
  r <- terra::wrap(r)
  expect_true(inherits(.first_spatraster(r), "SpatRaster"))
})

test_that(".first_spatraster finds a raster nested inside a list", {
  r <- terra::rast(nrows = 2, ncols = 2)
  terra::values(r) <- 1:4
  r <- terra::wrap(r)
  nested <- list(a = 1, b = list(c = r))
  expect_true(inherits(.first_spatraster(nested), "SpatRaster"))
})

test_that(".first_spatraster errors when nothing raster-like is found", {
  expect_error(.first_spatraster(list(a = 1, b = "text")), "No SpatRaster found")
})
