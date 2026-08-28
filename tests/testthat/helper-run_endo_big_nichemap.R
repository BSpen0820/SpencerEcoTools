# Shared fixture for test-run_endo_big_nichemap.R: builds a minimal but
# complete microclim_dir (AbvGrd/BlwGrd/Snow tiles), tile_map, valid_cells_mask,
# dem, and refl_dir - enough for one tile, one cell, forcing >= 2 chunks.
.rebn_build_fixture <- function(n_days = 60, study_area = "TestArea") {
  root <- tempfile("rebn_fixture_")
  dir.create(root)

  nrow_ <- 2; ncol_ <- 2
  dtm <- terra::rast(nrows = nrow_, ncols = ncol_,
                     xmin = 500000, xmax = 500000 + ncol_ * 30,
                     ymin = 4800000, ymax = 4800000 + nrow_ * 30,
                     crs = "EPSG:32612")

  period_label <- "20220701_to_20230630"
  ntime_full_year <- 8760L
  start_full_year <- as.POSIXct("2022-07-01 00:00:00", tz = "UTC")
  tme_full <- seq(start_full_year, by = "hour", length.out = ntime_full_year)

  set.seed(1)
  mk <- function(ntime) array(round(stats::runif(nrow_ * ncol_ * ntime, 0, 30), 4),
                              dim = c(nrow_, ncol_, ntime))
  mout <- list(Tz = mk(ntime_full_year), tleaf = mk(ntime_full_year), relhum = mk(ntime_full_year),
              soilm = mk(ntime_full_year), windspeed = mk(ntime_full_year), Rdirdown = mk(ntime_full_year),
              Rdifdown = mk(ntime_full_year), Rlwdown = mk(ntime_full_year), Rswup = mk(ntime_full_year),
              Rlwup = mk(ntime_full_year), tme = tme_full)

  abv_path <- .endo_microclim_path(root, study_area, period_label, tile_id = 1, hgt_lbl = "AbvGrd", file_fmt = "nc")
  dir.create(dirname(abv_path), recursive = TRUE)
  write_tile(mout, abv_path, dtm = dtm, tme = tme_full, file_fmt = "nc")

  blw_path <- .endo_microclim_path(root, study_area, period_label, tile_id = 1, hgt_lbl = "BlwGrd", file_fmt = "nc")
  dir.create(dirname(blw_path), recursive = TRUE)
  for (depth_mm in c(0, 15, 50, 100, 150, 200, 300, 500, 1000, 2000)) {
    blw_arr <- mk(ntime_full_year)
    write_tile(list(Tz = blw_arr, tme = tme_full), blw_path, dtm = dtm, tme = tme_full,
              file_fmt = "nc", depth_label = sprintf("BlwGrd_%04d", depth_mm))
  }

  snow_path <- .endo_snow_path(root, study_area, period_label, tile_id = 1, file_fmt = "nc")
  dir.create(dirname(snow_path), recursive = TRUE)
  smod <- list(Tc = mk(ntime_full_year), Tg = mk(ntime_full_year),
              groundsnowdepth = mk(ntime_full_year), totalSWE = mk(ntime_full_year),
              snowden = mk(ntime_full_year), umu = mk(ntime_full_year))
  write_tile(smod, snow_path, dtm = dtm, tme = tme_full, file_fmt = "nc")

  tile_map_path <- file.path(root, "tile_map.tif")
  tile_map <- terra::rast(dtm); terra::values(tile_map) <- 1L
  terra::writeRaster(tile_map, tile_map_path, overwrite = TRUE)

  valid_mask_path <- file.path(root, "valid_mask.tif")
  valid_mask <- terra::rast(dtm); terra::values(valid_mask) <- c(1, NA, NA, NA)  # exactly one valid cell
  terra::writeRaster(valid_mask, valid_mask_path, overwrite = TRUE)

  dem_path <- file.path(root, "dem.tif")
  dem <- terra::rast(dtm); terra::values(dem) <- 2000
  terra::writeRaster(dem, dem_path, overwrite = TRUE)

  refl_dir <- file.path(root, "refl")
  gref_dir <- file.path(refl_dir, "Gref")
  dir.create(gref_dir, recursive = TRUE)
  for (mo in 12:12) {
    gref <- terra::rast(dtm); terra::values(gref) <- 0.2
    terra::writeRaster(gref, file.path(gref_dir, sprintf("GF_Refl_%s_2022_%02d.tif", study_area, mo)), overwrite = TRUE)
  }
  for (mo in 1:4) {
    gref <- terra::rast(dtm); terra::values(gref) <- 0.3
    terra::writeRaster(gref, file.path(gref_dir, sprintf("GF_Refl_%s_2023_%02d.tif", study_area, mo)), overwrite = TRUE)
  }

  list(root = root, tile_map = tile_map_path, valid_cells_mask = valid_mask_path,
      microclim_dir = root, dem = dem_path, refl_dir = refl_dir,
      dates = data.frame(Start_Dates = as.Date("2022-07-01"), End_Dates = as.Date("2023-06-30"),
                         Sim_Start = as.Date("2022-12-09"),
                         Sim_End = as.Date("2022-12-09") + n_days - 1))
}
