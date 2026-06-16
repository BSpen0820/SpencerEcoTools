
setwd('D:\\Code\\PhD\\Chapter 3 - Tradeoffs\\Landscape Thermal Modeling')

## ---- libraries ------------------------------------------------------------
library(microclimfPara)
library(terra)
library(tidyverse)
library(rhdf5)
library(sf)

devtools::load_all('D:/Code/PhD/R_Packages/SpencerEcoTools')

# HPC Option
# terraOptions(
#   threads = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "16")),
#   memfrac = 0.7,
#   tempdir = "~/terra_temp"
# )

## Define Date Range and read in static dem -----------------------------------------


moose <- st_read('./Data/Moose MCPs/MooseGPSLocs.shp')
moose$t_<- as.POSIXct(moose$t_, tz = "UTC")

dates <- unique(moose$t_)
dates <- dates[month(dates) %in% 3:8 & day(dates) == 1]
dates <- as.Date(dates)
dates_df <- tibble(dates = dates) %>%
  group_by(year(dates)) %>%
  summarise(Start_Dates = min(dates), End_Dates = max(dates)) %>%
  select(-1)

dem_fine <- rast('./Data/DEM/DEM_GLO30.tif')
dem_coarse <- rast('./Data/DEM/DEM_800m.tif')

rm(moose)

dates <- as.Date(t(dates_df[1,])[,1])

tiles <- create_tiles(coarse_dem = dem_coarse, fine_dem = dem_fine, dates = dates,
                      snow_modeling = T, return_tiles_rast = T)




# Internal helper: height to CLAUDE.md standard label
.hgt_label <- function(h) {
  if (h > 0) "AbvGrd"
  else sprintf("BlwGrd_%04d", as.integer(round(abs(h) * 1000)))
}

# Internal helper: suppress cat(), messages, and warnings from noisy calls
.quiet_run <- function(expr) {
  local({
    invisible(utils::capture.output(
      suppressMessages(suppressWarnings(r <- expr)),
      type = "output"
    ))
    r
  })
}


run_micro_big_nichemap <- function(tiles,        # tile object from create_tiles
                                   clim,         # file path to packaged climate RDS
                                   dates,        # data.frame(Start_Dates, End_Dates) or length-2 Date vector
                                   dtm_fine,     # fine DEM SpatRaster or file path
                                   dtm_coarse,   # coarse DEM SpatRaster or file path
                                   vegp,         # file path to packaged veg params RDS
                                   soilc,        # file path to packaged soil params RDS
                                   output_dir,   # root output folder
                                   reqhgt       = 2,
                                   zref         = 2,
                                   windhgt      = 10,
                                   matemp       = NA,
                                   snow         = FALSE,
                                   snowenv      = "Taiga",
                                   Dynreqhgt    = FALSE,
                                   altcorrect   = 0,
                                   parallel     = FALSE,
                                   ncores       = 2,
                                   study_area   = NULL,
                                   file_fmt     = c("h5", "nc"),
                                   compression  = 4L,
                                   ...) {

  file_fmt <- match.arg(file_fmt)

  # ... SLURM hidden args -------------------------------------------------------
  dots    <- list(...)
  allowed <- c("clust_array_arg", "clust_array_size")
  unknown <- setdiff(names(dots), allowed)
  if (length(unknown) > 0)
    stop("Unknown argument(s): ", paste(unknown, collapse = ", "))

  clust_array_arg  <- dots$clust_array_arg
  clust_array_size <- dots$clust_array_size

  if (!is.null(clust_array_arg) &&
      (!is.numeric(clust_array_arg) || length(clust_array_arg) != 1))
    stop("clust_array_arg must be a single numeric value or NULL")
  if (!is.null(clust_array_size) &&
      (!is.numeric(clust_array_size) || length(clust_array_size) != 1))
    stop("clust_array_size must be a single numeric value or NULL")
  if (!is.null(clust_array_arg) && is.null(clust_array_size))
    stop("clust_array_size must be provided when clust_array_arg is set")
  if (!is.null(clust_array_arg) &&
      (clust_array_arg < 1 || clust_array_arg > clust_array_size))
    stop("clust_array_arg must be between 1 and clust_array_size")

  if (reqhgt <= 0) stop("reqhgt must be positive")

  # --- normalise dates ---------------------------------------------------------
  if (is.data.frame(dates)) {
    if (!all(c("Start_Dates", "End_Dates") %in% names(dates)))
      stop("dates data.frame must contain columns 'Start_Dates' and 'End_Dates'")
    date_ranges <- dates
  } else if (is.vector(dates) && length(dates) == 2) {
    date_ranges <- data.frame(
      Start_Dates      = as.Date(dates[1]),
      End_Dates        = as.Date(dates[2]),
      stringsAsFactors = FALSE
    )
  } else {
    stop("dates must be a data.frame with Start_Dates/End_Dates columns, or a length-2 Date vector")
  }

  # --- heights -----------------------------------------------------------------
  sdepth     <- c(0, 1.5, 5, 10, 15, 20, 30, 50, 100, 200) / -100
  allheights <- c(reqhgt, sdepth)

  out_ext <- if (file_fmt == "h5") ".h5" else ".nc"

  # --- terrain features (full DEM, computed once) ------------------------------
  cat(sprintf("\n--- run_micro_big_nichemap: computing terrain features ---\n"))

  slope <- terra::terrain(dtm_fine, v = "slope")
  aspect <- terra::terrain(dtm_fine, v = "aspect")
  twi   <- microclimf:::.topidx(dtm_fine)

  hor <- array(NA, dim = c(dim(dtm_fine)[1:2], 24))
  for (i in 1:24) hor[,,i] <- microclimf:::.horizon(dtm_fine, (i - 1) * 15)

  msl  <- tan(apply(atan(hor), c(1, 2), mean))
  svfa <- 0.5 * cos(2 * msl) + 0.5
  wsa  <- microclimf:::.windsheltera(dtm_fine, 2, 1)
  rm(msl); invisible(gc())

  # --- flat task table: all (tile, date) combinations --------------------------
  n_tiles <- nrow(tiles$tiles_proc)
  n_dates <- nrow(date_ranges)

  all_combos <- expand.grid(
    tile_idx = seq_len(n_tiles),
    date_idx = seq_len(n_dates),
    KEEP.OUT.ATTRS = FALSE
  )

  all_combos$node <- if (is.null(clust_array_size)) 1L else
    rep(seq_len(clust_array_size), length.out = nrow(all_combos))

  task_combos <- if (is.null(clust_array_arg)) all_combos else
    all_combos[all_combos$node == clust_array_arg, ]

  cat(sprintf("Tasks this node: %d of %d total (tile x period combinations)\n",
              nrow(task_combos), nrow(all_combos)))

  # --- log data.frame ----------------------------------------------------------
  log_df <- data.frame(
    tile_id      = integer(),
    period_label = character(),
    height_label = character(),
    step         = character(),
    status       = character(),
    file_path    = character(),
    timestamp    = character(),
    stringsAsFactors = FALSE
  )

  .log_row <- function(tile_id, period_label, height_label, step, status, file_path) {
    data.frame(
      tile_id      = tile_id,
      period_label = period_label,
      height_label = height_label,
      step         = step,
      status       = status,
      file_path    = file_path,
      timestamp    = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      stringsAsFactors = FALSE
    )
  }

  # --- flat loop over (tile, date) tasks ---------------------------------------
  for (k in seq_len(nrow(task_combos))) {

    tile_i <- task_combos$tile_idx[k]
    d      <- task_combos$date_idx[k]

    tile_proc <- tiles$tiles_proc[tile_i, ]
    tile_core <- tiles$tiles_core[tile_i, ]

    start_date   <- as.Date(date_ranges$Start_Dates[d])
    end_date     <- as.Date(date_ranges$End_Dates[d])
    period_label <- sprintf("%s_to_%s",
                            format(start_date, "%Y%m%d"),
                            format(end_date,   "%Y%m%d"))

    cat(sprintf("\n=== Task %d/%d | Tile %d | Period: %s ===\n",
                k, nrow(task_combos), tile_i, period_label))

    dem_coarse_tile <- terra::crop(dtm_coarse, terra::ext(tile_proc))
    dem_fine_tile   <- terra::crop(dtm_fine,   dem_coarse_tile, snap = "out")
    dem_fine_core   <- terra::crop(dtm_fine,   terra::ext(tile_core))

    slope_tile  <- terra::crop(slope,  dem_fine_tile)
    aspect_tile <- terra::crop(aspect, dem_fine_tile)
    twi_tile    <- terra::crop(twi,    dem_fine_tile)

    crop_cells  <- terra::cellFromXY(
      dtm_fine,
      terra::xyFromCell(dem_fine_tile,
                        seq_len(terra::nrow(dem_fine_tile) * terra::ncol(dem_fine_tile)))
    )
    crop_rowcol <- terra::rowColFromCell(dtm_fine, crop_cells)
    row_idx <- unique(crop_rowcol[, 1])
    col_idx <- unique(crop_rowcol[, 2])

    hor_tile  <- hor [row_idx, col_idx, , drop = FALSE]
    svfa_tile <- svfa[row_idx, col_idx,   drop = FALSE]
    wsa_tile  <- wsa [row_idx, col_idx, , drop = FALSE]
    invisible(gc())

    tme <- as.POSIXlt(
      seq(as.POSIXct(sprintf("%s 00:00:00", start_date), tz = "UTC"),
          as.POSIXct(sprintf("%s 23:00:00", end_date),   tz = "UTC"),
          by = "1 hour")
    )

    # --- load and crop inputs --------------------------------------------------
    cat(sprintf("  Loading and cropping inputs...\n"))

    soilc.d   <- readr::read_rds(soilc)
    soil.crop <- lapply(soilc.d, function(x) terra::wrap(terra::crop(terra::unwrap(x), dem_fine_tile)))
    attr(soil.crop, "class") <- attr(soilc.d, "class")
    rm(soilc.d); invisible(gc())

    vegp.d   <- readr::read_rds(vegp)
    veg.crop <- lapply(vegp.d, function(x) terra::wrap(terra::crop(terra::unwrap(x), dem_fine_tile)))
    attr(veg.crop, "class") <- attr(vegp.d, "class")
    rm(vegp.d); invisible(gc())

    climdatag <- readr::read_rds(clim)
    clim.crop <- lapply(climdatag, function(x) terra::wrap(terra::crop(terra::unwrap(x), dem_coarse_tile)))
    rm(climdatag); invisible(gc())

    # --- micropoint models for all heights -------------------------------------
    for (h in allheights) {

      hgt_lbl  <- .hgt_label(h)
      prefix   <- if (!is.null(study_area)) sprintf("%s_%s", study_area, hgt_lbl) else hgt_lbl

      mp_parts <- c(output_dir)
      if (!is.null(study_area)) mp_parts <- c(mp_parts, study_area)
      mp_parts <- c(mp_parts, "Micropoint_Models", period_label, hgt_lbl)
      mp_dir   <- do.call(file.path, as.list(mp_parts))
      dir.create(mp_dir, recursive = TRUE, showWarnings = FALSE)

      mp_file <- file.path(mp_dir,
                           sprintf("Tile_%03d_%s_MicropointModel_%s.RDS",
                                   tile_i, prefix, period_label))

      cat(sprintf("  [Tile %d | %s | %s] Running micropoint model...",
                  tile_i, period_label, hgt_lbl))

      tryCatch({
        pointa <- .quiet_run(
          microclimfPara::runpointmodela(
            climarrayr = clim.crop, tme = tme, reqhgt = h,
            dtm = dem_fine_tile, vegp = veg.crop, soilc = soil.crop,
            matemp = matemp, zref = zref, windhgt = windhgt
          )
        )
        readr::write_rds(pointa, mp_file)
        cat(" done.\n")
        log_df <<- rbind(log_df, .log_row(tile_i, period_label, hgt_lbl,
                                          "micropoint", "success", mp_file))
      }, error = function(e) {
        cat(sprintf(" FAILED: %s\n", conditionMessage(e)))
        warning(sprintf("Tile %d | %s | %s | micropoint: %s",
                        tile_i, period_label, hgt_lbl, conditionMessage(e)))
        log_df <<- rbind(log_df, .log_row(tile_i, period_label, hgt_lbl,
                                          "micropoint",
                                          paste0("error: ", conditionMessage(e)),
                                          mp_file))
      })
    }

    # --- snow model (once per tile+period, uses AbvGrd micropoint) -------------
    smod <- NULL

    if (snow) {

      abv_lbl    <- .hgt_label(reqhgt)
      abv_prefix <- if (!is.null(study_area)) sprintf("%s_%s", study_area, abv_lbl) else abv_lbl

      abv_parts <- c(output_dir)
      if (!is.null(study_area)) abv_parts <- c(abv_parts, study_area)
      abv_parts <- c(abv_parts, "Micropoint_Models", period_label, abv_lbl)
      abv_dir   <- do.call(file.path, as.list(abv_parts))

      abv_file <- file.path(abv_dir,
                            sprintf("Tile_%03d_%s_MicropointModel_%s.RDS",
                                    tile_i, abv_prefix, period_label))

      smod_parts <- c(output_dir)
      if (!is.null(study_area)) smod_parts <- c(smod_parts, study_area)
      smod_parts <- c(smod_parts, "Snow_Models", period_label)
      smod_dir   <- do.call(file.path, as.list(smod_parts))
      dir.create(smod_dir, recursive = TRUE, showWarnings = FALSE)

      smod_prefix <- if (!is.null(study_area)) study_area else "SnowModel"
      smod_file <- file.path(smod_dir,
                             sprintf("Tile_%03d_%s_SnowModel_%s%s",
                                     tile_i, smod_prefix, period_label, out_ext))

      cat(sprintf("  [Tile %d | %s] Running snow model...", tile_i, period_label))

      tryCatch({
        pointa_abv <- readr::read_rds(abv_file)

        smod <- .quiet_run(
          microclimfPara::runsnowmodel(
            weather    = clim.crop,   micropoint = pointa_abv,
            vegp       = veg.crop,    soilc      = soil.crop,
            dtm        = dem_fine_tile, dtmc     = dem_coarse_tile,
            tme        = tme,         altcorrect = altcorrect,
            snowenv    = snowenv,     method     = "slow",
            zref       = zref,        windhgt    = windhgt,
            parallel   = parallel,    ncores     = ncores
          )
        )

        smod.trim <- trim_tile_buffer(smod, dem_proc = dem_fine_tile, dem_core = dem_fine_core)
        write_tile(smod.trim, out_path = smod_file, tme = tme,
                   compression = compression, file_fmt = file_fmt, dtm = dem_fine_core)
        rm(smod.trim)
        cat(" done.\n")
        log_df <<- rbind(log_df, .log_row(tile_i, period_label, "",
                                          "snow", "success", smod_file))
      }, error = function(e) {
        cat(sprintf(" FAILED: %s\n", conditionMessage(e)))
        warning(sprintf("Tile %d | %s | snow: %s",
                        tile_i, period_label, conditionMessage(e)))
        log_df <<- rbind(log_df, .log_row(tile_i, period_label, "",
                                          "snow",
                                          paste0("error: ", conditionMessage(e)),
                                          smod_file))
      })
    }

    # --- microclimate models for all heights -----------------------------------
    for (h in allheights) {

      hgt_lbl <- .hgt_label(h)
      prefix  <- if (!is.null(study_area)) sprintf("%s_%s", study_area, hgt_lbl) else hgt_lbl

      mp_parts <- c(output_dir)
      if (!is.null(study_area)) mp_parts <- c(mp_parts, study_area)
      mp_parts <- c(mp_parts, "Micropoint_Models", period_label, hgt_lbl)
      mp_dir   <- do.call(file.path, as.list(mp_parts))

      mp_file <- file.path(mp_dir,
                           sprintf("Tile_%03d_%s_MicropointModel_%s.RDS",
                                   tile_i, prefix, period_label))

      mc_parts <- c(output_dir)
      if (!is.null(study_area)) mc_parts <- c(mc_parts, study_area)
      mc_parts <- c(mc_parts, "Microclim_Models", period_label, hgt_lbl)
      mc_dir   <- do.call(file.path, as.list(mc_parts))
      dir.create(mc_dir, recursive = TRUE, showWarnings = FALSE)

      mc_file <- file.path(mc_dir,
                           sprintf("Tile_%03d_%s_MicroclimModel_%s%s",
                                   tile_i, prefix, period_label, out_ext))

      cat(sprintf("  [Tile %d | %s | %s] Running microclimate model...",
                  tile_i, period_label, hgt_lbl))

      tryCatch({
        pointa <- readr::read_rds(mp_file)

        if (snow && !is.null(smod)) {
          mout <- .quiet_run(
            microclimfPara::runmicro(
              micropoint = pointa,       reqhgt     = h,
              vegp       = veg.crop,     soilc      = soil.crop,
              dtm        = dem_fine_tile, dtmc      = dem_coarse_tile,
              altcorrect = altcorrect,   snow       = snow,
              snowmod    = smod,         runchecks  = TRUE,
              slr        = slope_tile,   apr        = aspect_tile,
              hor        = hor_tile,     twi        = twi_tile,
              wsa        = wsa_tile,     svf        = svfa_tile,
              Dynreqhgt  = Dynreqhgt,   parallel   = parallel,
              ncores     = ncores
            )
          )
        } else {
          mout <- .quiet_run(
            microclimfPara::runmicro(
              micropoint = pointa,       reqhgt     = h,
              vegp       = veg.crop,     soilc      = soil.crop,
              dtm        = dem_fine_tile, dtmc      = dem_coarse_tile,
              altcorrect = altcorrect,   snow       = snow,
              runchecks  = TRUE,         slr        = slope_tile,
              apr        = aspect_tile,  hor        = hor_tile,
              twi        = twi_tile,     wsa        = wsa_tile,
              svf        = svfa_tile,    Dynreqhgt  = Dynreqhgt,
              parallel   = parallel,     ncores     = ncores
            )
          )
        }

        mout.trim <- trim_tile_buffer(mout, dem_proc = dem_fine_tile, dem_core = dem_fine_core)
        write_tile(mout.trim, out_path = mc_file, tme = tme,
                   compression = compression, file_fmt = file_fmt, dtm = dem_fine_core)
        rm(mout, mout.trim)
        cat(" done.\n")
        log_df <<- rbind(log_df, .log_row(tile_i, period_label, hgt_lbl,
                                          "microclimate", "success", mc_file))
      }, error = function(e) {
        cat(sprintf(" FAILED: %s\n", conditionMessage(e)))
        warning(sprintf("Tile %d | %s | %s | microclimate: %s",
                        tile_i, period_label, hgt_lbl, conditionMessage(e)))
        log_df <<- rbind(log_df, .log_row(tile_i, period_label, hgt_lbl,
                                          "microclimate",
                                          paste0("error: ", conditionMessage(e)),
                                          mc_file))
      })
    }

    if (!is.null(smod)) rm(smod)
    invisible(gc())
  }

  cat(sprintf("\n--- run_micro_big_nichemap complete: %d tile(s), %d period(s) ---\n",
              n_tiles, n_dates))
  invisible(log_df)
}
