.get_total_ram <- function() {
  if (.Platform$OS.type == "windows") {
    raw <- system("wmic OS get TotalVisibleMemorySize /Value", intern = TRUE)
    kb  <- as.numeric(gsub("[^0-9]", "", raw[grepl("=", raw)]))
    kb * 1024
  } else {
    lines <- readLines("/proc/meminfo", n = 1)
    kb    <- as.numeric(gsub("[^0-9]", "", lines[1]))
    kb * 1024
  }
}

#' Create Tiles for Large Raster Processing
#'
#' Creates a tile raster for processing large rasters in memory-efficient chunks.
#' When \code{tile_dims} is \code{NULL} (default), automatically selects the
#' largest tile size whose peak in-memory footprint stays within \code{mem_fraction}
#' of total system RAM. Peak usage accounts for \code{mout} (10 arrays),
#' optionally \code{smod} (6 arrays) when \code{snow_modeling = TRUE}, and
#' \code{micropointa} (one element per coarse cell) — all sized to the buffered
#' tile extent (\code{tiles_proc}).
#'
#' @param coarse_dem A \code{SpatRaster} at the weather-data resolution used as
#'   the tile grid. Must match the resolution of climate inputs passed to
#'   downstream micropoint model functions.
#' @param fine_dem A \code{SpatRaster} at the fine (e.g. 30 m) resolution.
#'   Used to compute the fine-to-coarse scale ratio for memory estimation and
#'   to resample the output tile ID raster.
#' @param dates Length-2 vector of class \code{Date} giving the start and end
#'   of the modeling period e.g. \code{as.Date(c("2020-01-01", "2020-12-31"))}.
#'   Used to compute the number of hourly time steps for memory estimation.
#' @param tile_dims Either \code{NULL} (default), a single numeric value, or a
#'   vector of two numeric values specifying \code{c(nrow, ncol)} of each tile
#'   in coarse DEM cells. When \code{NULL} the memory-based auto-sizing is used.
#'   When provided, a warning is issued if the tile exceeds the memory budget but
#'   processing continues.
#' @param buffer_size Integer. Number of coarse DEM cells added as buffer around
#'   each tile for \code{tiles_proc}. Default is 1. Memory estimation uses the
#'   buffered tile size as peak usage occurs on \code{tiles_proc}, not
#'   \code{tiles_core}.
#' @param snow_modeling Logical. If \code{TRUE}, the memory estimate includes
#'   \code{smod} (6 arrays) in addition to \code{mout} (10 arrays). Default is
#'   \code{FALSE}.
#' @param mem_fraction Numeric. Fraction of total system RAM to target for peak
#'   tile memory usage. Default is 0.7.
#' @param return_tiles_rast Logical. If \code{TRUE}, includes the fine-resolution
#'   tile ID raster (\code{tiles_rast}) in the returned list. Default is
#'   \code{FALSE}.
#' @param output_path Character. File path to save the fine-resolution tile ID
#'   raster. If \code{NULL} (default), not saved.
#'
#' @return A list containing:
#'   \item{tiles_proc}{Matrix of tile extents including buffer for processing}
#'   \item{tiles_core}{Matrix of tile extents without buffer (core areas)}
#'   \item{tiles_rast}{Fine-resolution \code{SpatRaster} with tile IDs (only if
#'     \code{return_tiles_rast = TRUE})}
#'
#' @details
#' Memory estimation formula per tile:
#' \code{n_eff_coarse * n_hours * (8 * n_arrays * ratio_r * ratio_c + 257)}
#' where \code{n_eff_coarse = (tile_nrow + 2*buffer_size) * (tile_ncol + 2*buffer_size)},
#' \code{n_arrays} is 16 (snow) or 10 (no snow), \code{ratio_r}/\code{ratio_c}
#' are the fine-to-coarse row/column ratios, and 257 bytes per coarse cell per
#' hour is an empirical estimate for \code{micropointa} overhead.
#'
#' @examples
#' \dontrun{
#'   # Memory-based auto sizing (recommended)
#'   tiles <- create_tiles(coarse_dem, fine_dem,
#'                         dates = as.Date(c("2020-01-01", "2020-12-31")))
#'
#'   # With snow modeling — more conservative tile size
#'   tiles <- create_tiles(coarse_dem, fine_dem,
#'                         dates = as.Date(c("2020-10-01", "2021-03-31")),
#'                         snow_modeling = TRUE)
#'
#'   # Manual override with 2-cell buffer, save fine-res tile raster
#'   tiles <- create_tiles(coarse_dem, fine_dem,
#'                         dates = as.Date(c("2020-01-01", "2020-12-31")),
#'                         tile_dims = c(4, 4),
#'                         buffer_size = 2,
#'                         output_path = "./Data/TileRast.tif")
#' }
#'
#' @export
create_tiles <- function(coarse_dem,
                         fine_dem,
                         dates,
                         tile_dims         = NULL,
                         buffer_size       = 1,
                         snow_modeling     = FALSE,
                         mem_fraction      = 0.7,
                         return_tiles_rast = FALSE,
                         output_path       = NULL) {

  dates   <- as.Date(dates)
  n_hours <- as.integer(difftime(dates[2], dates[1], units = "days") + 1L) * 24L

  n_arrays   <- if (snow_modeling) 16L else 10L
  mem_budget <- .get_total_ram() * mem_fraction

  ratio_r <- nrow(fine_dem) / nrow(coarse_dem)
  ratio_c <- ncol(fine_dem) / ncol(coarse_dem)

  bytes_per_eff_coarse <- n_hours * (8 * n_arrays * ratio_r * ratio_c + 257)
  max_eff_coarse       <- mem_budget / bytes_per_eff_coarse

  .est_mem_gb <- function(tnr, tnc) {
    n_eff <- (tnr + 2 * buffer_size) * (tnc + 2 * buffer_size)
    n_eff * bytes_per_eff_coarse / 1024^3
  }

  if (is.null(tile_dims)) {

    div_r <- which(nrow(coarse_dem) %% seq_len(nrow(coarse_dem)) == 0)
    div_c <- which(ncol(coarse_dem) %% seq_len(ncol(coarse_dem)) == 0)

    best_area <- 0L
    tile_nrow <- NA_integer_
    tile_ncol <- NA_integer_

    for (r in div_r) {
      for (c in div_c) {
        n_eff <- (r + 2 * buffer_size) * (c + 2 * buffer_size)
        if (n_eff <= max_eff_coarse && r * c > best_area) {
          best_area <- r * c
          tile_nrow <- r
          tile_ncol <- c
        }
      }
    }

    if (is.na(tile_nrow)) {
      stop(sprintf(
        "Even a 1x1 coarse tile (buffered to %dx%d) exceeds %.0f%% of RAM (%.1f GB).\n  Reduce mem_fraction, use a shorter date range, or add more RAM.",
        1 + 2 * buffer_size, 1 + 2 * buffer_size,
        mem_fraction * 100,
        mem_budget / 1024^3
      ))
    }

    cat(sprintf(
      "Auto tile size: %d x %d coarse cells (~%.2f GB estimated peak memory)\n",
      tile_nrow, tile_ncol, .est_mem_gb(tile_nrow, tile_ncol)
    ))

  } else if (length(tile_dims) == 1) {
    tile_nrow <- tile_dims
    tile_ncol <- tile_dims
  } else if (length(tile_dims) == 2) {
    tile_nrow <- tile_dims[1]
    tile_ncol <- tile_dims[2]
  } else {
    stop("tile_dims must be NULL, a single numeric value, or a vector of length 2.")
  }

  if (!is.null(tile_dims)) {
    est_gb <- .est_mem_gb(tile_nrow, tile_ncol)
    if (est_gb > mem_budget / 1024^3) {
      warning(sprintf(
        "Specified tile (%dx%d) estimated at %.2f GB, exceeding %.0f%% RAM budget (%.1f GB). Proceeding anyway.",
        tile_nrow, tile_ncol, est_gb, mem_fraction * 100, mem_budget / 1024^3
      ))
    } else {
      cat(sprintf(
        "Tile size: %d x %d coarse cells (~%.2f GB estimated peak memory)\n",
        tile_nrow, tile_ncol, est_gb
      ))
    }
  }

  if (nrow(coarse_dem) %% tile_nrow != 0 || ncol(coarse_dem) %% tile_ncol != 0) {
    stop(sprintf(
      "Tile dimensions (%d, %d) do not divide evenly into coarse DEM dimensions (%d, %d).",
      tile_nrow, tile_ncol, nrow(coarse_dem), ncol(coarse_dem)
    ))
  }

  n_tiles_r <- nrow(coarse_dem) / tile_nrow
  n_tiles_c <- ncol(coarse_dem) / tile_ncol

  tiles_rast_coarse <- terra::rast(
    nrow   = n_tiles_r,
    ncol   = n_tiles_c,
    extent = terra::ext(coarse_dem),
    crs    = terra::crs(coarse_dem),
    vals   = seq_len(n_tiles_r * n_tiles_c)
  )

  tiles_proc <- terra::getTileExtents(coarse_dem, tiles_rast_coarse, buffer = buffer_size, extend = TRUE)
  tiles_core <- terra::getTileExtents(coarse_dem, tiles_rast_coarse, buffer = 0,           extend = TRUE)

  tiles_rast <- terra::rast(fine_dem)
  terra::values(tiles_rast) <- NA_integer_
  for (i in seq_len(nrow(tiles_core))) {
    ext_i    <- terra::ext(tiles_core[i, "xmin"], tiles_core[i, "xmax"],
                            tiles_core[i, "ymin"], tiles_core[i, "ymax"])
    cell_ids <- terra::cells(tiles_rast, ext_i, touches = TRUE)[, "cell"]
    tiles_rast[cell_ids] <- i
  }

  cat(sprintf("Created %d tile(s) (%d x %d grid).\n", n_tiles_r * n_tiles_c, n_tiles_r, n_tiles_c))

  if (!is.null(output_path)) {
    terra::writeRaster(tiles_rast, output_path, overwrite = TRUE)
    cat(sprintf("Tile raster saved to: %s\n", output_path))
  }

  result <- list(tiles_proc = tiles_proc, tiles_core = tiles_core)
  if (return_tiles_rast) result$tiles_rast <- tiles_rast

  return(invisible(result))
}

#' Run Microclimf Micropoint Model - Preprocessing for NicheMapper
#'
#' Preprocessing step that runs the micropoint model at multiple soil depths to
#' prepare microclimate model inputs. This is the first step in the
#' microclimate modeling workflow, followed by optional snow modeling and then
#' the full microclimate model.
#'
#' Function arguments are those required to run the microclimf::runpointmodela
#' function, as well as additional functionality. microclimf::runpointmodela
#' documentation should be referenced for details on the required parameters
#' and their formats.
#'
#' @param climarrayr List of wrapped terra SpatRasters containing climate
#'   variables (e.g., precipitation, temperature, wind speed). Each raster
#'   should have the same extent, resolution, and temporal dimensions.
#' @param tme POSIXlt object representing the time dimension of the climate
#'   data. Should match the temporal dimension of climarrayr.
#' @param dtm terra SpatRaster of the digital terrain model (elevation). Should
#'   align with the extent and resolution of climate data.
#' @param vegp List or data frame containing vegetation parameters required by
#'   microclimf::runpointmodela (e.g., vegetation height, ground cover).
#' @param soilc List or data frame containing soil parameters required by
#'   microclimf::runpointmodela (e.g., soil texture, conductivity, heat
#'   capacity).
#' @param output_dir Character. Base directory where model outputs will be
#'   saved. Subdirectories are created as
#'   \code{output_dir/study_area/period_label/}.
#' @param reqhgt Numeric. Desired height above ground (in meters) for
#'   above-ground microclimate predictions. Default is 2.
#' @param zref Numeric. Reference height (in meters) for ambient measurements.
#'   Default is 2.
#' @param windhgt Numeric. Height (in meters) at which wind speed is measured
#'   in input data. Default is 10.
#' @param matemp Numeric. Mean annual temperature (°C). If \code{NA}
#'   (default), will be calculated from \code{climarrayr}.
#' @param maxiter Numeric. Maximum number of iterations for model convergence.
#'   Default is 20.
#' @param period_label Character. Date-range label appended to output file
#'   names (e.g., \code{"20200101_to_20201231"}). Optional; if \code{NULL} no
#'   label is added.
#' @param study_area Character. Study area identifier prepended to output file
#'   names. Optional.
#' @param ... Additional advanced options for single-period HPC distribution.
#'   Accepted values are \code{clust_array_arg} and \code{clust_array_size}.
#'   For multi-period HPC distribution use \code{\link{run_iter_micropoint}}.
#'
#' @return A data frame with columns:
#'   \item{depth}{Character. Soil depth in centimeters (negative for below
#'     surface) or the above-ground height in meters.}
#'   \item{file_path}{Character. Full file path where the model output was
#'     saved.}
#'
#' @details
#' This function runs microclimate preprocessing at 9 predefined soil depths:
#' 1.5 cm, 5 cm, 10 cm, 15 cm, 20 cm, 30 cm, 50 cm, 100 cm, and 200 cm
#' below surface.
#'
#' **Output files:** Two types of outputs are generated:
#' \itemize{
#'   \item Above-ground: \code{{study_area}_AbvGrd_MicropointModel_{period_label}.RDS}
#'   \item Below-ground: \code{{study_area}_BlwGrd_{depth_mm}_MicropointModel_{period_label}.RDS}
#'     where depth is zero-padded millimetres (e.g., \code{BlwGrd_0015} for
#'     1.5 cm).
#' }
#'
#' **Single-period HPC usage**
#'
#' To distribute the 10 tasks (1 above-ground + 9 soil depths) across a SLURM
#' array for a single period, pass \code{clust_array_arg} and
#' \code{clust_array_size} via \code{...}:
#' \preformatted{
#'   #SBATCH --array=1-9
#'   Rscript script.R --clust_array_arg=$SLURM_ARRAY_TASK_ID \
#'                    --clust_array_size=9
#' }
#' For multi-period HPC with an array larger than 9, see
#' \code{\link{run_iter_micropoint}}.
#'
#' @examples
#' \dontrun{
#'   # Basic usage
#'   log_df <- run_micropoint_NicheMapPrep(
#'     climarrayr = climate_list,
#'     tme        = time_vector,
#'     dtm        = dem_raster,
#'     vegp       = vegetation_params,
#'     soilc      = soil_params,
#'     output_dir = "./microclim_output"
#'   )
#'
#'   # With period label and study area
#'   log_df <- run_micropoint_NicheMapPrep(
#'     climarrayr   = climate_list,
#'     tme          = time_vector,
#'     dtm          = dem_raster,
#'     vegp         = vegetation_params,
#'     soilc        = soil_params,
#'     output_dir   = "./microclim_output",
#'     study_area   = "GMU1",
#'     period_label = "20200101_to_20201231"
#'   )
#' }
#'
#' @seealso
#' \code{\link[microclimf]{runpointmodela}} for the underlying microclimate
#' model. \code{\link{run_iter_micropoint}} for multi-period iteration and
#' large SLURM array support.
#'
#' @export
run_micropoint_NicheMapPrep <- function(
  climarrayr,
  tme,
  dtm,
  vegp,
  soilc,
  output_dir,
  reqhgt       = 2,
  zref         = 2,
  windhgt      = 10,
  matemp       = NA,
  maxiter      = 20,
  period_label = NULL,
  study_area   = NULL,
  ...
) {

  dots <- list(...)
  allowed <- c("clust_array_arg", "clust_array_size")
  unknown <- setdiff(names(dots), allowed)
  if (length(unknown) > 0) {
    stop("Unknown argument(s): ", paste(unknown, collapse = ", "))
  }

  clust_array_arg  <- dots$clust_array_arg
  clust_array_size <- dots$clust_array_size

  # Validate cluster array arguments
  if (!is.null(clust_array_arg) &&
      (!is.numeric(clust_array_arg) || length(clust_array_arg) != 1)) {
    stop("clust_array_arg must be a single numeric value or NULL")
  }
  if (!is.null(clust_array_size) &&
      (!is.numeric(clust_array_size) || length(clust_array_size) != 1)) {
    stop("clust_array_size must be a single numeric value or NULL")
  }
  if (!is.null(clust_array_arg) && is.null(clust_array_size)) {
    stop("clust_array_size must be provided when clust_array_arg is set")
  }
  if (!is.null(clust_array_arg) &&
      (clust_array_arg < 1 || clust_array_arg > clust_array_size)) {
    stop("clust_array_arg must be between 1 and clust_array_size")
  }

  # Initialize log
  log_entries <- data.frame(
    depth     = character(),
    file_path = character(),
    stringsAsFactors = FALSE
  )

  # Build output directory
  path_parts <- c(output_dir)
  if (!is.null(study_area))   path_parts <- c(path_parts, study_area)
  path_parts <- c(path_parts, "Micropoint_Model")
  if (!is.null(period_label)) path_parts <- c(path_parts, period_label)
  output_dir_final <- do.call(file.path, as.list(path_parts))
  dir.create(output_dir_final, recursive = TRUE, showWarnings = FALSE)

  prefix <- if (!is.null(study_area)) paste0(study_area, "_") else ""
  suffix <- if (!is.null(period_label)) paste0("_", period_label) else ""

  # Determine if above-ground model should run for this job
  run_abvgrd <- is.null(clust_array_arg) || clust_array_arg == 1L

  if (run_abvgrd) {
    cat(sprintf("  Running above-ground (%.1f m)\n", reqhgt))
    micropointa <- microclimf::runpointmodela(
      climarrayr, tme, reqhgt, dtm, vegp, soilc,
      matemp = matemp, zref = zref, windhgt = windhgt,
      soilm = NA, dTmx = 25, maxiter = maxiter, yearG = TRUE
    )
    outf <- file.path(output_dir_final,
      sprintf("%sAbvGrd_MicropointModel%s.RDS", prefix, suffix))
    readr::write_rds(micropointa, outf)
    log_entries <- rbind(log_entries,
      data.frame(depth = as.character(reqhgt), file_path = outf,
                 stringsAsFactors = FALSE))
    rm(micropointa); gc()
  }

  # Soil depths
  sdepth    <- c(1.5, 5, 10, 15, 20, 30, 50, 100, 200) / -100
  sdepth_cm <- c(1.5, 5, 10, 15, 20, 30, 50, 100, 200) * -1

  d.task    <- if (is.null(clust_array_size)) sdepth    else
    sdepth[rep(seq_len(clust_array_size), length.out = length(sdepth)) == clust_array_arg]
  d.task_cm <- if (is.null(clust_array_size)) sdepth_cm else
    sdepth_cm[rep(seq_len(clust_array_size), length.out = length(sdepth_cm)) == clust_array_arg]

  for (i in seq_along(d.task)) {
    d    <- d.task[i]
    d_cm <- d.task_cm[i]
    cat(sprintf("  Running below-ground (%.1f cm)\n", abs(d_cm)))
    micropointa <- microclimf::runpointmodela(
      climarrayr, tme, reqhgt = d, dtm, vegp, soilc,
      matemp = matemp, zref = zref, windhgt = windhgt,
      soilm = NA, dTmx = 25, maxiter = maxiter, yearG = TRUE
    )
    outf <- file.path(output_dir_final,
      sprintf("%sBlwGrd_%04d_MicropointModel%s.RDS",
              prefix, as.integer(-1000 * d), suffix))
    readr::write_rds(micropointa, outf)
    log_entries <- rbind(log_entries,
      data.frame(depth = as.character(d_cm), file_path = outf,
                 stringsAsFactors = FALSE))
    rm(micropointa); gc()
  }

  return(log_entries)
}

#' Run Microclimf Micropoint Model Iteratively Across Date Ranges
#'
#' Iterates \code{\link{run_micropoint_NicheMapPrep}} across one or more date
#' ranges, optionally distributing the work across a SLURM array job of any
#' size. Each combination of (period, model height) is treated as an
#' independent task and assigned to exactly one array job — no two nodes ever
#' run the same height for the same period.
#'
#' Input files must follow the naming conventions produced by
#' \code{\link{package_climate}} and \code{\link{package_veg_soil}}:
#' \itemize{
#'   \item \code{{study_area}_Climate_{start}_to_{end}.RDS}
#'   \item \code{{study_area}_VegPara_{start}_to_{end}.RDS}
#'   \item \code{{study_area}_SoilPara_{start}_to_{end}.RDS}
#' }
#' where \code{start}/\code{end} are formatted \code{YYYYMMDD}.
#'
#' @param dates Either a \code{data.frame} with columns \code{Start_Dates} and
#'   \code{End_Dates} (one row per period), or a length-2 \code{Date} vector
#'   for a single period.
#' @param clim_dir Character. Directory containing packaged climate RDS files
#'   from \code{\link{package_climate}}.
#' @param dtm \code{terra} SpatRaster of the digital terrain model.
#' @param vegp_dir Character. Directory containing vegetation parameter RDS
#'   files from \code{\link{package_veg_soil}}.
#' @param soilc_dir Character. Directory containing soil parameter RDS files
#'   from \code{\link{package_veg_soil}}.
#' @param output_dir Character. Base directory for model outputs. A
#'   subdirectory named \code{study_area/period_label} is created for each
#'   period.
#' @param reqhgt Numeric. Above-ground prediction height in metres. Default 2.
#' @param zref Numeric. Reference height for ambient measurements in metres.
#'   Default 2.
#' @param windhgt Numeric. Wind measurement height in metres. Default 10.
#' @param matemp Numeric. Mean annual temperature (°C). \code{NA} (default)
#'   estimates it internally.
#' @param maxiter Numeric. Maximum model iterations. Default 20.
#' @param study_area Character. Optional study area identifier used in input
#'   and output file names.
#' @param ... Advanced HPC options for distributing work across a SLURM array
#'   job. See the HPC section in Details. Most users can ignore these.
#'
#' @return
#' A data frame with columns \code{depth} and \code{file_path} covering every
#' task completed by this call (i.e., this array job's assigned tasks when
#' running under SLURM, or all tasks when running sequentially).
#'
#' @details
#' **Task definition**
#'
#' Each period contributes 10 tasks: one above-ground model (depth index 0)
#' and nine below-ground depths (1.5, 5, 10, 15, 20, 30, 50, 100, 200 cm).
#' For \eqn{N} periods the total task count is \eqn{10N}.
#'
#' **Task-to-job assignment**
#'
#' Tasks are numbered sequentially across periods (period 1 depths 0-9,
#' period 2 depths 0-9, …) and assigned to jobs by round-robin:
#' \deqn{\text{job}(i) = ((i - 1) \bmod \texttt{clust\_array\_size}) + 1}
#' This guarantees:
#' \itemize{
#'   \item No two jobs share the same (period, depth) combination.
#'   \item Loads are balanced as evenly as possible regardless of array size.
#'   \item An array size larger than \eqn{10N} is accepted; surplus jobs
#'     receive no tasks and exit cleanly with a message.
#' }
#'
#' Within each job, tasks are grouped by period so input files are loaded only
#' once per period.
#'
#' **SLURM script example (3 periods, array of 15)**
#' \preformatted{
#'   #!/bin/bash
#'   #SBATCH --array=1-15
#'   Rscript run_micropoint.R \
#'     --clust_array_arg=$SLURM_ARRAY_TASK_ID \
#'     --clust_array_size=15
#' }
#'
#' @examples
#' \dontrun{
#'
#' # --- Sequential (no HPC) ---
#' log_df <- run_iter_micropoint(
#'   dates      = as.Date(c("2020-01-01", "2021-12-31")),
#'   clim_dir   = "./Data/Weather/GMU1_Pkg",
#'   dtm        = terra::rast("./Data/DEM/DEM_GLO30.tif"),
#'   vegp_dir   = "./Data/VegPara",
#'   soilc_dir  = "./Data/SoilPara",
#'   output_dir = "./Microclim_out/PointModels",
#'   study_area = "GMU1"
#' )
#'
#' # --- Multiple periods via data.frame ---
#' date_df <- data.frame(
#'   Start_Dates = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01")),
#'   End_Dates   = as.Date(c("2020-12-31", "2021-12-31", "2022-12-31"))
#' )
#'
#' # --- SLURM array of 15 across 3 periods (30 total tasks) ---
#' log_df <- run_iter_micropoint(
#'   dates            = date_df,
#'   clim_dir         = "./Data/Weather/GMU1_Pkg",
#'   dtm              = terra::rast("./Data/DEM/DEM_GLO30.tif"),
#'   vegp_dir         = "./Data/VegPara",
#'   soilc_dir        = "./Data/SoilPara",
#'   output_dir       = "./Microclim_out/PointModels",
#'   study_area       = "GMU1",
#'   clust_array_arg  = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID")),
#'   clust_array_size = 15
#' )
#'
#' head(log_df)
#' }
#'
#' @seealso
#' \code{\link{run_micropoint_NicheMapPrep}} for single-period execution.
#' \code{\link{package_climate}} and \code{\link{package_veg_soil}} for
#' producing the required input files.
#'
#' @export
run_iter_micropoint <- function(dates,
                                clim_dir,
                                dtm,
                                vegp_dir,
                                soilc_dir,
                                output_dir,
                                reqhgt     = 2,
                                zref       = 2,
                                windhgt    = 10,
                                matemp     = NA,
                                maxiter    = 20,
                                study_area = NULL,
                                ...) {

  # --- Validate dates ----------------------------------------------------------
  if (is.data.frame(dates)) {
    if (!all(c("Start_Dates", "End_Dates") %in% names(dates)))
      stop("dates data.frame must contain columns 'Start_Dates' and 'End_Dates'")
    date_ranges <- dates
  } else if (is.vector(dates) && length(dates) == 2) {
    date_ranges <- data.frame(
      Start_Dates = as.Date(dates[1]),
      End_Dates   = as.Date(dates[2]),
      stringsAsFactors = FALSE
    )
  } else {
    stop("dates must be a data.frame with Start_Dates/End_Dates columns or a length-2 Date vector")
  }

  # --- Parse HPC arguments from ... --------------------------------------------
  dots <- list(...)
  allowed <- c("clust_array_arg", "clust_array_size")
  unknown <- setdiff(names(dots), allowed)
  if (length(unknown) > 0)
    stop("Unknown argument(s): ", paste(unknown, collapse = ", "))

  clust_array_arg  <- dots$clust_array_arg
  clust_array_size <- dots$clust_array_size

  # --- Validate HPC arguments --------------------------------------------------
  if (!is.null(clust_array_arg)) {
    if (!is.numeric(clust_array_arg) || length(clust_array_arg) != 1)
      stop("clust_array_arg must be a single integer or NULL")
    if (is.null(clust_array_size))
      stop("clust_array_size must be provided when clust_array_arg is set")
    if (!is.numeric(clust_array_size) || length(clust_array_size) != 1 ||
        clust_array_size < 1)
      stop("clust_array_size must be a single positive integer")
    if (clust_array_arg < 1 || clust_array_arg > clust_array_size)
      stop("clust_array_arg must be between 1 and clust_array_size")
  }

  # --- Define all tasks --------------------------------------------------------
  # depth_idx 0 = above-ground; 1-9 = soil depths in sdepth order
  sdepth    <- c(1.5, 5, 10, 15, 20, 30, 50, 100, 200) / -100   # metres, negative
  sdepth_cm <- c(1.5, 5, 10, 15, 20, 30, 50, 100, 200) * -1     # cm, negative (for log)

  N_periods   <- nrow(date_ranges)
  all_tasks   <- data.frame(
    period_idx = rep(seq_len(N_periods), each = 10L),
    depth_idx  = rep(0:9, times = N_periods),
    stringsAsFactors = FALSE
  )
  total_tasks <- nrow(all_tasks)

  # --- Select this job's tasks -------------------------------------------------
  if (!is.null(clust_array_arg)) {
    if (clust_array_size > total_tasks)
      warning(sprintf(
        "clust_array_size (%d) exceeds total tasks (%d). Some array jobs will have no work.",
        clust_array_size, total_tasks))
    assignment <- ((seq_len(total_tasks) - 1L) %% as.integer(clust_array_size)) + 1L
    my_tasks   <- all_tasks[assignment == as.integer(clust_array_arg), , drop = FALSE]
  } else {
    my_tasks <- all_tasks
  }

  if (nrow(my_tasks) == 0L) {
    message(sprintf(
      "Array job %d has no assigned tasks (total tasks = %d, array size = %d). Nothing to do.",
      clust_array_arg, total_tasks, clust_array_size))
    return(invisible(data.frame(
      depth = character(), file_path = character(), stringsAsFactors = FALSE)))
  }

  prefix <- if (!is.null(study_area)) paste0(study_area, "_") else ""

  log_all <- list()

  # --- Process tasks, grouped by period to minimise data re-loading ------------
  for (pi in unique(my_tasks$period_idx)) {

    period_tasks <- my_tasks[my_tasks$period_idx == pi, , drop = FALSE]

    start_date   <- as.Date(date_ranges$Start_Dates[pi])
    end_date     <- as.Date(date_ranges$End_Dates[pi])
    period_label <- sprintf("%s_to_%s",
                            format(start_date, "%Y%m%d"),
                            format(end_date,   "%Y%m%d"))

    cat(sprintf("\n--- Period %d/%d: %s (%d task(s) this job) ---\n",
                pi, N_periods, period_label, nrow(period_tasks)))

    # Resolve input files
    climf  <- file.path(clim_dir,  sprintf("%sClimate_%s.RDS",  prefix, period_label))
    vegf   <- file.path(vegp_dir,  sprintf("%sVegPara_%s.RDS",  prefix, period_label))
    soilf  <- file.path(soilc_dir, sprintf("%sSoilPara_%s.RDS", prefix, period_label))

    if (!file.exists(climf))  stop("Climate file not found: ",    climf)
    if (!file.exists(vegf))   stop("Vegetation file not found: ", vegf)
    if (!file.exists(soilf))  stop("Soil file not found: ",       soilf)

    clim <- readr::read_rds(climf)
    veg  <- readr::read_rds(vegf)
    soil <- readr::read_rds(soilf)
    tme  <- as.POSIXlt(terra::time(terra::unwrap(clim$precip)))

    # Create output directory for this period
    out_parts <- c(output_dir)
    if (!is.null(study_area)) out_parts <- c(out_parts, study_area)
    out_parts <- c(out_parts, "Micropoint_Models", period_label)
    out_dir   <- do.call(file.path, as.list(out_parts))
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    period_log <- list()

    for (j in seq_len(nrow(period_tasks))) {

      di <- period_tasks$depth_idx[j]

      if (di == 0L) {
        # Above-ground model
        cat(sprintf("  Running above-ground (%.1f m)\n", reqhgt))
        micropointa <- microclimf::runpointmodela(
          clim, tme, reqhgt, dtm, veg, soil,
          matemp = matemp, zref = zref, windhgt = windhgt,
          soilm = NA, dTmx = 25, maxiter = maxiter, yearG = TRUE
        )
        outf <- file.path(out_dir,
          sprintf("%sAbvGrd_MicropointModel_%s.RDS", prefix, period_label))
        readr::write_rds(micropointa, outf)
        period_log[[j]] <- data.frame(
          depth = as.character(reqhgt), file_path = outf,
          stringsAsFactors = FALSE)
        rm(micropointa); gc()

      } else {
        # Below-ground model
        d    <- sdepth[di]
        d_cm <- sdepth_cm[di]
        cat(sprintf("  Running below-ground (%.1f cm)\n", abs(d_cm)))
        micropointa <- microclimf::runpointmodela(
          clim, tme, reqhgt = d, dtm, veg, soil,
          matemp = matemp, zref = zref, windhgt = windhgt,
          soilm = NA, dTmx = 25, maxiter = maxiter, yearG = TRUE
        )
        outf <- file.path(out_dir,
          sprintf("%sBlwGrd_%04d_MicropointModel_%s.RDS",
                  prefix, as.integer(-1000 * d), period_label))
        readr::write_rds(micropointa, outf)
        period_log[[j]] <- data.frame(
          depth = as.character(d_cm), file_path = outf,
          stringsAsFactors = FALSE)
        rm(micropointa); gc()
      }
    }

    log_all[[pi]] <- do.call(rbind, period_log)
    rm(clim, veg, soil, tme); gc()
  }

  return(do.call(rbind, log_all))

}
