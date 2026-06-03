#' Create Tiles for Large Raster Processing
#'
#' Creates a tile raster for processing large rasters in memory-efficient chunks.
#' Useful for working with large spatial datasets where processing the full raster
#' would exceed memory limitations. The DEM raster should match the resolution of
#' weather data being used in downstream processing.
#'
#' @param dem A terra SpatRaster representing the digital elevation model. This raster
#'   should have a coarse resolution matching the weather data that will be used in
#'   the analysis.
#' @param tile_dims Either NULL (default), a single numeric value, or a vector of
#'   two numeric values. If NULL, automatically calculates the smallest tile dimensions
#'   that divide evenly into the raster. If a single number (e.g., 2), creates square
#'   tiles of size 2×2. If a vector c(nrow, ncol), creates rectangular tiles with
#'   specified dimensions.
#' @param buffer_size Numeric. Number of cells to use as buffer around each tile.
#'   Default is 1. Used to account for edge effects during processing.
#' @param return_tiles_rast Logical. If TRUE, includes the tile raster (tiles_rast) in
#'   the returned list. Default is FALSE.
#' @param output_path Character. File path where the tile raster should be saved
#'   (e.g., "./Data/TileProcessingRast.tif"). If NULL (default), the tile raster
#'   is not saved. If a path is provided, the tile raster is automatically saved.
#'
#' @return A list containing:
#'   \item{tiles_proc}{SpatRaster with tile extents including buffer for processing}
#'   \item{tiles_core}{SpatRaster with tile extents without buffer (core processing areas)}
#'   \item{tiles_rast}{SpatRaster with tile IDs (only if return_tiles_rast = TRUE)}
#'
#' @details
#' The function creates two sets of tile extents:
#' - **tiles_proc**: Processing tiles with specified buffer to account for edge effects
#' - **tiles_core**: Core tiles without buffer, representing the true processing area
#'
#' The function will print a message reminding users that the DEM resolution must
#' match the weather data resolution for accurate microclimate modeling.
#'
#' @examples
#' \dontrun{
#'   # Automatic tile size calculation with default 1-cell buffer
#'   tiles_list <- create_tiles(dem)
#'
#'   # Square 2x2 tiles with default buffer
#'   tiles_list <- create_tiles(dem, tile_dims = 2)
#'
#'   # Rectangular 2x3 tiles with 2-cell buffer, returning tile raster
#'   tiles_list <- create_tiles(dem, tile_dims = c(2, 3), buffer_size = 2, return_tiles_rast = TRUE)
#'
#'   # Save tile raster to file
#'   tiles_list <- create_tiles(dem,
#'                              tile_dims = 2,
#'                              buffer_size = 1,
#'                              output_path = "./Data/TileProcessingRast.tif")
#' }
#'
create_tiles <- function(dem, tile_dims = NULL, buffer_size = 1, return_tiles_rast = FALSE, output_path = NULL) {

  # Print informational message
  message("NOTE: DEM resolution must match weather data resolution for accurate microclimate modeling.")

  # Determine tile dimensions
  if (is.null(tile_dims)) {
    # Auto-calculate smallest tile size that divides evenly
    tile_nrow <- 1
    tile_ncol <- 1

    # Find smallest divisor for nrow
    for (i in 2:nrow(dem)) {
      if (nrow(dem) %% i == 0) {
        tile_nrow <- i
        cat('Tiles row size:', i, "cells\n")
        break
      }
    }

    # Find smallest divisor for ncol
    for (i in 2:ncol(dem)) {
      if (ncol(dem) %% i == 0) {
        tile_ncol <- i
        cat('Tiles column size:', i, "cells\n")
        break
      }
    }
  } else if (length(tile_dims) == 1) {
    # Single value: create square tiles
    tile_nrow <- tile_dims
    tile_ncol <- tile_dims
  } else if (length(tile_dims) == 2) {
    # Vector: separate nrow and ncol
    tile_nrow <- tile_dims[1]
    tile_ncol <- tile_dims[2]
  } else {
    stop("tile_dims must be NULL, a single numeric value, or a vector of length 2.")
  }

  # Validate that tile dimensions divide evenly
  if (nrow(dem) %% tile_nrow != 0 || ncol(dem) %% tile_ncol != 0) {
    stop(sprintf("Tile dimensions (%d, %d) do not divide evenly into DEM dimensions (%d, %d).",
                 tile_nrow, tile_ncol, nrow(dem), ncol(dem)))
  }

  # Create tile raster
  tiles.rast <- terra::rast(
    nrow = nrow(dem)/tile_nrow,
    ncol = ncol(dem)/tile_ncol,
    extent = terra::ext(dem),
    crs = terra::crs(dem),
    vals = 1:((nrow(dem)/tile_nrow) * (ncol(dem)/tile_ncol))
  )

  # Get tile extents with and without buffer
  tiles_proc <- terra::getTileExtents(dem, tiles.rast, buffer = buffer_size, extend = TRUE)
  tiles_core <- terra::getTileExtents(dem, tiles.rast, buffer = 0, extend = TRUE)

  # Save tile raster if output_path is provided
  if (!is.null(output_path)) {
    terra::writeRaster(tiles.rast, output_path, overwrite = TRUE)
    message(sprintf("Tile raster saved to: %s", output_path))
  }

  # Build return list
  result <- list(
    tiles_proc = tiles_proc,
    tiles_core = tiles_core
  )

  # Add tiles_rast to output if requested
  if (return_tiles_rast) {
    result$tiles_rast <- tiles.rast
  }

  return(result)
}

#' Run Microclimf Micropoint Model - Preprocessing for NicheMapper
#'
#' Preprocessing step that runs the micropoint model at multiple soil depths to
#' prepare microclimate model inputs. This is the first step in the
#' microclimate modeling workflow, followed by optional snow modeling and then
#' the full microclimate model.
#'
#' Function arguments are those required to run the microclimf::runpointmodela function, as well as additional
#' functionality. microclimf::runpointmodela documentation should be referenced for details on the required parameters and their formats.
#'
#' @param climarrayr List of wrapped terra SpatRasters containing climate variables
#'   (e.g., precipitation, temperature, wind speed). Each raster should have the
#'   same extent, resolution, and temporal dimensions.
#' @param tme POSIXlt object representing the time dimension of the climate data.
#'   Should match the temporal dimension of climarrayr.
#' @param dtm terra SpatRaster of the digital terrain model (elevation). Should align
#'   with the extent and resolution of climate data.
#' @param vegp List or data frame containing vegetation parameters required by
#'   microclimf::runpointmodela (e.g., vegetation height, ground cover).
#' @param soilc List or data frame containing soil parameters required by
#'   microclimf::runpointmodela (e.g., soil texture, conductivity, heat capacity).
#' @param output_dir Character. Base directory where model outputs will be saved.
#'   Subdirectories will be created following the structure: output_dir/study_area/year/
#' @param reqhgt Numeric. Desired height above ground (in meters) for above-ground
#'   microclimate predictions. Default is 2 (meters above ground).
#' @param zref Numeric. Reference height (in meters) for ambient measurements. Default is 10.
#' @param windhgt Numeric. Height (in meters) at which wind speed is measured in input data.
#'   Default is 10.
#' @param matemp Numeric. Mean annual temperature (°C). If NA (default), will be
#'   calculated from climarrayr.
#' @param maxiter Numeric. Maximum number of iterations for model convergence.
#'   Default is 20.
#' @param year Character or numeric. Year identifier for output file naming. Optional;
#'   if provided, will be appended to output file names.
#' @param study_area Character. Study area identifier for output file naming. Optional;
#'   if provided, will be prepended to output file names.
#' @param ... Additional advanced options. Most users can ignore these. See details related to HPC usage for more information.
#'
#' @return A data frame with columns:
#'   \item{depth}{Numeric. Soil depth in centimeters (negative for below surface).
#'     "Above ground" for above-ground model.}
#'   \item{file_path}{Character. Full file path where the model output was saved.}
#'
#' @details
#' This function runs microclimate preprocessing at 9 predefined soil depths:
#' 1.5 cm, 5 cm, 10 cm, 15 cm, 20 cm, 30 cm, 50 cm, 100 cm, and 200 cm below surface.
#'
#' **Output files:** Two types of outputs are generated:
#' - Above-ground model: `{study_area}_AbvGrd_MicropointModel_{year}.RDS`
#' - Below-ground models: `{study_area}_BlwGrd_{depth}mm_MicropointModel_{year}.RDS`
#'   where depth is in millimeters (e.g., BlwGrd_0015_MicropointModel for 1.5 cm)
#'
#' **Advanced HPC Usage**
#'
#' For high-performance computing environments, internal options can be
#' supplied through `...` to distribute soil-depth calculations across
#' multiple jobs. These options are not documented as part of the public API.
#'
#' For large datasets, distribute processing across HPC nodes by submitting a SLURM
#' array job. Set clust_array_size = n and use
#' the array index (1-9) as clust_array_arg in each job submission:
#' \preformatted{
#'   #SBATCH --array=1-9
#'   Rscript script.R --clust_array_arg=$SLURM_ARRAY_TASK_ID --clust_array_size=9
#' }
#'
#' @examples
#' \dontrun{
#'   # Basic usage with minimal parameters
#'   log_df <- run_micropoint_NicheMapPrep(
#'     climarrayr = climate_list,
#'     tme = time_vector,
#'     dtm = dem_raster,
#'     vegp = vegetation_params,
#'     soilc = soil_params,
#'     output_dir = "./microclim_output"
#'   )
#'   print(log_df)  # View saved files and depths
#'
#'   # With study area and year identifiers
#'   log_df <- run_micropoint_NicheMapPrep(
#'     climarrayr = climate_list,
#'     tme = time_vector,
#'     dtm = dem_raster,
#'     vegp = vegetation_params,
#'     soilc = soil_params,
#'     output_dir = "./microclim_output",
#'     study_area = "GMU1",
#'     year = 2020
#'   )
#'
#' }
#'
#' @seealso
#' \code{\link[microclimf]{runpointmodela}} for the underlying microclimate model function.
#'
run_micropoint_NicheMapPrep <- function(
  climarrayr,
  tme,
  dtm,
  vegp,
  soilc,
  output_dir,
  reqhgt = 2,
  zref = 2,
  windhgt = 10,
  matemp = NA,
  maxiter = 20,
  year = NULL,
  study_area = NULL,
  ...
) {

  dots <- list(...)

  allowed <- c("clust_array_arg", "clust_array_size")

  unknown <- setdiff(names(dots), allowed)

  if (length(unknown) > 0) {
    stop(
      "Unknown argument(s): ",
      paste(unknown, collapse = ", ")
    )
  }

  clust_array_arg  <- dots$clust_array_arg
  clust_array_size <- dots$clust_array_size

  # Initialize logging data frame
  log_entries <- data.frame(
    depth = character(),
    file_path = character(),
    stringsAsFactors = FALSE
  )

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
    stop("clust_array_size must have a numeric value if clust_array_arg is provided")
  }

  if (!is.null(clust_array_arg) &&
      (clust_array_arg < 1 || clust_array_arg > clust_array_size)) {
    stop("clust_array_arg must be between 1 and clust_array_size")
  }


  # Create standardized output directory structure
  path_parts <- c(output_dir)
  if (!is.null(study_area)) path_parts <- c(path_parts, study_area)
  if (!is.null(year))       path_parts <- c(path_parts, as.character(year))
  output_dir_final <- do.call(file.path, as.list(path_parts))

  dir.create(output_dir_final, recursive = TRUE, showWarnings = FALSE)

  # Determine if above-ground model should be run
  run_abvgrd <- if (is.null(clust_array_arg)) {
    TRUE
  } else if (clust_array_arg == 1) {
    TRUE
  } else {
    FALSE
  }

  # Run above-ground micropoint model
  if (run_abvgrd) {

    micropointa <- microclimf::runpointmodela(
      climarrayr,
      tme,
      reqhgt,
      dtm,
      vegp,
      soilc,
      matemp = matemp,
      zref = zref,
      windhgt = windhgt,
      soilm = NA,
      dTmx = 25,
      maxiter = maxiter,
      yearG = TRUE
    )

    outf <- file.path(
      output_dir_final,
      paste0(
        study_area,
        if (!is.null(study_area)) {'_'} else {''},
        'AbvGrd_MicropointModel',
        if (!is.null(year)) {'_'} else {''},
        year,
        '.RDS'
      )
    )

    readr::write_rds(micropointa, outf)

    # Log the above-ground output
    log_entries <- rbind(
      log_entries,
      data.frame(
        depth = as.character(reqhgt),
        file_path = outf,
        stringsAsFactors = FALSE
      )
    )

    rm(micropointa)
    gc()
  }

  # Define soil depths (cm converted to m below surface)
  sdepth <- c(1.5, 5, 10, 15, 20, 30, 50, 100, 200) / -100
  sdepth_cm <- c(1.5, 5, 10, 15, 20, 30, 50, 100, 200) * -1  # For logging

  # Determine which depths to process
  d.task <- if (is.null(clust_array_size)) {
    sdepth
  } else {
    sdepth[rep(1:clust_array_size, length = length(sdepth)) == clust_array_arg]
  }

  # Determine corresponding depth values in cm for logging
  d.task_cm <- if (is.null(clust_array_size)) {
    sdepth_cm
  } else {
    sdepth_cm[rep(1:clust_array_size, length = length(sdepth_cm)) == clust_array_arg]
  }

  # Run below-ground micropoint models at each soil depth
  for (i in seq_along(d.task)) {
    d <- d.task[i]
    d_cm <- d.task_cm[i]

    micropointa <- microclimf::runpointmodela(
      climarrayr,
      tme,
      reqhgt = d,
      dtm,
      vegp,
      soilc,
      matemp = matemp,
      zref = zref,
      windhgt = windhgt,
      soilm = NA,
      dTmx = 25,
      maxiter = maxiter,
      yearG = TRUE
    )

    outf <- file.path(
      output_dir_final,
      paste0(
        study_area,
        if (!is.null(study_area)) {'_'} else {''},
        'BlwGrd_',
        sprintf('%04d', (-1000 * d)),
        '_MicropointModel',
        if (!is.null(year)) {'_'} else {''},
        year,
        '.RDS'
      )
    )

    readr::write_rds(micropointa, outf)

    # Log the below-ground output
    log_entries <- rbind(
      log_entries,
      data.frame(
        depth = as.character(d_cm),
        file_path = outf,
        stringsAsFactors = FALSE
      )
    )

    rm(micropointa)
    gc()

  }

  return(log_entries)
}

#' Run Microclimf Micropoint Model for Multiple Years
#'
#' Wrapper function that automates execution of
#' \code{\link{run_micropoint_NicheMapPrep}} across multiple years.
#' Climate, vegetation, and soil parameter files are loaded from disk for each
#' year, the micropoint preprocessing model is executed, and output files are
#' written to the specified output directory.
#'
#' Input files must follow the naming conventions produced by the
#' microclimate data preparation workflow:
#'
#' - `{study_area}_Climate_{year}.RDS`
#' - `{study_area}_VegPara_{year}.RDS`
#' - `{study_area}_SoilPara_{year}.RDS`
#'
#' @param year Numeric or character vector of years to process
#'   (e.g., \code{2020:2023}).
#' @param clim_dir Character. Directory containing yearly climate files
#'   saved as RDS objects.
#' @param dtm terra SpatRaster of the digital terrain model (elevation).
#'   Should align with the climate data used to generate the input files.
#' @param vegp_dir Character. Directory containing yearly vegetation parameter
#'   files saved as RDS objects.
#' @param soilc_dir Character. Directory containing yearly soil parameter files
#'   saved as RDS objects.
#' @param output_dir Character. Base directory where micropoint model outputs
#'   will be saved. Subdirectories are created following the structure:
#'   `output_dir/study_area/year/`.
#' @param reqhgt Numeric. Desired height above ground (in meters) for
#'   above-ground microclimate predictions. Default is 2.
#' @param zref Numeric. Reference height (in meters) for ambient measurements.
#'   Default is 2.
#' @param windhgt Numeric. Height (in meters) at which wind speed is measured
#'   in the input climate data. Default is 10.
#' @param matemp Numeric. Mean annual temperature (°C). If \code{NA}
#'   (default), will be estimated within the micropoint model.
#' @param maxiter Numeric. Maximum number of iterations for model convergence.
#'   Default is 20.
#' @param study_area Character. Study area identifier used when constructing
#'   input file names and output file names. Optional.
#' @param ... Additional advanced options passed to
#'   \code{\link{run_micropoint_NicheMapPrep}}. Most users can ignore these.
#'
#' @return
#' A data frame summarizing all generated micropoint model outputs across all
#' years. The returned data frame contains:
#'
#' \item{depth}{Numeric. Soil depth in centimeters (negative for below surface)
#' or the above-ground prediction height.}
#'
#' \item{file_path}{Character. Full path to the saved output file.}
#'
#' @details
#' For each year specified in \code{year}, this function:
#'
#' \enumerate{
#'   \item Loads climate, vegetation, and soil parameter files from disk.
#'   \item Extracts the time dimension from the climate data.
#'   \item Executes \code{\link{run_micropoint_NicheMapPrep}}.
#'   \item Saves micropoint model outputs to disk.
#'   \item Records generated file paths in a log table.
#' }
#'
#' Processing stops if any required yearly input file cannot be found.
#'
#' Advanced HPC options supplied through \code{...} are forwarded directly to
#' \code{\link{run_micropoint_NicheMapPrep}}.
#'
#' @examples
#' \dontrun{
#'
#' log_df <- run_Yearly_micropoint(
#'   year = 2020:2023,
#'   clim_dir = "./Data/Weather/GMU1_Pkg",
#'   dtm = terra::rast("./Data/DEM/DEM_GLO30.tif"),
#'   vegp_dir = "./Data/VegPara",
#'   soilc_dir = "./Data/SoilPara",
#'   output_dir = "./Microclim_out/PointModels",
#'   study_area = "GMU1"
#' )
#'
#' head(log_df)
#' }
#'
#' @seealso
#' \code{\link{run_micropoint_NicheMapPrep}} for single-year micropoint model
#' preprocessing.
#'
run_Yearly_micropoint <- function(year,
                                  clim_dir,
                                  dtm,
                                  vegp_dir,
                                  soilc_dir,
                                  output_dir,
                                  reqhgt = 2,
                                  zref = 2,
                                  windhgt = 10,
                                  matemp = NA,
                                  maxiter = 20,
                                  study_area = NULL,
                                  ...){



  dots <- list(...)

  allowed <- c("clust_array_arg", "clust_array_size")

  unknown <- setdiff(names(dots), allowed)

  if (length(unknown) > 0) {
    stop(
      "Unknown argument(s): ",
      paste(unknown, collapse = ", ")
    )
  }

  log_all <- list()

  for(i in seq_along(year)){

    y <- year[i]

    climf <- file.path(clim_dir, paste0(
      study_area,
      if (!is.null(study_area)) {'_'} else {''},
      'Climate_', y, ".RDS"
    ))

    vegf <- file.path(vegp_dir, paste0(
      study_area,
      if (!is.null(study_area)) {'_'} else {''},
      'VegPara_', y, ".RDS"
    ))

    soilf <- file.path(soilc_dir, paste0(
      study_area,
      if (!is.null(study_area)) {'_'} else {''},
      'SoilPara_', y, ".RDS"
    ))

    if (!file.exists(climf))
      stop("Climate file not found: ", climf)

    if (!file.exists(vegf))
      stop("Vegetation file not found: ", vegf)

    if (!file.exists(soilf))
      stop("Soil file not found: ", soilf)


    clim <- readr::read_rds(climf)
    veg <- readr::read_rds(vegf)
    soil <- readr::read_rds(soilf)
    tme <- as.POSIXlt(terra::time(terra::unwrap(clim$precip)))


    log <- run_micropoint_NicheMapPrep(
      climarrayr = clim,
      tme = tme,
      dtm = dtm,
      vegp = veg,
      soilc = soil,
      output_dir = output_dir,
      reqhgt = reqhgt,
      zref = zref,
      windhgt = windhgt,
      matemp = matemp,
      maxiter = maxiter,
      year = y,
      study_area = study_area,
      ...
    )

    log_all[[i]] <- log

  }

  return(do.call(rbind, log_all))

}
