#' Define Area of Interest for GEE and Local Analysis
#'
#' Accepts an sf object, SpatVector, or SpatRaster, buffers it, optionally
#' reprojects it, optionally writes the result to disk, and returns a named
#' list containing a GEE ee$Geometry rectangle and the CRS string.
#'
#' @param aoi An sf object, SpatVector, or SpatRaster defining the study area
#' @param buffer_dist Buffer distance in meters. Default is 1000
#' @param crs_epsg Optional EPSG code as a string e.g. "EPSG:26911". If NULL,
#'   the CRS of the input object is used
#' @param write_shp Logical. Whether to write the AOI to disk. Default is FALSE
#' @param out_path File path for the output shapefile. Required if write_shp = TRUE
#'
#' @return A named list with elements:
#'   \item{geometry}{A GEE ee$Geometry$Rectangle object}
#'   \item{crs}{The CRS string used for the geometry}
#' @export
define_aoi <- function(aoi,
                       buffer_dist = 1000,
                       crs_epsg = NULL,
                       write_shp = FALSE,
                       out_path = NULL) {

  # Convert input to sf
  if (inherits(aoi, "SpatVector")) {
    aoi <- sf::st_as_sf(aoi)
  } else if (inherits(aoi, "SpatRaster")) {
    aoi <- sf::st_as_sf(terra::as.polygons(terra::ext(aoi), crs = terra::crs(aoi)))
  } else if (!inherits(aoi, "sf")) {
    stop("aoi must be an sf object, SpatVector, or SpatRaster")
  }

  # Use input CRS if none specified
  if (is.null(crs_epsg)) {
    crs_epsg <- sf::st_crs(aoi)$input
  }

  range_buf <- sf::st_buffer(aoi, dist = buffer_dist)
  ee.ext <- sf::st_bbox(range_buf)
  out <- sf::st_as_sfc(ee.ext)
  out <- sf::st_transform(out, crs_epsg)

  if (write_shp) {
    if (is.null(out_path)) stop("out_path must be provided if write_shp = TRUE")
    sf::st_write(out, out_path, delete_dsn = TRUE)
  }

  ee_aoi <- rgee::ee$Geometry$Rectangle(
    coords = as.numeric(ee.ext),
    proj = crs_epsg,
    geodesic = FALSE
  )

  return(list(geometry = ee_aoi, crs = crs_epsg))
}


#' Poll Google Drive and Download Files as They Appear
#'
#' Polls the GEE_Exports folder on Google Drive at a set interval, downloading
#' files as they appear. Keeps a running count of expected vs downloaded files.
#' Issues a warning if the timeout is reached before all files are downloaded.
#'
#' @param local_path Local directory path to save downloaded files
#' @param n_expected Integer. Number of files expected to download
#' @param poll_interval Polling interval in seconds. Default is 30
#' @param timeout Maximum wait time in seconds. NULL means poll indefinitely.
#'   Default is NULL
#'
#' @return Invisibly returns a named list with elements:
#'   \item{downloaded}{Number of files successfully downloaded}
#'   \item{expected}{Number of files expected}
#'   \item{timed_out}{Logical, whether the timeout was reached}
#' @export
poll_drive <- function(local_path,
                       n_expected,
                       poll_interval = 30,
                       timeout = NULL) {

  dir.create(local_path, showWarnings = FALSE, recursive = TRUE)

  downloaded <- c()
  n_downloaded <- 0
  start_time <- Sys.time()

  cat("\n--- GEE Export Download Manager ---\n")

  repeat {

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    elapsed_fmt <- sprintf("%dm %ds", floor(elapsed / 60), round(elapsed %% 60))

    # Check Drive folder
    files <- tryCatch(
      googledrive::drive_ls("GEE_Exports/"),
      error = function(e) {
        warning("Could not access GEE_Exports on Google Drive: ", e$message)
        return(NULL)
      }
    )

    if (!is.null(files) && nrow(files) > 0) {

      # Filter to only new files not yet downloaded
      new_files <- files[!files$name %in% downloaded, ]

      if (nrow(new_files) > 0) {
        for (i in seq_len(nrow(new_files))) {

          file_name <- new_files$name[i]
          local_file <- file.path(local_path, file_name)

          # Warn if overwriting
          if (file.exists(local_file)) {
            warning("File already exists and will be overwritten: ", local_file)
          }

          # Download and delete from Drive
          googledrive::drive_download(
            googledrive::as_id(new_files$id[i]),
            path = local_file,
            overwrite = TRUE
          )
          googledrive::drive_rm(googledrive::as_id(new_files$id[i]))

          downloaded <- c(downloaded, file_name)
          n_downloaded <- length(downloaded)

          cat(sprintf("Downloaded: %s\n", file_name))
        }
      }
    }

    # Progress message
    cat(sprintf(
      "Polling GEE_Exports... (elapsed: %s)\nFiles expected: %d | Downloaded: %d | Remaining: %d\nChecking again in %d seconds.\n\n",
      elapsed_fmt, n_expected, n_downloaded, n_expected - n_downloaded, poll_interval
    ))

    # Check if all files downloaded
    if (n_downloaded >= n_expected) {
      cat("All expected files downloaded successfully.\n")
      return(invisible(list(downloaded = n_downloaded,
                            expected = n_expected,
                            timed_out = FALSE)))
    }

    # Check timeout
    if (!is.null(timeout) && elapsed >= timeout) {
      warning(sprintf(
        "Timeout reached. Downloaded %d of %d expected files. You can re-run poll_drive() manually to resume.",
        n_downloaded, n_expected
      ))
      return(invisible(list(downloaded = n_downloaded,
                            expected = n_expected,
                            timed_out = TRUE)))
    }

    Sys.sleep(poll_interval)
  }
}


#' Download a Digital Elevation Model from Google Earth Engine
#'
#' Exports the Copernicus GLO-30 DEM clipped to the area of interest from GEE
#' to Google Drive, then downloads it locally using poll_drive().
#'
#' @param aoi A named list returned by define_aoi() containing elements
#'   \code{geometry} (ee$Geometry) and \code{crs} (CRS string)
#' @param local_path Local directory path to save the DEM. Default is "./Data/DEM"
#' @param scale Spatial resolution in meters. Default is 30
#' @param timeout Polling timeout in seconds. Default is 120 (2 minutes)
#' @param poll_interval Polling interval in seconds. Default is 30
#'
#' @return Invisibly returns the result of poll_drive()
#' @export
download_dem <- function(aoi,
                         local_path = "./Data/DEM",
                         scale = 30,
                         timeout = 120,
                         poll_interval = 30) {

  if (!all(c("geometry", "crs") %in% names(aoi))) {
    stop("aoi must be a named list with 'geometry' and 'crs' elements, as returned by define_aoi()")
  }

  ee_aoi <- aoi$geometry
  crs <- aoi$crs

  # Load and process DEM
  cat("Submitting DEM export task to GEE...\n")

  dem <- rgee::ee$ImageCollection("COPERNICUS/DEM/GLO30")$mosaic()
  dem_clipped <- dem$select("DEM")$clip(ee_aoi)

  task <- rgee::ee$batch$Export$image$toDrive(
    image = dem_clipped,
    description = "DEM_GLO30_AOI",
    folder = "GEE_Exports",
    fileNamePrefix = "DEM_GLO30",
    region = ee_aoi,
    scale = scale,
    crs = crs,
    maxPixels = 1e13
  )

  task$start()
  cat("GEE export task started. Polling Drive for output...\n")

  result <- poll_drive(
    local_path = local_path,
    n_expected = 1,
    poll_interval = poll_interval,
    timeout = timeout
  )

  return(invisible(result))
}

#' Downscale MODIS LAI to 30m Resolution Using NDVI
#'
#' Iterates over all combinations of years and months, loading HLS multispectral
#' imagery and coarse MODIS LAI, and downscales LAI to 30m resolution using
#' the lai_fromndvi() function from the microclimdata package.
#'
#' @param years Integer vector of years to process e.g. c(2020, 2021)
#' @param months Integer vector of months to process e.g. 3:8
#' @param img_dir Directory containing HLS imagery files
#' @param lai_dir Directory containing coarse MODIS LAI files
#' @param out_dir Directory to save downscaled LAI outputs
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, only files containing this string will be processed
#'
#' @return Invisibly returns a data frame logging the status of each
#'   year/month combination processed
#' @export
downscale_lai <- function(years,
                          months,
                          img_dir,
                          lai_dir,
                          out_dir,
                          study_area = NULL) {

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # List files in both directories
  img_files <- list.files(img_dir, pattern = "\\.tif$", full.names = TRUE)
  lai_files <- list.files(lai_dir, pattern = "\\.tif$", full.names = TRUE)

  # Optionally filter by study area
  if (!is.null(study_area)) {
    img_files <- img_files[grepl(study_area, img_files)]
    lai_files <- lai_files[grepl(study_area, lai_files)]
  }

  # Helper to extract YYYY_MM from file names
  extract_ym <- function(files) {
    matches <- regmatches(files, regexpr("[0-9]{4}_[0-9]{2}", files))
    data.frame(file = files, ym = matches, stringsAsFactors = FALSE)
  }

  img_df <- extract_ym(img_files)
  lai_df <- extract_ym(lai_files)

  # Build log from requested year/month combos
  log <- expand.grid(year = years, month = months)
  log$ym <- sprintf("%d_%02d", log$year, log$month)
  log$status <- NA_character_

  cat(sprintf("\n--- LAI Downscaling: %d year(s) x %d month(s) = %d combinations ---\n\n",
              length(years), length(months), nrow(log)))

  for (i in seq_len(nrow(log))) {

    y  <- log$year[i]
    mo <- log$month[i]
    ym <- log$ym[i]

    cat(sprintf("Processing year: %d | month: %02d\n", y, mo))

    # Match files by YYYY_MM
    img_match <- img_df$file[img_df$ym == ym]
    lai_match <- lai_df$file[lai_df$ym == ym]

    # Check files exist
    if (length(img_match) == 0) {
      stop(sprintf("HLS imagery not found for %s in:\n  %s", ym, img_dir))
    }
    if (length(lai_match) == 0) {
      stop(sprintf("MODIS LAI not found for %s in:\n  %s", ym, lai_dir))
    }
    if (length(img_match) > 1) {
      stop(sprintf("Multiple HLS imagery files found for %s in:\n  %s", ym, img_dir))
    }
    if (length(lai_match) > 1) {
      stop(sprintf("Multiple MODIS LAI files found for %s in:\n  %s", ym, lai_dir))
    }

    # Load imagery
    img <- terra::rast(img_match)

    # Build rgb and cir stacks
    rgb <- c(img$red, img$green, img$blue)
    cir <- c(img$nir, img$red, img$green)

    # Clamp to valid reflectance range and scale to 0-250
    rgb <- terra::clamp(rgb, lower = 0, upper = 1) * 250
    cir <- terra::clamp(cir, lower = 0, upper = 1) * 250

    # Load coarse LAI
    lai <- terra::rast(lai_match)

    # Downscale
    lai_fine <- microclimdata::lai_fromndvi(rgb, cir, lai)

    # Write output
    # Write output
    out_name <- if (!is.null(study_area)) {
      sprintf("LAI_fromNDVI_%s_%s.tif", study_area, ym)
    } else {
      sprintf("LAI_fromNDVI_%s.tif", ym)
    }

    out_file <- file.path(out_dir, out_name)
    terra::writeRaster(lai_fine, out_file, overwrite = TRUE)

    cat(sprintf("  Saved: %s\n", basename(out_file)))
    log$status[i] <- "success"
  }

  cat(sprintf("\nDone. %d/%d combinations processed successfully.\n",
              sum(log$status == "success", na.rm = TRUE), nrow(log)))

  return(invisible(log))
}