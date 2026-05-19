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