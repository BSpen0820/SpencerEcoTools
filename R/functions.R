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

#' Convert NLCD Land Cover to CORINE-Like Classification
#'
#' Reclassifies a National Land Cover Database (NLCD) raster to CORINE Land
#' Cover class codes. Optionally reprojects and/or crops the output to a
#' template raster.
#'
#' @param lc A SpatRaster containing NLCD land cover codes
#' @param new_crs Optional CRS string to reproject the output e.g. "EPSG:26911".
#'   If NULL no reprojection is applied
#' @param crop_template Optional SpatRaster to use as a crop and reproject
#'   template. If provided, output will be cropped and reprojected to match
#'   this raster's CRS and extent
#'
#' @return A SpatRaster with CORINE-like land cover codes and labels
#' @export
NLCD_2_CORINE <- function(lc, new_crs = NULL, crop_template = NULL) {

  # CORINE land cover code lookup table
  corine_lcm <- data.frame(
    value = c(
      111, 112, 121, 122, 123, 124, 131, 132, 133, 141, 142,
      211, 212, 213, 221, 222, 223, 231, 241, 242, 243, 244,
      311, 312, 313, 321, 322, 323, 324, 331, 332, 333, 334, 335,
      411, 412, 421, 422, 423,
      511, 512, 521, 522, 523
    ),
    label = c(
      "Continuous urban fabric", "Discontinuous urban fabric",
      "Industrial or commercial units", "Road and rail networks",
      "Port areas", "Airports", "Mineral extraction sites", "Dump sites",
      "Construction sites", "Green urban areas", "Sport and leisure facilities",
      "Non-irrigated arable land", "Permanently irrigated land", "Rice fields",
      "Vineyards", "Fruit trees and berry plantations", "Olive groves",
      "Pastures", "Annual crops with permanent crops",
      "Complex cultivation patterns", "Agriculture with natural vegetation",
      "Agro-forestry areas", "Broad-leaved forest", "Coniferous forest",
      "Mixed forest", "Natural grasslands", "Moors and heathland",
      "Sclerophyllous vegetation", "Transitional woodland-shrub",
      "Beaches, dunes, sands", "Bare rocks", "Sparsely vegetated areas",
      "Burnt areas", "Glaciers and perpetual snow", "Inland marshes",
      "Peat bogs", "Salt marshes", "Salines", "Intertidal flats",
      "Water courses", "Water bodies", "Coastal lagoons", "Estuaries",
      "Sea and ocean"
    ),
    stringsAsFactors = FALSE
  )

  # NLCD to CORINE crosswalk
  nlcd_to_corine <- data.frame(
    from = c(11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 52, 71, 81, 82, 90, 95),
    to   = c(512, 335, 141, 112, 112, 111, 333, 311, 312, 313, 324, 321, 231, 211, 411, 411)
  )

  # Reclassify
  corine_rast <- terra::classify(lc, nlcd_to_corine, include.lowest = TRUE)
  levels(corine_rast) <- corine_lcm

  # Handle reprojection and cropping
  if (is.null(new_crs) & is.null(crop_template)) {
    message("No new CRS or crop template provided, returning CORINE-like raster in original CRS and extent.")
    return(corine_rast)

  } else if (is.null(new_crs) & !is.null(crop_template)) {
    message("Crop template provided, reprojecting and cropping to template CRS and extent.")
    return(terra::crop(terra::project(corine_rast, terra::crs(crop_template), method = "near"), crop_template))

  } else if (!is.null(new_crs) & is.null(crop_template)) {
    message("New CRS provided, reprojecting to new CRS without cropping.")
    return(terra::project(corine_rast, new_crs, method = "near"))

  } else {
    message("Both new CRS and crop template provided, cropping to template then reprojecting to new CRS.")
    cropped <- terra::crop(terra::project(corine_rast, terra::crs(crop_template), method = "near"), crop_template)
    return(terra::project(cropped, new_crs, method = "near"))
  }
}

#' Convert LANDFIRE Vegetation Height Raster to Numeric
#'
#' Extracts numeric height values from LANDFIRE vegetation height raster class
#' names, converts the raster to numeric, and optionally crops and reprojects
#' to a template raster.
#'
#' @param rast A categorical SpatRaster of LANDFIRE vegetation height
#' @param crop_template Optional SpatRaster to use as a crop and reproject
#'   template. If provided, output will be cropped and reprojected to match
#'   this raster's CRS and extent
#' @param no_match_value Value to assign when no numeric value can be extracted
#'   from a class name. Default is 0
#'
#' @return A numeric SpatRaster of vegetation heights
#' @export
LandfireVegHght_AsNumeric <- function(rast,
                                           crop_template = NULL,
                                           no_match_value = 0) {

  lv <- levels(rast)

  m <- regexpr("[0-9]+\\.?[0-9]*", lv[[1]]$CLASSNAMES)
  numbers <- as.numeric(regmatches(lv[[1]]$CLASSNAMES, m))
  numbers <- c(m[m == -1], numbers)
  numbers[numbers == -1] <- no_match_value

  lv <- data.frame(lv)
  lv$height <- numbers

  rast <- terra::classify(rast, rcl = as.matrix(lv[, c("VALUE", "height")]))

  if (!is.null(crop_template)) {
    rast <- terra::crop(
      terra::project(rast, terra::crs(crop_template), method = "bilinear"),
      crop_template
    )
  }

  return(rast)
}

#' Download MODIS Albedo Data from Google Earth Engine
#'
#' Iterates over all combinations of years and months, exporting monthly median
#' MODIS shortwave black-sky albedo images from GEE to Google Drive, then
#' optionally downloads them locally using poll_drive().
#'
#' @param aoi A named list returned by define_aoi() containing elements
#'   \code{geometry} (ee$Geometry) and \code{crs} (CRS string)
#' @param years Integer vector of years to process e.g. c(2020, 2021)
#' @param months Integer vector of months to process e.g. 3:8
#' @param local_path Local directory path to save downloaded albedo files
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used as a prefix in output file names
#' @param scale Spatial resolution in meters. Default is 500
#' @param poll Logical. Whether to poll Google Drive and download files after
#'   all tasks are submitted. Default is TRUE
#' @param poll_interval Polling interval in seconds. Default is 30
#' @param timeout Polling timeout in seconds. Default is 300 (5 minutes)
#'
#' @return Invisibly returns a named list with elements:
#'   \item{submitted}{Number of GEE tasks successfully submitted}
#'   \item{skipped}{A data frame of skipped year/month combos}
#'   \item{poll_result}{Result of poll_drive() if poll = TRUE, otherwise NULL}
#' @export
download_albedo <- function(aoi,
                            years,
                            months,
                            local_path,
                            study_area = NULL,
                            scale = 500,
                            poll = TRUE,
                            poll_interval = 30,
                            timeout = 300) {

  if (!all(c("geometry", "crs") %in% names(aoi))) {
    stop("aoi must be a named list with 'geometry' and 'crs' elements, as returned by define_aoi()")
  }

  ee_aoi <- aoi$geometry
  crs    <- aoi$crs

  n_submitted <- 0
  skipped     <- data.frame(year = integer(), month = integer())

  cat(sprintf("\n--- MODIS Albedo Export: %d year(s) x %d month(s) = %d combinations ---\n\n",
              length(years), length(months), length(years) * length(months)))

  for (y in years) {
    for (mo in months) {

      start_date <- sprintf("%d-%02d-01", y, mo)
      end_date   <- as.character(lubridate::ceiling_date(as.Date(start_date), unit = "month"))

      modis_ic <- rgee::ee$ImageCollection("MODIS/061/MCD43A3")$
        filterDate(start_date, end_date)$
        filterBounds(ee_aoi)$
        select("Albedo_BSA_shortwave")

      # Scale to 0-1
      scaled_ic <- modis_ic$map(function(img) {
        img$multiply(0.001)
      })

      n <- scaled_ic$size()$getInfo()
      cat(sprintf("Year %d, Month %02d: %d images found\n", y, mo, n))

      # Skip if no images found
      if (n == 0) {
        warning(sprintf("No images found for year %d month %02d - skipping.", y, mo))
        skipped <- rbind(skipped, data.frame(year = y, month = mo))
        next
      }

      # Monthly median composite
      clim_img <- scaled_ic$median()$clip(ee_aoi)

      # Build file name
      fname <- if (!is.null(study_area)) {
        sprintf("MODIS_Albedo_%s_%d_%02d", study_area, y, mo)
      } else {
        sprintf("MODIS_Albedo_%d_%02d", y, mo)
      }

      task <- rgee::ee$batch$Export$image$toDrive(
        image          = clim_img,
        description    = fname,
        folder         = "GEE_Exports",
        fileNamePrefix = fname,
        region         = ee_aoi,
        scale          = scale,
        crs            = crs,
        maxPixels      = 1e13
      )

      task$start()
      n_submitted <- n_submitted + 1
      cat(sprintf("  Task submitted: %s\n", fname))
    }
  }

  cat(sprintf("\n%d task(s) submitted. %d combo(s) skipped.\n", n_submitted, nrow(skipped)))

  # Poll and download
  poll_result <- NULL
  if (poll && n_submitted > 0) {
    cat(sprintf(
      "\nPolling Google Drive for %d file(s). This may take a while depending on the number of images.\n",
      n_submitted
    ))
    poll_result <- poll_drive(
      local_path    = local_path,
      n_expected    = n_submitted,
      poll_interval = poll_interval,
      timeout       = timeout
    )
  } else if (poll && n_submitted == 0) {
    cat("No tasks were submitted, skipping Drive polling.\n")
  }

  return(invisible(list(
    submitted   = n_submitted,
    skipped     = skipped,
    poll_result = poll_result
  )))
}

# Internal masking helpers ------------------------------------------------

.maskHLS_full <- function(img) {
  fmask <- img$select("Fmask")
  cloud    <- fmask$bitwiseAnd(2^1)$neq(0)
  adjacent <- fmask$bitwiseAnd(2^2)$neq(0)
  shadow   <- fmask$bitwiseAnd(2^3)$neq(0)
  high_aot <- fmask$rightShift(6)$bitwiseAnd(3)$eq(3)
  mask <- cloud$Or(adjacent)$Or(shadow)$Or(high_aot)$Not()
  img$updateMask(mask)
}

.maskHLS_no_aot <- function(img) {
  fmask <- img$select("Fmask")
  cloud    <- fmask$bitwiseAnd(2^1)$neq(0)
  adjacent <- fmask$bitwiseAnd(2^2)$neq(0)
  shadow   <- fmask$bitwiseAnd(2^3)$neq(0)
  mask <- cloud$Or(adjacent)$Or(shadow)$Not()
  img$updateMask(mask)
}

.maskHLS_no_adjacent <- function(img) {
  fmask <- img$select("Fmask")
  cloud  <- fmask$bitwiseAnd(2^1)$neq(0)
  shadow <- fmask$bitwiseAnd(2^3)$neq(0)
  mask <- cloud$Or(shadow)$Not()
  img$updateMask(mask)
}

.maskHLS_cloud_only <- function(img) {
  fmask <- img$select("Fmask")
  cloud <- fmask$bitwiseAnd(2^1)$neq(0)
  img$updateMask(cloud$Not())
}

.focalFillMasked <- function(img, radius = 10) {
  kernel <- rgee::ee$Kernel$square(radius = radius)
  focal  <- img$focal_median(kernel = kernel, iterations = 1)
  img$unmask(focal)
}

# Main function -----------------------------------------------------------

#' Download HLS Sentinel-2 Imagery from Google Earth Engine
#'
#' Iterates over all combinations of years and months, applying a hierarchical
#' cloud/shadow masking strategy to HLS S30 Sentinel-2 imagery before exporting
#' monthly median composites from GEE to Google Drive, then optionally
#' downloading them locally using poll_drive().
#'
#' The masking hierarchy applied is:
#' \enumerate{
#'   \item Full mask (cloud, adjacent, shadow, high AOT) + focal median gap fill
#'   \item Drop high AOT requirement
#'   \item Drop adjacent cloud/shadow requirement
#'   \item Cloud only mask
#'   \item Raw unmasked median
#' }
#'
#' @param aoi A named list returned by define_aoi() containing elements
#'   \code{geometry} (ee$Geometry) and \code{crs} (CRS string)
#' @param years Integer vector of years to process e.g. c(2020, 2021)
#' @param months Integer vector of months to process e.g. 3:8
#' @param local_path Local directory path to save downloaded imagery files
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used as a prefix in output file names
#' @param scale Spatial resolution in meters. Default is 30
#' @param focal_radius Kernel radius in pixels for focal median gap filling.
#'   Default is 10 (21x21 pixel window)
#' @param poll Logical. Whether to poll Google Drive and download files after
#'   all tasks are submitted. Default is TRUE
#' @param poll_interval Polling interval in seconds. Default is 30
#' @param timeout Polling timeout in seconds. Default is 300 (5 minutes)
#'
#' @return Invisibly returns a named list with elements:
#'   \item{submitted}{Number of GEE tasks successfully submitted}
#'   \item{skipped}{A data frame of skipped year/month combos}
#'   \item{poll_result}{Result of poll_drive() if poll = TRUE, otherwise NULL}
#' @export
download_hls <- function(aoi,
                         years,
                         months,
                         local_path,
                         study_area = NULL,
                         scale = 30,
                         focal_radius = 10,
                         poll = TRUE,
                         poll_interval = 30,
                         timeout = 300) {

  if (!all(c("geometry", "crs") %in% names(aoi))) {
    stop("aoi must be a named list with 'geometry' and 'crs' elements, as returned by define_aoi()")
  }

  ee_aoi <- aoi$geometry
  crs    <- aoi$crs

  # Hardcoded HLS S30 band names and rename
  band_names  <- c("B4", "B3", "B2", "B8", "Fmask")
  band_rename <- c("red", "green", "blue", "nir", "Fmask")

  n_submitted <- 0
  skipped     <- data.frame(year = integer(), month = integer())

  cat(sprintf("\n--- HLS Imagery Export: %d year(s) x %d month(s) = %d combinations ---\n\n",
              length(years), length(months), length(years) * length(months)))

  for (y in years) {
    for (mo in months) {

      start_date <- sprintf("%d-%02d-01", y, mo)
      end_date   <- as.character(lubridate::ceiling_date(as.Date(start_date), unit = "month"))

      # Raw collection for fallback
      hls_ic_raw <- rgee::ee$ImageCollection("NASA/HLS/HLSS30/v002")$
        filterDate(start_date, end_date)$
        filterBounds(ee_aoi)$
        select(band_names, band_rename)

      n <- hls_ic_raw$size()$getInfo()

      if (n == 0) {
        warning(sprintf("No images found for year %d month %02d - skipping.", y, mo))
        skipped <- rbind(skipped, data.frame(year = y, month = mo))
        next
      }

      cat(sprintf("Year %d, Month %02d: %d images found\n", y, mo, n))

      # Build median composites at each masking level
      median_full       <- hls_ic_raw$map(.maskHLS_full)$select("red","green","blue","nir")$median()$clip(ee_aoi)
      median_no_aot     <- hls_ic_raw$map(.maskHLS_no_aot)$select("red","green","blue","nir")$median()$clip(ee_aoi)
      median_no_adj     <- hls_ic_raw$map(.maskHLS_no_adjacent)$select("red","green","blue","nir")$median()$clip(ee_aoi)
      median_cloud_only <- hls_ic_raw$map(.maskHLS_cloud_only)$select("red","green","blue","nir")$median()$clip(ee_aoi)
      median_raw        <- hls_ic_raw$select("red","green","blue","nir")$median()$clip(ee_aoi)

      # Focal median gap fill on full mask
      median_full_filled <- .focalFillMasked(median_full, radius = focal_radius)

      # Hierarchical fallback fill
      normal_img <- median_full_filled$
        unmask(median_no_aot)$
        unmask(median_no_adj)$
        unmask(median_cloud_only)$
        unmask(median_raw)

      # Build file name
      fname <- if (!is.null(study_area)) {
        sprintf("HLS_RGBNIR_%s_%d_%02d", study_area, y, mo)
      } else {
        sprintf("HLS_RGBNIR_%d_%02d", y, mo)
      }

      task <- rgee::ee$batch$Export$image$toDrive(
        image          = normal_img,
        description    = fname,
        folder         = "GEE_Exports",
        fileNamePrefix = fname,
        region         = ee_aoi,
        scale          = scale,
        crs            = crs,
        maxPixels      = 1e13
      )

      task$start()
      n_submitted <- n_submitted + 1
      cat(sprintf("  Task submitted: %s\n", fname))
    }
  }

  cat(sprintf("\n%d task(s) submitted. %d combo(s) skipped.\n", n_submitted, nrow(skipped)))

  # Poll and download
  poll_result <- NULL
  if (poll && n_submitted > 0) {
    cat(sprintf(
      "\nPolling Google Drive for %d file(s). This may take a while depending on the number of images.\n",
      n_submitted
    ))
    poll_result <- poll_drive(
      local_path    = local_path,
      n_expected    = n_submitted,
      poll_interval = poll_interval,
      timeout       = timeout
    )
  } else if (poll && n_submitted == 0) {
    cat("No tasks were submitted, skipping Drive polling.\n")
  }

  return(invisible(list(
    submitted   = n_submitted,
    skipped     = skipped,
    poll_result = poll_result
  )))
}

# Internal helpers --------------------------------------------------------

.maskMODIS_LAI <- function(img) {
  qc <- img$select("FparLai_QC")

  # Bit 0: MODLAND QC - mask backup/fill
  bad_quality  <- qc$bitwiseAnd(2^0)$neq(0)

  # Bits 3-4: CloudState - mask clouds present (1) and mixed (2)
  cloud_state  <- qc$rightShift(3)$bitwiseAnd(3)
  cloud        <- cloud_state$eq(1)
  mixed_cloud  <- cloud_state$eq(2)

  # Bits 5-7: SCF_QC - mask pixel not produced (4)
  scf_qc       <- qc$rightShift(5)$bitwiseAnd(7)
  not_produced <- scf_qc$eq(4)

  mask <- bad_quality$Or(cloud)$Or(mixed_cloud)$Or(not_produced)$Not()
  img$updateMask(mask)
}

.scaleMODIS_LAI <- function(img) {
  img$select("Lai")$multiply(0.1)$rename("lai")
}

# Main function -----------------------------------------------------------

#' Download MODIS LAI Data from Google Earth Engine
#'
#' Iterates over all combinations of years and months, applying QC masking
#' and scaling to MODIS MCD15A3H LAI imagery before exporting monthly median
#' composites from GEE to Google Drive, then optionally downloading them
#' locally using poll_drive().
#'
#' The masking strategy applies the following QC filters:
#' \enumerate{
#'   \item Bit 0: Masks backup/fill quality pixels
#'   \item Bits 3-4: Masks cloudy and mixed cloud pixels
#'   \item Bits 5-7: Masks pixels that could not be produced
#' }
#' Remaining gaps are filled using a focal median, with unmasked raw values
#' as a final fallback.
#'
#' @param aoi A named list returned by define_aoi() containing elements
#'   \code{geometry} (ee$Geometry) and \code{crs} (CRS string)
#' @param years Integer vector of years to process e.g. c(2020, 2021)
#' @param months Integer vector of months to process e.g. 3:8
#' @param local_path Local directory path to save downloaded LAI files
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used as a prefix in output file names
#' @param scale Spatial resolution in meters. Default is 500
#' @param focal_radius Kernel radius in pixels for focal median gap filling.
#'   Default is 10 (21x21 pixel window)
#' @param poll Logical. Whether to poll Google Drive and download files after
#'   all tasks are submitted. Default is TRUE
#' @param poll_interval Polling interval in seconds. Default is 30
#' @param timeout Polling timeout in seconds. Default is 300 (5 minutes)
#'
#' @return Invisibly returns a named list with elements:
#'   \item{submitted}{Number of GEE tasks successfully submitted}
#'   \item{skipped}{A data frame of skipped year/month combos}
#'   \item{poll_result}{Result of poll_drive() if poll = TRUE, otherwise NULL}
#' @export
download_modis_lai <- function(aoi,
                               years,
                               months,
                               local_path,
                               study_area = NULL,
                               scale = 500,
                               focal_radius = 10,
                               poll = TRUE,
                               poll_interval = 30,
                               timeout = 300) {

  if (!all(c("geometry", "crs") %in% names(aoi))) {
    stop("aoi must be a named list with 'geometry' and 'crs' elements, as returned by define_aoi()")
  }

  ee_aoi <- aoi$geometry
  crs    <- aoi$crs

  n_submitted <- 0
  skipped     <- data.frame(year = integer(), month = integer())

  cat(sprintf("\n--- MODIS LAI Export: %d year(s) x %d month(s) = %d combinations ---\n\n",
              length(years), length(months), length(years) * length(months)))

  for (y in years) {
    for (mo in months) {

      start_date <- sprintf("%d-%02d-01", y, mo)
      end_date   <- as.character(lubridate::ceiling_date(as.Date(start_date), unit = "month"))

      modis_ic_raw <- rgee::ee$ImageCollection("MODIS/061/MCD15A3H")$
        filterDate(start_date, end_date)$
        filterBounds(ee_aoi)$
        select(c("Lai", "FparLai_QC"))

      n <- modis_ic_raw$size()$getInfo()

      if (n == 0) {
        warning(sprintf("No images found for year %d month %02d - skipping.", y, mo))
        skipped <- rbind(skipped, data.frame(year = y, month = mo))
        next
      }

      cat(sprintf("Year %d, Month %02d: %d images found\n", y, mo, n))

      # Apply QC mask, scale, and compute median
      lai_masked <- modis_ic_raw$map(.maskMODIS_LAI)$map(.scaleMODIS_LAI)$median()$clip(ee_aoi)
      lai_raw    <- modis_ic_raw$map(.scaleMODIS_LAI)$median()$clip(ee_aoi)

      # Focal fill then fallback to raw
      lai_filled <- .focalFillMasked(lai_masked, radius = focal_radius)$unmask(lai_raw)

      # Build file name
      fname <- if (!is.null(study_area)) {
        sprintf("MODIS_LAI_%s_%d_%02d", study_area, y, mo)
      } else {
        sprintf("MODIS_LAI_%d_%02d", y, mo)
      }

      task <- rgee::ee$batch$Export$image$toDrive(
        image          = lai_filled,
        description    = fname,
        folder         = "GEE_Exports",
        fileNamePrefix = fname,
        region         = ee_aoi,
        scale          = scale,
        crs            = crs,
        maxPixels      = 1e13
      )

      task$start()
      n_submitted <- n_submitted + 1
      cat(sprintf("  Task submitted: %s\n", fname))
    }
  }

  cat(sprintf("\n%d task(s) submitted. %d combo(s) skipped.\n", n_submitted, nrow(skipped)))

  # Poll and download
  poll_result <- NULL
  if (poll && n_submitted > 0) {
    cat(sprintf(
      "\nPolling Google Drive for %d file(s). This may take a while depending on the number of images.\n",
      n_submitted
    ))
    poll_result <- poll_drive(
      local_path    = local_path,
      n_expected    = n_submitted,
      poll_interval = poll_interval,
      timeout       = timeout
    )
  } else if (poll && n_submitted == 0) {
    cat("No tasks were submitted, skipping Drive polling.\n")
  }

  return(invisible(list(
    submitted   = n_submitted,
    skipped     = skipped,
    poll_result = poll_result
  )))
}

#' Compute and Adjust Albedo from HLS Sentinel-2 Imagery
#'
#' Iterates over all combinations of years and months, computing photographic
#' albedo from HLS Sentinel-2 imagery using \code{microclimdata::albedo_fromaerial()}
#' and adjusting it to MODIS broadband albedo using
#' \code{microclimdata::albedo_adjust()}. Default band wavelength parameters
#' are specific to the HLS Sentinel-2 (HLSS30) product. If using a different
#' sensor, adjust the band wavelength arguments accordingly. For HLS band
#' specifications see:
#' \url{https://lpdaac.usgs.gov/documents/1698/HLS_User_Guide_V2.pdf}
#'
#' @param years Integer vector of years to process e.g. c(2020, 2021)
#' @param months Integer vector of months to process e.g. 3:8
#' @param modis_dir Directory containing MODIS albedo files
#' @param hls_dir Directory containing HLS Sentinel-2 imagery files
#' @param out_dir Directory to save output albedo files
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used to filter input files and as a prefix in output file names
#' @param rgb_band_mins Numeric vector of RGB band minimum wavelengths in nm.
#'   Default is c(640, 530, 450) for HLS Sentinel-2
#' @param rgb_band_maxs Numeric vector of RGB band maximum wavelengths in nm.
#'   Default is c(670, 590, 510) for HLS Sentinel-2
#' @param cir_band_mins Numeric vector of CIR band minimum wavelengths in nm.
#'   Default is c(780, 640, 530) for HLS Sentinel-2
#' @param cir_band_maxs Numeric vector of CIR band maximum wavelengths in nm.
#'   Default is c(880, 670, 590) for HLS Sentinel-2
#' @param max_albedo Hard ceiling for albedo values before MODIS adjustment.
#'   Pixels above this value are clamped down to it. Default is 0.6, common for fresh snow
#'
#' @return Invisibly returns a data frame logging the status of each
#'   year/month combination processed
#' @export
compute_albedo <- function(years,
                           months,
                           modis_dir,
                           hls_dir,
                           out_dir,
                           study_area = NULL,
                           rgb_band_mins = c(640, 530, 450),
                           rgb_band_maxs = c(670, 590, 510),
                           cir_band_mins = c(780, 640, 530),
                           cir_band_maxs = c(880, 670, 590),
                           max_albedo = 0.6) {

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # List files in both directories
  modis_files <- list.files(modis_dir, pattern = "\\.tif$", full.names = TRUE)
  hls_files   <- list.files(hls_dir,   pattern = "\\.tif$", full.names = TRUE)

  # Optionally filter by study area
  if (!is.null(study_area)) {
    modis_files <- modis_files[grepl(study_area, modis_files)]
    hls_files   <- hls_files[grepl(study_area, hls_files)]
  }

  # Helper to extract YYYY_MM from file names
  extract_ym <- function(files) {
    matches <- regmatches(files, regexpr("[0-9]{4}_[0-9]{2}", files))
    data.frame(file = files, ym = matches, stringsAsFactors = FALSE)
  }

  modis_df <- extract_ym(modis_files)
  hls_df   <- extract_ym(hls_files)

  # Build log from requested year/month combos
  log <- expand.grid(year = years, month = months)
  log$ym     <- sprintf("%d_%02d", log$year, log$month)
  log$status <- NA_character_

  cat(sprintf("\n--- Albedo Computation: %d year(s) x %d month(s) = %d combinations ---\n\n",
              length(years), length(months), nrow(log)))

  for (i in seq_len(nrow(log))) {

    y  <- log$year[i]
    mo <- log$month[i]
    ym <- log$ym[i]

    cat(sprintf("Processing year: %d | month: %02d\n", y, mo))

    # Match files by YYYY_MM
    modis_match <- modis_df$file[modis_df$ym == ym]
    hls_match   <- hls_df$file[hls_df$ym == ym]

    # Check files exist and are unambiguous
    if (length(modis_match) == 0) {
      stop(sprintf("MODIS albedo file not found for %s in:\n  %s", ym, modis_dir))
    }
    if (length(hls_match) == 0) {
      stop(sprintf("HLS imagery file not found for %s in:\n  %s", ym, hls_dir))
    }
    if (length(modis_match) > 1) {
      stop(sprintf("Multiple MODIS albedo files found for %s in:\n  %s", ym, modis_dir))
    }
    if (length(hls_match) > 1) {
      stop(sprintf("Multiple HLS imagery files found for %s in:\n  %s", ym, hls_dir))
    }

    # Load imagery
    s_rast <- terra::rast(hls_match)
    m_rast <- terra::rast(modis_match)

    # Build rgb and cir stacks
    rgb <- c(s_rast$red, s_rast$green, s_rast$blue)
    cir <- c(s_rast$nir, s_rast$red, s_rast$green)
    rm(s_rast)

    # Clamp and scale to 0-250
    rgb <- terra::clamp(rgb, lower = 0, upper = 1) * 250
    cir <- terra::clamp(cir, lower = 0, upper = 1) * 250

    # Compute photographic albedo
    albphoto <- microclimdata::albedo_fromaerial(
      rgb,
      cir,
      RGBbandmins = rgb_band_mins,
      RGBbandmaxs = rgb_band_maxs,
      CIRbandmins = cir_band_mins,
      CIRbandmaxs = cir_band_maxs
    )

    # Clamp to valid range
    albphoto <- terra::clamp(albphoto, lower = 1e-6, upper = 1 - 1e-6)

    # Apply hard ceiling before MODIS adjustment
    albphoto <- terra::clamp(albphoto, upper = max_albedo, values = TRUE)

    # Adjust to MODIS broadband albedo
    albadjusted <- microclimdata::albedo_adjust(albphoto, m_rast)

    # Build output file name
    out_name <- if (!is.null(study_area)) {
      sprintf("HLS_Albedo_%s_%s.tif", study_area, ym)
    } else {
      sprintf("HLS_Albedo_%s.tif", ym)
    }

    terra::writeRaster(
      albadjusted,
      file.path(out_dir, out_name),
      overwrite = TRUE
    )

    cat(sprintf("  Saved: %s\n", out_name))
    log$status[i] <- "success"
  }

  cat(sprintf("\nDone. %d/%d combinations processed successfully.\n",
              sum(log$status == "success", na.rm = TRUE), nrow(log)))

  return(invisible(log))
}