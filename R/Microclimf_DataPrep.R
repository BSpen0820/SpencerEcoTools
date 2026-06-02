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
    crs_epsg <- paste0("EPSG:", sf::st_crs(aoi)$epsg)
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

    # Rescale each band to 0-1

    if(any(terra::values(img) > 1 | terra::values(img) < 0)){

          img <- terra::sapp(img, function(x) {
      rng <- range(terra::values(x), na.rm = TRUE)
      (x - rng[1]) / (rng[2] - rng[1])
    })

    }

    # Build rgb and cir stacks
    rgb <- c(img$red, img$green, img$blue)
    cir <- c(img$nir, img$red, img$green)

    # Load coarse LAI
    lai <- terra::rast(lai_match)

    # Downscale
    lai_fine <- microclimdata::lai_fromndvi(rgb, cir, lai, maxlai = max(terra::values(lai)) + 1)

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

# Internal helpers --------------------------------------------------------

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

.focalFillMasked <- function(img, radius = 10) {
  kernel <- rgee::ee$Kernel$square(radius = radius)
  focal  <- img$focal_median(kernel = kernel, iterations = 1)
  img$unmask(focal)
}

.float32_cast <- function(img) {
  img$cast(rgee::ee$Dictionary(list(
    red   = "float",
    green = "float",
    blue  = "float",
    nir   = "float"
  )))
}

.temporal_weighted_fill <- function(ee_aoi, target_date, band_names, band_rename,
                                    max_months = 2) {

  target_center       <- rgee::ee$Date(target_date)
  target_month_center <- target_center$advance(15, "day")
  filled              <- NULL

  for (offset in 1:max_months) {
    for (direction in c(-1, 1)) {

      neighbor_start <- target_center$advance(direction * offset, "month")
      neighbor_end   <- neighbor_start$advance(1, "month")

      neighbor_ic <- rgee::ee$ImageCollection("NASA/HLS/HLSS30/v002")$
        filterDate(neighbor_start, neighbor_end)$
        filterBounds(ee_aoi)$
        select(band_names, band_rename)

      n <- neighbor_ic$size()$getInfo()
      if (n == 0) next

      # Apply masks
      masked_ic <- neighbor_ic$
        map(.maskHLS_full)$
        map(function(img) .maskHLS_no_aot(img))

      # Cast to float32 and compute effective weights
      weighted_ic <- masked_ic$map(function(img) {
        img <- img$select("red", "green", "blue", "nir")$cast(
          rgee::ee$Dictionary(list(
            red   = "float",
            green = "float",
            blue  = "float",
            nir   = "float"
          ))
        )

        img_date         <- img$date()
        day_dist         <- img_date$difference(target_month_center, "day")$abs()$add(1)
        weight           <- rgee::ee$Number(1)$divide(day_dist)
        pixel_mask       <- img$select("red")$mask()
        effective_weight <- pixel_mask$multiply(weight)$
          rename("effective_weight")$
          cast(rgee::ee$Dictionary(list(effective_weight = "float")))

        img$multiply(weight)$
          addBands(effective_weight)$
          set("weight", weight)
      })

      # Weighted sum of valid pixels only
      weighted_sum <- weighted_ic$select(c("red", "green", "blue", "nir"))$sum()

      # Sum of effective weights only
      weight_sum <- weighted_ic$select("effective_weight")$sum()

      # Divide only where weight_sum > 0
      neighbor_mean <- weighted_sum$divide(weight_sum)$
        updateMask(weight_sum$gt(0))

      filled <- if (is.null(filled)) neighbor_mean else filled$unmask(neighbor_mean)
    }
  }

  return(filled)
}

# Main function -----------------------------------------------------------

#' Download HLS Sentinel-2 Imagery from Google Earth Engine
#'
#' Iterates over all combinations of years and months, applying a two-level
#' cloud/shadow masking strategy to HLS S30 Sentinel-2 imagery. If gaps remain
#' after masking, temporally adjacent images (up to 2 months either side) are
#' used to fill gaps via inverse distance weighted mean, weighted by day
#' distance from the center of the target month. A focal median is applied
#' as a final fallback for any remaining gaps.
#'
#' The masking strategy applied is:
#' \enumerate{
#'   \item Full mask (cloud, adjacent, shadow, high AOT) median composite
#'   \item No AOT mask median fills remaining gaps
#'   \item Temporally weighted mean from surrounding months fills remaining gaps
#'   \item Focal median fills any final remaining gaps
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
#' @param max_months Maximum number of months to search either side of target
#'   month for temporal interpolation. Default is 2
#' @param poll Logical. Whether to poll Google Drive and download files after
#'   all tasks are submitted. Default is TRUE
#' @param poll_interval Polling interval in seconds. Default is 30
#' @param timeout Polling timeout in seconds. Default is 300 (5 minutes)
#'
#' @return Invisibly returns a named list with elements:
#'   \item{submitted}{Number of GEE tasks successfully submitted}
#'   \item{skipped}{A data frame of skipped year/month combos}
#'   \item{interpolated}{A data frame of year/month combos where temporal interpolation was applied}
#'   \item{poll_result}{Result of poll_drive() if poll = TRUE, otherwise NULL}
#' @export
download_hls <- function(aoi,
                         years,
                         months,
                         local_path,
                         study_area = NULL,
                         scale = 30,
                         focal_radius = 10,
                         max_months = 2,
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

  n_submitted  <- 0
  skipped      <- data.frame(year = integer(), month = integer())
  interpolated <- data.frame(year = integer(), month = integer())

  cat(sprintf("\n--- HLS Imagery Export: %d year(s) x %d month(s) = %d combinations ---\n\n",
              length(years), length(months), length(years) * length(months)))

  for (y in years) {
    for (mo in months) {

      start_date <- sprintf("%d-%02d-01", y, mo)
      end_date   <- as.character(lubridate::ceiling_date(as.Date(start_date), unit = "month"))

      # Raw collection
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

      # Level 1: Full mask median
      median_full <- hls_ic_raw$
        map(.maskHLS_full)$
        select("red", "green", "blue", "nir")$
        map(.float32_cast)$
        median()$
        clip(ee_aoi)

      # Level 2: No AOT mask fills gaps
      median_no_aot <- hls_ic_raw$
        map(.maskHLS_no_aot)$
        select("red", "green", "blue", "nir")$
        map(.float32_cast)$
        median()$
        clip(ee_aoi)

      combined <- median_full$unmask(median_no_aot)

      # Check for remaining gaps
      any_masked_result <- combined$select("red")$mask()$Not()$
        reduceRegion(
          reducer   = rgee::ee$Reducer$anyNonZero(),
          geometry  = ee_aoi,
          scale     = scale,
          maxPixels = 1e13
        )$getInfo()

      needs_interp <- isTRUE(as.logical(unlist(any_masked_result)[1]))

      if (needs_interp) {
        cat(sprintf("  Gaps detected, applying temporal interpolation (up to %d months either side)...\n",
                    max_months))

        target_date <- sprintf("%d-%02d-15", y, mo)

        temporal_fill <- .temporal_weighted_fill(
          ee_aoi      = ee_aoi,
          target_date = target_date,
          band_names  = band_names,
          band_rename = band_rename,
          max_months  = max_months
        )

        if (!is.null(temporal_fill)) {
          combined <- combined$unmask(temporal_fill$clip(ee_aoi))
        }

        interpolated <- rbind(interpolated, data.frame(year = y, month = mo))
      }

      # Final fallback: focal median
      combined <- .focalFillMasked(combined, radius = focal_radius)

      # Build file name
      fname <- if (!is.null(study_area)) {
        sprintf("HLS_RGBNIR_%s_%d_%02d", study_area, y, mo)
      } else {
        sprintf("HLS_RGBNIR_%d_%02d", y, mo)
      }

      task <- rgee::ee$batch$Export$image$toDrive(
        image          = combined,
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

  cat(sprintf("\n%d task(s) submitted. %d combo(s) skipped. %d combo(s) used temporal interpolation.\n",
              n_submitted, nrow(skipped), nrow(interpolated)))

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
    submitted    = n_submitted,
    skipped      = skipped,
    interpolated = interpolated,
    poll_result  = poll_result
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

    # Rescale each band to 0-1

    if(any(terra::values(s_rast) > 1 | terra::values(s_rast) < 0)){

      s_rast <- terra::sapp(s_rast, function(x) {
        rng <- range(terra::values(x), na.rm = TRUE)
        (x - rng[1]) / (rng[2] - rng[1])
      })

    }

    # Build rgb and cir stacks
    rgb <- c(s_rast$red, s_rast$green, s_rast$blue)
    cir <- c(s_rast$nir, s_rast$red, s_rast$green)
    rm(s_rast)

    # Clamp and scale to 0-250
    rgb <- rgb * 250
    cir <- cir * 250

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

#' Compute Leaf and Ground Reflectance from LAI, Albedo and Land Cover
#'
#' Iterates over all combinations of years and months, computing leaf and
#' ground reflectance using \code{microclimdata::reflectance_calc()}. Land
#' cover based x values are computed annually and reused across all months
#' within a year. LAI and albedo are processed monthly.
#'
#' @param years Integer vector of years to process e.g. c(2020, 2021)
#' @param months Integer vector of months to process e.g. 3:8
#' @param lc_dir Directory containing annual land cover files
#' @param lai_dir Directory containing fine resolution LAI files
#' @param alb_dir Directory containing HLS albedo files
#' @param out_dir_lref Directory to save leaf reflectance output files
#' @param out_dir_gref Directory to save ground reflectance output files
#' @param xcalc_dir Optional directory to save annual x calculation rasters.
#'   If NULL x rasters are kept in memory only. Default is NULL
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used to filter input files and as a prefix in output file names
#' @param lctype Land cover classification type passed to \code{microclimdata::x_calc()}.
#'   Must be either "CORINE" or "ESA". Default is "CORINE"
#'
#' @return Invisibly returns a data frame logging the status of each
#'   year/month combination processed
#' @export
compute_reflectance <- function(years,
                                months,
                                lc_dir,
                                lai_dir,
                                alb_dir,
                                out_dir_lref,
                                out_dir_gref,
                                xcalc_dir = NULL,
                                study_area = NULL,
                                lctype = "CORINE") {

  # Validate lctype
  if (!lctype %in% c("CORINE", "ESA")) {
    stop("lctype must be either 'CORINE' or 'ESA'")
  }

  # Create output directories
  dir.create(out_dir_lref, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_dir_gref, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(xcalc_dir)) dir.create(xcalc_dir, recursive = TRUE, showWarnings = FALSE)

  # Helper to extract YYYY from file names
  extract_year <- function(files) {
    matches <- regmatches(files, regexpr("[0-9]{4}", files))
    data.frame(file = files, year = as.integer(matches), stringsAsFactors = FALSE)
  }

  # Helper to extract YYYY_MM from file names
  extract_ym <- function(files) {
    matches <- regmatches(files, regexpr("[0-9]{4}_[0-9]{2}", files))
    data.frame(file = files, ym = matches, stringsAsFactors = FALSE)
  }

  # List input files
  lc_files  <- list.files(lc_dir,  pattern = "\\.tif$", full.names = TRUE)
  lai_files <- list.files(lai_dir, pattern = "\\.tif$", full.names = TRUE)
  alb_files <- list.files(alb_dir, pattern = "\\.tif$", full.names = TRUE)

  # Optionally filter by study area
  if (!is.null(study_area)) {
    lc_files  <- lc_files[grepl(study_area, lc_files)]
    lai_files <- lai_files[grepl(study_area, lai_files)]
    alb_files <- alb_files[grepl(study_area, alb_files)]
  }

  lc_df  <- extract_year(lc_files)
  lai_df <- extract_ym(lai_files)
  alb_df <- extract_ym(alb_files)

  # Build log
  log <- expand.grid(year = years, month = months)
  log$ym     <- sprintf("%d_%02d", log$year, log$month)
  log$status <- NA_character_

  cat(sprintf("\n--- Reflectance Computation: %d year(s) x %d month(s) = %d combinations ---\n\n",
              length(years), length(months), nrow(log)))

  for (y in years) {

    cat(sprintf("Year %d: computing x values from land cover...\n", y))

    # Match land cover file for this year
    lc_match <- lc_df$file[lc_df$year == y]
    if (length(lc_match) == 0) stop(sprintf("Land cover file not found for year %d in:\n  %s", y, lc_dir))
    if (length(lc_match) > 1) stop(sprintf("Multiple land cover files found for year %d in:\n  %s", y, lc_dir))

    lc <- terra::rast(lc_match)
    x  <- microclimdata::x_calc(lc, lctype = lctype)

    # Use month 3 LAI as template for common extent and resolution
    ref_ym       <- sprintf("%d_%02d", y, 3)
    lai_ref      <- lai_df$file[lai_df$ym == ref_ym]
    if (length(lai_ref) == 0) stop(sprintf("Reference LAI (month 03) not found for year %d", y))

    lai_template <- terra::rast(lai_ref)
    com_ext      <- terra::intersect(terra::ext(lai_template), terra::ext(x))

    x <- terra::crop(x, com_ext)
    x <- terra::resample(x, lai_template)
    rm(lai_template)

    # Optionally save annual x raster
    if (!is.null(xcalc_dir)) {
      x_out_name <- if (!is.null(study_area)) {
        sprintf("x_calc_%s_%d.tif", study_area, y)
      } else {
        sprintf("x_calc_%d.tif", y)
      }
      terra::writeRaster(x, file.path(xcalc_dir, x_out_name), overwrite = TRUE)
      cat(sprintf("  Saved: %s\n", x_out_name))
    }

    for (m in months) {

      ym <- sprintf("%d_%02d", y, m)
      cat(sprintf("  Processing month: %02d\n", m))

      # Match LAI and albedo files
      lai_match <- lai_df$file[lai_df$ym == ym]
      alb_match <- alb_df$file[alb_df$ym == ym]

      if (length(lai_match) == 0) stop(sprintf("LAI file not found for %s in:\n  %s", ym, lai_dir))
      if (length(alb_match) == 0) stop(sprintf("Albedo file not found for %s in:\n  %s", ym, alb_dir))
      if (length(lai_match) > 1) stop(sprintf("Multiple LAI files found for %s in:\n  %s", ym, lai_dir))
      if (length(alb_match) > 1) stop(sprintf("Multiple albedo files found for %s in:\n  %s", ym, alb_dir))

      lai <- terra::rast(lai_match)
      alb <- terra::rast(alb_match)

      # Crop to common extent
      lai <- terra::crop(lai, com_ext)
      alb <- terra::crop(alb, com_ext)

      # Validate matching extent and resolution before combining
      if (!isTRUE(all.equal(terra::ext(x), terra::ext(lai)))  ||
          !isTRUE(all.equal(terra::ext(x), terra::ext(alb)))  ||
          !isTRUE(all.equal(terra::res(x), terra::res(lai)))  ||
          !isTRUE(all.equal(terra::res(x), terra::res(alb)))) {
        stop(sprintf(
          "Extent or resolution of x, alb, and lai did not match for %s. Check preprocessing steps.", ym
        ))
      }

      # Compute reflectance
      refldata <- microclimdata::reflectance_calc(alb, lai, x, plotprogress = FALSE)

      # Build output file names
      lref_name <- if (!is.null(study_area)) {
        sprintf("LF_Refl_%s_%s.tif", study_area, ym)
      } else {
        sprintf("LF_Refl_%s.tif", ym)
      }

      gref_name <- if (!is.null(study_area)) {
        sprintf("GF_Refl_%s_%s.tif", study_area, ym)
      } else {
        sprintf("GF_Refl_%s.tif", ym)
      }

      terra::writeRaster(refldata$lref, file.path(out_dir_lref, lref_name), overwrite = TRUE)
      terra::writeRaster(refldata$gref, file.path(out_dir_gref, gref_name), overwrite = TRUE)

      cat(sprintf("    Saved: %s\n", lref_name))
      cat(sprintf("    Saved: %s\n", gref_name))

      log$status[log$ym == ym] <- "success"
    }
  }

  cat(sprintf("\nDone. %d/%d combinations processed successfully.\n",
              sum(log$status == "success", na.rm = TRUE), nrow(log)))

  return(invisible(log))
}

#' Download and Downscale Soil Data
#'
#' Downloads coarse resolution soil data using \code{microclimdata::soildata_download()}
#' and downscales it to fine resolution using \code{microclimdata::soildata_downscale()}.
#' Requires a Digital Elevation Model and a single static land cover raster.
#'
#' @param dtm A SpatRaster or file path to a Digital Elevation Model raster
#' @param lc A SpatRaster or file path to a land cover raster. Should represent
#'   a single static year
#' @param out_dir Directory to save soil data outputs
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used as a prefix in output file names
#' @param water Integer. CORINE or ESA land cover code for water bodies passed
#'   to \code{soildata_downscale()}. Default is 512 for CORINE water class
#' @param delete_tmp Logical. Whether to delete temporary files after processing.
#'   Default is TRUE
#'
#' @return Invisibly returns a named list with elements:
#'   \item{coarse}{The coarse resolution soil SpatRaster}
#'   \item{fine}{The fine resolution soil SpatRaster}
#' @export
download_soil <- function(dtm,
                          lc,
                          out_dir,
                          study_area = NULL,
                          water = 512,
                          delete_tmp = TRUE) {

  # Accept file path or SpatRaster for dtm
  if (is.character(dtm)) {
    dtm <- terra::rast(dtm)
  } else if (!inherits(dtm, "SpatRaster")) {
    stop("dtm must be a SpatRaster or a file path to a raster file")
  }

  # Accept file path or SpatRaster for lc
  if (is.character(lc)) {
    lc <- terra::rast(lc)
  } else if (!inherits(lc, "SpatRaster")) {
    stop("lc must be a SpatRaster or a file path to a raster file")
  }

  # Create output and temp directories
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  tmp_dir <- file.path(out_dir, "tmp")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  # Project DTM to WGS84 for soildata_download
  dtm_wgs84 <- terra::project(dtm, "epsg:4326")

  # Download coarse soil data
  cat("Downloading coarse soil data...\n")
  soildata <- microclimdata::soildata_download(
    dtm_wgs84,
    pathdir     = tmp_dir,
    deletefiles = !delete_tmp
  )

  # Build coarse output file name
  coarse_name <- if (!is.null(study_area)) {
    sprintf("SoilData_Coarse_%s.tif", study_area)
  } else {
    "SoilData_Coarse.tif"
  }

  terra::writeRaster(soildata, file.path(out_dir, coarse_name), overwrite = TRUE)
  cat(sprintf("  Saved: %s\n", coarse_name))

  # Project land cover to WGS84 for soildata_downscale
  lc_wgs84 <- terra::project(lc, "epsg:4326")

  # Downscale soil data
  cat("Downscaling soil data...\n")
  soildata_fine <- microclimdata::soildata_downscale(soildata, lc_wgs84, water = water)

  # Reproject back to original DTM CRS
  soildata_fine <- terra::project(soildata_fine, dtm)

  # Build fine output file name
  fine_name <- if (!is.null(study_area)) {
    sprintf("SoilData_Fine_%s.tif", study_area)
  } else {
    "SoilData_Fine.tif"
  }

  terra::writeRaster(soildata_fine, file.path(out_dir, fine_name), overwrite = TRUE)
  cat(sprintf("  Saved: %s\n", fine_name))

  # Clean up temp directory
  if (delete_tmp) {
    unlink(tmp_dir, recursive = TRUE)
    cat("  Temporary files removed.\n")
  }

  return(invisible(list(
    coarse = soildata,
    fine   = soildata_fine
  )))
}
#' Download NOAA AORC Climate Data
#'
#' Downloads hourly AORC v1.1 climate data from NOAA's S3 bucket for a given
#' area of interest and time period. Data is downloaded variable by variable
#' to limit RAM usage and saved as NetCDF files organized by year and month.
#'
#' @details Requires the following Python packages to be installed in your
#'   conda environment: xarray, s3fs, zarr, netCDF4. These can be installed
#'   via conda: \code{conda install -c conda-forge xarray s3fs zarr netCDF4}
#'   If not found, the function will attempt to install them via
#'   \code{reticulate::py_install()}.
#'
#'   Downloaded files are returned in the native WGS84 (EPSG:4326) coordinate
#'   system of the AORC dataset. Reprojection to a different CRS is intentionally
#'   not performed here, as \code{estimate_diffuse_rad()} requires WGS84 to
#'   correctly compute solar geometry. Reprojection of all AORC and diffuse
#'   radiation outputs should be performed after \code{estimate_diffuse_rad()}
#'   has been run.
#'
#' @param aoi A SpatRaster, SpatVector, sf object, or named list returned by
#'   \code{define_aoi()}. Used to derive the bounding box for download
#' @param years Integer vector of years to process e.g. c(2020, 2021)
#' @param months Integer vector of months to process e.g. 3:8
#' @param out_dir Base directory to save downloaded AORC files. Files are
#'   organized as \code{out_dir/study_area/year/month/} if study_area is
#'   provided, otherwise \code{out_dir/year/month/}
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used as a prefix in output file names and added to directory
#'   structure
#' @param overwrite Logical. Whether to overwrite existing files. If FALSE
#'   existing files are skipped allowing the function to resume interrupted
#'   downloads. Default is FALSE
#' @param workers Integer. Number of parallel workers for month-level
#'   parallelization via \code{future.apply}. Default is 1 (sequential)
#' @param python_path Optional path to Python executable or conda environment.
#'   If NULL uses the currently configured Python environment. Default is NULL
#'
#' @return Invisibly returns a data frame logging the status of each
#'   year/month/variable combination processed
#' @export
download_aorc <- function(aoi,
                          years,
                          months,
                          out_dir,
                          study_area = NULL,
                          overwrite = FALSE,
                          workers = 1,
                          python_path = NULL) {

  # Optionally set Python environment
  if (!is.null(python_path)) {
    reticulate::use_python(python_path, required = TRUE)
  }

  # Check and install required Python packages
  required_pkgs <- c("xarray", "s3fs", "zarr", "netCDF4")
  missing_pkgs  <- required_pkgs[!sapply(required_pkgs, reticulate::py_module_available)]

  if (length(missing_pkgs) > 0) {
    message("Installing missing Python packages: ", paste(missing_pkgs, collapse = ", "))
    reticulate::py_install(missing_pkgs)
  }

  # Extract extent from aoi in WGS84
  if (is.list(aoi) && all(c("geometry", "crs") %in% names(aoi))) {
    aoi_sf    <- rgee::ee_as_sf(aoi$geometry)
    ext_wgs84 <- sf::st_bbox(sf::st_transform(aoi_sf, 4326))
  } else if (inherits(aoi, "SpatRaster") || inherits(aoi, "SpatVector")) {
    ext_wgs84 <- terra::ext(terra::project(aoi, "epsg:4326"))
  } else if (inherits(aoi, "sf") || inherits(aoi, "sfc")) {
    ext_wgs84 <- sf::st_bbox(sf::st_transform(aoi, 4326))
  } else {
    stop("aoi must be a SpatRaster, SpatVector, sf object, or named list from define_aoi()")
  }

  lon_min <- as.numeric(ext_wgs84[1])
  lon_max <- as.numeric(ext_wgs84[3])
  lat_min <- as.numeric(ext_wgs84[2])
  lat_max <- as.numeric(ext_wgs84[4])

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Build list of all year/month combos
  combos <- expand.grid(year = years, month = months)

  cat(sprintf("\n--- AORC Download: %d year(s) x %d month(s) ---\n\n",
              length(years), length(months)))

  # Set up parallel plan
  if (workers > 1) {
    future::plan(future::multisession, workers = workers)
    on.exit(future::plan(future::sequential), add = TRUE)
  }

  # Process each year/month combo
  results <- future.apply::future_lapply(seq_len(nrow(combos)), function(i) {

    y <- combos$year[i]
    m <- combos$month[i]

    combo_log <- data.frame(
      year     = integer(),
      month    = integer(),
      variable = character(),
      status   = character(),
      stringsAsFactors = FALSE
    )

    # Set Python environment in each worker
    if (!is.null(python_path)) {
      reticulate::use_python(python_path, required = TRUE)
    }

    # Re-import Python modules in each worker
    xr       <- reticulate::import("xarray")
    builtins <- reticulate::import("builtins")

    # Build month directory
    month_dir <- if (!is.null(study_area)) {
      file.path(out_dir, study_area, y, m)
    } else {
      file.path(out_dir, y, m)
    }
    dir.create(month_dir, recursive = TRUE, showWarnings = FALSE)

    start <- sprintf("%d-%02d-01", y, m)
    end   <- as.character(lubridate::ceiling_date(as.Date(start), unit = "month") - 1)

    # Open yearly Zarr store
    ds_url <- sprintf("https://noaa-nws-aorc-v1-1-1km.s3.amazonaws.com/%d.zarr", y)
    message(sprintf("Opening %s", ds_url))

    ds <- tryCatch(
      xr$open_zarr(ds_url, consolidated = TRUE),
      error = function(e) {
        message(sprintf("Failed to open Zarr store for year %d: %s", y, e$message))
        return(NULL)
      }
    )

    if (is.null(ds)) {
      combo_log <- rbind(combo_log, data.frame(
        year = y, month = m, variable = "ALL",
        status = "failed - could not open zarr",
        stringsAsFactors = FALSE
      ))
      return(combo_log)
    }

    vars <- reticulate::py_to_r(builtins$list(ds$data_vars$keys()))

    for (varname in vars) {

      # Build output file name
      out_file <- if (!is.null(study_area)) {
        file.path(month_dir, sprintf("AORC_%s_%d_%02d_%s.nc", study_area, y, m, varname))
      } else {
        file.path(month_dir, sprintf("AORC_%d_%02d_%s.nc", y, m, varname))
      }

      if (file.exists(out_file) && !overwrite) {
        message(sprintf("  Skipping %s (already exists)", basename(out_file)))
        combo_log <- rbind(combo_log, data.frame(
          year = y, month = m, variable = varname, status = "skipped",
          stringsAsFactors = FALSE
        ))
        next
      }

      message(sprintf("  Year %d, Month %02d, Variable: %s", y, m, varname))

      tryCatch({
        var        <- ds[[varname]]
        time_slice <- builtins$slice(start, end)
        lat_slice  <- builtins$slice(lat_min, lat_max)
        lon_slice  <- builtins$slice(lon_min, lon_max)

        sub_var <- var$sel(
          time      = time_slice,
          longitude = lon_slice,
          latitude  = lat_slice
        )

        sub_var$to_netcdf(out_file)
        message(sprintf("    Saved %s", basename(out_file)))

        combo_log <- rbind(combo_log, data.frame(
          year = y, month = m, variable = varname, status = "success",
          stringsAsFactors = FALSE
        ))
      },
      error = function(e) {
        message(sprintf("    Failed %s: %s", basename(out_file), e$message))
        combo_log <<- rbind(combo_log, data.frame(
          year = y, month = m, variable = varname,
          status = paste("failed -", e$message),
          stringsAsFactors = FALSE
        ))
      })
    }

    return(combo_log)
  }, future.seed = TRUE)

  # Combine logs from all workers
  log <- do.call(rbind, results)

  # Summary
  n_success <- sum(log$status == "success",  na.rm = TRUE)
  n_skipped <- sum(log$status == "skipped",  na.rm = TRUE)
  n_failed  <- sum(grepl("failed", log$status), na.rm = TRUE)

  cat(sprintf("\nDone. %d downloaded, %d skipped, %d failed.\n",
              n_success, n_skipped, n_failed))

  if (n_failed > 0) {
    cat("Failed downloads:\n")
    print(log[grepl("failed", log$status), ])
  }

  return(invisible(log))
}


#' Estimate Diffuse Solar Radiation from AORC Shortwave Radiation
#'
#' Estimates hourly diffuse solar radiation from AORC downward shortwave
#' radiation (DSWRF) using the clearness index method. Solar geometry is
#' computed per latitude using the Michalsky method via the solaR package.
#' Timestamps are converted to mean solar time using the centroid longitude
#' of the AORC raster via \code{solaR::local2Solar()} to ensure correct
#' alignment of solar position calculations with local sunrise/sunset.
#' Output files are saved alongside their corresponding AORC DSWRF files.
#'
#' @details The diffuse fraction is estimated using the Erbs et al. (1982)
#'   piecewise model:
#'   \itemize{
#'     \item k <= 0: f = 0 (night)
#'     \item 0 < k <= 0.22: f = 1 - 0.09k
#'     \item 0.22 < k <= 0.80: f = 0.9511 - 0.1604k + 4.388k^2 - 16.638k^3 + 12.336k^4
#'     \item k > 0.80: f = 0.165
#'   }
#'
#'   Output files are returned in WGS84 (EPSG:4326) matching the native AORC
#'   coordinate system. This is intentional, reprojection prior to diffuse
#'   radiation estimation introduces empty edge cells due to projection warping
#'   which corrupts the solar geometry calculations. Reprojection of outputs
#'   should be performed after this function has been run.
#'
#'   Requires the \code{solaR} package available on CRAN and GitHub.
#'
#' @param years Integer vector of years to process e.g. c(2020, 2021)
#' @param months Integer vector of months to process e.g. 3:8
#' @param base_dir Base directory containing AORC data organized as
#'   \code{base_dir/study_area/year/month/} or \code{base_dir/year/month/}
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used to locate input files and prefix output file names
#' @param workers Integer. Number of parallel workers via \code{future.apply}.
#'   Default is 1 (sequential)
#'
#' @return Invisibly returns a data frame logging the status of each
#'   year/month combination processed
#' @export
estimate_diffuse_rad <- function(years,
                                 months,
                                 base_dir,
                                 study_area = NULL,
                                 workers = 1) {

  # Build list of all year/month combos
  combos <- expand.grid(year = years, month = months)

  cat(sprintf("\n--- Diffuse Radiation Estimation: %d year(s) x %d month(s) = %d combinations ---\n\n",
              length(years), length(months), nrow(combos)))

  # Set up parallel plan
  if (workers > 1) {
    future::plan(future::multisession, workers = workers)
    on.exit(future::plan(future::sequential), add = TRUE)
  }

  results <- future.apply::future_lapply(seq_len(nrow(combos)), function(i) {

    y <- combos$year[i]
    m <- combos$month[i]

    cat(sprintf("Working on %d %02d\n", y, m))

    # Build input directory
    month_dir <- if (!is.null(study_area)) {
      file.path(base_dir, study_area, y, m)
    } else {
      file.path(base_dir, y, m)
    }

    # Build DSWRF file path
    dswrf_file <- if (!is.null(study_area)) {
      file.path(month_dir, sprintf("AORC_%s_%d_%02d_DSWRF_surface.nc",
                                   study_area, y, m))
    } else {
      file.path(month_dir, sprintf("AORC_%d_%02d_DSWRF_surface.nc", y, m))
    }

    # Flag missing files and continue
    if (!file.exists(dswrf_file)) {
      warning(sprintf("DSWRF file not found for %d-%02d, skipping:\n  %s",
                      y, m, dswrf_file))
      return(data.frame(year = y, month = m,
                        status = "skipped - file not found",
                        stringsAsFactors = FALSE))
    }

    tryCatch({

      # Save and restore system timezone to avoid side effects
      old_tz <- Sys.getenv("TZ")
      Sys.setenv(TZ = "UTC")
      on.exit(Sys.setenv(TZ = old_tz), add = TRUE)

      AORC <- terra::rast(dswrf_file)

      ## Time handling
      AORC_time <- terra::time(AORC)
      attr(AORC_time, "tzone") <- "UTC"
      BTi_local <- lubridate::with_tz(AORC_time, "America/Denver")

      ## Extract centroid longitude from raster
      lon <- terra::xFromCol(AORC, col = round(terra::ncol(AORC) / 2))

      ## Convert to mean solar time using centroid longitude
      BTi <- solaR::local2Solar(BTi_local, lon = lon)

      ## Daily sequence for fSolD in solar time
      BTd <- as.POSIXct(unique(lubridate::date(BTi)), tz = "UTC")

      ## Latitude per cell
      coords      <- terra::crds(AORC, df = FALSE)
      lat_vals    <- coords[, 2]
      unique_lats <- sort(unique(round(lat_vals, 4)))
      lat_index   <- match(round(lat_vals, 4), unique_lats)

      ## Bo0 matrix [lat x time]
      n_lat   <- length(unique_lats)
      n_time  <- length(BTi)
      Bo0_mat <- matrix(NA_real_, n_lat, n_time)

      for (j in seq_len(n_lat)) {
        solD <- solaR::fSolD(
          lat    = unique_lats[j],
          BTd    = BTd,
          method = "michalsky"
        )
        solI <- solaR::fSolI(
          solD       = solD,
          BTi        = BTi,
          sample     = "hour",
          EoT        = TRUE,
          keep.night = TRUE
        )
        Bo0_mat[j, ] <- zoo::coredata(solI$Bo0)
      }

      ## Build Bo0 raster stack
      Bo_rast    <- terra::rast(AORC)
      bo0_values <- Bo0_mat[lat_index, ]
      terra::values(Bo_rast) <- bo0_values

      ## Clearness index
      k.out <- AORC / Bo_rast
      k.out[is.na(k.out)] <- 0

      ## Diffuse fraction function
      Diff.frac.calc <- function(k) {
        f <- numeric(length(k))
        f[k <= 0]               <- 0
        f[k > 0 & k <= 0.22]   <- 1 - 0.09 * k[k > 0 & k <= 0.22]
        f[k > 0.22 & k <= 0.8] <- 0.9511 - 0.1604 * k[k > 0.22 & k <= 0.8] +
          4.388  * k[k > 0.22 & k <= 0.8]^2 -
          16.638 * k[k > 0.22 & k <= 0.8]^3 +
          12.336 * k[k > 0.22 & k <= 0.8]^4
        f[k > 0.8]              <- 0.165
        f <- pmin(pmax(f, 0), 1)
        return(f)
      }

      ## Apply diffuse fraction
      f.out    <- terra::app(k.out, Diff.frac.calc)
      Diff.out <- f.out * AORC
      terra::time(Diff.out) <- terra::time(AORC)
      names(Diff.out) <- sprintf("DiffuseRad_%s",
                                 format(terra::time(Diff.out), "%Y%m%d_%H"))

      ## Write output alongside DSWRF file
      out_file <- if (!is.null(study_area)) {
        file.path(month_dir, sprintf("SolaR_%s_%d_%02d_DifRad_surface.tif",
                                     study_area, y, m))
      } else {
        file.path(month_dir, sprintf("SolaR_%d_%02d_DifRad_surface.tif", y, m))
      }

      terra::writeRaster(Diff.out, out_file, overwrite = TRUE)
      cat(sprintf("  Saved: %s\n", basename(out_file)))

      rm(AORC, Bo_rast, Diff.out, Bo0_mat)
      gc()

      return(data.frame(year = y, month = m, status = "success",
                        stringsAsFactors = FALSE))

    }, error = function(e) {
      warning(sprintf("Failed %d-%02d: %s", y, m, e$message))
      return(data.frame(year = y, month = m,
                        status = paste("failed -", e$message),
                        stringsAsFactors = FALSE))
    })

  }, future.seed = TRUE)

  # Combine logs
  log <- do.call(rbind, results)

  # Summary
  n_success <- sum(log$status == "success",      na.rm = TRUE)
  n_skipped <- sum(grepl("skipped", log$status), na.rm = TRUE)
  n_failed  <- sum(grepl("failed",  log$status), na.rm = TRUE)

  cat(sprintf("\nDone. %d processed, %d skipped, %d failed.\n",
              n_success, n_skipped, n_failed))

  if (n_failed > 0) {
    cat("Failed combinations:\n")
    print(log[grepl("failed", log$status), ])
  }

  return(invisible(log))
}

#' Package Vegetation and Soil Parameters for Microclimate Modeling
#'
#' Iterates over years, assembling land cover, vegetation height, LAI, soil
#' data and reflectance into vegetation and soil parameter grids using
#' \code{microclimdata::create_veggrid()} and
#' \code{microclimdata::create_soilgrid()}. Reflectance data is averaged
#' across user specified snow free months. Outputs are saved as RDS files.
#'
#' @param years Integer vector of years to process e.g. c(2020, 2021)
#' @param months Integer vector of months for LAI e.g. 3:8
#' @param snow_free_months Integer vector of months to use for reflectance
#'   averaging e.g. 6:8
#' @param lc_dir Directory containing annual land cover files
#' @param vh_dir Directory containing annual vegetation height files
#' @param soil_path File path to the fine resolution soil data raster
#' @param lai_dir Directory containing fine resolution LAI files
#' @param refl_dir Directory containing leaf and ground reflectance files
#'   organized as \code{refl_dir/Lref/} and \code{refl_dir/Gref/}
#' @param vegpara_dir Directory to save vegetation parameter RDS outputs
#' @param soilpara_dir Directory to save soil parameter RDS outputs
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used to filter input files and prefix output file names
#' @param lctype Land cover classification type passed to
#'   \code{microclimdata::create_veggrid()} and
#'   \code{microclimdata::create_soilgrid()}. Must be either "CORINE" or "ESA".
#'   Default is "CORINE"
#' @param water Integer. Land cover code for water bodies passed to
#'   \code{microclimdata::create_soilgrid()}. Default is 512
#'
#' @return Invisibly returns a data frame logging the status of each
#'   year processed
#' @export
Pkg_Veg_Soil_data <- function(years,
                              months,
                              snow_free_months,
                              lc_dir,
                              vh_dir,
                              soil_path,
                              lai_dir,
                              refl_dir,
                              vegpara_dir,
                              soilpara_dir,
                              study_area = NULL,
                              lctype = "CORINE",
                              water = 512) {

  # Validate lctype
  if (!lctype %in% c("CORINE", "ESA")) {
    stop("lctype must be either 'CORINE' or 'ESA'")
  }

  dir.create(vegpara_dir,  recursive = TRUE, showWarnings = FALSE)
  dir.create(soilpara_dir, recursive = TRUE, showWarnings = FALSE)

  # Helper to extract YYYY_MM from file names
  extract_ym <- function(files) {
    matches <- regmatches(files, regexpr("[0-9]{4}_[0-9]{2}", files))
    data.frame(file = files, ym = matches, stringsAsFactors = FALSE)
  }

  # Helper to extract YYYY from file names
  extract_year <- function(files) {
    matches <- regmatches(files, regexpr("[0-9]{4}", files))
    data.frame(file = files, year = as.integer(matches), stringsAsFactors = FALSE)
  }

  # Load soil data once - does not change by year
  if (!file.exists(soil_path)) stop(sprintf("Soil file not found:\n  %s", soil_path))
  SD <- terra::rast(soil_path)

  # Build log
  log <- data.frame(year = years, status = NA_character_, stringsAsFactors = FALSE)

  cat(sprintf("\n--- Vegetation and Soil Parameter Packaging: %d year(s) ---\n\n",
              length(years)))

  for (y in years) {

    cat(sprintf("Processing year: %d\n", y))

    tryCatch({

      # --- Land cover ---
      lc_files <- list.files(lc_dir, pattern = "\\.tif$", full.names = TRUE)
      if (!is.null(study_area)) lc_files <- lc_files[grepl(study_area, lc_files)]
      lc_df    <- extract_year(lc_files)
      lc_match <- lc_df$file[lc_df$year == y]
      if (length(lc_match) == 0) stop(sprintf("Land cover file not found for year %d", y))
      if (length(lc_match) > 1) stop(sprintf("Multiple land cover files found for year %d", y))

      # --- Vegetation height ---
      vh_files <- list.files(vh_dir, pattern = "\\.tif$", full.names = TRUE)
      if (!is.null(study_area)) vh_files <- vh_files[grepl(study_area, vh_files)]
      vh_df    <- extract_year(vh_files)
      vh_match <- vh_df$file[vh_df$year == y]
      if (length(vh_match) == 0) stop(sprintf("Vegetation height file not found for year %d", y))
      if (length(vh_match) > 1) stop(sprintf("Multiple vegetation height files found for year %d", y))

      # --- LAI ---
      lai_files <- list.files(lai_dir, pattern = "\\.tif$", full.names = TRUE)
      if (!is.null(study_area)) lai_files <- lai_files[grepl(study_area, lai_files)]
      lai_df    <- extract_ym(lai_files)
      yms       <- sprintf("%d_%02d", y, months)
      lai_match <- lai_df$file[lai_df$ym %in% yms]
      lai_match <- lai_match[order(match(
        regmatches(lai_match, regexpr("[0-9]{4}_[0-9]{2}", lai_match)), yms
      ))]
      if (length(lai_match) == 0) stop(sprintf("No LAI files found for year %d", y))
      if (length(lai_match) != length(months)) {
        warning(sprintf("Expected %d LAI files for year %d, found %d",
                        length(months), y, length(lai_match)))
      }

      # --- Reflectance files filtered by year and snow free months ---
      sf_pattern <- paste(sprintf("%02d", snow_free_months), collapse = "|")

      lf_files <- list.files(file.path(refl_dir, "Lref"),
                             pattern = "\\.tif$", full.names = TRUE)
      lf_files <- lf_files[grepl(as.character(y), lf_files) &
                             (if (!is.null(study_area)) grepl(study_area, lf_files) else TRUE) &
                             grepl(sf_pattern, lf_files)]
      if (length(lf_files) == 0) stop(sprintf("No leaf reflectance files found for year %d", y))

      gf_files <- list.files(file.path(refl_dir, "Gref"),
                             pattern = "\\.tif$", full.names = TRUE)
      gf_files <- gf_files[grepl(as.character(y), gf_files) &
                             (if (!is.null(study_area)) grepl(study_area, gf_files) else TRUE) &
                             grepl(sf_pattern, gf_files)]
      if (length(gf_files) == 0) stop(sprintf("No ground reflectance files found for year %d", y))

      # --- Load rasters ---
      lc   <- terra::rast(lc_match)
      vght <- terra::rast(vh_match)
      lai  <- terra::rast(lai_match)

      refldata <- list(
        gref = terra::mean(terra::rast(gf_files)),
        lref = terra::mean(terra::rast(lf_files))
      )

      gc()

      # --- Align geometries ---
      all_rasters <- c(list(lai = lai, lc = lc, vght = vght, SD = SD), refldata)

      if (!terra::compareGeom(lai,
                              list(SD, vght, refldata$gref, refldata$lref, lc),
                              stopOnError = FALSE)) {

        cat("  Geometries differ: cropping and resampling to common extent...\n")

        common_ext <- Reduce(terra::intersect, lapply(all_rasters, terra::ext))
        template   <- terra::crop(lai, common_ext)

        lai_rs  <- terra::resample(terra::crop(lai,  common_ext), template, method = "bilinear")
        vght_rs <- terra::resample(terra::crop(vght, common_ext), template, method = "bilinear")
        SD_rs   <- terra::resample(terra::crop(SD,   common_ext), template, method = "bilinear")
        lc_rs   <- terra::resample(terra::crop(lc,   common_ext), template, method = "near")

        refldata_rs <- lapply(refldata, function(r) {
          terra::resample(terra::crop(r, common_ext), template, method = "bilinear")
        })

      } else {

        cat("  Geometries match: no resampling needed.\n")
        lai_rs      <- lai
        vght_rs     <- vght
        SD_rs       <- SD
        lc_rs       <- lc
        refldata_rs <- refldata

      }

      gc()

      # --- Name LAI layers by month ---
      names(lai_rs) <- sprintf("month_%02d", months[seq_len(terra::nlyr(lai_rs))])

      # --- Create vegetation parameter grid ---
      cat("  Building vegetation parameter grids...\n")

      vegp.list <- lapply(seq_len(terra::nlyr(lai_rs)), function(x) {
        microclimdata::create_veggrid(
          landcover = lc_rs,
          vhgt      = vght_rs,
          lai       = lai_rs[[x]],
          refldata  = refldata_rs,
          lctype    = lctype
        )
      })

      vegp <- vegp.list[[1]]

      if (terra::nlyr(lai_rs) > 1) {
        for (i in 2:terra::nlyr(lai_rs)) {
          pai   <- vegp.list[[i]]$pai
          clump <- vegp.list[[i]]$clump
          vegp$pai   <- terra::wrap(c(terra::unwrap(vegp$pai),   terra::unwrap(pai)))
          vegp$clump <- terra::wrap(c(terra::unwrap(vegp$clump), terra::unwrap(clump)))
        }
      }

      gc()

      # --- Save vegetation parameters ---
      veg_out <- file.path(vegpara_dir, if (!is.null(study_area)) {
        sprintf("%s_VegPara_%d.RDS", study_area, y)
      } else {
        sprintf("VegPara_%d.RDS", y)
      })
      readr::write_rds(vegp, file = veg_out)
      cat(sprintf("  Saved: %s\n", basename(veg_out)))

      gc()

      # --- Create soil parameter grid ---
      cat("  Building soil parameter grid...\n")

      soilc <- microclimdata::create_soilgrid(
        soildata  = SD_rs,
        refldata  = refldata_rs,
        landcover = lc_rs,
        water     = water
      )

      soil_out <- file.path(soilpara_dir, if (!is.null(study_area)) {
        sprintf("%s_SoilPara_%d.RDS", study_area, y)
      } else {
        sprintf("SoilPara_%d.RDS", y)
      })
      readr::write_rds(soilc, file = soil_out)
      cat(sprintf("  Saved: %s\n", basename(soil_out)))

      log$status[log$year == y] <- "success"

    }, error = function(e) {
      warning(sprintf("Failed year %d: %s", y, e$message))
      log$status[log$year == y] <<- paste("failed -", e$message)
    })

    gc()
  }

  cat(sprintf("\nDone. %d/%d years processed successfully.\n",
              sum(log$status == "success", na.rm = TRUE), length(years)))

  return(invisible(log))
}

#' Compute Multi-Year Climate Normals from AORC and Diffuse Radiation Data
#'
#' For each month, stacks a given hour across all available years and computes
#' user-specified summary statistics. Output is one multi-layer GeoTIFF per
#' variable per month per statistic, with layers ordered chronologically
#' representing each hour of the month. The representative hour sequence is
#' derived from the most recent non-leap year.
#'
#' @details AORC variables processed are: APCP_surface, DLWRF_surface,
#'   DSWRF_surface, PRES_surface, SPFH_2maboveground, TMP_2maboveground,
#'   UGRD_10maboveground, VGRD_10maboveground, and DifRad_surface.
#'   Input files must follow the directory structure produced by
#'   \code{download_aorc()} and \code{estimate_diffuse_rad()}.
#'   Missing files for individual years are skipped and logged.
#'
#' @param months Integer vector of months to include in the summary e.g. 3:6
#' @param years Integer vector of years to include in the summary e.g. 1995:2024
#' @param aorc_dir Base directory containing AORC data organized as
#'   \code{aorc_dir/study_area/year/month/} or \code{aorc_dir/year/month/}
#' @param out_dir Base directory for output files organized as
#'   \code{out_dir/month/}
#' @param stats Character vector of summary statistics to compute. Must include
#'   at least one of: "mean", "median", "mode", "sd", "min", "max"
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used to locate input files and prefix output file names
#' @param workers Integer. Number of parallel workers via \code{future.apply}.
#'   Default is 1 (sequential)
#'
#' @return Invisibly returns a data frame logging the status of each
#'   month/variable combination processed
#' @export
summarize_climate_normals <- function(months,
                                      years,
                                      aorc_dir,
                                      out_dir,
                                      stats = c("mean", "median", "mode", "sd", "min", "max"),
                                      study_area = NULL,
                                      workers = 1) {

  # Validate stats
  valid_stats <- c("mean", "median", "mode", "sd", "min", "max")
  invalid     <- stats[!stats %in% valid_stats]
  if (length(stats) == 0) stop("At least one stat must be specified.")
  if (length(invalid) > 0) {
    stop(sprintf("Invalid stat(s): %s. Must be one of: %s",
                 paste(invalid, collapse = ", "),
                 paste(valid_stats, collapse = ", ")))
  }

  # AORC variables
  aorc_vars <- c("APCP_surface", "DLWRF_surface", "DSWRF_surface",
                 "PRES_surface", "SPFH_2maboveground", "TMP_2maboveground",
                 "UGRD_10maboveground", "VGRD_10maboveground")

  # Find most recent non-leap year for reference hour sequence
  ref_year <- max(years[!lubridate::leap_year(years)])
  cat(sprintf("Using %d as reference year for hour sequence.\n", ref_year))

  # Build month/variable combos
  combos <- expand.grid(
    month = months,
    var   = c(aorc_vars, "DifRad_surface"),
    stringsAsFactors = FALSE
  )

  # Build log
  log <- data.frame(
    month    = combos$month,
    variable = combos$var,
    status   = NA_character_,
    stringsAsFactors = FALSE
  )

  cat(sprintf("\n--- Climate Normals: %d year(s), %d month/variable combos ---\n\n",
              length(years), nrow(combos)))

  # Mode helper
  rast_mode <- function(x) {
    ux <- unique(x[!is.na(x)])
    if (length(ux) == 0) return(NA_real_)
    ux[which.max(tabulate(match(x, ux)))]
  }

  # Set up parallel plan
  if (workers > 1) {
    future::plan(future::multisession, workers = workers)
    on.exit(future::plan(future::sequential), add = TRUE)
  }

  results <- future.apply::future_lapply(seq_len(nrow(combos)), function(i) {

    m   <- combos$month[i]
    var <- combos$var[i]

    is_difrad <- var == "DifRad_surface"

    cat(sprintf("Processing month: %02d | variable: %s\n", m, var))

    # Output directory
    month_out <- file.path(out_dir, sprintf("%02d", m))
    dir.create(month_out, recursive = TRUE, showWarnings = FALSE)

    # Reference hour sequence for this month
    ref_start <- as.POSIXct(sprintf("%d-%02d-01 00:00:00", ref_year, m),
                            tz = "UTC")
    ref_end   <- lubridate::add_with_rollback(ref_start, months(1), roll_to_first = T) -
      lubridate::hours(1)

    dtime     <- seq.POSIXt(ref_start, ref_end, by = "1 hour")

    # Missing file log
    missing_log <- character(0)

    # For each reference hour, stack across years
    hour_means   <- if ("mean"   %in% stats) vector("list", length(dtime)) else NULL
    hour_medians <- if ("median" %in% stats) vector("list", length(dtime)) else NULL
    hour_modes   <- if ("mode"   %in% stats) vector("list", length(dtime)) else NULL
    hour_sds     <- if ("sd"     %in% stats) vector("list", length(dtime)) else NULL
    hour_mins    <- if ("min"    %in% stats) vector("list", length(dtime)) else NULL
    hour_maxs    <- if ("max"    %in% stats) vector("list", length(dtime)) else NULL

    for (t_idx in seq_along(dtime)) {

      t <- dtime[t_idx]

      # Build file pattern for this month across all years
      year_layers <- vector("list", length(years))

      for (y_idx in seq_along(years)) {

        y <- years[y_idx]

        # Build file path based on variable type
        month_dir <- file.path(aorc_dir, y, m)

        if (!is.null(study_area)) {
          if (is_difrad) {
            fname <- sprintf("SolaR_%s_%d_%02d_DifRad_surface.tif", study_area, y, m)
          } else {
            fname <- sprintf("AORC_%s_%d_%02d_%s.nc", study_area, y, m, var)
          }
        } else {
          if (is_difrad) {
            fname <- sprintf("SolaR_%d_%02d_DifRad_surface.tif", y, m)
          } else {
            fname <- sprintf("AORC_%d_%02d_%s.nc", y, m, var)
          }
        }

        fpath <- file.path(month_dir, fname)

        if (!file.exists(fpath)) {
          missing_log <- c(missing_log, fpath)
          next
        }

        r <- tryCatch(terra::rast(fpath), error = function(e) NULL)
        if (is.null(r)) {
          missing_log <- c(missing_log, fpath)
          next
        }

        # Match layer by month-day-hour
        t_match <- format(t, "%m-%d %H")
        r_times <- terra::time(r)
        lay     <- which(format(r_times, "%m-%d %H") == t_match)

        if (length(lay) == 0) {
          missing_log <- c(missing_log, sprintf("%s [hour not found: %s]", fpath, t_match))
          next
        }

        year_layers[[y_idx]] <- r[[lay]]
        rm(r)
      }

      # Remove NULLs
      year_layers <- Filter(Negate(is.null), year_layers)

      if (length(year_layers) == 0) next

      # Stack across years
      year_stack <- do.call(c, year_layers)

      # Compute requested stats
      if ("mean"   %in% stats) hour_means[[t_idx]]   <- terra::mean(year_stack,   na.rm = TRUE)
      if ("median" %in% stats) hour_medians[[t_idx]] <- terra::median(year_stack, na.rm = TRUE)
      if ("sd"     %in% stats) hour_sds[[t_idx]]     <- terra::stdev(year_stack,  na.rm = TRUE)
      if ("min"    %in% stats) hour_mins[[t_idx]]     <- min(year_stack,           na.rm = TRUE)
      if ("max"    %in% stats) hour_maxs[[t_idx]]     <- max(year_stack,           na.rm = TRUE)
      if ("mode"   %in% stats) {
        hour_modes[[t_idx]] <- terra::app(year_stack, rast_mode)
      }

      rm(year_stack)
    }

    # Build base name without year
    base_name <- if (!is.null(study_area)) {
      if (is_difrad) {
        sprintf("SolaR_%s_%02d_DifRad_surface", study_area, m)
      } else {
        sprintf("AORC_%s_%02d_%s", study_area, m, var)
      }
    } else {
      if (is_difrad) {
        sprintf("SolaR_%02d_DifRad_surface", m)
      } else {
        sprintf("AORC_%02d_%s", m, var)
      }
    }

    # Helper to assemble and write a stat raster
    write_stat <- function(hour_list, stat_name) {
      valid <- Filter(Negate(is.null), hour_list)
      if (length(valid) == 0) return(invisible(NULL))
      stack <- do.call(c, valid)
      stack <- stack[[order(terra::time(stack))]]
      out_file <- file.path(month_out,
                            sprintf("%s_%s.tif", base_name, stat_name))
      terra::writeRaster(stack, out_file, overwrite = TRUE)
      cat(sprintf("  Saved: %s\n", basename(out_file)))
    }

    if ("mean"   %in% stats) write_stat(hour_means,   "mean")
    if ("median" %in% stats) write_stat(hour_medians, "median")
    if ("sd"     %in% stats) write_stat(hour_sds,     "sd")
    if ("min"    %in% stats) write_stat(hour_mins,    "min")
    if ("max"    %in% stats) write_stat(hour_maxs,    "max")
    if ("mode"   %in% stats) write_stat(hour_modes,   "mode")

    # Log missing files
    if (length(missing_log) > 0) {
      warning(sprintf("Month %02d | %s: %d file(s) missing or unreadable:\n%s",
                      m, var, length(missing_log),
                      paste(" ", missing_log, collapse = "\n")))
    }

    return(data.frame(
      month    = m,
      variable = var,
      status   = "success",
      missing  = length(missing_log),
      stringsAsFactors = FALSE
    ))

  }, future.seed = TRUE)

  # Combine logs
  log <- do.call(rbind, results)

  # Summary
  n_success <- sum(log$status == "success", na.rm = TRUE)
  n_missing <- sum(log$missing,             na.rm = TRUE)

  cat(sprintf("\nDone. %d/%d combos processed. %d file(s) skipped due to missing data.\n",
              n_success, nrow(combos), n_missing))

  return(invisible(log))
}

#' Package AORC Climate Data for Microclimate Modeling
#'
#' Loads AORC climate data, converts units, computes derived variables
#' (relative humidity, wind speed, wind direction), reprojects and crops
#' to a template raster, and assembles everything into a microclimf-ready
#' named list. Outputs one RDS per year concatenating all months in the
#' order they are provided. Monthly RDS files are saved to a temporary
#' subdirectory and optionally cleaned up after concatenation.
#'
#' @details Unit conversions applied:
#'   \itemize{
#'     \item Air temperature: Kelvin to Celsius (subtract 273.15)
#'     \item Pressure: Pa to kPa (divide by 1000)
#'     \item Specific humidity + pressure converted to relative humidity
#'     \item U and V wind components converted to wind speed and direction
#'   }
#'   Diffuse radiation layers are aligned to match shortwave radiation
#'   timestamps, with any unmatched layers dropped.
#'
#'   A warning is issued reminding the user that the template raster should
#'   be representative of vegetation or soil parameter outputs.
#'
#' @param months Integer vector of months to process in the desired
#'   concatenation order e.g. c(3:8) for spring/summer or c(7:12, 1:6)
#'   for a water year. Months are processed and concatenated in this order
#' @param years Integer vector of years to process e.g. c(2020, 2021).
#'   A separate RDS is created per year
#' @param input_dir Base directory containing AORC data organized as
#'   \code{input_dir/year/month/} matching the structure produced by
#'   \code{download_aorc()}
#' @param output_dir Directory to save final yearly output RDS files.
#'   Temporary monthly RDS files are saved to \code{output_dir/year/}
#'   subdirectories
#' @param template A SpatRaster used as the CRS and extent template for
#'   reprojection and cropping. Should be representative of vegetation or
#'   soil parameter outputs
#' @param study_area Optional character string identifying the study area
#'   e.g. "GMU1". If provided, used to filter input files and prefix output
#'   file names
#' @param keep_monthly Logical. Whether to keep the temporary monthly RDS
#'   files after the yearly RDS has been created. If FALSE the temporary
#'   year subdirectory and its contents are deleted after concatenation.
#'   Default is FALSE
#'
#' @return Invisibly returns a named list of output file paths keyed by year
#' @export
package_climate <- function(months,
                            years,
                            input_dir,
                            output_dir,
                            template,
                            study_area = NULL,
                            keep_monthly = FALSE) {

  if (!inherits(template, "SpatRaster")) {
    stop("template must be a SpatRaster")
  }

  warning("Ensure the template raster is representative of vegetation or soil parameter outputs for correct spatial alignment.")

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Helper to load, sort, reproject and crop a variable raster
  load_var <- function(files, template) {
    r <- terra::rast(files)
    names(r) <- ifelse(
      grepl("\\d{2}:\\d{2}", names(r)),
      names(r),
      paste0(names(r), " 00:00:00")
    )
    t.r <- as.POSIXct(names(r), format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
    if (all(is.na(t.r))) t.r <- terra::time(r)
    r   <- r[[order(t.r)]]
    terra::time(r) <- t.r[order(t.r)]
    r <- terra::project(r, terra::crs(template), method = "near", threads = TRUE)
    r <- terra::crop(r, terra::ext(template))
    return(r)
  }

  # Build log
  log <- list()

  for (y in years) {

    cat(sprintf("\n--- Packaging climate for year: %d ---\n", y))

    # Create temp directory for monthly RDS files
    temp_dir <- file.path(output_dir, as.character(y))
    dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

    month_rds_paths <- list()

    for (m in months) {

      cat(sprintf("  Processing month: %02d\n", m))

      # Build month directory matching download_aorc() structure
      month_dir <- file.path(input_dir, y, m)

      if (!dir.exists(month_dir)) {
        warning(sprintf("Directory not found for year %d month %02d:\n  %s", y, m, month_dir))
        next
      }

      clim_files <- list.files(month_dir, full.names = TRUE, recursive = FALSE)

      # Filter by study area if provided
      if (!is.null(study_area)) {
        clim_files <- clim_files[grepl(study_area, basename(clim_files))]
      }

      names(clim_files) <- basename(clim_files)

      if (length(clim_files) == 0) {
        warning(sprintf("No climate files found for year %d month %02d", y, m))
        next
      }

      # --- Shortwave radiation ---
      sw_f <- clim_files[grepl("DSWRF", names(clim_files))]
      if (length(sw_f) == 0) stop(sprintf("No DSWRF files found for year %d month %02d", y, m))
      r.sw <- load_var(sw_f, template)
      sw.t <- terra::time(r.sw)
      r.sw <- terra::wrap(r.sw)

      # --- Diffuse radiation - align to shortwave timestamps ---
      df_f <- clim_files[grepl("DifRad|DiffRad", names(clim_files))]
      if (length(df_f) == 0) {
        warning(sprintf("No diffuse radiation files found for year %d month %02d", y, m))
        r.df <- NULL
      } else {
        r_df              <- terra::rast(df_f)
        names(sw.t)       <- format(sw.t, "%m%d%H")
        r_df_times        <- terra::time(r_df)
        names(r_df_times) <- format(r_df_times, "%m%d%H")
        names_only_in_r   <- setdiff(names(r_df_times), names(sw.t))
        r_df              <- r_df[[!(names(r_df_times) %in% names_only_in_r)]]
        r_df_times        <- terra::time(r_df)
        names(r_df_times) <- format(r_df_times, "%m%d%H")
        sw.t_aligned      <- sw.t[names(r_df_times)]
        terra::time(r_df) <- sw.t_aligned
        names(r_df) <- ifelse(
          grepl("\\d{2}:\\d{2}", as.character(terra::time(r_df))),
          as.character(terra::time(r_df)),
          paste0(terra::time(r_df), " 00:00:00")
        )
        t.r               <- as.POSIXct(names(r_df), format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
        r_df              <- r_df[[order(t.r)]]
        terra::time(r_df) <- t.r[order(t.r)]
        r_df <- terra::project(r_df, terra::crs(template), method = "near", threads = TRUE)
        r_df <- terra::crop(r_df, terra::ext(template))
        r.df <- terra::wrap(r_df)
        rm(r_df)
      }

      # --- Longwave radiation ---
      lw_f <- clim_files[grepl("DLWRF", names(clim_files))]
      if (length(lw_f) == 0) stop(sprintf("No DLWRF files found for year %d month %02d", y, m))
      r.lw <- terra::wrap(load_var(lw_f, template))

      # --- Precipitation ---
      pr_f <- clim_files[grepl("APCP", names(clim_files))]
      if (length(pr_f) == 0) stop(sprintf("No APCP files found for year %d month %02d", y, m))
      r.pr <- terra::wrap(load_var(pr_f, template))

      # --- Air temperature: K to C ---
      at_f <- clim_files[grepl("TMP", names(clim_files))]
      if (length(at_f) == 0) stop(sprintf("No TMP files found for year %d month %02d", y, m))
      r_at <- load_var(at_f, template)
      p.at <- r_at
      r_at <- r_at - 273.15
      r.at <- terra::wrap(r_at)

      # --- Pressure: Pa to kPa ---
      pa_f <- clim_files[grepl("PRES", names(clim_files))]
      if (length(pa_f) == 0) stop(sprintf("No PRES files found for year %d month %02d", y, m))
      r_pa <- load_var(pa_f, template)
      p.Pa <- r_pa
      r_pa <- r_pa / 1000
      r.pa <- terra::wrap(r_pa)

      # --- Relative humidity from specific humidity + pressure ---
      sh_f <- clim_files[grepl("SPFH", names(clim_files))]
      if (length(sh_f) == 0) stop(sprintf("No SPFH files found for year %d month %02d", y, m))
      r_sh     <- load_var(sh_f, template)
      e        <- (r_sh * p.Pa) / (0.622 + 0.378 * r_sh)
      es_water <- 611.2 * exp((17.67 * p.at) / (p.at + 243.5))
      es       <- es_water
      RH       <- 100 * (e / es)
      RH       <- terra::clamp(RH, lower = 0, upper = 100)
      r.rh     <- terra::wrap(RH)
      rm(e, es, es_water, RH, p.at, p.Pa, r_sh)

      # --- Wind speed and direction from U and V components ---
      uw_f <- clim_files[grepl("UGRD", names(clim_files))]
      vw_f <- clim_files[grepl("VGRD", names(clim_files))]
      if (length(uw_f) == 0) stop(sprintf("No UGRD files found for year %d month %02d", y, m))
      if (length(vw_f) == 0) stop(sprintf("No VGRD files found for year %d month %02d", y, m))
      r_u  <- load_var(uw_f, template)
      r_v  <- load_var(vw_f, template)
      ws   <- sqrt(r_u^2 + r_v^2)
      wd   <- (180 + terra::atan2(r_u, r_v) * 180 / pi) %% 360
      r.ws <- terra::wrap(ws)
      r.wd <- terra::wrap(wd)
      rm(ws, wd, r_u, r_v)

      gc()

      # --- Assemble output list ---
      out <- list(
        precip    = r.pr,
        temp      = r.at,
        relhum    = r.rh,
        lwdown    = r.lw,
        swdown    = r.sw,
        pres      = r.pa,
        windspeed = r.ws,
        winddir   = r.wd,
        difrad    = r.df
      )

      # --- Save monthly RDS to temp directory ---
      month_file <- if (!is.null(study_area)) {
        file.path(temp_dir, sprintf("%s_Climate_%d_Month_%02d.RDS", study_area, y, m))
      } else {
        file.path(temp_dir, sprintf("Climate_%d_Month_%02d.RDS", y, m))
      }

      readr::write_rds(out, file = month_file)
      cat(sprintf("    Saved temp: %s\n", basename(month_file)))
      month_rds_paths[[sprintf("%02d", m)]] <- month_file

      rm(out, r.pr, r.at, r.rh, r.lw, r.sw, r.pa, r.ws, r.wd, r.df)
      gc()
    }

    # --- Concatenate months in the order provided ---
    if (length(month_rds_paths) >= 1) {

      cat(sprintf("  Concatenating months for year %d in specified order...\n", y))

      ordered_keys  <- sprintf("%02d", months)
      ordered_keys  <- ordered_keys[ordered_keys %in% names(month_rds_paths)]
      ordered_paths <- month_rds_paths[ordered_keys]

      # Load first month as base
      int    <- readr::read_rds(ordered_paths[[1]])
      vnames <- names(int)

      # Concatenate remaining months if more than one
      if (length(ordered_paths) > 1) {
        for (k in seq_along(ordered_paths)[-1]) {
          out <- readr::read_rds(ordered_paths[[k]])
          int <- lapply(vnames, function(x) {
            var1 <- terra::rast(int[[x]])
            var2 <- terra::rast(out[[x]])
            terra::wrap(c(var1, var2))
          })
          names(int) <- vnames
          rm(out)
          gc()
        }
      }

      # --- Save year RDS to base output directory ---
      year_file <- if (!is.null(study_area)) {
        file.path(output_dir, sprintf("%s_Climate_%d.RDS", study_area, y))
      } else {
        file.path(output_dir, sprintf("Climate_%d.RDS", y))
      }

      readr::write_rds(int, file = year_file)
      cat(sprintf("  Saved: %s\n", basename(year_file)))
      log[[as.character(y)]] <- year_file

      rm(int)
      gc()

      # --- Clean up temp directory if keep_monthly is FALSE ---
      if (!keep_monthly) {
        unlink(temp_dir, recursive = TRUE)
        cat(sprintf("  Temporary monthly files removed for year %d.\n", y))
      } else {
        cat(sprintf("  Temporary monthly files kept in: %s\n", temp_dir))
      }
    }
  }

  cat("\nDone.\n")
  return(invisible(log))
}
