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

# --------------------------------------------------------------------------- #
#  Metadata lookup lists for write_tile / stitch_tiles
# --------------------------------------------------------------------------- #

.mout_meta <- list(
  Tz        = list(units = "degrees_C", long_name = "Air/soil temperature at model height"),
  tleaf     = list(units = "degrees_C", long_name = "Leaf temperature"),
  relhum    = list(units = "percent",   long_name = "Relative humidity"),
  soilm     = list(units = "m3 m-3",   long_name = "Volumetric soil moisture"),
  windspeed = list(units = "m s-1",    long_name = "Wind speed at model height"),
  Rdirdown  = list(units = "W m-2",    long_name = "Downward direct shortwave radiation"),
  Rdifdown  = list(units = "W m-2",    long_name = "Downward diffuse shortwave radiation"),
  Rlwdown   = list(units = "W m-2",    long_name = "Downward longwave radiation"),
  Rswup     = list(units = "W m-2",    long_name = "Upward shortwave radiation"),
  Rlwup     = list(units = "W m-2",    long_name = "Upward longwave radiation")
)

.smod_meta <- list(
  Tc              = list(units = "degrees_C", long_name = "Canopy air temperature"),
  Tg              = list(units = "degrees_C", long_name = "Ground surface temperature"),
  groundsnowdepth = list(units = "m",         long_name = "Snow depth on ground"),
  totalSWE        = list(units = "mm",        long_name = "Total snow water equivalent"),
  snowden         = list(units = "kg m-3",    long_name = "Snow density"),
  umu             = list(units = "",          long_name = "umu")
)

.detect_data_type <- function(data) {
  nm <- names(data)
  if ("Tz" %in% nm) return("mout")
  if ("Tc" %in% nm) return("smod")
  stop("Cannot determine data type: names must include 'Tz' (mout) or 'Tc' (smod)")
}

# --------------------------------------------------------------------------- #
#  HDF5 write helper
# --------------------------------------------------------------------------- #

.write_h5 <- function(data_list, var_meta, tme, out_path, dtm, data_type,
                      compression) {
  rhdf5::h5createFile(out_path)

  nrow_  <- dim(data_list[[1]])[1]
  ncol_  <- dim(data_list[[1]])[2]
  ntime_ <- dim(data_list[[1]])[3]
  chunk  <- c(nrow_, ncol_, min(ntime_, 24L))

  fid <- rhdf5::H5Fopen(out_path)
  tryCatch({
    for (vn in names(var_meta)) {
      rhdf5::h5createDataset(fid, vn,
                             dims         = c(nrow_, ncol_, ntime_),
                             storage.mode = "double",
                             chunk        = chunk,
                             level        = compression)
      rhdf5::h5write(data_list[[vn]], fid, vn)
      did <- rhdf5::H5Dopen(fid, vn)
      rhdf5::h5writeAttribute(var_meta[[vn]]$units,     did, "units")
      rhdf5::h5writeAttribute(var_meta[[vn]]$long_name, did, "long_name")
      rhdf5::H5Dclose(did)
    }

    if (!is.null(tme)) {
      time_str <- format(as.POSIXct(tme), "%Y-%m-%dT%H:%M:%S", tz = "UTC")
      rhdf5::h5write(time_str, fid, "time")
    }

    gid <- rhdf5::H5Gopen(fid, "/")
    tryCatch({
      if (!is.null(dtm)) {
        e <- terra::ext(dtm)
        rhdf5::h5writeAttribute(as.numeric(e$xmin),               gid, "xmin")
        rhdf5::h5writeAttribute(as.numeric(e$xmax),               gid, "xmax")
        rhdf5::h5writeAttribute(as.numeric(e$ymin),               gid, "ymin")
        rhdf5::h5writeAttribute(as.numeric(e$ymax),               gid, "ymax")
        rhdf5::h5writeAttribute(terra::res(dtm)[1],               gid, "res_x")
        rhdf5::h5writeAttribute(terra::res(dtm)[2],               gid, "res_y")
        rhdf5::h5writeAttribute(as.integer(terra::nrow(dtm)),     gid, "nrow")
        rhdf5::h5writeAttribute(as.integer(terra::ncol(dtm)),     gid, "ncol")
        rhdf5::h5writeAttribute(terra::crs(dtm, proj = FALSE),    gid, "crs_wkt")
      } else {
        warning("dtm is NULL: spatial metadata will not be written to HDF5 file.")
      }
      rhdf5::h5writeAttribute(data_type, gid, "data_type")
      if (!is.null(tme)) {
        rhdf5::h5writeAttribute(
          format(as.POSIXct(tme[1]),          "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
          gid, "period_start")
        rhdf5::h5writeAttribute(
          format(as.POSIXct(tme[length(tme)]), "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
          gid, "period_end")
      }
    }, finally = rhdf5::H5Gclose(gid))
  }, finally = {
    rhdf5::H5Fclose(fid)
    rhdf5::H5close()
  })
  invisible(NULL)
}

# --------------------------------------------------------------------------- #
#  NetCDF write helper
# --------------------------------------------------------------------------- #

.write_nc <- function(data_list, var_meta, tme, out_path, dtm, data_type,
                      compression) {
  nrow_  <- dim(data_list[[1]])[1]
  ncol_  <- dim(data_list[[1]])[2]
  ntime_ <- dim(data_list[[1]])[3]

  if (!is.null(dtm)) {
    x_vals <- terra::xFromCol(dtm, seq_len(ncol_))
    y_vals <- terra::yFromRow(dtm, seq_len(nrow_))
  } else {
    warning("dtm is NULL: x/y coordinate variables will use integer indices.")
    x_vals <- seq_len(ncol_)
    y_vals <- seq_len(nrow_)
  }

  t_origin <- if (!is.null(tme)) {
    format(as.POSIXct(tme[1], tz = "UTC"), "%Y-%m-%dT%H:%M:%S")
  } else {
    "1970-01-01T00:00:00"
  }
  t_vals <- if (!is.null(tme)) {
    as.numeric(difftime(as.POSIXct(tme, tz = "UTC"),
                        as.POSIXct(tme[1], tz = "UTC"), units = "hours"))
  } else {
    seq_len(ntime_) - 1L
  }

  dim_x    <- ncdf4::ncdim_def("x",    "m",     x_vals, longname = "x coordinate")
  dim_y    <- ncdf4::ncdim_def("y",    "m",     y_vals, longname = "y coordinate")
  dim_time <- ncdf4::ncdim_def("time", sprintf("hours since %s UTC", t_origin),
                               t_vals, unlim = TRUE,
                               longname = "time", calendar = "standard")

  var_crs <- ncdf4::ncvar_def("crs", "", list(), prec = "integer",
                              longname = "CRS definition")

  data_vars <- lapply(names(var_meta), function(vn) {
    ncdf4::ncvar_def(vn, var_meta[[vn]]$units,
                     list(dim_x, dim_y, dim_time),
                     missval     = -9999.0,
                     longname    = var_meta[[vn]]$long_name,
                     compression = compression,
                     prec        = "double")
  })
  names(data_vars) <- names(var_meta)

  nc <- ncdf4::nc_create(out_path, c(list(var_crs), data_vars))
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  ncdf4::ncvar_put(nc, var_crs, 0L)
  if (!is.null(dtm)) {
    ncdf4::ncatt_put(nc, "crs", "crs_wkt",          terra::crs(dtm, proj = FALSE))
    ncdf4::ncatt_put(nc, "crs", "grid_mapping_name", "unknown")
  }

  for (vn in names(var_meta)) {
    arr <- aperm(data_list[[vn]], c(2L, 1L, 3L))
    arr[is.na(arr)] <- -9999.0
    ncdf4::ncvar_put(nc, data_vars[[vn]], arr)
    ncdf4::ncatt_put(nc, vn, "units",        var_meta[[vn]]$units)
    ncdf4::ncatt_put(nc, vn, "long_name",    var_meta[[vn]]$long_name)
    ncdf4::ncatt_put(nc, vn, "grid_mapping", "crs")
  }

  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.8")
  ncdf4::ncatt_put(nc, 0, "data_type",   data_type)
  ncdf4::ncatt_put(nc, 0, "history",
                   sprintf("Created %s by R %s / SpencerEcoTools",
                           format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                           paste(R.version$major, R.version$minor, sep = ".")))
  invisible(NULL)
}

# --------------------------------------------------------------------------- #
#  write_tile — exported
# --------------------------------------------------------------------------- #

#' Write a microclimf tile output to HDF5 or NetCDF
#'
#' Serializes a single \code{mout} or \code{smod} list returned by
#' \code{microclimf} spatial model functions to an HDF5 or NetCDF-4 file with
#' full variable metadata, spatial coordinates, and CRS information.
#'
#' @param data List. Either an \code{mout} object (must contain element
#'   \code{"Tz"}) or an \code{smod} object (must contain element \code{"Tc"}).
#'   All data arrays must have dimensions \code{[nrow, ncol, ntime]}.
#'   \code{mout} lists may include a \code{$tme} POSIXct element which is
#'   used automatically when \code{tme = NULL}.
#' @param out_path Character. Output file path. The extension is ignored; format
#'   is controlled by \code{file_fmt}.
#' @param dtm \code{terra::SpatRaster} at tile resolution. Used to embed spatial
#'   extent, CRS, and cell coordinates. If \code{NULL}, spatial metadata is
#'   omitted with a warning.
#' @param tme POSIXct vector of length \code{ntime}. Required for \code{smod}
#'   objects (which have no \code{$tme} element). For \code{mout} objects,
#'   defaults to \code{data$tme} when \code{NULL}.
#' @param file_fmt Character. Output format: \code{"h5"} (HDF5 via
#'   \code{rhdf5}) or \code{"nc"} (NetCDF-4 via \code{ncdf4}). Default
#'   \code{"h5"}.
#' @param compression Integer 0–9. Gzip compression level applied to each data
#'   variable. 0 = no compression; 9 = maximum compression. Default 4.
#'
#' @return Invisibly, a one-row \code{data.frame} with columns \code{file_path}
#'   and \code{data_type}.
#'
#' @details
#' **HDF5 layout** (\code{file_fmt = "h5"}, requires \code{rhdf5}):
#' One dataset per variable at \code{[nrow, ncol, ntime]} plus a \code{time}
#' dataset (ISO 8601 character strings). Spatial and period metadata are stored
#' as HDF5 attributes on the root group \code{"/"}. Per-variable \code{units}
#' and \code{long_name} attributes are written on each dataset.
#'
#' **NetCDF layout** (\code{file_fmt = "nc"}, requires \code{ncdf4}):
#' CF-1.8 compliant. Dimensions \code{x}, \code{y}, \code{time} with
#' cell-centre coordinate variables. A scalar \code{crs} variable carries
#' the WKT string. All data variables carry \code{grid_mapping = "crs"}.
#' Array dimensions are permuted from R's \code{[nrow, ncol, ntime]} to
#' NetCDF's \code{(x, y, time)} storage order before writing.
#'
#' Both formats apply gzip compression at the level set by \code{compression}.
#' HDF5 uses chunk dimensions \code{[nrow, ncol, min(ntime, 24)]} (time slabs).
#'
#' @seealso \code{\link{stitch_tiles}} to combine tile files into a single
#'   domain-wide file. \code{\link{create_tiles}} for generating tile extents.
#'
#' @export
write_tile <- function(data, out_path, dtm = NULL, tme = NULL,
                       file_fmt = "h5", compression = 4L) {
  file_fmt    <- match.arg(file_fmt, c("h5", "nc"))
  compression <- as.integer(compression)
  if (compression < 0L || compression > 9L)
    stop("compression must be an integer between 0 and 9")

  data_type <- .detect_data_type(data)
  var_meta  <- if (data_type == "mout") .mout_meta else .smod_meta

  if (is.null(tme)) {
    if (data_type == "mout" && !is.null(data$tme)) {
      tme <- data$tme
    } else {
      warning("tme is NULL and could not be extracted from data. Time metadata will be omitted.")
    }
  }

  data_list <- data[names(var_meta)]

  pkg <- if (file_fmt == "h5") "rhdf5" else "ncdf4"
  if (!requireNamespace(pkg, quietly = TRUE))
    stop(sprintf("Package '%s' is required. Install with: %s", pkg,
                 if (pkg == "rhdf5") 'BiocManager::install("rhdf5")'
                 else sprintf('install.packages("%s")', pkg)))

  if (file_fmt == "h5") {
    .write_h5(data_list, var_meta, tme, out_path, dtm, data_type, compression)
  } else {
    .write_nc(data_list, var_meta, tme, out_path, dtm, data_type, compression)
  }

  invisible(data.frame(file_path = out_path, data_type = data_type,
                       stringsAsFactors = FALSE))
}

# --------------------------------------------------------------------------- #
#  Stitch helpers
# --------------------------------------------------------------------------- #

.read_h5_tile_attrs <- function(path) {
  attrs <- rhdf5::h5readAttributes(path, "/")
  rhdf5::H5close()
  attrs
}

.read_nc_tile_attrs <- function(path) {
  nc    <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  x_v   <- nc$dim$x$vals
  y_v   <- nc$dim$y$vals
  res_x <- abs(diff(x_v))[1]
  res_y <- abs(diff(y_v))[1]
  list(
    xmin  = min(x_v) - res_x / 2,
    xmax  = max(x_v) + res_x / 2,
    ymin  = min(y_v) - res_y / 2,
    ymax  = max(y_v) + res_y / 2,
    res_x = res_x,
    res_y = res_y,
    nrow  = nc$dim$y$len,
    ncol  = nc$dim$x$len,
    ntime = nc$dim$time$len
  )
}

.rel_path <- function(target, from_file) {
  t_norm  <- normalizePath(target,    mustWork = FALSE)
  f_dir   <- dirname(normalizePath(from_file, mustWork = FALSE))
  t_parts <- strsplit(t_norm, "[/\\\\]")[[1]]
  f_parts <- strsplit(f_dir,  "[/\\\\]")[[1]]
  t_parts <- t_parts[nchar(t_parts) > 0L]
  f_parts <- f_parts[nchar(f_parts) > 0L]
  n_common <- 0L
  for (i in seq_len(min(length(t_parts), length(f_parts)))) {
    if (tolower(t_parts[i]) == tolower(f_parts[i])) n_common <- i else break
  }
  ups   <- rep("..", length(f_parts) - n_common)
  downs <- t_parts[seq(n_common + 1L, length(t_parts))]
  paste(c(ups, downs), collapse = "/")
}

.stitch_h5_extlinks <- function(tile_files, out_file, var_names) {
  rhdf5::h5createFile(out_file)
  fid_out <- rhdf5::H5Fopen(out_file)
  on.exit({ rhdf5::H5Fclose(fid_out); rhdf5::H5close() }, add = TRUE)
  for (tf in tile_files) {
    rel   <- .rel_path(tf, out_file)
    tname <- tools::file_path_sans_ext(basename(tf))
    rhdf5::H5Lcreate_external(rel, "/", fid_out, tname)
  }
  invisible(NULL)
}

.stitch_h5_vds <- function(tile_files, out_file, var_names, full_nrow, full_ncol,
                            ntime, full_xmin, full_ymax, res_x, res_y,
                            fill_value, python_path) {
  if (!is.null(python_path))
    reticulate::use_python(python_path, required = TRUE)

  if (!reticulate::py_module_available("h5py")) {
    warning("h5py not found; falling back to HDF5 external-links master file.")
    return(.stitch_h5_extlinks(tile_files, out_file, var_names))
  }

  h5py <- reticulate::import("h5py")
  np   <- reticulate::import("numpy")
  bi   <- reticulate::import("builtins")

  fv <- if (is.na(fill_value)) np$nan else fill_value

  fout <- h5py$File(out_file, "w")
  on.exit(fout$close(), add = TRUE)

  out_dir <- dirname(normalizePath(out_file, mustWork = FALSE))

  # rhdf5 reverses dim order: R array [nrow,ncol,ntime] → HDF5 (ntime,ncol,nrow)
  # h5py sees (ntime,ncol,nrow), so VDS layout must match that convention.
  for (vn in var_names) {
    layout <- h5py$VirtualLayout(
      shape = reticulate::tuple(as.integer(ntime),
                                as.integer(full_ncol),
                                as.integer(full_nrow)),
      dtype = np$float64
    )
    for (tf in tile_files) {
      attrs   <- .read_h5_tile_attrs(tf)
      row_off <- as.integer(round((full_ymax - as.numeric(attrs$ymax)) / res_y))
      col_off <- as.integer(round((as.numeric(attrs$xmin) - full_xmin) / res_x))
      t_nrow  <- as.integer(attrs$nrow)
      t_ncol  <- as.integer(attrs$ncol)
      rel_path <- .rel_path(tf, out_file)
      vsource <- h5py$VirtualSource(
        path_or_dataset = rel_path,
        name            = vn,
        shape           = reticulate::tuple(as.integer(ntime), t_ncol, t_nrow)
      )
      t_sl <- bi$slice(0L, as.integer(ntime))
      c_sl <- bi$slice(col_off, col_off + t_ncol)
      r_sl <- bi$slice(row_off, row_off + t_nrow)
      idx  <- reticulate::tuple(t_sl, c_sl, r_sl)
      layout$`__setitem__`(idx, vsource)
    }
    fout$create_virtual_dataset(vn, layout, fillvalue = fv)
  }
}

.stitch_nc_merge <- function(tile_files, out_file, var_names, var_meta,
                              full_nrow, full_ncol, full_xmin, full_xmax,
                              full_ymin, full_ymax, data_type, fill_value,
                              compression) {
  # Derive resolution from full domain
  res_x <- (full_xmax - full_xmin) / full_ncol
  res_y <- (full_ymax - full_ymin) / full_nrow

  # Time axis and CRS from first tile
  nc0     <- ncdf4::nc_open(tile_files[1])
  t_vals  <- ncdf4::ncvar_get(nc0, "time")
  t_units <- nc0$dim$time$units
  crs_wkt <- tryCatch(ncdf4::ncatt_get(nc0, "crs", "crs_wkt")$value,
                      error = function(e) "")
  ncdf4::nc_close(nc0)

  t_origin <- as.POSIXct(sub("hours since ", "", t_units), tz = "UTC")
  tme      <- t_origin + as.numeric(t_vals) * 3600

  # Build full-domain SpatRaster for coordinate generation
  full_dtm <- terra::rast(nrows = full_nrow, ncols = full_ncol,
                           xmin = full_xmin, xmax = full_xmax,
                           ymin = full_ymin, ymax = full_ymax)
  if (nchar(crs_wkt) > 0L) terra::crs(full_dtm) <- crs_wkt

  # Initialize full arrays to fill value
  fv <- if (is.na(fill_value)) NA_real_ else fill_value
  full_arrays <- setNames(
    lapply(var_names, function(v) array(fv, dim = c(full_nrow, full_ncol, length(tme)))),
    var_names
  )

  # Read each tile and place at correct row/col offset
  for (tf in tile_files) {
    attrs   <- .read_nc_tile_attrs(tf)
    row_off <- as.integer(round((full_ymax - attrs$ymax) / res_y))
    col_off <- as.integer(round((attrs$xmin  - full_xmin) / res_x))
    t_nrow  <- as.integer(attrs$nrow)
    t_ncol  <- as.integer(attrs$ncol)
    row_idx <- seq(row_off + 1L, row_off + t_nrow)
    col_idx <- seq(col_off + 1L, col_off + t_ncol)
    nc_t    <- ncdf4::nc_open(tf)
    for (vn in var_names) {
      arr <- ncdf4::ncvar_get(nc_t, vn)   # ncdf4 returns [ncol, nrow, ntime]
      arr[arr == -9999] <- NA_real_
      full_arrays[[vn]][row_idx, col_idx, ] <- aperm(arr, c(2L, 1L, 3L))
    }
    ncdf4::nc_close(nc_t)
  }

  .write_nc(full_arrays, var_meta, tme, out_file, full_dtm, data_type, compression)
  invisible(NULL)
}

# --------------------------------------------------------------------------- #
#  stitch_tiles — exported
# --------------------------------------------------------------------------- #

#' Stitch microclimf tile output files into a single domain-wide file
#'
#' Discovers all per-tile HDF5 or NetCDF files produced by
#' \code{\link{write_tile}} in \code{tile_dir}, reads their embedded spatial
#' metadata to determine each tile's position within the full domain, and
#' assembles them into either an HDF5 virtual dataset (VDS) or a single merged
#' NetCDF file.
#'
#' @param tile_dir Character. Directory containing tile files written by
#'   \code{\link{write_tile}}.
#' @param out_file Character. Output path for the assembled file.
#' @param data_type Character. \code{"mout"} or \code{"smod"}. Used to filter
#'   which files in \code{tile_dir} are included.
#' @param file_fmt Character. \code{"h5"} builds an HDF5 Virtual Dataset
#'   (requires h5py in the active Python environment, or falls back to external
#'   links); \code{"nc"} reads all tile NetCDF files into memory, assembles the
#'   full domain arrays, and writes a single merged NetCDF file (requires
#'   \code{ncdf4}). Default \code{"h5"}.
#' @param dtm Optional \code{terra::SpatRaster} covering the full domain. When
#'   supplied, it defines the authoritative full-domain extent, row count, and
#'   column count. When \code{NULL}, these are inferred from the union of all
#'   tile spatial attrs embedded by \code{write_tile}.
#' @param python_path Character. Path to a Python executable that has
#'   \code{h5py} installed. Only used when \code{file_fmt = "h5"}. Defaults to
#'   \code{reticulate}'s currently configured Python.
#' @param fill_value Numeric. Value used to initialise cells not covered by any
#'   tile. For \code{file_fmt = "h5"} this becomes the VDS fill value (NaN when
#'   \code{NA_real_}). For \code{file_fmt = "nc"} full arrays are initialised to
#'   this value before tiles are placed. Default \code{NA_real_}.
#' @param compression Integer 0–9. Gzip compression level for the merged output
#'   file. Only used when \code{file_fmt = "nc"}. Default \code{4L}.
#'
#' @return Invisibly, a \code{data.frame} with columns \code{tile_file} and
#'   \code{status}.
#'
#' @details
#' **HDF5 VDS path** (\code{file_fmt = "h5"}): Uses \code{reticulate} and
#' \code{h5py} to create a virtual dataset of shape
#' \code{[full_nrow, full_ncol, ntime]} per variable, mapping each tile into
#' its correct spatial position without copying data. Tile files must remain at
#' their original paths for the VDS to remain readable. If h5py is unavailable,
#' falls back to a master file with HDF5 external links.
#'
#' **NetCDF merge path** (\code{file_fmt = "nc"}): Reads all tile files into
#' memory, assembles the full-domain arrays, and writes a single merged NetCDF
#' file using the same CF-1.8 layout as \code{\link{write_tile}}. Peak memory
#' cost is approximately \code{full_nrow × full_ncol × ntime × n_vars × 8}
#' bytes. Tile files are not needed after the merged file is written.
#'
#' Tile files are discovered by listing all files in \code{tile_dir} whose
#' names contain \code{data_type} (case-insensitive) and end in \code{.h5} or
#' \code{.nc} according to \code{file_fmt}.
#'
#' @seealso \code{\link{write_tile}} for writing individual tile files.
#'   \code{\link{create_tiles}} for generating tile extents.
#'
#' @export
stitch_tiles <- function(tile_dir, out_file, data_type = "mout", file_fmt = "h5",
                         dtm = NULL, python_path = NULL, fill_value = NA_real_,
                         compression = 4L) {
  data_type   <- match.arg(data_type, c("mout", "smod"))
  file_fmt    <- match.arg(file_fmt, c("h5", "nc"))
  compression <- as.integer(compression)
  var_meta    <- if (data_type == "mout") .mout_meta else .smod_meta
  var_names   <- names(var_meta)

  ext_pat   <- if (file_fmt == "h5") "\\.h5$" else "\\.nc$"
  all_files  <- list.files(tile_dir, full.names = TRUE)
  tile_files <- all_files[grepl(data_type, basename(all_files), ignore.case = TRUE) &
                           grepl(ext_pat, all_files)]
  if (length(tile_files) == 0)
    stop(sprintf("No %s files matching '%s' found in: %s", file_fmt, data_type, tile_dir))

  # --- Resolve full domain ---------------------------------------------------
  if (!is.null(dtm)) {
    e         <- terra::ext(dtm)
    full_xmin <- e$xmin
    full_xmax <- e$xmax
    full_ymin <- e$ymin
    full_ymax <- e$ymax
    res_x     <- terra::res(dtm)[1]
    res_y     <- terra::res(dtm)[2]
    full_nrow <- terra::nrow(dtm)
    full_ncol <- terra::ncol(dtm)
  } else {
    if (file_fmt == "h5") {
      if (!requireNamespace("rhdf5", quietly = TRUE))
        stop('Package \'rhdf5\' is required. Install with: BiocManager::install("rhdf5")')
      all_attrs <- lapply(tile_files, .read_h5_tile_attrs)
    } else {
      if (!requireNamespace("ncdf4", quietly = TRUE))
        stop('Package \'ncdf4\' is required. Install with: install.packages("ncdf4")')
      all_attrs <- lapply(tile_files, .read_nc_tile_attrs)
    }
    full_xmin <- min(sapply(all_attrs, function(a) as.numeric(a$xmin)))
    full_xmax <- max(sapply(all_attrs, function(a) as.numeric(a$xmax)))
    full_ymin <- min(sapply(all_attrs, function(a) as.numeric(a$ymin)))
    full_ymax <- max(sapply(all_attrs, function(a) as.numeric(a$ymax)))
    res_x     <- as.numeric(all_attrs[[1]]$res_x)
    res_y     <- as.numeric(all_attrs[[1]]$res_y)
    full_nrow <- as.integer(round((full_ymax - full_ymin) / res_y))
    full_ncol <- as.integer(round((full_xmax - full_xmin) / res_x))
  }

  # --- Get ntime from first tile file ----------------------------------------
  if (file_fmt == "h5") {
    if (!requireNamespace("rhdf5", quietly = TRUE))
      stop('Package \'rhdf5\' is required. Install with: BiocManager::install("rhdf5")')
    ntime <- length(rhdf5::h5read(tile_files[1], "time"))
  } else {
    if (!requireNamespace("ncdf4", quietly = TRUE))
      stop('Package \'ncdf4\' is required. Install with: install.packages("ncdf4")')
    nc0   <- ncdf4::nc_open(tile_files[1])
    ntime <- nc0$dim$time$len
    ncdf4::nc_close(nc0)
  }

  # --- Dispatch ---------------------------------------------------------------
  if (file_fmt == "h5") {
    .stitch_h5_vds(tile_files, out_file, var_names,
                   full_nrow, full_ncol, ntime,
                   full_xmin, full_ymax, res_x, res_y,
                   fill_value, python_path)
  } else {
    .stitch_nc_merge(tile_files, out_file, var_names, var_meta,
                     full_nrow, full_ncol, full_xmin, full_xmax,
                     full_ymin, full_ymax, data_type, fill_value, compression)
  }

  invisible(data.frame(tile_file = tile_files, status = "stitched",
                       stringsAsFactors = FALSE))
}
