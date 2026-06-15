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
