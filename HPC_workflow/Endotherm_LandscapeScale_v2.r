#!/usr/bin/env Rscript
# Test copy for validating run_endo_big_nichemap() on the real cluster before
# switching Endotherm_LandscapeScale.r over. Run side-by-side with the
# production script (different output_dir) for comparison.
#
# SUBSET RUN: the full production run (both scenarios, all tiles, all years)
# takes multiple days. This copy runs climatology only, restricted to
# `subset_n_tiles` tiles (the first N tile IDs found in the masked domain),
# for a fast smoke test. Set I es <- Inf below (or pass
# --subset_n_tiles=Inf) to fall back to the full climatology domain; add the
# year-specific block back in (see git history) once you're ready for a full
# run - it was removed here to keep this variant single-purpose.
#
# Prerequisites - all three are in the external correction pipeline
# (7_apply_corrections.R, outside this repo), not in this script. Fix them
# there before running this workflow:
#   1. Corrected tiles must be written under the standard filename/location
#      /corrected_models/{study_area}/Microclim_Models/... that
#      run_endo_big_nichemap() constructs - NOT a separate
#      *_corrected.nc-suffixed file.
#   2. BlwGrd must also exist in the corrected tree. The correction pipeline
#      currently only produces corrected AbvGrd + Snow (the old production
#      script read BlwGrd from the uncorrected root), so copy or link the
#      uncorrected BlwGrd tile into the corrected location if it isn't
#      already there.
#   3. The corrected AbvGrd file must span the FULL period (e.g. the whole
#      Jul-Jun year), not just the Dec-Apr simulation window. tannul (mean
#      annual temperature, a micro_to_csv() input) is computed from whatever
#      time range is actually in that file; run_endo_big_nichemap() now
#      stop()s if the file covers < 90% of the period rather than silently
#      producing a winter-biased "annual" mean. The old production script
#      sidestepped this by reading tannul from the uncorrected full-year tile.
#
# Required on-cluster validation before this replaces the production script
# (none of it can be checked from a Windows dev machine - see the design
# spec's "Wine invocation" / "Required pre-production validation" section):
#   1. Absolute-path exe under Wine: run one known-good cell's inputs (a
#      Debug_CSVs dump is exactly this shape) with the exe invoked by its
#      absolute /NicheMapExe/... path under the shared prefix. Confirm
#      ErrorMsgs.dat says "Calculations completed.", HOURPLOT.csv is
#      produced, and wine_stderr.log has no "could not find DOS drive" line.
#      If this fails, the fallback is the original per-chunk exe copy - do
#      not add a copy_exe argument to hedge it.
#   2. Prefix readiness: after init_wine_prefix(), ls -la $wineprefix and
#      confirm .update-timestamp exists.
#   3. No pipe-hang regression: run one array task end-to-end and confirm it
#      completes without a stalled worker.

library(terra)
library(SpencerEcoTools)

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default = NA) {
  x <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(x) == 0) return(default)
  sub(paste0("^--", name, "="), "", x)
}

clust_array_arg  <- as.integer(get_arg("clust_array_arg", default = 1))
clust_array_size <- as.integer(get_arg("clust_array_size", default = 40))
n_threads        <- as.integer(get_arg("n_threads", default = 1))
subset_n_tiles   <- as.numeric(get_arg("subset_n_tiles", default = 2))  # Inf = full domain

wineprefix <- Sys.getenv("ENDO_WINEPREFIX", unset = "/temp/wine/prefix")
init_wine_prefix(wineprefix, headless = TRUE)

exe_path   <- "/NicheMapExe/Endo2022a.exe"
output_dir <- "/Endo_out_v2"  # separate from production's /Endo_out for comparison

tile_map_path      <- "/Microclim_out/TetonsClimatologyTileMap.tif"
valid_mask_path    <- "/Data/Endo-Valid-Cells-Mask.tif"  # winter range x not-water, precombined

# --- restrict to the first `subset_n_tiles` tiles for a fast smoke test -----
if (is.finite(subset_n_tiles)) {
  tile_map_r   <- terra::rast(tile_map_path)
  valid_mask_r <- terra::rast(valid_mask_path)

  vm <- valid_mask_r
  vm[vm != 1] <- NA
  tile_ids_all <- sort(unique(terra::values(terra::mask(tile_map_r, vm), mat = FALSE, na.rm = TRUE)))
  keep_ids     <- head(tile_ids_all, subset_n_tiles)

  cat(sprintf("Subset run: restricting to tile ID(s) %s (of %d total in the masked domain)\n",
              paste(keep_ids, collapse = ", "), length(tile_ids_all)))

  subset_mask <- valid_mask_r
  subset_mask[!(tile_map_r %in% keep_ids)] <- NA

  valid_cells_mask_path <- file.path(tempdir(), "subset_valid_cells_mask.tif")
  terra::writeRaster(subset_mask, valid_cells_mask_path, overwrite = TRUE)
} else {
  valid_cells_mask_path <- valid_mask_path
}

# --- climatology only -------------------------------------------------------
run_endo_big_nichemap(
  tile_map = tile_map_path,
  valid_cells_mask = valid_cells_mask_path,
  dates = data.frame(Start_Dates = as.Date("2024-07-01"), End_Dates = as.Date("2025-06-01"),
                     Sim_Start = as.Date("2024-12-09"), Sim_End = as.Date("2025-04-15")),
  microclim_dir = "/corrected_models",
  dem = "/Data/DEM/DEM_GLO30.tif", refl_dir = "/Data/Climatology",
  exe_path = exe_path, output_dir = output_dir, wineprefix = wineprefix,
  study_area = "TetonsClimatology", snow = TRUE, headless = TRUE,
  parallel = TRUE, ncores = n_threads,
  clust_array_arg = clust_array_arg, clust_array_size = clust_array_size
)

cat("\nDone.\n")
