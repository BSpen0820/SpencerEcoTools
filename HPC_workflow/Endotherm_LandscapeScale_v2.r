#!/usr/bin/env Rscript
# Test copy for validating run_endo_big_nichemap() on the real cluster before
# switching Endotherm_LandscapeScale.r over. Run side-by-side with the
# production script (different output_dir) for comparison.

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

wineprefix <- Sys.getenv("ENDO_WINEPREFIX", unset = "/temp/wine/prefix")
init_wine_prefix(wineprefix, headless = TRUE)

exe_path   <- "/NicheMapExe/Endo2022a.exe"
output_dir <- "/Endo_out_v2"  # separate from production's /Endo_out for comparison

# --- climatology ---
run_endo_big_nichemap(
  tile_map = "/Microclim_out/TetonsClimatologyTileMap.tif",
  valid_cells_mask = "/Data/Endo-Valid-Cells-Mask.tif",  # winter range x not-water, precombined
  dates = data.frame(Start_Dates = as.Date("2024-07-01"), End_Dates = as.Date("2025-06-01"),
                     Sim_Start = as.Date("2024-12-09"), Sim_End = as.Date("2025-04-15")),
  microclim_dir = "/corrected_models/TetonsClimatology",
  dem = "/Data/DEM/DEM_GLO30.tif", refl_dir = "/Data/Climatology",
  exe_path = exe_path, output_dir = output_dir, wineprefix = wineprefix,
  study_area = "TetonsClimatology", snow = TRUE, headless = TRUE,
  parallel = TRUE, ncores = n_threads,
  clust_array_arg = clust_array_arg, clust_array_size = clust_array_size
)

# --- year-specific ---
dates_ys <- data.frame(
  Start_Dates = as.Date(c("2022-07-01", "2023-07-01", "2021-07-01", "2017-07-01")),
  End_Dates   = as.Date(c("2023-06-30", "2024-06-30", "2022-06-30", "2018-06-30"))
)
dates_ys$Sim_Start <- as.Date(sprintf("%d-12-09", format(dates_ys$Start_Dates, "%Y")))
dates_ys$Sim_End   <- as.Date(sprintf("%d-04-15", format(dates_ys$End_Dates, "%Y")))

run_endo_big_nichemap(
  tile_map = "/Microclim_out/YearSpecificTileMap.tif",
  valid_cells_mask = "/Data/Endo-Valid-Cells-Mask.tif",
  dates = dates_ys,
  microclim_dir = "/corrected_models/TetonsYearSpecific",
  dem = "/Data/DEM/DEM_GLO30.tif", refl_dir = "/Data/YearSpecific",
  exe_path = exe_path, output_dir = output_dir, wineprefix = wineprefix,
  study_area = "TetonsYearSpecific", snow = TRUE, headless = TRUE,
  parallel = TRUE, ncores = n_threads,
  clust_array_arg = clust_array_arg, clust_array_size = clust_array_size
)

cat("\nDone.\n")
