#!/usr/bin/env Rscript
# --------------------------------------------------------------------------------
#   Endotherm Model Workflow (Tiled, SLURM Array) - example
#
#   Usage: Rscript Endotherm_LandscapeScale.R --clust_array_arg=<N> --clust_array_size=<M> --n_threads=<T>
#
#   This is a template: replace every $PROJECT_ROOT-relative path below with
#   your own layout before running. See docs/superpowers/specs/
#   2026-08-28-endotherm-landscape-scale-design.md for the full design.
# --------------------------------------------------------------------------------

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

project_root <- Sys.getenv("PROJECT_ROOT", unset = ".")
wineprefix   <- Sys.getenv("ENDO_WINEPREFIX", unset = file.path(project_root, "temp", "wine", "prefix"))

# Initialize the shared Wine prefix once, before any parallel workers start.
init_wine_prefix(wineprefix, headless = TRUE)

run_endo_big_nichemap(
  tile_map         = file.path(project_root, "Microclim_out", "TileMap.tif"),
  valid_cells_mask = file.path(project_root, "Data", "ValidCellsMask.tif"),
  dates            = data.frame(Start_Dates = as.Date("2022-07-01"), End_Dates = as.Date("2023-06-30"),
                                Sim_Start = as.Date("2022-12-09"), Sim_End = as.Date("2023-04-15")),
  microclim_dir    = file.path(project_root, "Microclim_out"),
  dem              = file.path(project_root, "Data", "DEM", "DEM_GLO30.tif"),
  refl_dir         = file.path(project_root, "Data"),
  exe_path         = file.path(project_root, "NicheMapExe", "Endo2022a.exe"),
  output_dir       = file.path(project_root, "Endo_out"),
  wineprefix       = wineprefix,
  study_area       = "MyStudyArea",
  headless         = TRUE,
  parallel         = TRUE,
  ncores           = n_threads,
  clust_array_arg  = clust_array_arg,
  clust_array_size = clust_array_size
)

cat("\nDone.\n")
