# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package overview

SpencerEcoTools is an R package that automates download, preprocessing, and packaging of spatial/remote sensing data for microclimate modeling using the microclimf/microclimdata/NicheMapR ecosystem. It also wraps HPC (SLURM) cluster workflows into reusable functions. The package is authored by Bryan Spencer (PhD, University of Idaho, ungulate ecology).

## Development commands

```r
# Build documentation and install
devtools::document()
devtools::install()

# Check the full package
devtools::check()

# Load without installing (for interactive dev)
devtools::load_all()

# Run a single function's examples
example("run_micro_big_nichemap", package = "SpencerEcoTools")
```

GEE must be initialized before calling any GEE functions:
```r
reticulate::use_python("C:\\Users\\Bdspe\\miniforge3\\envs\\rgee")  # set conda env if needed
reticulate::import("ee")$Initialize(project = "ee-bdspen")
googledrive::drive_auth()  # separate auth needed for poll_drive()
```

AORC download requires Python packages (`xarray`, `s3fs`, `zarr`, `netCDF4`) in the active conda environment. `download_aorc()` will attempt `reticulate::py_install()` if they are missing.

## Source file layout

- `R/Microclimf_DataPrep.R` — all data download/preprocessing functions (GEE exports, AORC, soil, LAI, albedo, reflectance, diffuse radiation, packaging)
- `R/Microclimf_Modeling.R` — tile creation and micropoint model wrappers
- `R/data.R` — roxygen2 documentation for the four package datasets
- `data/` — pre-built `.rda` files (`fmask_bits`, `FparLAI_QC`, `AORC_meterodf`, `Microclim_meterodf`)

## Data pipeline order

The full preprocessing pipeline runs in this sequence:

1. `define_aoi()` → GEE geometry + CRS
2. `download_dem()` → Copernicus GLO-30 DEM via GEE
3. `download_hls()` → HLS S30+L30 imagery (4-band: red/green/blue/nir) via GEE. By default (`use_cloud_score = TRUE`) the S30/Sentinel-2 half is masked with `GOOGLE/CLOUD_SCORE_PLUS/V1/S2_HARMONIZED` (`cs_cdf`) — linked per scene by exact MGRS-tile + sensing-datetime key, with per-pixel spectral fallback where no CS+ partner exists (e.g. before the CS+ archive start of 2015-06-27) — while the L30/Landsat half keeps the spectral mask (CS+ is Sentinel-2-only). Set `use_cloud_score = FALSE` for the original all-spectral behavior. Alternative: `download_s2()` builds the same 4-band 0-1 composites from `COPERNICUS/S2_SR_HARMONIZED` + CS+ (Sentinel-2 only; reliable from ~2019, no Landsat).
4. `download_modis_lai()` → MODIS MCD15A3H LAI via GEE
5. `download_albedo()` → MODIS MCD43A3 albedo via GEE
6. `download_soil()` → soil data via `microclimdata::soildata_download/downscale`
7. `download_aorc()` → hourly AORC climate NetCDF from NOAA S3
8. `estimate_diffuse_rad()` → diffuse radiation from AORC DSWRF (must be in WGS84)
9. `downscale_lai()` → 30 m LAI using NDVI from HLS
10. `NLCD_2_CORINE()` / `LandfireVegHght_AsNumeric()` → land cover + veg height preprocessing
11. `compute_albedo()` → photographic + MODIS-adjusted albedo
12. `compute_reflectance()` → leaf and ground reflectance
13. `summarize_climate_normals()` → multi-year hourly climate normals from AORC
14. `package_climate()` → packages AORC into microclimf-ready climate arrays
15. `package_veg_soil()` → packages vegetation and soil parameter grids
16. `create_tiles()` → tile extents for memory-efficient large-raster processing
17. `run_micro_big_nichemap()` → tiled microclimf point models with NicheMapR below-ground

## Function design conventions

**`dates` argument:** Always a vector of `Date` objects. Day component is ignored; only year and month matter. This allows cross-year ranges like Oct 2019–Mar 2020. Exception: `package_climate()`, `package_veg_soil()`, and `run_micro_big_nichemap()` accept either a `data.frame` with `Start_Dates`/`End_Dates` columns OR a length-2 Date vector.

**`study_area`:** Always optional (`default NULL`). When provided, filters input files by that string and prefixes output file names.

**Return values:** All functions return invisibly with a log `data.frame` or list.

**Error handling:** `stop()` for unrecoverable errors; `warning()` + skip for recoverable ones (e.g., missing files for one month in a multi-month run).

**Path construction:** Always `file.path()`, never `paste0()` with slashes.

**Namespacing:** Always use explicit `terra::`, `sf::` prefixes. For the `ee` module (earthengine-api), use the package-level `ee` object defined in `R/zzz.R` — call `ee$...` directly, never `library()` inside functions.

**Internal helpers:** Prefixed with `.` (e.g., `.maskHLS_full`, `.generate_month_sequence`). No `@export`, minimal comments.

**Memory management:** `gc()` after heavy raster operations in loops; `rm()` large objects before `gc()`.

**String formatting:** `sprintf()` for all strings that include numeric values.

**Variable naming:** `snake_case` for local variables; `camelCase` only when matching microclimdata/microclimf conventions (`vegp`, `soilc`, `refldata`).

## HPC / SLURM conventions

HPC support is passed via hidden `...` args (`clust_array_arg`, `clust_array_size`) — **not** explicit function parameters. This keeps the public API clean.

- **`run_micro_big_nichemap()`** — multi-period, tiled. Tasks are `N_tiles × N_periods` combinations distributed via round-robin: `rep(seq_len(clust_array_size), length.out = N_tasks)`. Each task runs all heights (above-ground + 9 soil depths) in an inner loop. Surplus jobs exit cleanly.

Bryan writes SBATCH scripts manually and passes `$SLURM_ARRAY_TASK_ID`. Functions do **not** submit jobs themselves.

## Output file naming

- Above-ground: `{study_area}_AbvGrd_MicropointModel_{period_label}.RDS`
- Below-ground: `{study_area}_BlwGrd_{depth_mm}_MicropointModel_{period_label}.RDS` (depth in zero-padded 4-digit mm, e.g., `BlwGrd_0015` for 1.5 cm)
- Climate: `{study_area}_Climate_{period_label}.RDS`
- Veg: `{study_area}_VegPara_{period_label}.RDS`
- Soil: `{study_area}_SoilPara_{period_label}.RDS`
- Period labels: `YYYYMMDD_to_YYYYMMDD`
- AORC dirs: `aorc_dir/study_area/year/month/`
- AORC files: `AORC_{study_area}_{YEAR}_{MM}_{VARNAME}.nc`

## Critical technical constraints

These are non-obvious bugs that were found through testing — do not change without understanding the consequences:

- **WGS84 until after `estimate_diffuse_rad()`**: AORC and diffuse radiation files must stay in WGS84 (EPSG:4326). Reprojecting first causes empty edge cells that corrupt solar geometry calculations.
- **HLS reflectance scaling**: HLS from GEE is already scaled 0–1. Scale to 0–253 (not 255, not 250) before passing to microclimdata albedo functions.
- **Air temp conversion**: Kelvin − 273.15 (not 274.15).
- **Wind direction**: `(180 + atan2(u, v) * 180/pi) %% 360`
- **solaR timezone side effect**: `solaR` sets the system TZ to UTC. Always save/restore with `Sys.getenv("TZ")` / `Sys.setenv(TZ = old_tz)` inside `estimate_diffuse_rad()` workers (already implemented via `on.exit()`).
- **GitHub-only dependencies**: `microclimdata` and `luna` cannot go in `DESCRIPTION Imports` (CRAN policy). Document manual install in README.

## Parallelization pattern

```r
future::plan(future::multisession, workers = n)
on.exit(future::plan(future::sequential), add = TRUE)
results <- future.apply::future_lapply(...)
```

When using `reticulate` inside parallel workers, pass `python_path` explicitly and call `reticulate::use_python(python_path, required = TRUE)` at the top of each worker — the Python environment does not inherit automatically.

## Documentation standards

All exported functions require full roxygen2 blocks: `@param`, `@return`, `@details`, `@export`. Use `@seealso` to link related microclimdata/microclimf functions. Package datasets go in `R/data.R` with `@source` URLs. Run `devtools::document()` after any roxygen changes.
