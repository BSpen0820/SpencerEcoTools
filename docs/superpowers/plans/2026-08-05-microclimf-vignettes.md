# microclimf Vignettes Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add three non-executed (`eval = FALSE`) R vignettes to SpencerEcoTools covering GEE setup, a full data-prep pipeline walkthrough on a small dummy site, and running `microclimf` models locally vs. on a SLURM cluster.

**Architecture:** Pure-documentation vignettes (`vignettes/*.Rmd`, `rmarkdown::html_vignette` output). Every chunk is `eval = FALSE` — nothing executes GEE calls, downloads, or heavy compute at build/check time. "Testing" a vignette task means confirming the `.Rmd` knits cleanly (valid YAML, valid R syntax in chunks, no execution errors since nothing executes).

**Tech Stack:** R, `knitr`, `rmarkdown`, `devtools`. No new runtime dependencies — `knitr`/`rmarkdown` are documentation-build-only (`Suggests`).

## Global Constraints

- All vignette code chunks are `eval = FALSE` (set once per file via `knitr::opts_chunk$set(eval = FALSE)` in the setup chunk) — nothing may execute GEE, AORC, SLURM, or `microclimf`/`microclimdata` calls during `devtools::document()`, `R CMD check`, or a `pkgdown` build.
- File naming/order: `vignettes/01-gee-setup.Rmd`, `vignettes/02-data-pipeline.Rmd`, `vignettes/03-running-microclimf.Rmd`.
- Dummy site used in vignettes 2 & 3: a single point near the University of Idaho Arboretum, Moscow, ID (`lon = -117.0166`, `lat = 46.7197`, WGS84), buffered to a small AOI via `define_aoi(buffer_dist = 600, crs_epsg = "EPSG:32611")` (UTM 11N) — a ~1.2 km square, ~1.44 km².
- Dummy date range: July–August 2023 (`dates.v <- seq(as.Date("2023-07-01"), as.Date("2023-08-01"), by = "month")`), snow-free, avoiding `download_hls()`'s seasonal snow-gate complexity.
- `study_area = "UI_Arboretum"` used consistently as the prefix argument across all pipeline calls in vignettes 2 & 3.
- Every function call shown must use **exact current parameter names** as defined in `R/Microclimf_DataPrep.R` / `R/Microclimf_Modeling.R` — several differ from the older example scripts this plan was informed by (e.g. `define_aoi()`'s buffer arg is `buffer_dist`, not `buffer`; `download_dem()` takes no `study_area` argument at all).
- Out of scope: endotherm/NicheMapR modeling (`write_endotherm_inputs()`, `run_endotherm_model()`, `Endotherm_Modeling.R`). `micro_to_csv()` gets only a brief closing mention in vignette 3 as a way to inspect output, not a full walkthrough.
- No files are committed to `vignettes/doc/` or `Meta/` — vignette build verification happens in a temp directory (`devtools::build(path = tempdir())`), never via `devtools::build_vignettes()` in the package root (which would write `doc/`/`Meta/` into the repo).

---

### Task 1: Vignette build infrastructure

**Files:**
- Modify: `DESCRIPTION`
- Create: `vignettes/` (directory, populated in Task 2)

**Interfaces:**
- Produces: `Suggests: knitr, rmarkdown` and `VignetteBuilder: knitr` fields in `DESCRIPTION`, which every later task's `rmarkdown::render()` check depends on.

- [ ] **Step 1: Add knitr/rmarkdown to DESCRIPTION**

Edit `DESCRIPTION`'s `Suggests:` block from:

```
Suggests:
    rhdf5,
    ncdf4,
    testthat (>= 3.0.0)
```

to:

```
Suggests:
    rhdf5,
    ncdf4,
    knitr,
    rmarkdown,
    testthat (>= 3.0.0)
```

Then add a `VignetteBuilder: knitr` line directly after the `Suggests:` block (before `Config/testthat/edition:`).

- [ ] **Step 2: Create the vignettes directory**

Run: `mkdir -p vignettes` (or `New-Item -ItemType Directory -Force vignettes` on Windows) from the package root `D:\Code\PhD\R_Packages\SpencerEcoTools`.

- [ ] **Step 3: Verify DESCRIPTION parses correctly**

Run:
```r
Rscript -e 'd <- read.dcf("DESCRIPTION"); stopifnot(grepl("knitr", d[,"Suggests"]), grepl("rmarkdown", d[,"Suggests"]), d[,"VignetteBuilder"] == "knitr"); cat("OK\n")'
```
Expected: `OK` printed, no error.

- [ ] **Step 4: Commit**

```bash
git add DESCRIPTION
git commit -m "build: add knitr/rmarkdown vignette infrastructure"
```

(The empty `vignettes/` directory has nothing to commit yet — Task 2 adds its first file, which brings the directory into git.)

---

### Task 2: Vignette 1 — GEE setup

**Files:**
- Create: `vignettes/01-gee-setup.Rmd`

**Interfaces:**
- Consumes: nothing (first vignette, no dependency on other tasks' content).
- Produces: the GEE initialization pattern (`reticulate::use_python()` → `ee <- reticulate::import("ee")` → `ee$Initialize(project = ...)`) that vignette 2 references by name in its own setup section.

- [ ] **Step 1: Write the vignette file**

Create `vignettes/01-gee-setup.Rmd` with this content:

````markdown
---
title: "Setting Up Google Earth Engine"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Setting Up Google Earth Engine}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)
```

## What this package uses Google Earth Engine for

SpencerEcoTools pulls several of its raw inputs directly from Google Earth
Engine (GEE):

- `download_dem()` — Copernicus GLO-30 elevation
- `download_hls()` / `download_s2()` — Harmonized Landsat Sentinel-2 or
  Sentinel-2 surface reflectance imagery (red/green/blue/NIR), with
  Cloud Score+ cloud masking
- `download_modis_lai()` — MODIS MCD15A3H Leaf Area Index
- `download_albedo()` — MODIS MCD43A3 broadband albedo
- `microclimdata::lcover_download()` / `microclimdata::vegheight_download()`
  — ESA WorldCover land cover and ETH Global Canopy Height, as alternatives
  to this package's own NLCD/LANDFIRE preprocessing (see the
  `02-data-pipeline` vignette)

Everything else in the pipeline is **not** GEE: `download_aorc()` pulls
hourly climate data directly from NOAA's AORC Zarr store on S3, and
`download_soil()`/`microclimdata::soildata_download()` pull SoilGrids data
through their own API.

## 1. Get a Google Cloud project with Earth Engine enabled

1. Sign in at <https://console.cloud.google.com/> with the Google account
   you want to use for Earth Engine.
2. Create a new Cloud project (or pick an existing one) and note its
   **project ID** — you'll pass this to `ee$Initialize()` below.
3. Enable the Earth Engine API for that project at
   <https://console.cloud.google.com/apis/library/earthengine.googleapis.com>.
4. Register the project for Earth Engine access at
   <https://code.earthengine.google.com/register> if you haven't used Earth
   Engine with this Google account before.

## 2. Set up a Python environment with the Earth Engine API

SpencerEcoTools calls Earth Engine through `reticulate`, which drives a
Python environment containing the `earthengine-api` package. A conda/
miniforge environment keeps this isolated from your system Python:

```bash
conda create -n rgee python=3.10
conda activate rgee
conda install -c conda-forge earthengine-api
```

`download_aorc()` additionally needs `xarray`, `s3fs`, `zarr`, and
`netCDF4` in the same environment (it will attempt to `reticulate::py_install()`
them automatically if missing, but installing up front avoids a mid-run
pause):

```bash
conda install -c conda-forge xarray s3fs zarr netCDF4
```

## 3. Point R at that environment and initialize Earth Engine

Every session that calls a GEE-backed SpencerEcoTools function needs this
done once, before any package function is called:

```r
library(reticulate)

reticulate::use_python("C:/Users/you/miniforge3/envs/rgee", required = TRUE)

ee <- reticulate::import("ee")
ee$Authenticate()                              # first time only, opens a browser
ee$Initialize(project = "your-gcp-project-id")

# Smoke test
ee$Number(1)$getInfo()
#> [1] 1
```

`ee$Authenticate()` only needs to run once per machine (it caches
credentials); after that, `ee$Initialize()` alone is enough at the start of
each session.

**Why this works without a package-level init function**: SpencerEcoTools
itself never calls `ee$Initialize()`. Internally, `R/zzz.R` lazily imports
Earth Engine once when the package loads:

```r
ee <- NULL
.onLoad <- function(libname, pkgname) {
  ee <<- reticulate::import("ee", delay_load = TRUE)
}
```

Because `reticulate` runs a single Python process per R session, this
lazy-loaded internal `ee` binding shares the exact same Python `ee` module
your own `ee$Initialize()` call configured above. Initializing Earth Engine
yourself, as shown above, is what makes every SpencerEcoTools GEE function
work — there is no separate SpencerEcoTools-specific setup call.

## 4. Google Drive authentication (for polling GEE exports)

GEE image exports land in a Google Drive folder before SpencerEcoTools
downloads them locally (`poll_drive()`, called internally by every
`download_*()` GEE function). This needs its own, separate authentication:

```r
library(googledrive)
googledrive::drive_auth()
```

This opens a browser OAuth flow the first time; tokens are cached for
subsequent sessions.

## Next steps

With GEE and Drive both authenticated, continue to the
`02-data-pipeline` vignette for a full worked example on a small dummy
site, or `03-running-microclimf` to see how the final packaged data feeds
into `microclimf` model runs.
````

- [ ] **Step 2: Knit-check the vignette**

Run:
```r
Rscript -e 'rmarkdown::render("vignettes/01-gee-setup.Rmd", output_dir = tempdir(), quiet = TRUE); cat("RENDER OK\n")'
```
Expected: `RENDER OK` printed, no error (confirms valid YAML front matter and R syntax; no chunks execute since `eval = FALSE`).

- [ ] **Step 3: Commit**

```bash
git add vignettes/01-gee-setup.Rmd
git commit -m "docs: add GEE setup vignette"
```

---

### Task 3: Vignette 2 — Data download & preprocessing walkthrough

**Files:**
- Create: `vignettes/02-data-pipeline.Rmd`

**Interfaces:**
- Consumes: the dummy-site AOI/date-range constants defined in Global Constraints (this task defines them concretely in the vignette's own code, matching those constants exactly).
- Produces: the packaged `_Climate_...RDS` and `_VegPara_.../_SoilPara_...RDS` file-naming pattern (`study_area = "UI_Arboretum"`, period label `20230701_to_20230831`) that vignette 3 (Task 4) reads from.

- [ ] **Step 1: Write the front matter, setup, and AOI/DEM sections**

Create `vignettes/02-data-pipeline.Rmd` starting with:

````markdown
---
title: "Data Download & Preprocessing Walkthrough"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Data Download & Preprocessing Walkthrough}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)
```

This vignette walks the full data-prep pipeline (`CLAUDE.md`'s pipeline
order) end to end on a small dummy site, so every function's real inputs
and outputs are visible in one place. See `01-gee-setup` first for GEE/Drive
authentication — every code chunk below assumes that's already done.

The dummy site is a ~1.2 km square near the University of Idaho Arboretum
in Moscow, ID, over two summer months (July-August 2023) — small and
snow-free enough to keep the walkthrough fast if you were to run it for
real, while exercising every step of the pipeline.

```{r}
library(sf)
library(terra)
library(SpencerEcoTools)

study_area <- "UI_Arboretum"

dates.v <- seq(as.Date("2023-07-01"), as.Date("2023-08-01"), by = "month")
#> [1] "2023-07-01" "2023-08-01"
```

## 1. Define the area of interest

```{r}
pt <- sf::st_sfc(sf::st_point(c(-117.0166, 46.7197)), crs = 4326)
pt <- sf::st_sf(id = 1, geometry = pt)

ee_aoi <- define_aoi(
  aoi         = pt,
  buffer_dist = 600,           # meters -> ~1.2 x 1.2 km AOI
  crs_epsg    = "EPSG:32611"   # UTM zone 11N
)
```

`ee_aoi` is a named list (`geometry`, `crs`, `bbox_wgs84`) that every
`download_*()` function below expects as its `aoi` argument.

## 2. Download the DEM

```{r}
download_dem(
  aoi     = ee_aoi,
  out_dir = "./Data/DEM",
  scale   = 30
)

dtm <- terra::rast("./Data/DEM/DEM_GLO30.tif")
```

`download_dem()` doesn't take a `study_area` argument — the output is
always named `DEM_GLO30.tif` in `out_dir`, since a study area typically has
just one DEM shared across every period modeled.
````

- [ ] **Step 2: Append the imagery, LAI, and albedo download sections**

Append this to `vignettes/02-data-pipeline.Rmd`:

````markdown

## 3. Download multispectral imagery (HLS or Sentinel-2)

```{r}
download_hls(
  aoi        = ee_aoi,
  dates      = dates.v,
  out_dir    = "./Data/Imagery",
  study_area = study_area
)
```

`download_hls()` blends Harmonized Landsat Sentinel-2 (S30 + L30), which
covers years before 2019. For a study window entirely after ~2019,
`download_s2()` is a simpler Sentinel-2-only alternative with the same
0-1 scaled 4-band output:

```{r}
download_s2(
  aoi        = ee_aoi,
  dates      = dates.v,
  out_dir    = "./Data/Imagery",
  study_area = study_area
)
```

Only one of the two is needed — both write the same `red`/`green`/`blue`/
`nir` band layout that every downstream function below expects.

## 4. Download and downscale LAI

```{r}
download_modis_lai(
  aoi        = ee_aoi,
  dates      = dates.v,
  out_dir    = "./Data/LAI/Coarse",
  study_area = study_area
)

downscale_lai(
  dates      = dates.v,
  img_dir    = "./Data/Imagery",
  lai_dir    = "./Data/LAI/Coarse",
  out_dir    = "./Data/LAI/Fine",
  study_area = study_area
)
```

`downscale_lai()` calls `microclimdata::lai_fromndvi()` internally,
fusing the coarse (500 m) MODIS LAI with NDVI computed from the fine (30 m)
imagery downloaded in step 3 — this is the package's first direct use of
`microclimdata`.

## 5. Download and compute albedo

```{r}
download_albedo(
  aoi        = ee_aoi,
  dates      = dates.v,
  out_dir    = "./Data/Albedo/Modis",
  study_area = study_area
)

compute_albedo(
  dates      = dates.v,
  modis_dir  = "./Data/Albedo/Modis",
  img_dir    = "./Data/Imagery",
  out_dir    = "./Data/Albedo/Fine",
  study_area = study_area
)
```

`compute_albedo()` calls `microclimdata::albedo_fromaerial()` to compute
photographic albedo from the imagery, then `microclimdata::albedo_adjust()`
to rescale it to match the coarse MODIS broadband albedo.
````

- [ ] **Step 3: Append the land cover / veg height / soil section**

Append this to `vignettes/02-data-pipeline.Rmd`:

````markdown

## 6. Land cover, vegetation height, and soil

Land cover and vegetation height each have two sources: this package's own
NLCD/LANDFIRE preprocessing (what you'd already have if you work with
those products), or pulling ready-to-use rasters directly from
`microclimdata`'s own Earth Engine downloads. Both paths feed the same
downstream arguments — pick whichever source you have. Only ESA + ETH is
shown running end-to-end below; the NLCD/LANDFIRE side is included so both
are visible.

### Land cover

```{r}
# (a) Primary: your own NLCD raster, converted to CORINE classes
corine_lc <- NLCD_2_CORINE(
  lc            = terra::rast("./Data/LandCover/NLCD_2021.tif"),
  crop_template = dtm
)

# (b) Alternative: ESA WorldCover pulled directly via microclimdata (no
#     conversion step needed)
esa_lc <- microclimdata::lcover_download(
  r                 = dtm,
  type              = "ESA",
  year              = 2020,
  GoogleDrivefolder = "GEE_Exports",
  pathtopython      = "C:/Users/you/miniforge3/envs/rgee"
)
```

### Vegetation height

```{r}
# (a) Primary: your own LANDFIRE Existing Vegetation Height raster
lf_veg_height <- LandfireVegHght_AsNumeric(
  rast          = terra::rast("./Data/VegHeight/LF2020_EVH.tif"),
  crop_template = dtm
)

# (b) Alternative: ETH Global Canopy Height pulled directly via microclimdata
veg_height <- microclimdata::vegheight_download(
  r                 = dtm,
  GoogleDrivefolder = "GEE_Exports",
  pathtopython      = "C:/Users/you/miniforge3/envs/rgee"
)
```

Whichever land cover source you use determines the `lctype` argument
downstream (`"CORINE"` for NLCD-derived, `"ESA"` for WorldCover) in
`compute_reflectance()` and `package_veg_soil()` below — and the matching
`water` land-cover code (512 for CORINE's water class, 80 for ESA
WorldCover's).

### Soil

```{r}
soil <- download_soil(
  dtm        = dtm,
  landcover  = esa_lc,
  out_dir    = "./Data/Soil",
  study_area = study_area,
  water      = 80    # ESA WorldCover water class code
)
```

`download_soil()` wraps `microclimdata::soildata_download()` (coarse
SoilGrids pull) and `microclimdata::soildata_downscale()` (fine-resolution
downscale using the land cover raster) into one call. Calling those two
`microclimdata` functions directly gives more control — e.g. inspecting or
saving the coarse raster before downscaling:

```{r}
dtm_wgs84 <- terra::project(dtm, "epsg:4326")

soil_coarse <- microclimdata::soildata_download(
  dtm_wgs84,
  pathdir     = "./Data/Soil/tmp",
  deletefiles = FALSE
)
terra::writeRaster(soil_coarse, "./Data/Soil/SoilData_Coarse.tif", overwrite = TRUE)

lc_wgs84  <- terra::project(esa_lc, "epsg:4326")
soil_fine <- microclimdata::soildata_downscale(soil_coarse, lc_wgs84, water = 80)
soil_fine <- terra::project(soil_fine, dtm)
```
````

- [ ] **Step 4: Append the AORC/diffuse-radiation, reflectance, and packaging sections**

Append this to `vignettes/02-data-pipeline.Rmd`:

````markdown

## 7. Download AORC climate data and estimate diffuse radiation

```{r}
aorc_log <- download_aorc(
  aoi         = ee_aoi,
  dates       = dates.v,
  out_dir     = "./Data/Weather",
  study_area  = study_area,
  workers     = 2,
  python_path = "C:/Users/you/miniforge3/envs/rgee"
)

estimate_diffuse_rad(
  dates      = dates.v,
  aorc_dir   = "./Data/Weather",
  study_area = study_area,
  workers    = 2
)
```

Both functions keep their output in WGS84 (EPSG:4326) — reprojecting AORC
data *before* `estimate_diffuse_rad()` introduces empty edge cells from
projection warping that corrupt its solar geometry calculations.
Reprojection happens later, inside `package_climate()`.

## 8. Compute leaf and ground reflectance

```{r}
compute_reflectance(
  dates        = dates.v,
  landcover    = esa_lc,
  lai_dir      = "./Data/LAI/Fine",
  alb_dir      = "./Data/Albedo/Fine",
  out_dir_lref = "./Data/Reflectance/Lref",
  out_dir_gref = "./Data/Reflectance/Gref",
  study_area   = study_area,
  lctype       = "ESA"
)
```

`compute_reflectance()` calls `microclimdata::x_calc()` (land-cover-based x
values) and `microclimdata::reflectance_calc()` to solve for leaf (`Lref`)
and ground (`Gref`) reflectance from LAI and albedo.

## 9. (Optional) Multi-year climate normals

If you have multiple years of AORC data and want long-term hourly normals
rather than a specific period's actual weather, `summarize_climate_normals()`
stacks each calendar hour across years:

```{r}
summarize_climate_normals(
  dates      = dates.v,
  aorc_dir   = "./Data/Weather",
  out_dir    = "./Data/ClimateNormals",
  stats      = c("mean", "sd"),
  study_area = study_area
)
```

This dummy example only has one summer of data, so it's skipped below —
`package_climate()` in step 11 uses the actual AORC data directly instead.

## 10. Package vegetation and soil parameters

```{r}
dates_df <- data.frame(
  Start_Dates = as.Date("2023-07-01"),
  End_Dates   = as.Date("2023-08-31")
)

veg_soil_log <- package_veg_soil(
  dates            = dates_df,
  snow_free_months = 7:8,
  landcover        = esa_lc,
  veg_height       = veg_height,
  soil_path        = "./Data/Soil/SoilData_Fine_UI_Arboretum.tif",
  lai_dir          = "./Data/LAI/Fine",
  refl_dir         = "./Data/Reflectance",
  vegpara_dir      = "./Data/VegPara",
  soilpara_dir     = "./Data/SoilPara",
  study_area       = study_area,
  lctype           = "ESA",
  water            = 80
)
```

This produces `./Data/VegPara/UI_Arboretum_VegPara_20230701_to_20230831.RDS`
and the equivalent `SoilPara` file, following the
`{study_area}_VegPara_{period_label}.RDS` naming convention (period labels
are always `YYYYMMDD_to_YYYYMMDD`).

## 11. Package climate data

`package_climate()` needs a template raster to reproject and crop AORC
data to — the vegetation parameter grid just produced is the natural
choice, since it's on the same grid `run_micro_big_nichemap()` expects
downstream:

```{r}
veg_template <- terra::rast(readRDS(
  "./Data/VegPara/UI_Arboretum_VegPara_20230701_to_20230831.RDS"
)$hgt)

package_climate(
  dates        = dates_df,
  aorc_dir     = "./Data/Weather",
  out_dir      = "./Data/Weather_Pkg",
  study_area   = study_area,
  template     = veg_template,
  keep_monthly = TRUE
)
```

This produces `./Data/Weather_Pkg/UI_Arboretum_Climate_20230701_to_20230831.RDS`.

## Next steps

The `02-data-pipeline` outputs — the `VegPara`, `SoilPara`, and `Climate`
RDS files, plus the fine and coarse DEMs — are exactly what
`03-running-microclimf` uses to run the tiled `microclimf` model, locally
or on a SLURM cluster.
````

- [ ] **Step 5: Knit-check the vignette**

Run:
```r
Rscript -e 'rmarkdown::render("vignettes/02-data-pipeline.Rmd", output_dir = tempdir(), quiet = TRUE); cat("RENDER OK\n")'
```
Expected: `RENDER OK` printed, no error.

- [ ] **Step 6: Commit**

```bash
git add vignettes/02-data-pipeline.Rmd
git commit -m "docs: add data pipeline walkthrough vignette"
```

---

### Task 4: Vignette 3 — Running microclimf models locally and on a cluster

**Files:**
- Create: `vignettes/03-running-microclimf.Rmd`

**Interfaces:**
- Consumes: the exact output paths/filenames vignette 2 (Task 3) produces — `./Data/DEM/DEM_GLO30.tif`, `./Data/Weather_Pkg/UI_Arboretum_Climate_20230701_to_20230831.RDS`, `./Data/VegPara`, `./Data/SoilPara`, `study_area = "UI_Arboretum"`, period `20230701_to_20230831`.
- Produces: nothing consumed by later tasks (final vignette).

- [ ] **Step 1: Write the front matter, coarse-DEM, and create_tiles sections**

Create `vignettes/03-running-microclimf.Rmd` with:

````markdown
---
title: "Running microclimf Models: Local and Cluster"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Running microclimf Models: Local and Cluster}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)
```

This picks up where `02-data-pipeline` left off: a packaged climate RDS,
vegetation/soil parameter RDS files, and a fine-resolution DEM for the
UI Arboretum dummy site. It covers tiling, running `microclimf` locally,
and running the same model as a SLURM array job.

## 1. Build the coarse (climate-resolution) DEM

`create_tiles()` and `run_micro_big_nichemap()` need a `coarse_dem` on the
same grid as the packaged climate data — not the fine 30 m DEM. Build it by
resampling the fine DEM onto the climate raster's actual grid:

```{r}
library(terra)
library(SpencerEcoTools)

dem_fine <- terra::rast("./Data/DEM/DEM_GLO30.tif")

clim_rds     <- readRDS("./Data/Weather_Pkg/UI_Arboretum_Climate_20230701_to_20230831.RDS")
clim_template <- terra::unwrap(clim_rds[[1]])

dem_coarse <- terra::resample(dem_fine, clim_template)
terra::writeRaster(dem_coarse, "./Data/DEM/DEM_Coarse_UI_Arboretum.tif", overwrite = TRUE)
```

(Climate RDS files store a wrapped `SpatRaster` — `terra::unwrap()` is
required before use, matching how `package_climate()` writes them.)

## 2. Create tiles

```{r}
dates_df <- data.frame(
  Start_Dates = as.Date("2023-07-01"),
  End_Dates   = as.Date("2023-08-31")
)

tiles <- create_tiles(
  coarse_dem        = dem_coarse,
  fine_dem          = dem_fine,
  dates             = as.Date(c("2023-07-01", "2023-08-31")),
  tile_dims         = c(1, 1),
  snow_modeling     = FALSE,
  return_tiles_rast = TRUE
)
```

The dummy site is small enough to fit in a single tile
(`tile_dims = c(1, 1)`). On a real, larger study area, either pass bigger
`tile_dims` or omit `tile_dims` entirely (`NULL`, the default) to let
`create_tiles()` pick the largest tile size that fits within
`mem_fraction` (default 70%) of available system RAM — this is what keeps
`run_micro_big_nichemap()` memory-safe on large domains.
````

- [ ] **Step 2: Append the local-run and cluster-run sections**

Append this to `vignettes/03-running-microclimf.Rmd`:

````markdown

## 3. Run locally

```{r}
log_local <- run_micro_big_nichemap(
  tiles       = tiles,
  clim        = "./Data/Weather_Pkg",
  dates       = dates_df,
  dtm_fine    = dem_fine,
  dtm_coarse  = dem_coarse,
  vegp        = "./Data/VegPara",
  soilc       = "./Data/SoilPara",
  output_dir  = "./Microclim_out",
  reqhgt      = 2,
  zref        = 2,
  windhgt     = 10,
  matemp      = NA,
  snow        = FALSE,
  Dynreqhgt   = TRUE,
  altcorrect  = 2,
  parallel    = TRUE,
  ncores      = 4,
  study_area  = "UI_Arboretum",
  file_fmt    = "nc",
  compression = 6
)
```

This models every height in one process: the above-ground height
(`reqhgt = 2`, i.e. 2 m) plus all ten fixed soil depths (0-200 cm), for
every tile and every period in `dates`. With no `clust_array_arg`/
`clust_array_size` supplied, all `(tile, period)` combinations run in this
one R session.

## 4. Run on a SLURM cluster

`run_micro_big_nichemap()` never submits jobs itself — `clust_array_arg`
and `clust_array_size` are hidden `...` arguments an SBATCH array script
passes in, so each array task processes a slice of the same
`(tile, period)` combinations the local run processed all at once. Put the
model call in its own script:

`run_microclimf_cluster.R`:
```r
#!/usr/bin/env Rscript
library(terra)
library(SpencerEcoTools)

n_threads <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = "1"))
terraOptions(threads = n_threads, memfrac = 0.7, tempdir = "~/terra_temp")

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default = NA) {
  x <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(x) == 0) return(default)
  sub(paste0("^--", name, "="), "", x)
}
clust_array_arg  <- get_arg("clust_array_arg")
clust_array_size <- get_arg("clust_array_size")

dem_fine   <- rast("./Data/DEM/DEM_GLO30.tif")
dem_coarse <- rast("./Data/DEM/DEM_Coarse_UI_Arboretum.tif")

dates_df <- data.frame(
  Start_Dates = as.Date("2023-07-01"),
  End_Dates   = as.Date("2023-08-31")
)

tiles <- create_tiles(
  coarse_dem = dem_coarse, fine_dem = dem_fine,
  dates      = as.Date(c("2023-07-01", "2023-08-31")),
  tile_dims  = c(1, 1), return_tiles_rast = TRUE
)

log <- run_micro_big_nichemap(
  tiles = tiles, clim = "./Data/Weather_Pkg", dates = dates_df,
  dtm_fine = dem_fine, dtm_coarse = dem_coarse,
  vegp = "./Data/VegPara", soilc = "./Data/SoilPara",
  output_dir = "./Microclim_out",
  reqhgt = 2, zref = 2, windhgt = 10, matemp = NA,
  snow = FALSE, Dynreqhgt = TRUE, altcorrect = 2,
  parallel = TRUE, ncores = n_threads,
  study_area = "UI_Arboretum", file_fmt = "nc", compression = 6,
  clust_array_arg  = as.integer(clust_array_arg),
  clust_array_size = as.integer(clust_array_size)
)

cat(sprintf("Task %s/%s complete.\n", clust_array_arg, clust_array_size))
```

And a companion SBATCH script that drives it as an array job. Suppose a
real (non-dummy) run of this study area produces 4 tiles over 1 period —
4 total `(tile, period)` tasks, so the array is sized to 4:

`run_microclimf.sbatch`:
```bash
#!/bin/bash
#SBATCH --job-name=microclimf_UI_Arboretum
#SBATCH --array=1-4
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --time=02:00:00
#SBATCH --output=logs/microclimf_%A_%a.out

module load R

Rscript run_microclimf_cluster.R \
  --clust_array_arg=${SLURM_ARRAY_TASK_ID} \
  --clust_array_size=4
```

`run_micro_big_nichemap()` enumerates all `(tile, period)` combinations
1-N and distributes them round-robin across the array
(`rep(seq_len(clust_array_size), length.out = N_tasks)`, tile index
cycling fastest) — so `--array=1-4` here means each of the 4 array tasks
gets exactly one `(tile, period)` combination in this example. Size
`--array` and `--clust_array_size` to `N_tiles x N_periods`, or fewer if
running multiple combinations per node.
````

- [ ] **Step 3: Append the stitching and inspection sections**

Append this to `vignettes/03-running-microclimf.Rmd`:

````markdown

## 5. Stitch tile outputs back together

Whether the run was local or a completed SLURM array, each `(tile, period)`
combination is written as a separate file under `output_dir`.
`stitch_tiles_runmicro()` assembles them into one full-domain file per
period and data type:

```{r}
stitch_tiles_runmicro(
  output_dir = "./Microclim_out",
  dates      = dates_df,
  study_area = "UI_Arboretum",
  file_fmt   = "nc"
)
```

With `file_fmt = "nc"`, this builds a GDAL VRT per variable (no data
copied, tile files must stay at their original paths); `file_fmt = "h5"`
instead builds an HDF5 virtual dataset. Use whichever `file_fmt` matches
what `run_micro_big_nichemap()` was run with.

## 6. Inspect results at a point

`micro_to_csv()` extracts one grid cell's modeled time series from a
stitched above-ground + below-ground output pair, reshaped into
NicheMapR-style data frames — useful for a quick sanity check, or as input
to point-model workflows downstream of this package's scope:

```{r}
pt_series <- micro_to_csv(
  abvgrd_input    = "./Microclim_out/UI_Arboretum/Stitched/20230701_to_20230831/UI_Arboretum_AbvGrd_MicroclimModel_20230701_to_20230831",
  blwgrd_input    = "./Microclim_out/UI_Arboretum/Stitched/20230701_to_20230831/UI_Arboretum_BlwGrd_MicroclimModel_20230701_to_20230831",
  cell            = 1,
  cell_input_type = "cellnumber",
  dates           = as.Date(c("2023-07-01", "2023-07-07")),
  elev            = 790
)
```

Note the `.vrt`-mode inputs above are the shared *stem* path — no
extension, no variable suffix — since `stitch_tiles_runmicro(file_fmt =
"nc")` writes one `{stem}_{varname}.vrt` file per variable rather than one
combined file.
````

- [ ] **Step 4: Knit-check the vignette**

Run:
```r
Rscript -e 'rmarkdown::render("vignettes/03-running-microclimf.Rmd", output_dir = tempdir(), quiet = TRUE); cat("RENDER OK\n")'
```
Expected: `RENDER OK` printed, no error.

- [ ] **Step 5: Commit**

```bash
git add vignettes/03-running-microclimf.Rmd
git commit -m "docs: add running-microclimf local/cluster vignette"
```

---

### Task 5: Full-package vignette build verification

**Files:**
- No new files. Verification only.

**Interfaces:**
- Consumes: all three vignette files from Tasks 2-4, plus the `DESCRIPTION`/`VignetteBuilder` wiring from Task 1.

- [ ] **Step 1: Build the full package tarball with vignettes, into a temp directory**

Run:
```r
Rscript -e 'path <- devtools::build(pkg = ".", path = tempdir(), vignettes = TRUE, quiet = FALSE); cat("BUILT:", path, "\n")'
```
Expected: completes without error, prints a `BUILT: <path-to-.tar.gz-in-tempdir>` line. This exercises the real `VignetteBuilder: knitr` wiring from Task 1 against all three real vignette files, unlike the per-file `rmarkdown::render()` checks in Tasks 2-4 — it's the closest thing to what CRAN/`R CMD check` does. Because `path = tempdir()`, no `doc/`/`Meta/` build artifacts are written into the repo.

- [ ] **Step 2: Confirm the repo working tree is clean**

Run: `git status --short`
Expected: no output (everything from Tasks 1-4 already committed, and Step 1's build artifacts went to `tempdir()`, not the repo).

- [ ] **Step 3: If Step 1 failed, fix and re-verify**

If `devtools::build()` errored, the error will name which vignette and what failed (most likely a YAML front-matter typo or unbalanced fence in one of the three files). Fix the specific file with `Edit`, then re-run Step 1 until it passes. Commit any fix as its own commit:

```bash
git add vignettes/<fixed-file>.Rmd
git commit -m "fix: correct vignette build error in <fixed-file>"
```

No further step needed if Step 1 passed on the first try.
