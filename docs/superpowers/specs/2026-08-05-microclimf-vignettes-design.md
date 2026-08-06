# microclimf vignettes — design

Status: **approved**. Ready for `superpowers:writing-plans`.

## Context

SpencerEcoTools has no `vignettes/` directory and no `knitr`/`rmarkdown` in
`DESCRIPTION` yet. The package wraps a long GEE + `microclimdata` data-prep
pipeline (documented in order in `CLAUDE.md`) followed by tiled `microclimf`
point modeling (`create_tiles()` → `run_micro_big_nichemap()`, optionally
distributed over a SLURM array via hidden `clust_array_arg`/
`clust_array_size` args — the functions never submit jobs themselves).

Two real-world scripts were used as source material for realistic call
patterns and argument values:
- `Remote_data_Download_SY.R` — the full data-download/preprocessing run for
  a real study area ("Tetons"), including GEE init, `download_s2()`,
  `download_modis_lai()`/`downscale_lai()`, `compute_albedo()`,
  `compute_reflectance()`, manual (not-wrapped) calls to
  `microclimdata::soildata_download()`/`soildata_downscale()`,
  `download_aorc()`/`estimate_diffuse_rad()`, `package_veg_soil()`,
  `package_climate()`.
- `YearSpecific_MicroClimModel.R` — the SLURM-array driver script:
  `create_tiles()`, then `run_micro_big_nichemap()` with
  `clust_array_arg`/`clust_array_size` parsed from `commandArgs()`.

`R/data.R`'s `data_sources.Rd` already documents, as citable alternates,
that land cover and veg height can come either from the package's own
NLCD/LANDFIRE preprocessing (`NLCD_2_CORINE()`, `LandfireVegHght_AsNumeric()`)
or directly from `microclimdata::lcover_download(type = "ESA")` and
`microclimdata::vegheight_download()`. Soil, LAI, and albedo wrappers
(`download_soil()`, `downscale_lai()`, `compute_albedo()`) already call
specific `microclimdata` functions internally
(`soildata_download()`/`soildata_downscale()`, `lai_fromndvi()`,
`albedo_fromaerial()`/`albedo_adjust()` respectively).

Because GEE calls require live credentials and real download/compute time,
and cluster runs require an actual SLURM allocation, none of this can
execute during `devtools::document()`, `R CMD check`, or a `pkgdown` build.

## Decisions locked in

1. **Three vignettes**, each independently readable in the pkgdown Articles
   menu, numbered for ordering:
   - `vignettes/01-gee-setup.Rmd` — "Setting Up Google Earth Engine"
   - `vignettes/02-data-pipeline.Rmd` — "Data Download & Preprocessing
     Walkthrough"
   - `vignettes/03-running-microclimf.Rmd` — "Running microclimf Models:
     Local and Cluster"

2. **No code executes at build time.** Every vignette sets
   `knitr::opts_chunk$set(eval = FALSE)` in its setup chunk. Code is shown
   for the reader to copy/adapt, never run, cached, or checked by CI/CRAN.
   No plots, no stored outputs — this is deliberate given GEE/SLURM
   dependencies.

3. **Package infra additions** (not vignette content, but required to build
   vignettes at all):
   - Add `knitr`, `rmarkdown` to `Suggests` in `DESCRIPTION`.
   - Add `VignetteBuilder: knitr`.
   - Create `vignettes/` directory.

4. **Dummy site**: a small (~1–2 km²) real bounding box near Moscow, ID
   (University of Idaho Arboretum area), UTM zone 11N (`EPSG:32611`), used
   consistently across vignettes 2 and 3. Concrete coordinates make the
   CRS/DEM-tile/study-area reasoning grounded rather than abstract. A short
   (e.g. 2-month) date range keeps the walkthrough "dummy"-scaled.

5. **Audience: public package users**, not just the author. Vignette 1
   explains GEE/conda/reticulate setup from scratch (assume the reader has
   never touched Earth Engine or `reticulate`), rather than assuming prior
   familiarity.

6. **Vignette 1 (`01-gee-setup.Rmd`) content**:
   - What GEE is used for in this package (DEM, HLS/S2 imagery, MODIS
     LAI/albedo) vs. what is *not* GEE (AORC via NOAA S3, soil/veg/landcover
     alternates via `microclimdata`).
   - Getting a Google Cloud project with Earth Engine enabled.
   - Conda/miniforge env with Python `earthengine-api` (plus `xarray`,
     `s3fs`, `zarr`, `netCDF4` for `download_aorc()`), pointed to via
     `reticulate::use_python(..., required = TRUE)`.
   - Exact init sequence from the real script: `ee <-
     reticulate::import("ee")`, `ee$Initialize(project = "your-project-id")`,
     smoke test `ee$Number(1)$getInfo()`.
   - Brief explanation that the package's internal `ee` binding
     (`R/zzz.R`, lazy `delay_load`) shares the same Python session, so this
     user-level init — not a separate package call — is what makes package
     functions work.
   - Separate `googledrive::drive_auth()` requirement for `poll_drive()`
     (GEE exports land in Drive before being pulled locally).

7. **Vignette 2 (`02-data-pipeline.Rmd`) content**: walks the dummy site
   through the full `CLAUDE.md` pipeline order, steps 1–15, each step shown
   with its real call signature and a one-line note on what it produces /
   what the next step reads:

   `define_aoi()` → `download_dem()` → `download_hls()` (with a callout on
   `download_s2()` as the Sentinel-2-only alternative actually used in the
   source script) → `download_modis_lai()` → `download_albedo()` →
   `download_soil()` → `download_aorc()` → `estimate_diffuse_rad()` (WGS84
   constraint called out explicitly, per `CLAUDE.md`) → `downscale_lai()` →
   land cover + veg height (see below) → `compute_albedo()` →
   `compute_reflectance()` → `summarize_climate_normals()` (flagged
   optional, only needed for multi-year normals) → `package_climate()` →
   `package_veg_soil()`.

   Four sub-sections get explicit "SpencerEcoTools wrapper +
   underlying `microclimdata` call(s)" treatment, since this is where the
   two packages blend most directly:
   - **LAI**: `download_modis_lai()` (coarse MODIS LAI via GEE) →
     `downscale_lai()`, noting it calls `microclimdata::lai_fromndvi()`
     internally to fuse coarse LAI with NDVI from the HLS/S2 imagery down
     to 30 m.
   - **Albedo**: `download_albedo()` (coarse MODIS MCD43A3 via GEE) →
     `compute_albedo()`, noting it calls
     `microclimdata::albedo_fromaerial()` (photographic albedo from
     imagery) then `microclimdata::albedo_adjust()` (adjusts to match MODIS
     broadband albedo) internally.
   - **Soil**: `download_soil(dtm, landcover, out_dir, ...)` shown as the
     one-call path (wraps `microclimdata::soildata_download()` +
     `soildata_downscale()` internally), with a note that calling those two
     `microclimdata` functions directly (as the source script actually
     does) gives more control — e.g. inspecting/saving the coarse raster
     before downscaling.
   - **Land cover / veg height**: presented as a primary/alternate choice,
     reusing the framing already in `data_sources.Rd`:
     - Land cover: (a) NLCD → `NLCD_2_CORINE()`, or (b)
       `microclimdata::lcover_download(type = "ESA")` — ready to use, no
       conversion step.
     - Veg height: (a) LANDFIRE EVH → `LandfireVegHght_AsNumeric()`, or (b)
       `microclimdata::vegheight_download()` (ETH Global Canopy Height) —
       ready to use directly.
     - Both feed the same downstream `lctype` (`"CORINE"` vs `"ESA"`) and
       `veg_height` arguments in `compute_reflectance()`/`package_veg_soil()`.

   Ends with the two packaged artifacts (`_Climate_...RDS` and
   `_VegPara_.../_SoilPara_...RDS`, renamed to the dummy site) that
   vignette 3 consumes.

8. **Vignette 3 (`03-running-microclimf.Rmd`) content**:
   - `create_tiles()` on the dummy site with small `tile_dims` (e.g.
     `c(1,1)` or `c(2,2)`); explain tiling exists for memory efficiency on
     large real study areas even though the dummy site only needs one tile.
   - **Local run**: full `run_micro_big_nichemap()` call mirroring
     `YearSpecific_MicroClimModel.R`, without `clust_array_arg`/
     `clust_array_size`, `parallel = TRUE` with `future` workers.
   - **Cluster run**: same call, now with `clust_array_arg`/
     `clust_array_size` parsed from `commandArgs()` in a trimmed Rscript
     modeled on the source script, plus a literal example SBATCH script
     (`#SBATCH --array=1-N`, `$SLURM_ARRAY_TASK_ID` →
     `--clust_array_arg=`) with a short note on the `N_tiles × N_periods`
     round-robin task distribution described in `CLAUDE.md`.
   - Brief closing mention of `stitch_tiles()`/`stitch_tiles_runmicro()`
     (reassembling array-job outputs into full study-area rasters) and
     `micro_to_csv()` (inspecting/extracting point time series from
     results).

9. **Out of scope**: endotherm/NicheMapR modeling
   (`Endotherm_DataPrep.R`/`Endotherm_Modeling.R`,
   `write_endotherm_inputs()`, `run_endotherm_model()`) — deferred per an
   explicit ask to cover `microclimf` modeling only for now.

## Non-goals / explicitly excluded

- No executed code, no cached vignette outputs, no committed sample data
  large enough to require download — all chunks are illustrative
  (`eval = FALSE`).
- No coverage of the endotherm/NicheMapR modeling components.
- No CI wiring to build/check vignettes against live GEE or SLURM — the
  `eval = FALSE` approach means standard `R CMD check`/`devtools::check()`
  is sufficient (vignettes must still parse and knit without executing).
