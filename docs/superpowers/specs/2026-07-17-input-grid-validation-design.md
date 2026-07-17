# Input Grid Validation Design

Date: 2026-07-17

## Problem

`run_micro_big_nichemap()` crops packaged climate data against `dtm_coarse` and
packaged vegetation/soil data against `dtm_fine`, and passes the results
straight into `microclimfPara::runpointmodela()` / `runmicro()`. Nothing
verifies that `clim` actually shares `dtm_coarse`'s grid, or that `vegp`/`soilc`
share `dtm_fine`'s grid. `terra::crop()` does not error on a resolution or CRS
mismatch -- it silently produces misaligned output, which can corrupt model
results without any visible failure.

Similarly, `reqhgt` (the above-ground model height) is only loosely checked
today (`reqhgt <= 0`), which doesn't reject `NA`, non-numeric, or vector input.

Three upstream packaging/tiling functions (`create_tiles()`, `package_climate()`,
`package_veg_soil()`) have related, lower-stakes versions of the same class of
problem and should get advisory warnings rather than hard failures.

## Design

### Shared helper: `.check_grid_match()`

New internal helper in `R/Microclimf_Modeling.R` (internal functions are visible
package-wide regardless of source file):

```r
.check_grid_match <- function(ref, target, ref_label, target_label,
                               action = c("stop", "warn"), tol = 1e-6) {
  action <- match.arg(action)
  ref_res    <- terra::res(ref)
  target_res <- terra::res(target)

  res_ok <- isTRUE(all.equal(ref_res, target_res, tolerance = tol))
  crs_ok <- terra::same.crs(ref, target)

  if (res_ok && crs_ok) return(invisible(TRUE))

  msg <- sprintf(
    "%s and %s do not share the same grid:\n  %s resolution: %s\n  %s resolution: %s\n  CRS match: %s",
    ref_label, target_label,
    ref_label, paste(signif(ref_res, 10), collapse = " x "),
    target_label, paste(signif(target_res, 10), collapse = " x "),
    crs_ok
  )

  if (action == "stop") stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  invisible(FALSE)
}
```

- Resolution comparison uses a relative tolerance (`all.equal`, default
  `tol = 1e-6`) to absorb floating-point noise from reprojection, not exact
  equality.
- CRS comparison uses `terra::same.crs()`, which handles CRS objects that are
  written as different WKT text but are equivalent -- not a string compare.

### Companion helper: `.first_spatraster()`

The `clim`/`vegp`/`soilc` RDS objects are lists of `PackedSpatRaster`s (with a
custom class attribute in the veg/soil case). A small recursive helper finds
and unwraps the first raster-like element for comparison purposes:

```r
.first_spatraster <- function(x) {
  if (inherits(x, "SpatRaster")) return(x)
  if (inherits(x, "PackedSpatRaster")) return(terra::unwrap(x))
  if (is.list(x)) {
    for (el in x) {
      r <- tryCatch(.first_spatraster(el), error = function(e) NULL)
      if (!is.null(r)) return(r)
    }
  }
  stop("No SpatRaster found")
}
```

### `run_micro_big_nichemap()` -- hard checks (`stop()`)

**`reqhgt` validation** (replaces the existing `reqhgt <= 0` check):

```r
if (!is.numeric(reqhgt) || length(reqhgt) != 1 || is.na(reqhgt) || reqhgt <= 0)
  stop("reqhgt must be a single positive numeric value")
```

**Grid validation pre-flight pass.** Added after the existing
clim/vegp/soilc existence checks (around line 1976) and *before* the terrain
pre-computation (slope/aspect/horizon over the full fine DEM), so a bad input
fails immediately rather than after several minutes of terrain computation.

The date-range-to-period-label normalization currently appears once, inline,
inside the main tile loop (lines ~2085-2089). It will be factored into a small
helper, `.period_label(start, end)`, used both by the new pre-flight pass and
the existing main loop, so the two can't drift out of sync.

```r
# --- validate clim / vegp / soilc grids against dtm_coarse / dtm_fine ------
cat("--- run_micro_big_nichemap: validating input grids ---\n")
for (i in seq_len(nrow(date_ranges))) {
  period_label <- .period_label(date_ranges$Start_Dates[i], date_ranges$End_Dates[i])

  clim_path  <- .resolve_rds_path(clim,  "Climate",  period_label, study_area)
  vegp_path  <- .resolve_rds_path(vegp,  "VegPara",  period_label, study_area)
  soilc_path <- .resolve_rds_path(soilc, "SoilPara", period_label, study_area)

  clim_r <- .first_spatraster(readr::read_rds(clim_path))
  .check_grid_match(dtm_coarse, clim_r, "dtm_coarse", sprintf("clim (%s)", period_label), action = "stop")
  rm(clim_r)

  vegp_r <- .first_spatraster(readr::read_rds(vegp_path))
  .check_grid_match(dtm_fine, vegp_r, "dtm_fine", sprintf("vegp (%s)", period_label), action = "stop")
  rm(vegp_r)

  soilc_r <- .first_spatraster(readr::read_rds(soilc_path))
  .check_grid_match(dtm_fine, soilc_r, "dtm_fine", sprintf("soilc (%s)", period_label), action = "stop")
  rm(soilc_r)

  invisible(gc())
}
cat("  All input grids match.\n")
```

This loads each period's climate/veg/soil RDS once during pre-flight (in
addition to the per-tile reloads the main loop already does) -- a small,
one-time cost relative to the full tiled run.

### `create_tiles()` -- warnings only

Inserted right where `ratio_r`/`ratio_c` are already computed (no duplicate
computation):

```r
if (!terra::same.crs(coarse_dem, fine_dem)) {
  warning("coarse_dem and fine_dem have different CRS; tile extents may not align correctly.")
}
if (abs(ratio_r - round(ratio_r)) > 1e-6 || abs(ratio_c - round(ratio_c)) > 1e-6) {
  warning(sprintf(
    "fine_dem resolution is not a whole-number multiple of coarse_dem resolution (row ratio = %.4f, col ratio = %.4f); tile boundaries may not align to whole fine-DEM cells.",
    ratio_r, ratio_c))
}
```

### `package_climate()` -- warning only

`load_var()` reprojects each climate variable to `template`'s CRS but never
resamples to `template`'s resolution, so a mismatch can pass through silently
today. Check once, against the first variable loaded for the first
month/period (avoids a warning flood across every month/variable):

```r
if (i == 1L && j == 1L) {
  .check_grid_match(template, r_sw, "template", "packaged climate", action = "warn")
}
```

Inserted immediately after `r_sw <- load_var(sw_f, template)` (before it is
wrapped).

### `package_veg_soil()` -- warning only, new optional parameter

This function has no fine-DEM reference today; it only self-aligns its own
inputs (LAI/landcover/veg-height/soil) to each other. Add an **optional**
`dtm_fine = NULL` parameter (default preserves the existing signature/behavior
for current callers). When supplied, after the geometry-alignment block
produces the common-grid `lai_rs`, warn if it doesn't match `dtm_fine`:

```r
if (!is.null(dtm_fine)) {
  .check_grid_match(dtm_fine, lai_rs, "dtm_fine", sprintf("packaged veg/soil (%s)", period_label), action = "warn")
}
```

Roxygen `@param dtm_fine` documents it as an optional validation-only input.

## Testing plan

The package already has `tests/testthat/`. Add `testthat` cases there
(`test-check_grid_match.R` or similar) covering, with small in-memory
`SpatRaster` objects built via `terra::rast()`:

- `.check_grid_match()`: matching grid passes silently; mismatched resolution
  stops/warns per `action`; mismatched CRS (same resolution) stops/warns;
  near-equal resolution within tolerance passes.
- `.first_spatraster()`: unwraps a plain `SpatRaster`, a `PackedSpatRaster`,
  and a nested list (as in the real `vegp`/`soilc` objects).
- `reqhgt` validation in `run_micro_big_nichemap()`: `NA`, non-numeric,
  length-2, and `<= 0` all rejected; a valid positive scalar passes through
  (can be tested by calling the validation logic directly if extracted, or by
  confirming the function stops before any file I/O for invalid `reqhgt`).
- `create_tiles()` warnings: non-integer ratio and CRS mismatch each produce
  exactly one `warning()`, matching rasters produce none.

These are pure/metadata-only checks (no NicheMapR/microclimf model runs, no
GEE, no real DEM files needed), so no new test fixtures beyond small
`terra::rast()` objects constructed inline are required.

## Out of scope

- No changes to `microclimfPara::runpointmodela()` / `runmicro()` internals.
- No automatic resampling/correction of mismatched grids -- these are
  validation-only checks; fixing a mismatch is left to the user (re-run
  packaging with a consistent template/DEM).
- No change to the per-tile reload behavior in the main loop (existing
  design, not this task's concern).
