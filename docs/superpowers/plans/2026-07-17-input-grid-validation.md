# Input Grid Validation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add resolution/CRS validation between DEMs and the packaged climate/vegetation/soil data they're cropped against, so a mismatch fails loudly (in `run_micro_big_nichemap()`) or warns (in `create_tiles()`, `package_climate()`, `package_veg_soil()`) instead of silently corrupting model output.

**Architecture:** Two small internal helpers (`.check_grid_match()`, `.first_spatraster()`) added to `R/Microclimf_Modeling.R`, reused across all four functions. `run_micro_big_nichemap()` also gets a `.normalize_period()` helper to deduplicate date-to-period-label logic between its existing main loop and a new pre-flight validation pass.

**Tech Stack:** R, `terra` (resolution/CRS comparisons), `readr` (RDS I/O), `testthat` 3e (existing test suite in `tests/testthat/`).

## Global Constraints

- Namespacing: always use explicit `terra::`, `readr::` prefixes (per `CLAUDE.md`).
- Internal helpers are prefixed with `.`, no `@export`, minimal comments (per `CLAUDE.md`).
- `stop()` for unrecoverable errors; `warning()` + continue for recoverable ones (per `CLAUDE.md`).
- Run `devtools::document()` after any roxygen changes (per `CLAUDE.md`).
- Spec: `docs/superpowers/specs/2026-07-17-input-grid-validation-design.md`.

---

### Task 1: `.check_grid_match()` and `.first_spatraster()` helpers

**Files:**
- Modify: `R/Microclimf_Modeling.R` (insert after the `.get_total_ram()` helper, i.e. after line 11, before the `create_tiles` roxygen block)
- Test: `tests/testthat/test-check_grid_match.R`

**Interfaces:**
- Produces: `.check_grid_match(ref, target, ref_label, target_label, action = c("stop", "warn"), tol = 1e-6)` — returns `invisible(TRUE)` silently if `ref`/`target` (both `SpatRaster`) match resolution (within relative `tol`) and CRS (via `terra::same.crs()`); otherwise `stop()`s or `warning()`s (per `action`) with a message containing `"do not share the same grid"`, both rasters' resolutions, and CRS match status. Returns `invisible(FALSE)` after warning.
- Produces: `.first_spatraster(x)` — if `x` inherits `SpatRaster`, returns it; if `PackedSpatRaster`, returns `terra::unwrap(x)`; if a list, recursively searches elements and returns the first raster found; otherwise `stop("No SpatRaster found")`.

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-check_grid_match.R`:

```r
test_that(".check_grid_match passes silently when resolution and CRS match", {
  r1 <- terra::rast(resolution = 0.01, crs = "EPSG:4326",
                     xmin = 0, xmax = 0.05, ymin = 0, ymax = 0.05)
  r2 <- terra::rast(resolution = 0.01, crs = "EPSG:4326",
                     xmin = 0, xmax = 0.05, ymin = 0, ymax = 0.05)
  expect_silent(.check_grid_match(r1, r2, "ref", "target"))
})

test_that(".check_grid_match stops on resolution mismatch when action = 'stop'", {
  r1 <- terra::rast(resolution = 0.01, crs = "EPSG:4326",
                     xmin = 0, xmax = 0.05, ymin = 0, ymax = 0.05)
  r2 <- terra::rast(resolution = 0.02, crs = "EPSG:4326",
                     xmin = 0, xmax = 0.1, ymin = 0, ymax = 0.1)
  expect_error(.check_grid_match(r1, r2, "ref", "target", action = "stop"),
               "do not share the same grid")
})

test_that(".check_grid_match warns on resolution mismatch when action = 'warn'", {
  r1 <- terra::rast(resolution = 0.01, crs = "EPSG:4326",
                     xmin = 0, xmax = 0.05, ymin = 0, ymax = 0.05)
  r2 <- terra::rast(resolution = 0.02, crs = "EPSG:4326",
                     xmin = 0, xmax = 0.1, ymin = 0, ymax = 0.1)
  expect_warning(.check_grid_match(r1, r2, "ref", "target", action = "warn"),
                 "do not share the same grid")
})

test_that(".check_grid_match stops on CRS mismatch even with matching resolution", {
  r1 <- terra::rast(resolution = 0.01, crs = "EPSG:4326",
                     xmin = 0, xmax = 0.05, ymin = 0, ymax = 0.05)
  r2 <- terra::rast(resolution = 0.01, crs = "EPSG:3857",
                     xmin = 0, xmax = 0.05, ymin = 0, ymax = 0.05)
  expect_error(.check_grid_match(r1, r2, "ref", "target", action = "stop"),
               "do not share the same grid")
})

test_that(".check_grid_match tolerates tiny floating-point resolution differences", {
  r1 <- terra::rast(resolution = 0.01, crs = "EPSG:4326",
                     xmin = 0, xmax = 0.05, ymin = 0, ymax = 0.05)
  r2 <- terra::rast(resolution = 0.01 + 1e-10, crs = "EPSG:4326",
                     xmin = 0, xmax = 0.05, ymin = 0, ymax = 0.05)
  expect_silent(.check_grid_match(r1, r2, "ref", "target"))
})

test_that(".first_spatraster unwraps a plain SpatRaster", {
  r <- terra::rast(nrows = 2, ncols = 2)
  expect_true(inherits(.first_spatraster(r), "SpatRaster"))
})

test_that(".first_spatraster unwraps a PackedSpatRaster", {
  r <- terra::wrap(terra::rast(nrows = 2, ncols = 2))
  expect_true(inherits(.first_spatraster(r), "SpatRaster"))
})

test_that(".first_spatraster finds a raster nested inside a list", {
  r <- terra::wrap(terra::rast(nrows = 2, ncols = 2))
  nested <- list(a = 1, b = list(c = r))
  expect_true(inherits(.first_spatraster(nested), "SpatRaster"))
})

test_that(".first_spatraster errors when nothing raster-like is found", {
  expect_error(.first_spatraster(list(a = 1, b = "text")), "No SpatRaster found")
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e "devtools::test(filter = 'check_grid_match')"`
Expected: FAIL with errors like `could not find function ".check_grid_match"`.

- [ ] **Step 3: Implement the helpers**

In `R/Microclimf_Modeling.R`, insert immediately after the closing `}` of `.get_total_ram()` (line 11) and before the `create_tiles` roxygen block:

```r
# --------------------------------------------------------------------------- #
#  Grid-matching validation helpers
# --------------------------------------------------------------------------- #

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

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e "devtools::test(filter = 'check_grid_match')"`
Expected: PASS, all 9 tests green.

- [ ] **Step 5: Commit**

```bash
git add R/Microclimf_Modeling.R tests/testthat/test-check_grid_match.R
git commit -m "Add .check_grid_match()/.first_spatraster() validation helpers"
```

---

### Task 2: `.normalize_period()` helper (deduplicate date/period-label logic)

**Files:**
- Modify: `R/Microclimf_Modeling.R:1728-1743` area (add helper near `.resolve_rds_path`)
- Modify: `R/Microclimf_Modeling.R:2085-2089` (main loop, replace inline normalization with helper call)
- Test: `tests/testthat/test-normalize_period.R`

**Interfaces:**
- Consumes: nothing new.
- Produces: `.normalize_period(start, end)` — takes two values coercible via `as.Date()`, returns a list with `start_date` (Date, forced to first-of-month), `end_date` (Date, forced to first-of-month), and `period_label` (character, `"YYYYMMDD_to_YYYYMMDD"`). Used by Task 4's pre-flight loop and the existing main loop in `run_micro_big_nichemap()`.

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-normalize_period.R`:

```r
test_that(".normalize_period floors to first-of-month and builds the period label", {
  p <- .normalize_period(as.Date("2020-01-15"), as.Date("2020-03-20"))
  expect_equal(p$start_date, as.Date("2020-01-01"))
  expect_equal(p$end_date, as.Date("2020-03-01"))
  expect_equal(p$period_label, "20200101_to_20200301")
})

test_that(".normalize_period handles same-month start and end", {
  p <- .normalize_period(as.Date("2020-06-01"), as.Date("2020-06-28"))
  expect_equal(p$start_date, as.Date("2020-06-01"))
  expect_equal(p$end_date, as.Date("2020-06-01"))
  expect_equal(p$period_label, "20200601_to_20200601")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e "devtools::test(filter = 'normalize_period')"`
Expected: FAIL with `could not find function ".normalize_period"`.

- [ ] **Step 3: Implement the helper**

In `R/Microclimf_Modeling.R`, immediately after the closing `}` of `.resolve_rds_path` (currently ending at line 1743, right before the `# --- run_micro_big_nichemap -- exported` banner comment), add:

```r
# Normalizes a date range to first-of-month start/end and builds the
# "YYYYMMDD_to_YYYYMMDD" period label. Shared by the pre-flight grid
# validation pass and the main per-task loop in run_micro_big_nichemap().
.normalize_period <- function(start, end) {
  start_date <- as.Date(sprintf("%s-01", format(as.Date(start), "%Y-%m")))
  end_date   <- as.Date(sprintf("%s-01", format(as.Date(end),   "%Y-%m")))
  period_label <- sprintf("%s_to_%s",
                          format(start_date, "%Y%m%d"),
                          format(end_date,   "%Y%m%d"))
  list(start_date = start_date, end_date = end_date, period_label = period_label)
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e "devtools::test(filter = 'normalize_period')"`
Expected: PASS, both tests green.

- [ ] **Step 5: Replace the inline duplicate in the main loop**

In `R/Microclimf_Modeling.R`, find this block inside `run_micro_big_nichemap()`'s main `for (k in seq_len(nrow(task_combos)))` loop (currently lines 2085-2089):

```r
    start_date   <- as.Date(sprintf("%s-01", format(as.Date(date_ranges$Start_Dates[d]), "%Y-%m")))
    end_date     <- as.Date(sprintf("%s-01", format(as.Date(date_ranges$End_Dates[d]),   "%Y-%m")))
    period_label <- sprintf("%s_to_%s",
                            format(start_date, "%Y%m%d"),
                            format(end_date,   "%Y%m%d"))
```

Replace with:

```r
    .p           <- .normalize_period(date_ranges$Start_Dates[d], date_ranges$End_Dates[d])
    start_date   <- .p$start_date
    end_date     <- .p$end_date
    period_label <- .p$period_label
```

- [ ] **Step 6: Confirm no other code in the file depends on the old inline form**

Run: `Rscript -e 'cat(grep("Start_Dates\\[d\\]|End_Dates\\[d\\]", readLines("R/Microclimf_Modeling.R"), value = TRUE), sep = "\n")'`
Expected: no output (no remaining direct references outside the helper).

- [ ] **Step 7: Run full test suite to confirm nothing else broke**

Run: `Rscript -e "devtools::test()"`
Expected: PASS (same pass count as before this task, no new failures).

- [ ] **Step 8: Commit**

```bash
git add R/Microclimf_Modeling.R tests/testthat/test-normalize_period.R
git commit -m "Factor date/period-label normalization into .normalize_period()"
```

---

### Task 3: Harden `reqhgt` validation in `run_micro_big_nichemap()`

**Files:**
- Modify: `R/Microclimf_Modeling.R:1955`
- Test: `tests/testthat/test-run_micro_big_nichemap_validation.R` (new file, will also gain Task 4's tests)

**Interfaces:**
- Consumes: nothing new.
- Produces: nothing new (behavior change only). `reqhgt` must now be a single, non-`NA`, positive numeric value or `run_micro_big_nichemap()` stops with `"reqhgt must be a single positive numeric value"`. This check runs before `tiles`, `clim`, `dates`, `dtm_fine`, `dtm_coarse`, `vegp`, or `soilc` are ever touched, so it is safe to test with placeholder/invalid values for all other arguments.

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-run_micro_big_nichemap_validation.R`:

```r
test_that("run_micro_big_nichemap rejects a negative reqhgt", {
  expect_error(
    run_micro_big_nichemap(
      tiles = NULL, clim = "does/not/exist", dates = NULL,
      dtm_fine = NULL, dtm_coarse = NULL,
      vegp = "does/not/exist", soilc = "does/not/exist",
      output_dir = tempdir(), reqhgt = -1
    ),
    "reqhgt must be a single positive numeric value"
  )
})

test_that("run_micro_big_nichemap rejects NA reqhgt", {
  expect_error(
    run_micro_big_nichemap(
      tiles = NULL, clim = "x", dates = NULL, dtm_fine = NULL, dtm_coarse = NULL,
      vegp = "x", soilc = "x", output_dir = tempdir(), reqhgt = NA
    ),
    "reqhgt must be a single positive numeric value"
  )
})

test_that("run_micro_big_nichemap rejects non-numeric reqhgt", {
  expect_error(
    run_micro_big_nichemap(
      tiles = NULL, clim = "x", dates = NULL, dtm_fine = NULL, dtm_coarse = NULL,
      vegp = "x", soilc = "x", output_dir = tempdir(), reqhgt = "2"
    ),
    "reqhgt must be a single positive numeric value"
  )
})

test_that("run_micro_big_nichemap rejects a length-2 reqhgt", {
  expect_error(
    run_micro_big_nichemap(
      tiles = NULL, clim = "x", dates = NULL, dtm_fine = NULL, dtm_coarse = NULL,
      vegp = "x", soilc = "x", output_dir = tempdir(), reqhgt = c(1, 2)
    ),
    "reqhgt must be a single positive numeric value"
  )
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e "devtools::test(filter = 'run_micro_big_nichemap_validation')"`
Expected: FAIL — current message is `"reqhgt must be positive"`, which does not match the new expected regex, and `NA`/`"2"`/`c(1,2)` do not currently error at all (they fall through to `reqhgt <= 0`, which for `NA` and `"2"` produces a different error, and for `c(1,2)` silently proceeds using only the first element with a `length(x) > 1` warning from `if()` or worse). Confirms the hardening is needed.

- [ ] **Step 3: Implement the fix**

In `R/Microclimf_Modeling.R`, replace line 1955:

```r
  if (reqhgt <= 0) stop("reqhgt must be positive")
```

with:

```r
  if (!is.numeric(reqhgt) || length(reqhgt) != 1 || is.na(reqhgt) || reqhgt <= 0)
    stop("reqhgt must be a single positive numeric value")
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e "devtools::test(filter = 'run_micro_big_nichemap_validation')"`
Expected: PASS, all 4 tests green.

- [ ] **Step 5: Commit**

```bash
git add R/Microclimf_Modeling.R tests/testthat/test-run_micro_big_nichemap_validation.R
git commit -m "Harden reqhgt validation in run_micro_big_nichemap"
```

---

### Task 4: Pre-flight grid validation in `run_micro_big_nichemap()`

**Files:**
- Modify: `R/Microclimf_Modeling.R:1972-1976` area (insert pre-flight loop after existing clim/vegp/soilc existence checks)
- Modify: `R/Microclimf_Modeling.R` roxygen `@details` for `run_micro_big_nichemap` (document the new behavior)
- Modify: `tests/testthat/test-run_micro_big_nichemap_validation.R` (add cases from this task)

**Interfaces:**
- Consumes: `.normalize_period()` (Task 2), `.resolve_rds_path()` (existing, `R/Microclimf_Modeling.R:1728`), `.first_spatraster()` and `.check_grid_match()` (Task 1).
- Produces: nothing new (behavior change only). Before any terrain computation, `run_micro_big_nichemap()` now loads each period's resolved `clim`/`vegp`/`soilc` RDS and stops if `clim`'s grid doesn't match `dtm_coarse`, or if `vegp`'s or `soilc`'s grid doesn't match `dtm_fine`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-run_micro_big_nichemap_validation.R`:

```r
.build_grid_test_fixture <- function(clim_res, vegp_res, soilc_res) {
  work_dir <- tempfile("rmbn_test_")
  dir.create(work_dir)

  clim_dir  <- file.path(work_dir, "clim");  dir.create(clim_dir)
  vegp_dir  <- file.path(work_dir, "vegp");  dir.create(vegp_dir)
  soilc_dir <- file.path(work_dir, "soilc"); dir.create(soilc_dir)

  dtm_coarse <- terra::rast(resolution = 0.05, crs = "EPSG:4326",
                             xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5)
  dtm_fine   <- terra::rast(resolution = 0.005, crs = "EPSG:4326",
                             xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5)
  terra::values(dtm_coarse) <- 1
  terra::values(dtm_fine)   <- 1

  period_label <- "20200101_to_20200101"

  mk <- function(res) {
    r <- terra::rast(resolution = res, crs = "EPSG:4326",
                      xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5)
    terra::values(r) <- 1
    r
  }

  readr::write_rds(list(TMP = terra::wrap(mk(clim_res))),
                    file.path(clim_dir, sprintf("Climate_%s.RDS", period_label)))
  readr::write_rds(list(pai = terra::wrap(mk(vegp_res))),
                    file.path(vegp_dir, sprintf("VegPara_%s.RDS", period_label)))
  readr::write_rds(list(Bulk = terra::wrap(mk(soilc_res))),
                    file.path(soilc_dir, sprintf("SoilPara_%s.RDS", period_label)))

  list(work_dir = work_dir, clim_dir = clim_dir, vegp_dir = vegp_dir,
       soilc_dir = soilc_dir, dtm_coarse = dtm_coarse, dtm_fine = dtm_fine)
}

test_that("run_micro_big_nichemap stops when clim resolution does not match dtm_coarse", {
  f <- .build_grid_test_fixture(clim_res = 0.1, vegp_res = 0.005, soilc_res = 0.005)
  on.exit(unlink(f$work_dir, recursive = TRUE), add = TRUE)

  expect_error(
    run_micro_big_nichemap(
      tiles = list(), clim = f$clim_dir,
      dates = as.Date(c("2020-01-01", "2020-01-31")),
      dtm_fine = f$dtm_fine, dtm_coarse = f$dtm_coarse,
      vegp = f$vegp_dir, soilc = f$soilc_dir,
      output_dir = file.path(f$work_dir, "out"), reqhgt = 2
    ),
    "dtm_coarse.*clim.*do not share the same grid"
  )
})

test_that("run_micro_big_nichemap stops when vegp resolution does not match dtm_fine", {
  f <- .build_grid_test_fixture(clim_res = 0.05, vegp_res = 0.02, soilc_res = 0.005)
  on.exit(unlink(f$work_dir, recursive = TRUE), add = TRUE)

  expect_error(
    run_micro_big_nichemap(
      tiles = list(), clim = f$clim_dir,
      dates = as.Date(c("2020-01-01", "2020-01-31")),
      dtm_fine = f$dtm_fine, dtm_coarse = f$dtm_coarse,
      vegp = f$vegp_dir, soilc = f$soilc_dir,
      output_dir = file.path(f$work_dir, "out"), reqhgt = 2
    ),
    "dtm_fine.*vegp.*do not share the same grid"
  )
})

test_that("run_micro_big_nichemap stops when soilc resolution does not match dtm_fine", {
  f <- .build_grid_test_fixture(clim_res = 0.05, vegp_res = 0.005, soilc_res = 0.02)
  on.exit(unlink(f$work_dir, recursive = TRUE), add = TRUE)

  expect_error(
    run_micro_big_nichemap(
      tiles = list(), clim = f$clim_dir,
      dates = as.Date(c("2020-01-01", "2020-01-31")),
      dtm_fine = f$dtm_fine, dtm_coarse = f$dtm_coarse,
      vegp = f$vegp_dir, soilc = f$soilc_dir,
      output_dir = file.path(f$work_dir, "out"), reqhgt = 2
    ),
    "dtm_fine.*soilc.*do not share the same grid"
  )
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e "devtools::test(filter = 'run_micro_big_nichemap_validation')"`
Expected: FAIL — no pre-flight check exists yet, so the function proceeds past validation and fails later (or differently) instead of raising the expected grid-mismatch message.

- [ ] **Step 3: Implement the pre-flight validation loop**

In `R/Microclimf_Modeling.R`, inside `run_micro_big_nichemap()`, find the existing block (currently lines 1972-1976):

```r
  # --- validate clim / vegp / soilc inputs ------------------------------------
  for (.inp in list(list(clim, "clim"), list(vegp, "vegp"), list(soilc, "soilc"))) {
    if (!file.exists(.inp[[1]]) && !dir.exists(.inp[[1]]))
      stop(sprintf("'%s' does not exist as a file or directory:\n  %s", .inp[[2]], .inp[[1]]))
  }
```

Immediately after it (and before the `# --- heights ---` section), insert:

```r

  # --- validate clim / vegp / soilc grids against dtm_coarse / dtm_fine ------
  cat("--- run_micro_big_nichemap: validating input grids ---\n")
  for (.i in seq_len(nrow(date_ranges))) {
    .p <- .normalize_period(date_ranges$Start_Dates[.i], date_ranges$End_Dates[.i])

    .clim_path  <- .resolve_rds_path(clim,  "Climate",  .p$period_label, study_area)
    .vegp_path  <- .resolve_rds_path(vegp,  "VegPara",  .p$period_label, study_area)
    .soilc_path <- .resolve_rds_path(soilc, "SoilPara", .p$period_label, study_area)

    .clim_r <- .first_spatraster(readr::read_rds(.clim_path))
    .check_grid_match(dtm_coarse, .clim_r, "dtm_coarse",
                       sprintf("clim (%s)", .p$period_label), action = "stop")
    rm(.clim_r)

    .vegp_r <- .first_spatraster(readr::read_rds(.vegp_path))
    .check_grid_match(dtm_fine, .vegp_r, "dtm_fine",
                       sprintf("vegp (%s)", .p$period_label), action = "stop")
    rm(.vegp_r)

    .soilc_r <- .first_spatraster(readr::read_rds(.soilc_path))
    .check_grid_match(dtm_fine, .soilc_r, "dtm_fine",
                       sprintf("soilc (%s)", .p$period_label), action = "stop")
    rm(.soilc_r)

    invisible(gc())
  }
  cat("  All input grids match.\n")
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e "devtools::test(filter = 'run_micro_big_nichemap_validation')"`
Expected: PASS, all 7 tests in the file green (4 from Task 3, 3 new ones).

- [ ] **Step 5: Update roxygen `@details` for `run_micro_big_nichemap`**

In `R/Microclimf_Modeling.R`, in the roxygen block above `run_micro_big_nichemap` (currently around line 1892, in the `@details` section after the "Terrain pre-computation" paragraph), add a new paragraph:

```r
#' **Input grid validation.**  Before terrain features are computed, every
#' period's resolved \code{clim}, \code{vegp}, and \code{soilc} files are
#' checked against \code{dtm_coarse} (\code{clim}) and \code{dtm_fine}
#' (\code{vegp}, \code{soilc}) for matching resolution and CRS. A mismatch
#' stops the function immediately, before any terrain or model computation.
```

- [ ] **Step 6: Regenerate documentation**

Run: `Rscript -e "devtools::document()"`
Expected: Completes without error; `man/run_micro_big_nichemap.Rd` is updated.

- [ ] **Step 7: Run full test suite**

Run: `Rscript -e "devtools::test()"`
Expected: PASS, no regressions.

- [ ] **Step 8: Commit**

```bash
git add R/Microclimf_Modeling.R man/run_micro_big_nichemap.Rd tests/testthat/test-run_micro_big_nichemap_validation.R
git commit -m "Add pre-flight clim/vegp/soilc grid validation to run_micro_big_nichemap"
```

---

### Task 5: `create_tiles()` grid-match warnings

**Files:**
- Modify: `R/Microclimf_Modeling.R:106-107` area (insert warnings right after `ratio_r`/`ratio_c` are computed)
- Modify: roxygen `@details` for `create_tiles`
- Test: `tests/testthat/test-create_tiles_grid_warnings.R`

**Interfaces:**
- Consumes: nothing new (uses `terra::same.crs()` directly, not `.check_grid_match()` — the ratio check is not a resolution-equality check, so the shared helper doesn't fit here).
- Produces: nothing new (behavior change only). `create_tiles()` now warns (does not stop) if `coarse_dem`/`fine_dem` have different CRS, and warns if the fine/coarse resolution ratio isn't a whole number.

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-create_tiles_grid_warnings.R`:

```r
test_that("create_tiles warns when coarse_dem and fine_dem have different CRS", {
  coarse <- terra::rast(nrows = 4, ncols = 4, crs = "EPSG:4326",
                         xmin = 0, xmax = 4, ymin = 0, ymax = 4)
  fine   <- terra::rast(nrows = 40, ncols = 40, crs = "EPSG:3857",
                         xmin = 0, xmax = 4, ymin = 0, ymax = 4)
  terra::values(coarse) <- 1
  terra::values(fine) <- 1

  expect_warning(
    create_tiles(coarse, fine, dates = as.Date(c("2020-01-01", "2020-01-02"))),
    "different CRS"
  )
})

test_that("create_tiles warns when fine/coarse resolution ratio is not a whole number", {
  coarse <- terra::rast(nrows = 4, ncols = 4, crs = "EPSG:4326",
                         xmin = 0, xmax = 4, ymin = 0, ymax = 4)
  fine   <- terra::rast(nrows = 15, ncols = 15, crs = "EPSG:4326",
                         xmin = 0, xmax = 4, ymin = 0, ymax = 4)
  terra::values(coarse) <- 1
  terra::values(fine) <- 1

  expect_warning(
    create_tiles(coarse, fine, dates = as.Date(c("2020-01-01", "2020-01-02"))),
    "not a whole-number multiple"
  )
})

test_that("create_tiles does not warn when CRS matches and ratio is a whole number", {
  coarse <- terra::rast(nrows = 4, ncols = 4, crs = "EPSG:4326",
                         xmin = 0, xmax = 4, ymin = 0, ymax = 4)
  fine   <- terra::rast(nrows = 40, ncols = 40, crs = "EPSG:4326",
                         xmin = 0, xmax = 4, ymin = 0, ymax = 4)
  terra::values(coarse) <- 1
  terra::values(fine) <- 1

  expect_warning(
    create_tiles(coarse, fine, dates = as.Date(c("2020-01-01", "2020-01-02"))),
    regexp = NA
  )
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e "devtools::test(filter = 'create_tiles_grid_warnings')"`
Expected: first two tests FAIL (no warning raised today); third test PASSES already (confirms baseline behavior is currently silent on this front).

- [ ] **Step 3: Implement the warnings**

In `R/Microclimf_Modeling.R`, find (currently lines 106-107):

```r
  ratio_r <- nrow(fine_dem) / nrow(coarse_dem)
  ratio_c <- ncol(fine_dem) / ncol(coarse_dem)
```

Replace with:

```r
  ratio_r <- nrow(fine_dem) / nrow(coarse_dem)
  ratio_c <- ncol(fine_dem) / ncol(coarse_dem)

  if (!terra::same.crs(coarse_dem, fine_dem)) {
    warning("coarse_dem and fine_dem have different CRS; tile extents may not align correctly.")
  }
  if (abs(ratio_r - round(ratio_r)) > 1e-6 || abs(ratio_c - round(ratio_c)) > 1e-6) {
    warning(sprintf(
      "fine_dem resolution is not a whole-number multiple of coarse_dem resolution (row ratio = %.4f, col ratio = %.4f); tile boundaries may not align to whole fine-DEM cells.",
      ratio_r, ratio_c))
  }
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e "devtools::test(filter = 'create_tiles_grid_warnings')"`
Expected: PASS, all 3 tests green.

- [ ] **Step 5: Update roxygen `@details` for `create_tiles`**

In `R/Microclimf_Modeling.R`, in the `@details` section of the `create_tiles` roxygen block (currently around line 61-67), add a sentence after the memory-estimation formula paragraph:

```r
#' \code{coarse_dem} and \code{fine_dem} are expected to share the same CRS
#' and to have a whole-number fine-to-coarse resolution ratio; a mismatch on
#' either produces a warning (not an error) since \code{create_tiles} only
#' uses the ratio for memory estimation and tile-ID resampling.
```

- [ ] **Step 6: Regenerate documentation**

Run: `Rscript -e "devtools::document()"`
Expected: Completes without error; `man/create_tiles.Rd` is updated.

- [ ] **Step 7: Run full test suite**

Run: `Rscript -e "devtools::test()"`
Expected: PASS, no regressions.

- [ ] **Step 8: Commit**

```bash
git add R/Microclimf_Modeling.R man/create_tiles.Rd tests/testthat/test-create_tiles_grid_warnings.R
git commit -m "Warn on CRS/resolution-ratio mismatch between coarse_dem and fine_dem in create_tiles"
```

---

### Task 6: `package_climate()` grid-match warning

**Files:**
- Modify: `R/Microclimf_DataPrep.R:3341-3345` area
- Modify: roxygen `@details` for `package_climate`

**Interfaces:**
- Consumes: `.check_grid_match()` (Task 1). `package_climate()` lives in `R/Microclimf_DataPrep.R`; internal helpers defined in `R/Microclimf_Modeling.R` are visible package-wide (same namespace), so no import/reference changes are needed.
- Produces: nothing new (behavior change only). `package_climate()` now warns once (on the first month of the first period) if the packaged climate raster's resolution/CRS doesn't match `template`'s.

**No dedicated automated test for this task** — per the approved spec
(`docs/superpowers/specs/2026-07-17-input-grid-validation-design.md`, Testing
Plan section), exercising this code path requires a full AORC-style
directory/file fixture (`aorc_dir/study_area/year/month/*.nc` with real
DSWRF/DLWRF/APCP/TMP naming) that is out of scope here. Verification is via
`devtools::document()` + full test suite (no regressions) plus a manual
reasoning check in Step 3 below.

- [ ] **Step 1: Locate the insertion point**

In `R/Microclimf_DataPrep.R`, confirm the exact block (currently lines 3341-3345) inside `package_climate()`'s `for (i in ...) { for (j in ...) { ... } }` loops:

```r
      # --- Shortwave radiation ---
      sw_f <- clim_files[grepl("DSWRF", names(clim_files))]
      if (length(sw_f) == 0) stop(sprintf("No DSWRF files found for year %d month %02d", y, m))
      r_sw <- load_var(sw_f, template)
      sw_t <- terra::time(r_sw)
      r_sw <- terra::wrap(r_sw)
```

- [ ] **Step 2: Implement the warning**

Replace with:

```r
      # --- Shortwave radiation ---
      sw_f <- clim_files[grepl("DSWRF", names(clim_files))]
      if (length(sw_f) == 0) stop(sprintf("No DSWRF files found for year %d month %02d", y, m))
      r_sw <- load_var(sw_f, template)

      if (i == 1L && j == 1L) {
        .check_grid_match(template, r_sw, "template", "packaged climate", action = "warn")
      }

      sw_t <- terra::time(r_sw)
      r_sw <- terra::wrap(r_sw)
```

- [ ] **Step 3: Manually reason through the warning condition**

`load_var()` calls `terra::project(r, terra::crs(template), method = "near", threads = TRUE)` then `terra::crop(r, terra::ext(template))` — this sets CRS and extent to match `template` but does **not** resample to `template`'s resolution, so `r_sw`'s resolution is whatever `terra::project()`'s default output resolution computation produced from the source AORC grid. Confirm by inspection that `r_sw`'s CRS will therefore always match `template` (making the CRS half of `.check_grid_match()` a no-op safety net here), while the resolution half is the one expected to actually catch real mismatches.

- [ ] **Step 4: Update roxygen `@details` for `package_climate`**

Find the roxygen block above `package_climate` (currently starts around line 3200s; locate via `grep -n "^package_climate" R/Microclimf_DataPrep.R` then search upward for the preceding `#'` block) and add, near the existing `template` parameter description:

```r
#' @details The packaged climate raster is reprojected to \code{template}'s
#'   CRS and cropped to its extent, but is not resampled to \code{template}'s
#'   resolution. A resolution mismatch against \code{template} triggers a
#'   one-time warning (checked on the first month of the first period) rather
#'   than an error, since \code{package_climate} does not know what
#'   downstream DEM the output will ultimately be cropped against.
```

- [ ] **Step 5: Regenerate documentation**

Run: `Rscript -e "devtools::document()"`
Expected: Completes without error; `man/package_climate.Rd` is updated.

- [ ] **Step 6: Run full test suite**

Run: `Rscript -e "devtools::test()"`
Expected: PASS, no regressions (this task adds no new tests, so the pass count is unchanged from Task 5).

- [ ] **Step 7: Commit**

```bash
git add R/Microclimf_DataPrep.R man/package_climate.Rd
git commit -m "Warn on template resolution mismatch in package_climate"
```

---

### Task 7: `package_veg_soil()` optional `dtm_fine` grid-match warning

**Files:**
- Modify: `R/Microclimf_DataPrep.R:2653-2664` (function signature — add `dtm_fine = NULL` parameter)
- Modify: `R/Microclimf_DataPrep.R:2845` area (insert warning after geometry alignment)
- Modify: roxygen block for `package_veg_soil` (add `@param dtm_fine`)

**Interfaces:**
- Consumes: `.check_grid_match()` (Task 1).
- Produces: `package_veg_soil()` gains a new **optional** parameter `dtm_fine = NULL`, appended after `water = 512` to preserve positional-argument compatibility for existing callers. When supplied, warns if the packaged veg/soil common grid (`lai_rs`) doesn't match `dtm_fine`'s resolution/CRS. Default `NULL` means no change in behavior for existing callers.

**No dedicated automated test for this task** — per the approved spec's
Testing Plan section, exercising this code path requires real
landcover/veg-height/LAI/soil/reflectance file fixtures matching this
function's glob/year-matching logic, which is out of scope here.
Verification is via `devtools::document()` + full test suite (no
regressions).

- [ ] **Step 1: Add the `dtm_fine` parameter**

In `R/Microclimf_DataPrep.R`, find the function signature (currently lines 2653-2664):

```r
package_veg_soil <- function(dates,
                             snow_free_months,
                             landcover,
                             veg_height,
                             soil_path,
                             lai_dir,
                             refl_dir,
                             vegpara_dir,
                             soilpara_dir,
                             study_area = NULL,
                             lctype = "CORINE",
                             water = 512) {
```

Replace with:

```r
package_veg_soil <- function(dates,
                             snow_free_months,
                             landcover,
                             veg_height,
                             soil_path,
                             lai_dir,
                             refl_dir,
                             vegpara_dir,
                             soilpara_dir,
                             study_area = NULL,
                             lctype = "CORINE",
                             water = 512,
                             dtm_fine = NULL) {
```

- [ ] **Step 2: Insert the warning after geometry alignment**

Find the block (currently lines 2834-2845):

```r
      } else {

        cat("  Geometries match: no resampling needed.\n")
        lai_rs      <- lai
        vght_rs     <- vght
        SD_rs       <- SD
        lc_rs       <- lc
        refldata_rs <- refldata

      }

      gc()
```

Replace with:

```r
      } else {

        cat("  Geometries match: no resampling needed.\n")
        lai_rs      <- lai
        vght_rs     <- vght
        SD_rs       <- SD
        lc_rs       <- lc
        refldata_rs <- refldata

      }

      if (!is.null(dtm_fine)) {
        .check_grid_match(dtm_fine, lai_rs, "dtm_fine",
                          sprintf("packaged veg/soil (%s)", period_label), action = "warn")
      }

      gc()
```

- [ ] **Step 3: Add `@param dtm_fine` to the roxygen block**

Find the roxygen block above `package_veg_soil` (locate via `grep -n "^package_veg_soil" R/Microclimf_DataPrep.R` then search upward for the preceding `#'` block; the existing `@param water` line is the anchor) and add immediately after the `@param water` line:

```r
#' @param dtm_fine Optional \code{SpatRaster}. Fine-resolution DEM (e.g. the
#'   \code{dtm_fine} passed to \code{\link{run_micro_big_nichemap}}) used only
#'   for validation: if supplied, a warning is issued when the packaged
#'   vegetation/soil grid's resolution or CRS does not match it. Default
#'   \code{NULL} skips this check.
```

- [ ] **Step 4: Regenerate documentation**

Run: `Rscript -e "devtools::document()"`
Expected: Completes without error; `man/package_veg_soil.Rd` is updated with the new `@param`.

- [ ] **Step 5: Run full test suite**

Run: `Rscript -e "devtools::test()"`
Expected: PASS, no regressions.

- [ ] **Step 6: Commit**

```bash
git add R/Microclimf_DataPrep.R man/package_veg_soil.Rd
git commit -m "Add optional dtm_fine grid-match warning to package_veg_soil"
```

---

### Task 8: Final package verification

**Files:** none modified — verification only.

**Interfaces:** none.

- [ ] **Step 1: Full document + install cycle**

Run: `Rscript -e "devtools::document(); devtools::install(quick = TRUE, upgrade = 'never')"`
Expected: Completes without error.

- [ ] **Step 2: Run the complete test suite one more time**

Run: `Rscript -e "devtools::test()"`
Expected: PASS — every test added across Tasks 1-5 (`.check_grid_match`, `.first_spatraster`, `.normalize_period`, `run_micro_big_nichemap` reqhgt + grid validation, `create_tiles` warnings) plus all pre-existing tests in `tests/testthat/` are green.

- [ ] **Step 3: `devtools::check()` for CRAN-style consistency (roxygen/NAMESPACE drift, etc.)**

Run: `Rscript -e "devtools::check(document = FALSE)"`
Expected: No new `ERROR`/`WARNING` introduced relative to a baseline run on `master` before this branch's changes (pre-existing NOTEs about GitHub-only `Remotes` dependencies are expected and unrelated to this work).

- [ ] **Step 4: Confirm the working tree is clean and all task commits are present**

Run: `git log --oneline -8`
Expected: 7 implementation commits (one each for Tasks 1-7, Task 8 has no commit of its own) shown above the earlier `"Add design spec for input grid validation checks"` commit.
