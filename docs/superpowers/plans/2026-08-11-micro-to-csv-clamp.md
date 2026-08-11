# micro_to_csv() value clamping Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add opt-in physical-range clamping to `micro_to_csv()`'s output, with a package default bounds table the caller can retrieve, edit, and patch.

**Architecture:** Three new internal helpers (`.mtc_clamp_defaults()`, `.mtc_resolve_clamp_bounds()`, `.mtc_apply_clamp()`) plus one new exported getter (`micro_to_csv_clamp_defaults()`) added to the existing `R/Endotherm_DataPrep.R` "micro_to_csv()" helper family. `micro_to_csv()` gains two new trailing arguments, `clamp = FALSE` and `clamp_bounds = NULL`, wired in at two points: bounds resolution/validation happens early (fail fast, before any file I/O), clamping itself happens right after `metout`/`soil` are built and before the `shadmet`/`shadsoil` duplicates are made (so shade frames inherit already-clamped values).

**Tech Stack:** R (base + `data.frame`), `testthat` (existing test file `tests/testthat/test-micro_to_csv.R` and its `helper-micro_to_csv.R` fixtures), `roxygen2`/`devtools::document()`.

## Global Constraints

- Follow `CLAUDE.md` conventions: internal helpers prefixed `.`, no `@export`/roxygen block on them; exported functions get full roxygen2 (`@param`, `@return`, `@details`, `@export`, `@seealso`).
- `sprintf()` for any string built from numeric values (none needed here — no new user-facing formatted strings).
- `clamp = FALSE` by default: existing `micro_to_csv()` callers see byte-identical output with no code changes.
- Default bounds table is exactly the 21 rows and values from the approved spec (`docs/superpowers/specs/2026-08-11-micro-to-csv-clamp-design.md`): reproduced verbatim in Task 1 below.
- Run `devtools::document()` after roxygen changes (Task 4) — do not hand-edit `NAMESPACE`/`man/*.Rd`.
- Always create commits with `git add <specific files>` (never `-A`/`.`), following this repo's existing history style (`type: short description`, no period, no PR/issue references).

---

### Task 1: Default clamp bounds table + `micro_to_csv_clamp_defaults()` getter

**Files:**
- Modify: `R/Endotherm_DataPrep.R` — insert a new section after `.mtc_build_soil()` (currently ending at line 1374) and before the `#  micro_to_csv(): top-level export` comment block (currently line 1376). Use `grep -n "micro_to_csv(): top-level export" R/Endotherm_DataPrep.R` to find the current line if the file has shifted.
- Test: `tests/testthat/test-micro_to_csv.R` — append new `test_that()` blocks at the end of the file (after the last existing test, "micro_to_csv() stops on more than 52 unique requested days").

**Interfaces:**
- Produces: `.mtc_clamp_defaults()` — zero-arg internal helper, returns `data.frame(variable, lower, upper)`, 21 rows, `lower`/`upper` numeric with `NA_real_` for unbounded sides. `micro_to_csv_clamp_defaults()` — zero-arg exported wrapper around it, identical return shape.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-micro_to_csv.R`:

```r
test_that("micro_to_csv_clamp_defaults() returns the expected 21-row bounds table", {
  bounds <- micro_to_csv_clamp_defaults()

  expect_s3_class(bounds, "data.frame")
  expect_equal(colnames(bounds), c("variable", "lower", "upper"))
  expect_equal(nrow(bounds), 21L)
  expect_equal(anyDuplicated(bounds$variable), 0L)

  expected_vars <- c("TALOC", "TAREF", "TANNUL", "RHLOC", "RH", "VLOC", "VREF",
                     "ZEN", "SOLR", "TSKYC", "ELEV",
                     "D0cm", "D2.5cm", "D5cm", "D10cm", "D15cm", "D20cm", "D30cm",
                     "D50cm", "D100cm", "D200cm")
  expect_setequal(bounds$variable, expected_vars)

  row <- function(v) bounds[bounds$variable == v, ]
  expect_equal(row("TALOC")$lower, -90); expect_equal(row("TALOC")$upper, 60)
  expect_equal(row("RH")$lower, 0);      expect_equal(row("RH")$upper, 100)
  expect_equal(row("VLOC")$lower, 0);    expect_true(is.na(row("VLOC")$upper))
  expect_equal(row("ZEN")$lower, 0);     expect_equal(row("ZEN")$upper, 90)
  expect_equal(row("SOLR")$lower, 0);    expect_true(is.na(row("SOLR")$upper))
  expect_equal(row("TSKYC")$lower, -100); expect_equal(row("TSKYC")$upper, 60)
  expect_equal(row("ELEV")$lower, -500); expect_equal(row("ELEV")$upper, 9000)
  expect_equal(row("D0cm")$lower, -90);  expect_equal(row("D0cm")$upper, 70)
  expect_equal(row("D200cm")$lower, -90); expect_equal(row("D200cm")$upper, 70)
})

test_that("micro_to_csv_clamp_defaults() returns a fresh, independently-editable copy each call", {
  b1 <- micro_to_csv_clamp_defaults()
  b1[b1$variable == "RH", "upper"] <- 50
  b2 <- micro_to_csv_clamp_defaults()
  expect_equal(b2[b2$variable == "RH", "upper"], 100)
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-micro_to_csv.R')"`
Expected: FAIL — `could not find function "micro_to_csv_clamp_defaults"`.

- [ ] **Step 3: Implement `.mtc_clamp_defaults()` and `micro_to_csv_clamp_defaults()`**

Insert into `R/Endotherm_DataPrep.R` between `.mtc_build_soil()` and the `#  micro_to_csv(): top-level export` section:

```r
# --------------------------------------------------------------------------- #
#  micro_to_csv(): value clamping
# --------------------------------------------------------------------------- #

.mtc_clamp_defaults <- function() {
  data.frame(
    variable = c("TALOC", "TAREF", "TANNUL", "RHLOC", "RH", "VLOC", "VREF",
                "ZEN", "SOLR", "TSKYC", "ELEV",
                "D0cm", "D2.5cm", "D5cm", "D10cm", "D15cm", "D20cm", "D30cm",
                "D50cm", "D100cm", "D200cm"),
    lower = c(-90, -90, -90, 0, 0, 0, 0,
             0, 0, -100, -500,
             rep(-90, 10)),
    upper = c(60, 60, 60, 100, 100, NA_real_, NA_real_,
             90, NA_real_, 60, 9000,
             rep(70, 10)),
    stringsAsFactors = FALSE
  )
}

#' Default Clamp Bounds for micro_to_csv()
#'
#' Returns the package's default physically-valid-range table used by
#' \code{\link{micro_to_csv}} to clamp its output columns when
#' \code{clamp = TRUE}.
#'
#' @return A \code{data.frame} with columns \code{variable} (character),
#'   \code{lower}, \code{upper} (numeric, \code{NA} meaning unbounded on
#'   that side). One row per clampable \code{metout}/\code{soil} column
#'   (21 rows). A fresh, independent copy is returned on every call.
#'
#' @details
#' Bounds are set at Earth's physical extremes, not typical values, so
#' clamping only catches genuinely invalid data (e.g. numerical noise
#' pushing \code{SOLR} slightly negative or \code{RH} fractionally over
#' 100) rather than trimming plausible weather. The 10 soil depth rows
#' (\code{D0cm} ... \code{D200cm}) match \code{\link{micro_to_csv}}'s
#' fixed, hard-required NicheMapR depth set (0, 2.5, 5, 10, 15, 20, 30, 50,
#' 100, 200 cm). \code{DOY}/\code{TIME} are index columns and are never
#' clamped, so they have no row here.
#'
#' @seealso \code{\link{micro_to_csv}}
#' @export
micro_to_csv_clamp_defaults <- function() {
  .mtc_clamp_defaults()
}
```

- [ ] **Step 4: Run `devtools::document()` to export the new function**

Run: `Rscript -e "devtools::document()"`
Expected: succeeds; `NAMESPACE` gains `export(micro_to_csv_clamp_defaults)` and `man/micro_to_csv_clamp_defaults.Rd` is created. Confirm with `grep -n "micro_to_csv_clamp_defaults" NAMESPACE`.

- [ ] **Step 5: Run tests to verify they pass**

Run: `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-micro_to_csv.R')"`
Expected: PASS, including the two new tests from Step 1.

- [ ] **Step 6: Commit**

```bash
git add R/Endotherm_DataPrep.R NAMESPACE man/micro_to_csv_clamp_defaults.Rd tests/testthat/test-micro_to_csv.R
git commit -m "feat: add micro_to_csv_clamp_defaults() bounds table getter"
```

---

### Task 2: `.mtc_apply_clamp()` — apply a bounds table to a data.frame

**Files:**
- Modify: `R/Endotherm_DataPrep.R` — add directly below `micro_to_csv_clamp_defaults()` from Task 1, still within the "micro_to_csv(): value clamping" section.
- Test: `tests/testthat/test-micro_to_csv.R` — append after Task 1's tests.

**Interfaces:**
- Consumes: nothing from Task 1 directly (takes any `bounds` data.frame with `variable`/`lower`/`upper` columns, e.g. the output of `.mtc_clamp_defaults()`/`micro_to_csv_clamp_defaults()`).
- Produces: `.mtc_apply_clamp(df, bounds)` — internal helper, returns `df` with each column present in `bounds$variable` clamped via `pmax`/`pmin`, `NA` bound sides skipped, columns not in `bounds` untouched, non-matching row order/other columns preserved. Used by Task 4.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-micro_to_csv.R`:

```r
test_that(".mtc_apply_clamp clamps only matching columns, respecting NA (unbounded) sides", {
  df <- data.frame(SOLR = c(-5, 10, 2000), RH = c(-1, 50, 150),
                   VLOC = c(-3, 5, 5000), TIME = c(0, 60, 120))
  bounds <- data.frame(variable = c("SOLR", "RH", "VLOC"),
                       lower = c(0, 0, 0), upper = c(NA_real_, 100, NA_real_))

  out <- .mtc_apply_clamp(df, bounds)

  expect_equal(out$SOLR, c(0, 10, 2000))   # lower clamped, no upper bound
  expect_equal(out$RH,   c(0, 50, 100))    # both bounds applied
  expect_equal(out$VLOC, c(0, 5, 5000))    # lower clamped, no upper bound
  expect_equal(out$TIME, df$TIME)          # untouched: not in bounds table
})

test_that(".mtc_apply_clamp leaves a data.frame with no matching columns unchanged", {
  df <- data.frame(TIME = c(0, 60), DOY = c(184L, 184L))
  bounds <- micro_to_csv_clamp_defaults()
  out <- .mtc_apply_clamp(df, bounds)
  expect_equal(out, df)
})

test_that(".mtc_apply_clamp clamps a soil-style D#cm column against the default table", {
  df <- data.frame(TIME = c(0, 60), D0cm = c(-95, 80))
  out <- .mtc_apply_clamp(df, micro_to_csv_clamp_defaults())
  expect_equal(out$D0cm, c(-90, 70))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-micro_to_csv.R')"`
Expected: FAIL — `could not find function ".mtc_apply_clamp"`.

- [ ] **Step 3: Implement `.mtc_apply_clamp()`**

```r
.mtc_apply_clamp <- function(df, bounds) {
  match_cols <- intersect(names(df), bounds$variable)
  for (col in match_cols) {
    b <- bounds[bounds$variable == col, ]
    x <- df[[col]]
    if (!is.na(b$lower)) x <- pmax(x, b$lower)
    if (!is.na(b$upper)) x <- pmin(x, b$upper)
    df[[col]] <- x
  }
  df
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-micro_to_csv.R')"`
Expected: PASS, all tests from Task 1 and Task 2.

- [ ] **Step 5: Commit**

```bash
git add R/Endotherm_DataPrep.R tests/testthat/test-micro_to_csv.R
git commit -m "feat: add .mtc_apply_clamp() internal helper"
```

---

### Task 3: `.mtc_resolve_clamp_bounds()` — merge caller patch onto defaults, validate

**Files:**
- Modify: `R/Endotherm_DataPrep.R` — add directly below `.mtc_apply_clamp()` from Task 2, still within the "micro_to_csv(): value clamping" section.
- Test: `tests/testthat/test-micro_to_csv.R` — append after Task 2's tests.

**Interfaces:**
- Consumes: `.mtc_clamp_defaults()` (Task 1).
- Produces: `.mtc_resolve_clamp_bounds(clamp_bounds)` — internal helper. `clamp_bounds = NULL` returns the unmodified defaults table. A `data.frame(variable, lower, upper)` patch overrides matching `variable` rows and adds new ones, keeping all unmentioned defaults. `stop()`s if `clamp_bounds` isn't a data.frame with exactly columns `variable`/`lower`/`upper`, or if any row (in the merged result) has non-NA `lower > upper`. Used by Task 4.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-micro_to_csv.R`:

```r
test_that(".mtc_resolve_clamp_bounds returns the unmodified defaults when clamp_bounds is NULL", {
  out <- .mtc_resolve_clamp_bounds(NULL)
  expect_equal(out, micro_to_csv_clamp_defaults())
})

test_that(".mtc_resolve_clamp_bounds patches one variable and keeps the rest at default", {
  patch <- data.frame(variable = "VLOC", lower = 0, upper = 40)
  out <- .mtc_resolve_clamp_bounds(patch)

  expect_equal(nrow(out), 21L)  # same row count: overriding, not adding
  expect_equal(out[out$variable == "VLOC", "upper"], 40)
  expect_equal(out[out$variable == "RH", ], micro_to_csv_clamp_defaults()[
    micro_to_csv_clamp_defaults()$variable == "RH", ])
})

test_that(".mtc_resolve_clamp_bounds adds a variable not present in the defaults", {
  patch <- data.frame(variable = "CUSTOMVAR", lower = 0, upper = 10)
  out <- .mtc_resolve_clamp_bounds(patch)
  expect_equal(nrow(out), 22L)
  expect_equal(out[out$variable == "CUSTOMVAR", "lower"], 0)
  expect_equal(out[out$variable == "CUSTOMVAR", "upper"], 10)
})

test_that(".mtc_resolve_clamp_bounds stops when clamp_bounds is missing a required column", {
  expect_error(.mtc_resolve_clamp_bounds(data.frame(variable = "RH", lower = 0)),
              "variable.*lower.*upper|data.frame")
})

test_that(".mtc_resolve_clamp_bounds stops when clamp_bounds is not a data.frame", {
  expect_error(.mtc_resolve_clamp_bounds(list(variable = "RH", lower = 0, upper = 100)),
              "data.frame")
})

test_that(".mtc_resolve_clamp_bounds stops when a merged row has lower > upper", {
  expect_error(.mtc_resolve_clamp_bounds(data.frame(variable = "RH", lower = 100, upper = 0)),
              "lower > upper|RH")
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-micro_to_csv.R')"`
Expected: FAIL — `could not find function ".mtc_resolve_clamp_bounds"`.

- [ ] **Step 3: Implement `.mtc_resolve_clamp_bounds()`**

```r
.mtc_resolve_clamp_bounds <- function(clamp_bounds) {
  defaults <- .mtc_clamp_defaults()

  if (is.null(clamp_bounds)) {
    bounds <- defaults
  } else {
    if (!is.data.frame(clamp_bounds) ||
        !identical(sort(names(clamp_bounds)), sort(c("variable", "lower", "upper"))))
      stop("'clamp_bounds' must be a data.frame with columns 'variable', 'lower', 'upper'")

    bounds <- defaults[!defaults$variable %in% clamp_bounds$variable, ]
    bounds <- rbind(bounds, clamp_bounds[, c("variable", "lower", "upper")])
    rownames(bounds) <- NULL
  }

  bad <- !is.na(bounds$lower) & !is.na(bounds$upper) & bounds$lower > bounds$upper
  if (any(bad))
    stop(sprintf("'clamp_bounds' has lower > upper for variable(s): %s",
                paste(bounds$variable[bad], collapse = ", ")))

  bounds
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-micro_to_csv.R')"`
Expected: PASS, all tests from Task 1, 2, and 3.

- [ ] **Step 5: Commit**

```bash
git add R/Endotherm_DataPrep.R tests/testthat/test-micro_to_csv.R
git commit -m "feat: add .mtc_resolve_clamp_bounds() merge/validate helper"
```

---

### Task 4: Wire `clamp`/`clamp_bounds` into `micro_to_csv()`, update roxygen, end-to-end tests

**Files:**
- Modify: `R/Endotherm_DataPrep.R` — the `micro_to_csv()` function itself (currently `R/Endotherm_DataPrep.R:1450-1492`; re-check with `grep -n "^micro_to_csv <- function" R/Endotherm_DataPrep.R` since line numbers shift after Tasks 1-3 insert code above it) and its roxygen block immediately above it.
- Test: `tests/testthat/test-micro_to_csv.R` — append after Task 3's tests.

**Interfaces:**
- Consumes: `.mtc_resolve_clamp_bounds()` (Task 3), `.mtc_apply_clamp()` (Task 2).
- Produces: `micro_to_csv(abvgrd_input, blwgrd_input, cell, cell_input_type, dates, elev, tannul = NULL, tz = "America/Denver", clamp = FALSE, clamp_bounds = NULL)` — same return shape as before (`list(metout, shadmet, soil, shadsoil)`), now clamped when `clamp = TRUE`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-micro_to_csv.R`:

```r
test_that("micro_to_csv() with clamp = FALSE (default) is unchanged from prior behavior", {
  skip_if_not_installed("ncdf4")
  fix <- .mtc_write_fixture_pair("nc", ntime_ = 48,
                                 start = as.POSIXct("2020-07-01 00:00:00", tz = "UTC"))
  args <- list(abvgrd_input = fix$abv_path, blwgrd_input = fix$blw_path,
              cell = c(1, 1), cell_input_type = "index",
              dates = as.Date("2020-07-01"), elev = 1500, tannul = 4, tz = "America/Denver")

  out_default   <- do.call(micro_to_csv, args)
  out_explicit  <- do.call(micro_to_csv, c(args, list(clamp = FALSE)))
  expect_equal(out_default, out_explicit)
})

test_that("micro_to_csv() with clamp = TRUE clamps an out-of-range metout column", {
  skip_if_not_installed("ncdf4")
  # ntime_ = 48 (not 24): dates = as.Date("2020-07-01") with the default
  # America/Denver tz needs UTC 06:00 07-01 through 06:00 07-02 for full
  # local-day coverage -- see .mtc_write_fixture_pair()'s use of ntime_ = 48
  # in the existing lonlat/cellnumber test above for the same reason.
  fx <- .mtc_test_fixture_data(nrow_ = 1, ncol_ = 1, ntime_ = 48)
  fx$mout$relhum[1, 1, ] <- 150       # force RH out of [0, 100]
  fx$mout$Rdirdown[1, 1, ] <- -50     # force SOLR negative
  fx$mout$Rdifdown[1, 1, ] <- 0

  abv_path <- tempfile(fileext = ".nc"); blw_path <- tempfile(fileext = ".nc")
  write_tile(fx$mout, abv_path, dtm = fx$dtm, tme = fx$tme, file_fmt = "nc")
  blw_arrs <- .mtc_test_fixture_blw_data(fx, depths_mm = c(0, 500))  # -> D0cm, D50cm
  for (dl in names(blw_arrs)) {
    write_tile(list(Tz = blw_arrs[[dl]], tme = fx$tme), blw_path,
              dtm = fx$dtm, tme = fx$tme, file_fmt = "nc", depth_label = dl)
  }

  out <- micro_to_csv(abv_path, blw_path, cell = c(1, 1), cell_input_type = "index",
                      dates = as.Date("2020-07-01"), elev = 1000, tannul = 5,
                      clamp = TRUE)

  expect_true(all(out$metout$RH <= 100))
  expect_true(all(out$metout$RHLOC <= 100))
  expect_true(all(out$metout$SOLR >= 0))
  expect_equal(out$shadmet, out$metout)  # shade frames still inherit clamped values
})

test_that("micro_to_csv() with clamp = TRUE and a clamp_bounds patch overrides only that variable", {
  skip_if_not_installed("ncdf4")
  fx <- .mtc_test_fixture_data(nrow_ = 1, ncol_ = 1, ntime_ = 48)  # see full-day-coverage note above
  fx$mout$windspeed[1, 1, ] <- 45

  abv_path <- tempfile(fileext = ".nc"); blw_path <- tempfile(fileext = ".nc")
  write_tile(fx$mout, abv_path, dtm = fx$dtm, tme = fx$tme, file_fmt = "nc")
  blw_arrs <- .mtc_test_fixture_blw_data(fx, depths_mm = c(0, 500))
  for (dl in names(blw_arrs)) {
    write_tile(list(Tz = blw_arrs[[dl]], tme = fx$tme), blw_path,
              dtm = fx$dtm, tme = fx$tme, file_fmt = "nc", depth_label = dl)
  }

  patch <- data.frame(variable = "VLOC", lower = 0, upper = 40)
  out <- micro_to_csv(abv_path, blw_path, cell = c(1, 1), cell_input_type = "index",
                      dates = as.Date("2020-07-01"), elev = 1000, tannul = 5,
                      clamp = TRUE, clamp_bounds = patch)

  expect_true(all(out$metout$VLOC <= 40))    # patched variable: clamped
  expect_true(all(out$metout$VREF == 45))    # VREF keeps its own default (unbounded upper),
                                              # even though it's the same raw windspeed series as VLOC
})

test_that("micro_to_csv() stops fast on an invalid clamp_bounds before touching input files", {
  expect_error(
    micro_to_csv("does_not_exist.nc", "does_not_exist.nc", cell = c(1, 1),
                cell_input_type = "index", dates = as.Date("2020-07-01"), elev = 1000,
                clamp = TRUE, clamp_bounds = data.frame(variable = "RH", lower = 100, upper = 0)),
    "lower > upper|RH"
  )
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-micro_to_csv.R')"`
Expected: FAIL — `unused arguments (clamp = ...)` / `unused arguments (clamp = TRUE, clamp_bounds = ...)`.

- [ ] **Step 3: Update the roxygen block and function signature/body**

In `R/Endotherm_DataPrep.R`, add two `@param` entries to the existing roxygen block above `micro_to_csv()` (insert after the existing `@param tz` entry, before `@return`):

```r
#' @param clamp Logical. If \code{TRUE}, clamp every column present in the
#'   resolved clamp-bounds table (see \code{clamp_bounds}) to its
#'   \code{[lower, upper]} range in all 4 returned data frames, using
#'   \code{\link{micro_to_csv_clamp_defaults}} unless overridden. Default
#'   \code{FALSE}: output is unchanged from prior behavior.
#' @param clamp_bounds Optional \code{data.frame} with columns
#'   \code{variable}, \code{lower}, \code{upper} (see
#'   \code{\link{micro_to_csv_clamp_defaults}}). Rows here patch the
#'   package defaults: a listed \code{variable} overrides that default's
#'   bounds (or is added if new), any \code{variable} not listed keeps its
#'   default bounds. Ignored when \code{clamp = FALSE}.
```

Add one line to `@seealso`:

```r
#' @seealso \code{\link{write_tile}}, \code{\link{stitch_tiles}},
#'   \code{\link{micro_to_csv_clamp_defaults}}, and
#'   \code{\link{write_endotherm_inputs}} (writes \code{endo.dat}/
#'   \code{alomvars.dat} -- CSV export of this function's output and running
#'   \code{Endo2022a.exe} are handled by later, separate functions).
```

Change the function signature:

```r
micro_to_csv <- function(abvgrd_input, blwgrd_input, cell, cell_input_type,
                         dates, elev, tannul = NULL, tz = "America/Denver",
                         clamp = FALSE, clamp_bounds = NULL) {
  cell_input_type <- match.arg(cell_input_type, c("index", "lonlat", "cellnumber"))

  bounds <- if (clamp) .mtc_resolve_clamp_bounds(clamp_bounds) else NULL

  abv_handle <- .mtc_open(abvgrd_input)
```

(This inserts the `bounds <-` line immediately after the existing `cell_input_type <- match.arg(...)` line and before the existing `abv_handle <- .mtc_open(abvgrd_input)` line — everything else in the function body up through `soil <- .mtc_build_soil(...)` is unchanged.)

Then change the tail of the function (currently):

```r
  metout <- .mtc_build_metout(abv_day_index, abv_series, zen, elev_val, tannul_val)
  soil   <- .mtc_build_soil(blw_day_index, blw_series)

  list(metout = metout, shadmet = metout, soil = soil, shadsoil = soil)
}
```

to:

```r
  metout <- .mtc_build_metout(abv_day_index, abv_series, zen, elev_val, tannul_val)
  soil   <- .mtc_build_soil(blw_day_index, blw_series)

  if (clamp) {
    metout <- .mtc_apply_clamp(metout, bounds)
    soil   <- .mtc_apply_clamp(soil, bounds)
  }

  list(metout = metout, shadmet = metout, soil = soil, shadsoil = soil)
}
```

- [ ] **Step 4: Run `devtools::document()` to regenerate `man/micro_to_csv.Rd`**

Run: `Rscript -e "devtools::document()"`
Expected: succeeds; `man/micro_to_csv.Rd` picks up the two new `@param` entries and the updated `@seealso`. Confirm with `grep -n "clamp" man/micro_to_csv.Rd`.

- [ ] **Step 5: Run the full test file to verify everything passes**

Run: `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-micro_to_csv.R')"`
Expected: PASS, all tests (pre-existing plus all new ones from Tasks 1-4).

- [ ] **Step 6: Run `devtools::check()` for a full package sanity check**

Run: `Rscript -e "devtools::check(quiet = TRUE)"`
Expected: no new `ERROR`/`WARNING` beyond whatever pre-existing baseline `devtools::check()` already reports on `master` (this repo has GitHub-only `Imports` per `CLAUDE.md`, which routinely triggers a pre-existing NOTE — do not treat that as a regression). If any new `ERROR`/`WARNING` mentions `micro_to_csv`, `clamp`, or `.mtc_`, fix before proceeding.

- [ ] **Step 7: Commit**

```bash
git add R/Endotherm_DataPrep.R man/micro_to_csv.Rd tests/testthat/test-micro_to_csv.R
git commit -m "feat: wire clamp/clamp_bounds arguments into micro_to_csv()"
```

---

## Self-Review Notes

- **Spec coverage:** `micro_to_csv_clamp_defaults()` getter (Task 1) ✓; default bounds table with exact 21 rows/values (Task 1) ✓; `clamp`/`clamp_bounds` arguments (Task 4) ✓; patch-not-replace merge semantics (Task 3) ✓; validation (`data.frame` shape + `lower > upper`) (Task 3) ✓; application point before `shadmet`/`shadsoil` duplication (Task 4) ✓; roxygen documentation (Task 4) ✓; all 6 verification-plan items from the spec are covered by Task 1/2/3/4 tests.
- **Type consistency:** `.mtc_apply_clamp(df, bounds)` (Task 2) and `.mtc_resolve_clamp_bounds(clamp_bounds)` (Task 3) signatures match their call sites in Task 4's `micro_to_csv()` body exactly. `micro_to_csv_clamp_defaults()` (Task 1) and `.mtc_clamp_defaults()` are both zero-arg and referenced consistently across all 4 tasks' tests.
- **No placeholders:** every step has runnable code, exact file locations, and concrete expected outcomes.
