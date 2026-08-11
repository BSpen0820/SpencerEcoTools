# micro_to_csv() value clamping Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add opt-in physical-range clamping to `micro_to_csv()`'s output, with a package default bounds table the caller can retrieve, edit, and patch.

**Architecture:** Three new internal helpers (`.mtc_clamp_defaults()`, `.mtc_resolve_clamp_bounds()`, `.mtc_apply_clamp()`) plus one new exported getter (`micro_to_csv_clamp_defaults()`) added to the existing `R/Endotherm_DataPrep.R` "micro_to_csv()" helper family. `micro_to_csv()` gains two new trailing arguments, `clamp = FALSE` and `clamp_bounds = NULL`, wired in at two points: bounds resolution/validation happens early (fail fast, before any file I/O), clamping itself happens right after `metout`/`soil` are built and before the `shadmet`/`shadsoil` duplicates are made (so shade frames inherit already-clamped values).

**Tech Stack:** R (base + `data.frame`), `roxygen2`/`devtools::document()`. No new automated tests are added for this change (explicit call — see Global Constraints); verification is by loading the package and running the existing test suite to confirm no regression, plus a one-off manual sanity call.

## Global Constraints

- No new `testthat` tests for this feature — user explicitly asked for the change without test additions, given its scope. Do not add files under `tests/testthat/`.
- Still run the existing test suite (`tests/testthat/test-micro_to_csv.R` and the full package suite) after each task to confirm no regression to current behavior — this is a safety check on unrelated existing tests, not new test-writing.
- Follow `CLAUDE.md` conventions: internal helpers prefixed `.`, no `@export`/roxygen block on them; exported functions get full roxygen2 (`@param`, `@return`, `@details`, `@export`, `@seealso`).
- `sprintf()` for any string built from numeric values (used in `.mtc_resolve_clamp_bounds()`'s error message).
- `clamp = FALSE` by default: existing `micro_to_csv()` callers see byte-identical output with no code changes.
- Default bounds table is exactly the 21 rows and values from the approved spec (`docs/superpowers/specs/2026-08-11-micro-to-csv-clamp-design.md`): reproduced verbatim in Task 1 below.
- Run `devtools::document()` after roxygen changes (Task 2) — do not hand-edit `NAMESPACE`/`man/*.Rd`.
- Always create commits with `git add <specific files>` (never `-A`/`.`), following this repo's existing history style (`type: short description`, no period, no PR/issue references).

---

### Task 1: Clamp bounds table, getter, and internal apply/resolve helpers

**Files:**
- Modify: `R/Endotherm_DataPrep.R` — insert a new section after `.mtc_build_soil()` (currently ending at line 1374) and before the `#  micro_to_csv(): top-level export` comment block (currently line 1376). Use `grep -n "micro_to_csv(): top-level export" R/Endotherm_DataPrep.R` to find the current line if the file has shifted.

**Interfaces:**
- Produces: `.mtc_clamp_defaults()` — zero-arg internal helper, returns `data.frame(variable, lower, upper)`, 21 rows, `lower`/`upper` numeric with `NA_real_` for unbounded sides. `micro_to_csv_clamp_defaults()` — zero-arg exported wrapper, identical return shape. `.mtc_apply_clamp(df, bounds)` — clamps every column of `df` named in `bounds$variable` via `pmax`/`pmin`, skipping `NA` bound sides, leaving other columns untouched. `.mtc_resolve_clamp_bounds(clamp_bounds)` — merges an optional caller patch onto `.mtc_clamp_defaults()` (patch rows override matching `variable`s, add new ones, leave the rest at default), `stop()`s on a malformed patch or any `lower > upper`. Used by Task 2.

- [ ] **Step 1: Implement the section**

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

- [ ] **Step 2: Run `devtools::document()` to export the new function**

Run: `Rscript -e "devtools::document()"`
Expected: succeeds; `NAMESPACE` gains `export(micro_to_csv_clamp_defaults)` and `man/micro_to_csv_clamp_defaults.Rd` is created. Confirm with `grep -n "micro_to_csv_clamp_defaults" NAMESPACE`.

- [ ] **Step 3: Load the package and manually sanity-check the new pieces**

Run:

```bash
Rscript -e '
devtools::load_all()
b <- micro_to_csv_clamp_defaults()
stopifnot(nrow(b) == 21, identical(colnames(b), c("variable", "lower", "upper")))
df <- data.frame(SOLR = c(-5, 10), RH = c(150, 50))
out <- .mtc_apply_clamp(df, b)
stopifnot(out$SOLR[1] == 0, out$RH[1] == 100)
patched <- .mtc_resolve_clamp_bounds(data.frame(variable = "VLOC", lower = 0, upper = 40))
stopifnot(patched[patched$variable == "VLOC", "upper"] == 40)
tryCatch({
  .mtc_resolve_clamp_bounds(data.frame(variable = "RH", lower = 100, upper = 0))
  stop("expected an error and did not get one")
}, error = function(e) message("got expected error: ", conditionMessage(e)))
cat("OK\n")
'
```

Expected: prints `got expected error: ...lower > upper...` followed by `OK`, no `stopifnot` failures.

- [ ] **Step 4: Run the existing test suite to confirm no regression**

Run: `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-micro_to_csv.R')"`
Expected: all pre-existing tests still PASS (this file has no new tests to run yet — Task 1 added no test file changes).

- [ ] **Step 5: Commit**

```bash
git add R/Endotherm_DataPrep.R NAMESPACE man/micro_to_csv_clamp_defaults.Rd
git commit -m "feat: add micro_to_csv_clamp_defaults() and clamp helpers"
```

---

### Task 2: Wire `clamp`/`clamp_bounds` into `micro_to_csv()` and update roxygen

**Files:**
- Modify: `R/Endotherm_DataPrep.R` — the `micro_to_csv()` function itself (re-check its line with `grep -n "^micro_to_csv <- function" R/Endotherm_DataPrep.R` since line numbers shift after Task 1's insert) and its roxygen block immediately above it.

**Interfaces:**
- Consumes: `.mtc_resolve_clamp_bounds()`, `.mtc_apply_clamp()` (Task 1).
- Produces: `micro_to_csv(abvgrd_input, blwgrd_input, cell, cell_input_type, dates, elev, tannul = NULL, tz = "America/Denver", clamp = FALSE, clamp_bounds = NULL)` — same return shape as before (`list(metout, shadmet, soil, shadsoil)`), now clamped when `clamp = TRUE`.

- [ ] **Step 1: Update the roxygen block**

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

Add `micro_to_csv_clamp_defaults` to `@seealso`:

```r
#' @seealso \code{\link{write_tile}}, \code{\link{stitch_tiles}},
#'   \code{\link{micro_to_csv_clamp_defaults}}, and
#'   \code{\link{write_endotherm_inputs}} (writes \code{endo.dat}/
#'   \code{alomvars.dat} -- CSV export of this function's output and running
#'   \code{Endo2022a.exe} are handled by later, separate functions).
```

- [ ] **Step 2: Update the function signature and body**

Change the signature and the two lines right after it:

```r
micro_to_csv <- function(abvgrd_input, blwgrd_input, cell, cell_input_type,
                         dates, elev, tannul = NULL, tz = "America/Denver",
                         clamp = FALSE, clamp_bounds = NULL) {
  cell_input_type <- match.arg(cell_input_type, c("index", "lonlat", "cellnumber"))

  bounds <- if (clamp) .mtc_resolve_clamp_bounds(clamp_bounds) else NULL

  abv_handle <- .mtc_open(abvgrd_input)
```

(The `bounds <-` line is new, inserted between the existing `cell_input_type <- match.arg(...)` line and the existing `abv_handle <- .mtc_open(abvgrd_input)` line — everything else in the function body up through `soil <- .mtc_build_soil(...)` is unchanged.)

Then change the tail of the function from:

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

- [ ] **Step 3: Run `devtools::document()` to regenerate `man/micro_to_csv.Rd`**

Run: `Rscript -e "devtools::document()"`
Expected: succeeds; `man/micro_to_csv.Rd` picks up the two new `@param` entries and the updated `@seealso`. Confirm with `grep -n "clamp" man/micro_to_csv.Rd`.

- [ ] **Step 4: Manually sanity-check `clamp`/`clamp_bounds` end-to-end**

This reuses the package's own `helper-micro_to_csv.R` fixture builder, without adding anything to `tests/testthat/`:

```bash
Rscript -e '
devtools::load_all()
testthat::source_test_helpers("tests/testthat")
fx <- .mtc_test_fixture_data(nrow_ = 1, ncol_ = 1, ntime_ = 48)
fx$mout$relhum[1, 1, ] <- 150
fx$mout$Rdirdown[1, 1, ] <- -50
fx$mout$Rdifdown[1, 1, ] <- 0
abv_path <- tempfile(fileext = ".nc"); blw_path <- tempfile(fileext = ".nc")
write_tile(fx$mout, abv_path, dtm = fx$dtm, tme = fx$tme, file_fmt = "nc")
blw_arrs <- .mtc_test_fixture_blw_data(fx, depths_mm = c(0, 500))
for (dl in names(blw_arrs)) {
  write_tile(list(Tz = blw_arrs[[dl]], tme = fx$tme), blw_path,
            dtm = fx$dtm, tme = fx$tme, file_fmt = "nc", depth_label = dl)
}

out_unclamped <- micro_to_csv(abv_path, blw_path, cell = c(1, 1), cell_input_type = "index",
                              dates = as.Date("2020-07-01"), elev = 1000, tannul = 5)
stopifnot(any(out_unclamped$metout$RH > 100), any(out_unclamped$metout$SOLR < 0))

out_clamped <- micro_to_csv(abv_path, blw_path, cell = c(1, 1), cell_input_type = "index",
                            dates = as.Date("2020-07-01"), elev = 1000, tannul = 5, clamp = TRUE)
stopifnot(all(out_clamped$metout$RH <= 100), all(out_clamped$metout$SOLR >= 0))
stopifnot(identical(out_clamped$shadmet, out_clamped$metout))

patched <- micro_to_csv(abv_path, blw_path, cell = c(1, 1), cell_input_type = "index",
                        dates = as.Date("2020-07-01"), elev = 1000, tannul = 5,
                        clamp = TRUE, clamp_bounds = data.frame(variable = "VLOC", lower = 0, upper = 5))
stopifnot(all(patched$metout$VLOC <= 5))
cat("OK\n")
'
```

Expected: prints `OK`, no `stopifnot` failures. This confirms unclamped output genuinely has out-of-range values (proving the fixture is a real test of clamping, not a no-op), clamping fixes them, shade frames inherit the clamped values, and a `clamp_bounds` patch is honored.

- [ ] **Step 5: Run the full package test suite to confirm no regression**

Run: `Rscript -e "devtools::test()"`
Expected: all existing tests PASS (no new tests were added, so this only guards against an accidental behavior change to the unclamped default path).

- [ ] **Step 6: Commit**

```bash
git add R/Endotherm_DataPrep.R man/micro_to_csv.Rd
git commit -m "feat: wire clamp/clamp_bounds arguments into micro_to_csv()"
```

---

## Self-Review Notes

- **Spec coverage:** `micro_to_csv_clamp_defaults()` getter (Task 1) ✓; default bounds table with exact 21 rows/values (Task 1) ✓; `clamp`/`clamp_bounds` arguments (Task 2) ✓; patch-not-replace merge semantics (Task 1) ✓; validation (`data.frame` shape + `lower > upper`) (Task 1) ✓; application point before `shadmet`/`shadsoil` duplication (Task 2) ✓; roxygen documentation (Task 2) ✓. The spec's formal `testthat`-based verification plan is intentionally not implemented as automated tests per explicit instruction; Task 1 Step 3 and Task 2 Step 4 cover the same scenarios as one-off manual checks instead.
- **Type consistency:** `.mtc_apply_clamp(df, bounds)` and `.mtc_resolve_clamp_bounds(clamp_bounds)` (Task 1) signatures match their call sites in Task 2's `micro_to_csv()` body exactly. `micro_to_csv_clamp_defaults()`/`.mtc_clamp_defaults()` (Task 1) are both zero-arg and referenced consistently in Task 2's sanity check.
- **No placeholders:** every step has runnable code, exact file locations, and concrete expected outcomes.
