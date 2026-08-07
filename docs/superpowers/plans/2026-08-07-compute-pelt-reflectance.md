# compute_pelt_reflectance() Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add one exported function, `compute_pelt_reflectance()`, to `R/Endotherm_DataPrep.R` that computes mean pelt reflectance from a single hand-held-spectrometer `.sed` file, porting the logic of `RefleCalc_NicheMap.r`.

**Architecture:** A single self-contained function plus one internal trapezoidal-integration helper, appended to the end of the existing `R/Endotherm_DataPrep.R` file (which already holds the rest of the fur/pelt-facing endotherm prep code). No new files, no new package dependencies.

**Tech Stack:** Base R only (`readLines`, `utils::read.table`). `pracma` is used only in an ad-hoc, uncommitted verification script — never added to `DESCRIPTION`.

## Global Constraints

- No `pracma` dependency added to `DESCRIPTION` — trapezoidal integration is implemented directly in the package.
- No `testthat` test file added for this function (explicit user direction) — verification is via an ad-hoc, uncommitted script instead.
- Mean reflectance denominator uses the file's **actual** `max(Wvl) - min(Wvl)`, not a hardcoded `2500 - 350`.
- The manual trapezoidal implementation's output must numerically match what `pracma::trapz()` would produce on the same data (user's explicit acceptance condition for this plan).
- Full roxygen2 block required (`@param`, `@return`, `@details`, `@export`, `@seealso`) per `CLAUDE.md` documentation standards.
- `stop()` for unrecoverable errors (missing file, missing `Data:` marker, missing reflectance column), per `CLAUDE.md` error-handling convention.

---

### Task 1: Implement `compute_pelt_reflectance()` and document it

**Files:**
- Modify: `R/Endotherm_DataPrep.R` (append to end of file, after `micro_to_csv()` at line 1493)
- Modify (generated): `NAMESPACE`, `man/compute_pelt_reflectance.Rd` (via `devtools::document()`)

**Interfaces:**
- Produces: `compute_pelt_reflectance(sed_path)` → named list `list(mean_reflectance, wvl_min, wvl_max, n_points, file)`, all as described in `@return` below. This is the only interface later tasks (verification) consume.
- Produces (internal, unexported): `.trapz_proportion(x, y)` → numeric, the trapezoidal-rule integral of `y` over `x`.

- [ ] **Step 1: Append the function to `R/Endotherm_DataPrep.R`**

Add this exact code to the end of `R/Endotherm_DataPrep.R` (after the closing `}` of `micro_to_csv()` on line 1493):

```r

.trapz_proportion <- function(x, y) {
  n <- length(x)
  sum(diff(x) * (y[-n] + y[-1]) / 2)
}

#' Compute mean pelt reflectance from a hand-held spectrometer file
#'
#' Reads a single PSR-3500 spectrometer \code{.sed} output file, converts its
#' \code{Reflect. \%} column to a 0-1 proportion, and trapezoidally integrates
#' it over wavelength to get a single mean reflectance value -- the input
#' expected by \code{\link{write_endotherm_inputs}}'s \code{fur$refld}/
#' \code{fur$reflv} (dorsal/ventral pelt reflectance) fields.
#'
#' @param sed_path Path to a single \code{.sed} spectrometer output file.
#'
#' @return A named list: \code{mean_reflectance} (numeric, 0-1, the
#'   integrated reflectance divided by the file's actual wavelength span),
#'   \code{wvl_min}, \code{wvl_max} (numeric, nm, the actual range of
#'   wavelengths present in the file), \code{n_points} (integer, number of
#'   spectral data rows), and \code{file} (the input \code{sed_path}).
#'
#' @details
#' Ported from \code{RefleCalc_NicheMap.r}. The reflectance column is located
#' by matching \code{"Reflect"} against the file's (\code{make.names()}-
#' mangled) column headers rather than hardcoding the exact mangled name, so
#' minor header-format differences across instrument software versions don't
#' break parsing. Integration uses the trapezoidal rule, implemented directly
#' (numerically equivalent to \code{pracma::trapz(x, y)}) rather than
#' depending on \code{pracma} for a single formula. Unlike the source script,
#' which divided by a hardcoded nominal instrument range of \code{2500 - 350}
#' nm, this function divides by the actual \code{max(Wvl) - min(Wvl)} present
#' in the file, so the result is correct even when a file's data doesn't span
#' the full nominal range.
#'
#' @seealso \code{\link{write_endotherm_inputs}}
#' @export
compute_pelt_reflectance <- function(sed_path) {
  if (!file.exists(sed_path))
    stop(sprintf("'sed_path' does not exist:\n  %s", sed_path))

  lines <- readLines(sed_path, warn = FALSE)
  data_marker <- grep("^Data:", lines)
  if (length(data_marker) == 0)
    stop(sprintf("Could not find 'Data:' marker in:\n  %s", sed_path))

  df <- utils::read.table(sed_path, skip = data_marker, header = TRUE,
                           sep = "\t", fill = TRUE, check.names = TRUE)

  reflect_col <- grep("Reflect", names(df), value = TRUE)
  if (length(reflect_col) == 0)
    stop(sprintf("No column matching 'Reflect' found in:\n  %s", sed_path))

  refl_prop <- df[[reflect_col[1]]] / 100
  wvl <- df$Wvl

  integrated <- .trapz_proportion(wvl, refl_prop)
  wvl_min <- min(wvl)
  wvl_max <- max(wvl)

  list(
    mean_reflectance = integrated / (wvl_max - wvl_min),
    wvl_min          = wvl_min,
    wvl_max          = wvl_max,
    n_points         = length(wvl),
    file             = sed_path
  )
}
```

- [ ] **Step 2: Generate docs and NAMESPACE entry**

Run:
```bash
cd "/Volumes/Spencer_PHD/Code/PhD/R_Packages/SpencerEcoTools"
Rscript -e 'devtools::document()'
```
Expected: completes without error/warning; creates `man/compute_pelt_reflectance.Rd`; adds `export(compute_pelt_reflectance)` to `NAMESPACE` in its alphabetized position (between `compute_albedo`/`compute_reflectance` and `create_tiles`).

- [ ] **Step 3: Load the package and smoke-test on a real `.sed` file**

Run:
```bash
Rscript -e '
devtools::load_all(".")
sed_path <- "/Volumes/Spencer_PHD/Code/PhD/Side Projects/SheepMigrationLoss/Endotherm Model/EndoPara/Hair/Sheep Pelt Refl/14C6051_pelt1a_00406.sed"
result <- compute_pelt_reflectance(sed_path)
str(result)
'
```
Expected: no error; prints a list with `mean_reflectance` a number between 0 and 1, `wvl_min` near 346.3, `wvl_max` near 2511.1, `n_points` matching the file'"'"'s data row count, `file` echoing the path.

- [ ] **Step 4: Commit**

```bash
cd "/Volumes/Spencer_PHD/Code/PhD/R_Packages/SpencerEcoTools"
git add R/Endotherm_DataPrep.R NAMESPACE man/compute_pelt_reflectance.Rd
git commit -m "$(cat <<'EOF'
feat: add compute_pelt_reflectance() for pelt spectrometer data

Ports RefleCalc_NicheMap.r into a package function that computes mean
pelt reflectance from a single .sed spectrometer file via trapezoidal
integration, feeding write_endotherm_inputs()'s fur$refld/reflv fields.
EOF
)"
```

---

### Task 2: Verify manual trapezoidal integration matches `pracma::trapz()`

**Files:**
- Create (uncommitted, scratch only): a verification script in your session's scratchpad directory, e.g. `verify_pelt_reflectance.R` — **do not** add this file to the git repo (per the design's decision not to ship a `testthat` test for this function).

**Interfaces:**
- Consumes: `compute_pelt_reflectance(sed_path)` from Task 1 (already committed and loadable via `devtools::load_all(".")`).

- [ ] **Step 1: Confirm `pracma` is installed locally**

Run:
```bash
Rscript -e 'if (requireNamespace("pracma", quietly = TRUE)) cat("pracma available\n") else cat("pracma NOT available - install with install.packages(\"pracma\") for this verification only\n")'
```
Expected: `pracma available`. If not available, run `Rscript -e 'install.packages("pracma")'` first (this is a local, ad-hoc install for verification only — it must not be added to `DESCRIPTION`).

- [ ] **Step 2: Write the verification script**

Write this to a scratch path (e.g. `<scratchpad_dir>/verify_pelt_reflectance.R`):

```r
devtools::load_all("/Volumes/Spencer_PHD/Code/PhD/R_Packages/SpencerEcoTools")

sed_path <- "/Volumes/Spencer_PHD/Code/PhD/Side Projects/SheepMigrationLoss/Endotherm Model/EndoPara/Hair/Sheep Pelt Refl/14C6051_pelt1a_00406.sed"

# Value from the package function under test
result <- compute_pelt_reflectance(sed_path)

# Independent computation using pracma::trapz on the same parsed data
lines <- readLines(sed_path, warn = FALSE)
data_marker <- grep("^Data:", lines)
df <- read.table(sed_path, skip = data_marker, header = TRUE, sep = "\t",
                  fill = TRUE, check.names = TRUE)
reflect_col <- grep("Reflect", names(df), value = TRUE)[1]
refl_prop <- df[[reflect_col]] / 100

pracma_integrated <- pracma::trapz(df$Wvl, refl_prop)
pracma_mean <- pracma_integrated / (max(df$Wvl) - min(df$Wvl))

cat(sprintf("compute_pelt_reflectance():   %.10f\n", result$mean_reflectance))
cat(sprintf("pracma::trapz() equivalent:   %.10f\n", pracma_mean))

if (isTRUE(all.equal(result$mean_reflectance, pracma_mean))) {
  cat("MATCH: manual trapezoidal implementation agrees with pracma::trapz()\n")
} else {
  stop(sprintf(
    "MISMATCH: compute_pelt_reflectance() = %.10f, pracma::trapz() equivalent = %.10f",
    result$mean_reflectance, pracma_mean
  ))
}
```

- [ ] **Step 3: Run it and confirm the match**

Run:
```bash
Rscript <scratchpad_dir>/verify_pelt_reflectance.R
```
Expected output ends with `MATCH: manual trapezoidal implementation agrees with pracma::trapz()`. If it instead errors with `MISMATCH: ...`, stop and re-examine `.trapz_proportion()` in `R/Endotherm_DataPrep.R` for an off-by-one in the `diff(x) * (y[-n] + y[-1]) / 2` formula before proceeding.

- [ ] **Step 4: Repeat against the second pelt sample for extra confidence (optional but recommended)**

Re-run Step 2's script with `sed_path` pointed at `.../Sheep Pelt Refl/14C6051_pelt2a_00415.sed` instead, and confirm `MATCH` again.

- [ ] **Step 5: No commit**

This task produces no repository changes — the verification script stays in the scratchpad directory only, per the design's decision not to add a `testthat` test for this function. Report the two `MATCH` results (or investigate/report any `MISMATCH`) as the completion signal for this task.

---

## Self-Review Notes

- **Spec coverage:** Every behavior in `docs/superpowers/specs/2026-08-07-compute-pelt-reflectance-design.md` (Data: marker lookup, column matching via `"Reflect"`, proportion conversion, manual trapezoidal integration, actual-range denominator, five-field return list, three `stop()` error paths, no `pracma` dependency, no `testthat` file) is implemented in Task 1's Step 1 code or exercised in Task 2. The user's added condition (manual trapz must match `pracma::trapz()`) is Task 2 in full.
- **Placeholder scan:** No TBD/TODO; all code blocks are complete and runnable as written.
- **Type consistency:** `compute_pelt_reflectance()` is defined once (Task 1) and consumed with the identical signature and return-field names (`mean_reflectance`, `wvl_min`, `wvl_max`, `n_points`, `file`) in Task 2 — no drift.
