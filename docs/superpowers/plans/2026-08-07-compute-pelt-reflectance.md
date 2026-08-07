# compute_pelt_reflectance() Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add one exported function, `compute_pelt_reflectance()`, to `R/Endotherm_DataPrep.R` that computes mean pelt reflectance from either a hand-held-spectrometer `.sed` file or an already-parsed data.frame of the same shape, porting the logic of `RefleCalc_NicheMap.r`.

**Architecture:** A single self-contained function plus one internal trapezoidal-integration helper, appended to the end of the existing `R/Endotherm_DataPrep.R` file (which already holds the rest of the fur/pelt-facing endotherm prep code). No new files, no new package dependencies.

**Tech Stack:** Base R only (`readLines`, `utils::read.table`). `pracma` is used only in an ad-hoc, uncommitted verification script — never added to `DESCRIPTION`.

## Global Constraints

- No `pracma` dependency added to `DESCRIPTION` — trapezoidal integration is implemented directly in the package.
- No `testthat` test file added for this function (explicit user direction) — verification is via an ad-hoc, uncommitted script instead.
- `compute_pelt_reflectance()` accepts **either** a character `.sed` file path **or** a `data.frame` already shaped like the parsed table (`Wvl` column + a column matching `"Reflect"`, 0-100 scale) — both paths converge on the same downstream integration logic.
- Mean reflectance denominator uses the data's **actual** `max(Wvl) - min(Wvl)`, not a hardcoded `2500 - 350`.
- The manual trapezoidal implementation's output must numerically match what `pracma::trapz()` would produce on the same data (user's explicit acceptance condition for this plan).
- Full roxygen2 block required (`@param`, `@return`, `@details`, `@export`, `@seealso`) per `CLAUDE.md` documentation standards.
- `stop()` for unrecoverable errors, per `CLAUDE.md` error-handling convention.

---

### Task 1: Implement `compute_pelt_reflectance()` and document it

**Files:**
- Modify: `R/Endotherm_DataPrep.R` (append to end of file, after `micro_to_csv()` at line 1493)
- Modify (generated): `NAMESPACE`, `man/compute_pelt_reflectance.Rd` (via `devtools::document()`)

**Interfaces:**
- Produces: `compute_pelt_reflectance(sed_input)` → named list `list(mean_reflectance, wvl_min, wvl_max, n_points, file)`, all as described in `@return` below. `sed_input` accepts a character file path or a `data.frame`. This is the only interface later tasks (verification) consume.
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
#' Reads a single PSR-3500 spectrometer \code{.sed} output file -- or accepts
#' an already-parsed data.frame of the same shape -- converts its
#' \code{Reflect. \%} column to a 0-1 proportion, and trapezoidally
#' integrates it over wavelength to get a single mean reflectance value --
#' the input expected by \code{\link{write_endotherm_inputs}}'s
#' \code{fur$refld}/\code{fur$reflv} (dorsal/ventral pelt reflectance)
#' fields.
#'
#' @param sed_input Either a path (character) to a single \code{.sed}
#'   spectrometer output file, or a \code{data.frame} already containing the
#'   spectral data: a \code{Wvl} column plus a column matching
#'   \code{"Reflect"} (on the same 0-100 percent scale as a \code{.sed}
#'   file's \code{Reflect. \%} column). The \code{data.frame} form lets
#'   callers who already have the data in memory skip file parsing.
#'
#' @return A named list: \code{mean_reflectance} (numeric, 0-1, the
#'   integrated reflectance divided by the data's actual wavelength span),
#'   \code{wvl_min}, \code{wvl_max} (numeric, nm, the actual range of
#'   wavelengths present in the data), \code{n_points} (integer, number of
#'   spectral data rows), and \code{file} (the input \code{sed_input} if it
#'   was a character path, else \code{NA_character_}).
#'
#' @details
#' Ported from \code{RefleCalc_NicheMap.r}. The reflectance column is located
#' by matching \code{"Reflect"} against the table's (\code{make.names()}-
#' mangled, for the file-path input) column headers rather than hardcoding
#' the exact mangled name, so minor header-format differences across
#' instrument software versions -- or a caller's own column naming for the
#' data.frame input -- don't break parsing. Integration uses the trapezoidal
#' rule, implemented directly (numerically equivalent to
#' \code{pracma::trapz(x, y)}) rather than depending on \code{pracma} for a
#' single formula. Unlike the source script, which divided by a hardcoded
#' nominal instrument range of \code{2500 - 350} nm, this function divides by
#' the actual \code{max(Wvl) - min(Wvl)} present in the data, so the result
#' is correct even when the data doesn't span the full nominal range.
#'
#' @seealso \code{\link{write_endotherm_inputs}}
#' @export
compute_pelt_reflectance <- function(sed_input) {
  if (is.character(sed_input)) {
    if (!file.exists(sed_input))
      stop(sprintf("'sed_input' does not exist:\n  %s", sed_input))

    lines <- readLines(sed_input, warn = FALSE)
    data_marker <- grep("^Data:", lines)
    if (length(data_marker) == 0)
      stop(sprintf("Could not find 'Data:' marker in:\n  %s", sed_input))

    df <- utils::read.table(sed_input, skip = data_marker, header = TRUE,
                             sep = "\t", fill = TRUE, check.names = TRUE)
    file_label <- sed_input
  } else if (is.data.frame(sed_input)) {
    df <- sed_input
    file_label <- NA_character_
  } else {
    stop("'sed_input' must be either a character file path to a .sed file or a data.frame")
  }

  if (!"Wvl" %in% names(df))
    stop("'sed_input' must contain a 'Wvl' column")

  reflect_col <- grep("Reflect", names(df), value = TRUE)
  if (length(reflect_col) == 0)
    stop("No column matching 'Reflect' found in 'sed_input'")

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
    file             = file_label
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

- [ ] **Step 3: Load the package and smoke-test both input forms on a real `.sed` file**

Run:
```bash
Rscript -e '
devtools::load_all(".")
sed_path <- "/Volumes/Spencer_PHD/Code/PhD/Side Projects/SheepMigrationLoss/Endotherm Model/EndoPara/Hair/Sheep Pelt Refl/14C6051_pelt1a_00406.sed"

# File-path input
result_path <- compute_pelt_reflectance(sed_path)
cat("== file-path input ==\n"); str(result_path)

# data.frame input, built from the same file
lines <- readLines(sed_path, warn = FALSE)
data_marker <- grep("^Data:", lines)
df <- read.table(sed_path, skip = data_marker, header = TRUE, sep = "\t",
                  fill = TRUE, check.names = TRUE)
result_df <- compute_pelt_reflectance(df)
cat("== data.frame input ==\n"); str(result_df)

stopifnot(isTRUE(all.equal(result_path$mean_reflectance, result_df$mean_reflectance)))
cat("Both input forms agree on mean_reflectance.\n")
'
```
Expected: no error; both `str()` calls print a list with `mean_reflectance` a number between 0 and 1, `wvl_min` near 346.3, `wvl_max` near 2511.1, `n_points` matching the file's data row count; `result_path$file` echoes the path, `result_df$file` is `NA`; final line prints `Both input forms agree on mean_reflectance.`

- [ ] **Step 4: Commit**

```bash
cd "/Volumes/Spencer_PHD/Code/PhD/R_Packages/SpencerEcoTools"
git add R/Endotherm_DataPrep.R NAMESPACE man/compute_pelt_reflectance.Rd
git commit -m "$(cat <<'EOF'
feat: add compute_pelt_reflectance() for pelt spectrometer data

Ports RefleCalc_NicheMap.r into a package function that computes mean
pelt reflectance from either a .sed spectrometer file or an
already-parsed data.frame, via trapezoidal integration, feeding
write_endotherm_inputs()'s fur$refld/reflv fields.
EOF
)"
```

---

### Task 2: Verify manual trapezoidal integration matches `pracma::trapz()`

**Files:**
- Create (uncommitted, scratch only): a verification script in your session's scratchpad directory, e.g. `verify_pelt_reflectance.R` — **do not** add this file to the git repo (per the design's decision not to ship a `testthat` test for this function).

**Interfaces:**
- Consumes: `compute_pelt_reflectance(sed_input)` from Task 1 (already committed and loadable via `devtools::load_all(".")`).

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

# Independently parse the file once, to drive both comparisons
lines <- readLines(sed_path, warn = FALSE)
data_marker <- grep("^Data:", lines)
df <- read.table(sed_path, skip = data_marker, header = TRUE, sep = "\t",
                  fill = TRUE, check.names = TRUE)
reflect_col <- grep("Reflect", names(df), value = TRUE)[1]
refl_prop <- df[[reflect_col]] / 100

pracma_integrated <- pracma::trapz(df$Wvl, refl_prop)
pracma_mean <- pracma_integrated / (max(df$Wvl) - min(df$Wvl))

check_match <- function(label, value) {
  cat(sprintf("%-30s %.10f\n", label, value))
  cat(sprintf("%-30s %.10f\n", "pracma::trapz() equivalent", pracma_mean))
  if (isTRUE(all.equal(value, pracma_mean))) {
    cat("MATCH\n\n")
  } else {
    stop(sprintf("MISMATCH: %s = %.10f, pracma equivalent = %.10f", label, value, pracma_mean))
  }
}

# File-path input path
result_path <- compute_pelt_reflectance(sed_path)
check_match("compute_pelt_reflectance(path)", result_path$mean_reflectance)

# data.frame input path (same underlying data)
result_df <- compute_pelt_reflectance(df)
check_match("compute_pelt_reflectance(data.frame)", result_df$mean_reflectance)

cat("All input forms agree with pracma::trapz().\n")
```

- [ ] **Step 3: Run it and confirm the match**

Run:
```bash
Rscript <scratchpad_dir>/verify_pelt_reflectance.R
```
Expected output ends with `All input forms agree with pracma::trapz().`, with a `MATCH` printed for both the file-path and data.frame input checks. If it instead errors with `MISMATCH: ...`, stop and re-examine `.trapz_proportion()` in `R/Endotherm_DataPrep.R` for an off-by-one in the `diff(x) * (y[-n] + y[-1]) / 2` formula before proceeding.

- [ ] **Step 4: Repeat against the second pelt sample for extra confidence (optional but recommended)**

Re-run Step 2's script with `sed_path` pointed at `.../Sheep Pelt Refl/14C6051_pelt2a_00415.sed` instead, and confirm both `MATCH` lines again.

- [ ] **Step 5: No commit**

This task produces no repository changes — the verification script stays in the scratchpad directory only, per the design's decision not to add a `testthat` test for this function. Report the `MATCH` results (or investigate/report any `MISMATCH`) as the completion signal for this task.

---

## Self-Review Notes

- **Spec coverage:** Every behavior in `docs/superpowers/specs/2026-08-07-compute-pelt-reflectance-design.md` (dual character-path/data.frame input dispatch, `Data:` marker lookup, `Wvl`-column check, column matching via `"Reflect"`, proportion conversion, manual trapezoidal integration, actual-range denominator, five-field return list with `NA_character_` file for the data.frame path, all `stop()` error paths, no `pracma` dependency, no `testthat` file) is implemented in Task 1's Step 1 code or exercised in Task 1 Step 3 / Task 2. The user's added condition (manual trapz must match `pracma::trapz()`) is Task 2 in full, covering both input forms.
- **Placeholder scan:** No TBD/TODO; all code blocks are complete and runnable as written.
- **Type consistency:** `compute_pelt_reflectance()` is defined once (Task 1) with parameter name `sed_input` throughout, and consumed with the identical signature and return-field names (`mean_reflectance`, `wvl_min`, `wvl_max`, `n_points`, `file`) in Task 1 Step 3 and Task 2 — no drift.
