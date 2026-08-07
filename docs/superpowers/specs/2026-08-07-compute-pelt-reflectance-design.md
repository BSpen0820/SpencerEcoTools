# compute_pelt_reflectance() design

## Context

Bryan has a standalone script (`RefleCalc_NicheMap.r`) that reads a single
hand-held-spectrometer output file (`.sed`, PSR-3500 format) and computes a
mean pelt reflectance value used as input to NicheMapR Endotherm modeling —
specifically the `fur$refld`/`fur$reflv` (dorsal/ventral reflectance) fields
consumed by `write_endotherm_inputs()` (`R/Endotherm_DataPrep.R`). The script:

1. Reads the `.sed` file, skipping past a `Data:` marker line to a
   tab-separated table (`Wvl`, `Rad. (Ref.)`, `Rad. (Target)`, `Reflect. %`).
2. Converts `Reflect. %` to a 0–1 proportion.
3. Trapezoidally integrates proportion vs. `Wvl` (via `pracma::trapz`).
4. Divides by a hardcoded `2500 - 350` (the instrument's nominal wavelength
   range) to get a mean reflectance.

A real dataset of 20 `.sed` files (2 pelt samples, replicate scans) exists
under `EndoPara/Hair/Sheep Pelt Refl/`, confirmed to share the same header/
column layout as the example file. The filenames don't encode which body
part was scanned, so there's no way to auto-aggregate replicates into the
per-body-part `refld`/`reflv` values `write_endotherm_inputs()` expects —
that grouping is left to the caller.

## Goal

Port the script into one exported function, `compute_pelt_reflectance()`,
that computes the mean reflectance for a single `.sed` file, following the
package's function-design conventions (`CLAUDE.md`).

## Function signature

```r
compute_pelt_reflectance(sed_path)
```

- `sed_path`: path to a single `.sed` spectrometer output file.

## Behavior

1. Read `sed_path` with `readLines()`, locate the `Data:` marker line via
   `grep("^Data:", lines)`. `stop()` if not found.
2. Read the table below it: `read.table(sed_path, skip = <marker line>,
   header = TRUE, sep = "\t", fill = TRUE, check.names = TRUE)`.
3. Identify the reflectance column by matching `"Reflect"` against
   `names(df)` (post-`check.names` mangling, e.g. `Reflect...`), rather than
   hardcoding the exact mangled name — robust to minor header-format
   differences across instrument software versions. `stop()` if no column
   matches.
4. Convert to proportion: `refl_prop <- df[[reflect_col]] / 100`.
5. Trapezoidal integration over `Wvl` vs. `refl_prop`, implemented directly
   (no new dependency on `pracma` for one formula):

   ```r
   .trapz <- function(x, y) {
     n <- length(x)
     sum(diff(x) * (y[-n] + y[-1]) / 2)
   }
   ```

6. Mean reflectance = integrated value divided by the **actual** wavelength
   span present in the file, `max(df$Wvl) - min(df$Wvl)` — not the original
   script's hardcoded `2500 - 350` — so the result is correct even if a
   file's data doesn't span the full nominal instrument range.

## Return value

A plain named list (this is a single calculator, like `chunk_days()` /
`read_valid_cell_indices()` — not a pipeline stage, so it does not use the
package's `log data.frame` convention):

```r
list(
  mean_reflectance = <numeric, 0-1>,   # trapz(Wvl, refl_prop) / (wvl_max - wvl_min)
  wvl_min          = <numeric, nm>,
  wvl_max          = <numeric, nm>,
  n_points         = <integer>,
  file             = sed_path
)
```

`wvl_min`/`wvl_max`/`n_points` are cheap to include and let Bryan sanity-check
that the full spectrum was read before hand-transcribing `mean_reflectance`
into `fur$refld`/`reflv`.

## Error handling

`stop()` (unrecoverable, single-file function, per `CLAUDE.md`) when:
- `sed_path` does not exist.
- No `Data:` marker line is found.
- No column name matches `"Reflect"`.

## Out of scope

- Batch/directory processing of multiple `.sed` files — filenames don't
  encode body part, so any aggregation grouping would be a guess. Callers
  loop over files themselves (e.g. `sapply(paths, compute_pelt_reflectance)`)
  once they know their own file-to-body-part mapping.
- Adding `pracma` as a package dependency — the trapezoidal rule is
  implemented directly instead.
- Wiring the result into `write_endotherm_inputs()`'s `fur` argument —
  Bryan assigns `mean_reflectance` to `refld`/`reflv` by hand.

## Documentation

Full roxygen2 block per `CLAUDE.md` conventions: `@param sed_path`, `@return`
describing the five-element list, `@details` noting the trapezoidal-rule
method and the actual-range (not hardcoded 350–2500) denominator, `@export`,
`@seealso write_endotherm_inputs`.

## Verification plan

Verified with an ad-hoc (uncommitted) script against a real `.sed` file from
`EndoPara/Hair/Sheep Pelt Refl/`, checked by hand against the original
script's output. No `testthat` file is added to the package's test suite for
this function, per Bryan's direction.
