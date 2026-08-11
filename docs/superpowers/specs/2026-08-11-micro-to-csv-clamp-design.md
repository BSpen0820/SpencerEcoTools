# micro_to_csv() value clamping design

## Context

`micro_to_csv()` (`R/Endotherm_DataPrep.R`) reshapes one grid cell's microclimf
output into the 4 data frames NicheMapR's Endotherm model expects (`metout`,
`shadmet`, `soil`, `shadsoil`), built by `.mtc_build_metout()` and
`.mtc_build_soil()`. Some columns can end up outside physically valid ranges
(e.g. `SOLR` slightly negative from interpolation noise, `RH` fractionally
over 100) and NicheMapR's `Endo2022a.exe` has no tolerance for that. Bryan
wants an optional way to clamp these columns to sane bounds, with the bounds
table itself inspectable and user-editable.

## Goal

Add two exported pieces to `R/Endotherm_DataPrep.R`:

1. `micro_to_csv_clamp_defaults()` — a getter returning the package's default
   clamp-bounds table as a plain `data.frame`.
2. Two new `micro_to_csv()` arguments, `clamp` and `clamp_bounds`, that
   apply that table (optionally patched by the caller) to the 4 returned
   data frames.

## `micro_to_csv_clamp_defaults()`

```r
micro_to_csv_clamp_defaults()
```

Returns a fresh copy of this `data.frame` (columns `variable`, `lower`,
`upper`; `NA_real_` = unbounded on that side) every call:

| variable | lower | upper | rationale |
|---|---|---|---|
| TALOC | -90 | 60 | Earth air-temp records (Vostok -89.2°C, Death Valley 56.7°C) |
| TAREF | -90 | 60 | same as TALOC (duplicated per micro_to_csv's design) |
| TANNUL | -90 | 60 | mean annual temp, same physical bound as air temp |
| RHLOC | 0 | 100 | percent |
| RH | 0 | 100 | percent |
| VLOC | 0 | NA | wind speed can't be negative, no natural cap |
| VREF | 0 | NA | same as VLOC |
| ZEN | 0 | 90 | below-horizon convention, matches existing `.mtc_compute_zen()` clamp |
| SOLR | 0 | NA | can't be negative, no natural cap |
| TSKYC | -100 | 60 | effective sky temp reads colder than air temp under clear skies |
| ELEV | -500 | 9000 | Earth elevation extremes (Dead Sea -430 m, Everest 8849 m) |
| D0cm | -90 | 70 | soil surface temp; ground can exceed air-temp records (~70°C recorded desert soil surface) |
| D1.5cm | -90 | 70 | `run_micro_big_nichemap()`'s actual second depth (`sdepth`, `R/Microclimf_Modeling.R`) — see correction below |
| D2.5cm | -90 | 70 | NicheMapR's stock second depth; kept alongside D1.5cm since either can appear depending on how the tile was built |
| D5cm | -90 | 70 | same rationale as D0cm |
| D10cm | -90 | 70 | same rationale as D0cm |
| D15cm | -90 | 70 | same rationale as D0cm |
| D20cm | -90 | 70 | same rationale as D0cm |
| D30cm | -90 | 70 | same rationale as D0cm |
| D50cm | -90 | 70 | same rationale as D0cm |
| D100cm | -90 | 70 | same rationale as D0cm |
| D200cm | -90 | 70 | same rationale as D0cm |

**Correction (found in final review, applied post-implementation):** this
table originally listed only `D2.5cm` as the second depth and called the
10-row set NicheMapR's "fixed, hard-required" depth set. That was wrong for
this package's own data: `run_micro_big_nichemap()` models soil at `sdepth
<- c(0, 1.5, 5, 10, 15, 20, 30, 50, 100, 200) / -100` (`R/Microclimf_Modeling.R`),
so `.mtc_build_soil()` produces a **`D1.5cm`** column on this package's
primary data path, not `D2.5cm` — confirmed by the existing test fixture
`tests/testthat/test-micro_to_csv.R` asserting `c("TIME", "D0cm", "D1.5cm",
"D5cm")` for a 3-depth tile. The shipped table now has 11 soil-depth rows:
`D1.5cm` for this package's tiles and `D2.5cm` kept for tiles built with
NicheMapR's stock depth set. `.mtc_build_soil()` derives its columns
dynamically from whatever `Tz_BlwGrd_*` variables are present in
`blwgrd_input` (not a fixed set enforced by `micro_to_csv()` itself), so any
depth column not listed in this table simply passes through unclamped
(`.mtc_apply_clamp()`'s `intersect()` makes an unmatched row/column
combination a no-op either way — a caller with a custom depth set can add
rows via `clamp_bounds`).

`DOY` and `TIME` are index/label columns, never included in the table and
never clamped.

## `micro_to_csv()` new arguments

```r
micro_to_csv(abvgrd_input, blwgrd_input, cell, cell_input_type,
             dates, elev, tannul = NULL, tz = "America/Denver",
             clamp = FALSE, clamp_bounds = NULL)
```

- **`clamp`** (default `FALSE`): opt-in. When `TRUE`, every column present
  in the (possibly patched) bounds table is clamped in the 4 returned data
  frames, immediately before they're assembled into the return `list()`.
  When `FALSE`, `clamp_bounds` is ignored entirely and output is byte-for-byte
  identical to today's behavior.
- **`clamp_bounds`** (default `NULL`): an optional `data.frame` with the same
  3 columns (`variable`, `lower`, `upper`). Rows here are a **patch**, not a
  replacement: each row overrides the default bounds for that `variable`
  name (matched exactly, case-sensitive); a `variable` not mentioned in
  `clamp_bounds` keeps its package default; a `variable` in `clamp_bounds`
  that isn't in the defaults table is added. Ignored if `clamp = FALSE`.

### Validation

If `clamp = TRUE` and `clamp_bounds` is supplied:
- `stop()` if it isn't a `data.frame` with exactly the columns
  `variable`, `lower`, `upper`.
- `stop()` if any `lower > upper` (comparing non-NA pairs) in the
  effective merged table — this catches typos in both the defaults and any
  user patch.

### Application point

A new internal helper, `.mtc_apply_clamp(df, bounds)`, is called on `metout`
and `soil` right after `.mtc_build_metout()`/`.mtc_build_soil()` produce
them and before the return `list()` is built (so `shadmet`/`shadsoil`
inherit the already-clamped values, matching their existing "exact
duplicate" behavior). For each column name in `df` that also appears in
`bounds$variable`, replace values via `pmin(pmax(x, lower), upper)`,
skipping the `pmin`/`pmax` call on whichever side is `NA`. Columns not
named in `bounds` pass through unchanged.

## Usage

```r
bounds <- micro_to_csv_clamp_defaults()
bounds[bounds$variable == "VLOC", "upper"] <- 40   # tighten wind speed cap

out <- micro_to_csv(abvgrd_input, blwgrd_input, cell = c(120, 84),
                     cell_input_type = "index", dates = date_range,
                     elev = dem_path, clamp = TRUE, clamp_bounds = bounds)
```

## Out of scope

- Clamping `DOY`/`TIME` or any structural/index column.
- Any smarter per-depth soil bounds (e.g. tighter bounds for `D200cm` than
  `D0cm`) — not requested; all 10 depth rows share the same bound for now.
- Warning/logging when a clamp actually changes a value — clamping is
  silent, matching the function's existing "no side output beyond the
  returned list" contract.

## Documentation

Full roxygen2 blocks per `CLAUDE.md` conventions for both the new
`micro_to_csv_clamp_defaults()` export and the `clamp`/`clamp_bounds`
`@param` entries on `micro_to_csv()`. `@seealso` cross-links between the two.

## Verification plan

1. `testthat`: `micro_to_csv_clamp_defaults()` returns a `data.frame` with
   columns `variable`, `lower`, `upper` and exactly the 22 rows above (post-correction).
2. `testthat`: `micro_to_csv(..., clamp = FALSE)` (or omitted) produces
   output identical to the current (pre-change) behavior on the existing
   test fixture.
3. `testthat`: `micro_to_csv(..., clamp = TRUE)` clamps a fixture value
   engineered to be out-of-range (e.g. inject `SOLR < 0` or `RH > 100` in
   the test double) to the boundary value.
4. `testthat`: `clamp_bounds` patch overrides one variable's bound while an
   unmentioned variable keeps the package default.
5. `testthat`: `clamp_bounds` with `lower > upper` errors; `clamp_bounds`
   missing a required column errors.
6. `testthat`: `clamp = TRUE, clamp_bounds = NULL` uses the package
   defaults table unmodified.
