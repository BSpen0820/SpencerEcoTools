# run_metabolic_chamber() design

## Context

NicheMapR's updated Endotherm-model script, `endo_alomvars_auto_V5.R`
(`D:\Code\Niche Mapper\NicheMap_UpdatedExe\Endo R Input_noexecutable\`, given
to Bryan by the NicheMapR maintainers), adds a "metabolic chamber
calibration" section (V5 lines 845-1537, appended after the real
`endo.dat`/`alomvars.dat` are written) not present in V3. It exposes an
animal model to a synthetic, hand-crafted temperature ramp (each hour ~1°C
warmer than the last, ~-40°C to +44°C, all other conditions held constant -
see `Metabolic Chamber Files\Metabolic Chamber Instructions.docx`) across 4
scenarios - {standing, curled} × {variable core temp, constant core temp} -
and plots predicted metabolic rate vs. air temperature against the animal's
resting metabolic rate, the way a real metabolic-chamber experiment would.
It also parses the exe's `OUTPUT` file for a body-part-dimensions sanity
check on the allometry inputs.

This brings that capability into `SpencerEcoTools` as `run_metabolic_chamber()`
+ `plot.metchamber_result()`, reusing the package's existing
`write_endotherm_inputs()`, `write_juldays_dat()`, and `run_endotherm_model()`
rather than re-implementing row-writing or exe invocation.

Source reference materials (read during design, not modified):
- `endo_alomvars_auto_V5.R` lines 845-1537 (the mc calibration section)
- `Metabolic Chamber Files\Metabolic Chamber Instructions.docx`
- `Metabolic Chamber Files\{metout.csv, soil.csv, JULDAYS.DAT, HOURPLOT.csv}`
  (sample data)
- [[endotherm-model-integration]] memory - prior port history, the V3→V5
  tcmax/tcmin fix, and this feature's original scoping note

## Goal

Add `run_metabolic_chamber(defaults, exe_path, scenarios, mc_overrides,
save_dir, sysname)` which runs some/all of the 4 canonical mc scenarios for
a given animal and returns a list of result data frames (`metchamber_result`
object), plus `plot.metchamber_result()` to visualize them.

## Key departure from V5: stateless, isolated scenario construction

V5 runs the 4 scenarios sequentially, mutating shared `rowb*` variables in
place and overwriting the user's real `endo.dat`/`alomvars.dat` on disk
(backed up first, restored at the end). This function instead builds each
requested scenario **independently** in its own temp directory: for every
scenario, take `defaults` (a 9-group list in the same shape
`write_endotherm_inputs()`/`get_endotherm_defaults()` use), `modifyList()` it
with the fixed mc-override table below plus that scenario's specific
overrides, and call `write_endotherm_inputs()` fresh. The real `defaults`
list - and any real `endo.dat`/`alomvars.dat` already on disk elsewhere -
is never read or touched. This also makes partial scenario selection trivial
(no dependency on run order, unlike V5).

## Bundled reference data

The ramp's `metout.csv`/`soil.csv` are hand-crafted calibration data (per the
instructions doc, "You should not need to modify these files") - not
something to regenerate parametrically. Store them as package data, read
via `data()`:

- `data/metchamber_metout.rda` - data frame, from
  `Metabolic Chamber Files\metout.csv` (289 rows incl. header: 12 "days" ×
  24 hourly rows, columns `JULDAY, TIME, TALOC, TALOC, RHLOC, RH, VLOC, VREF,
  ZEN, SOLR, TALOC, ELEV, TANNUL`)
- `data/metchamber_soil.rda` - data frame, from
  `Metabolic Chamber Files\soil.csv`

Documented in `R/data.R` following the existing `fmask_bits`/`AORC_meterodf`
pattern (`@format`, `@source` noting these are hand-crafted calibration
inputs, not observational data - cite the instructions doc).

`JULDAYS.DAT` is **not** bundled as a static file - it's reproduced by
calling the existing `write_juldays_dat()` with
`model_settings = list(julnum = 12, juldays = c(15, 46, 74, 105, 135, 166,
196, 227, 258, 288, 319, 349))` (matches `metchamber_metout$JULDAY`'s
distinct values) and `habitat_settings = list(startday = 1, endday = 365,
absorp = rep(0.8, 12), shade_min = rep(0, 12), shade_max = rep(100, 12),
surfwet = rep(5, 12), multihab = "N")` (matches the sample `JULDAYS.DAT`
exactly, cross-checked field by field).

## Function signature

```r
run_metabolic_chamber(
  defaults,
  exe_path,
  scenarios  = c("standing_variable", "curled_variable",
                 "curled_constant", "standing_constant"),
  mc_overrides = list(),
  save_dir   = NULL,
  sysname    = Sys.info()[["sysname"]]
)
```

- `defaults`: required, 9-group list (`model_settings, animal, fur,
  physiology, diet, thermoreg, flying_digging, nest_shelter, allometry`) -
  typically `get_endotherm_defaults()`'s output, edited by the caller.
- `exe_path`: required, full path to the Endotherm exe (any version/name -
  copied into each scenario's temp workspace before running, so the exe's
  own filename is derived from `basename(exe_path)` and passed to
  `run_endotherm_model()`'s `exe_name`).
- `scenarios`: character vector, subset/reorder of the 4 IDs below.
- `mc_overrides`: optional list in the same 9-group shape, `modifyList()`-ed
  on top of the fixed mc-override table (not the scenario-specific fields,
  which always apply) - see "Fixed mc overrides" below for what's safe to
  override without breaking the "metabolic chamber" methodology.
- `save_dir`: optional. If given, each scenario's `endo.dat`/`alomvars.dat`
  (post-override, pre-run) is copied there as
  `{scenario}_endo.dat`/`{scenario}_alomvars.dat` before the temp dir is
  cleaned up.
- `sysname`: passed through to `run_endotherm_model()` (Windows vs. Wine).

## Scenario definitions

`model_settings$julnum`/`juldays` are always forced to `12`/the ramp's
julian days (see "Bundled reference data") for every scenario - not
user-configurable via `mc_overrides`, since they're tied to the bundled
ramp's row structure.

| Scenario ID | `diet$diurn/noct/crep` | `physiology$tcmax`/`tcmin` | `allometry` posture |
|---|---|---|---|
| `standing_variable` | `rep("Y", 12)` (active always) | real `defaults$physiology$tcmax`/`tcmin` | real `defaults$allometry` (unchanged - inactive posture never triggers since the animal is always active) |
| `curled_variable` | `rep("N", 12)` (never active) | real `defaults$physiology$tcmax`/`tcmin` | `post3 = "Y"`, `post4 = "N"`, `slpstrt = shdstrt = endpost = 3` (forces posture 3 = "legs lumped into torso, head/neck on ground"); `post1`/`post2` stay at real values (irrelevant once `slpstrt=shdstrt=endpost=3` pins the model to posture 3 only) |
| `curled_constant` | `rep("N", 12)` | `tcmax = tcreg + 0.1`, `tcmin = tcreg - 0.1` (near-constant core temp) | same forced posture as `curled_variable` |
| `standing_constant` | `rep("Y", 12)` | `tcmax = tcreg + 0.1`, `tcmin = tcreg - 0.1` | real `defaults$allometry` (unchanged, same reasoning as `standing_variable`) |

These four rows were verified against V5's actual "Replace the lines that
need to standardized..." patch block (V5 lines 1251-1268 for the endo.dat
overrides, lines 1354-1361 for the alomvars posture override) - not its
first-pass row construction, which uses real values for several fields V5
appears to override but doesn't (e.g. `hrout`, `outunits`, `trord`, `burTR`
are never actually replaced with `_mc` values despite similarly-named
variables being declared).

## Fixed mc overrides (always applied, all 4 scenarios)

Verified line-by-line against V5's patch block. `mc_overrides` may override
any of these (each subgroup gets its own `modifyList()`), but doing so
changes what "metabolic chamber simulation" means scientifically - that's
the caller's call, not this function's to prevent.

```r
list(
  model_settings = list(outout = "Y", microin = "CSV", outfile = "CSV", strht = "N"),
  physiology     = list(sweat = "N", pilo = "N"),
  diet           = list(act = rep(1.0, 12), repro = rep(0.0, 12)),
  thermoreg      = list(burrow = "N", nest = "N", climb = "N", shdseek = "N",
                         dive = "N", wind = "N", niteshd = "N", dive2 = "N",
                         shdact = "Y", shdpost = "S", treeslp = "N", hudl = "N",
                         tcconcur = "N", tcconcur2 = "N"),
  flying_digging = list(flight = "N", foss = "N", dig = "N", arb = "N")
)
```

Everything else in `defaults` (animal, fur, remaining physiology/thermoreg/
flying_digging/nest_shelter/allometry fields) passes through unchanged -
this is what makes the 4 runs a genuine test of the *caller's* animal
model, not a generic canned animal.

## Per-scenario execution

For each requested scenario, in its own `tempfile()` directory:

1. Build the merged 9-group list (`defaults` → fixed mc overrides →
   `mc_overrides` → scenario-specific fields, each layer via `modifyList()`).
2. Call `write_endotherm_inputs(output_dir = scenario_dir, ...)` with the
   merged groups.
3. Call `write_juldays_dat(scenario_dir, model_settings = list(julnum = 12,
   juldays = <ramp days>), habitat_settings = <ramp habitat, see above>)`.
4. Write `metchamber_metout` to `metout.csv` **and** `shadmet.csv`, and
   `metchamber_soil` to `soil.csv` **and** `shadsoil.csv` (matches V5 - the
   chamber has no shade differentiation, so shaded/unshaded microclimate
   inputs are identical).
5. If `save_dir` is set, copy `endo.dat`/`alomvars.dat` there now, prefixed
   with the scenario ID.
6. Copy `exe_path` into `scenario_dir`.
7. Call `run_endotherm_model(scenario_dir, exe_name = basename(exe_path),
   sysname = sysname)`.
8. On success: read `Hourplot.csv` (`skip = 1`), subset rows
   `c(2:23, 27:48, 52:73, 77:96)` (verbatim V5 indices - matches the script,
   not the instructions doc's "first 3 days" text, per your call), store in
   `result$hourplot[[scenario_id]]`. Record success in `result$log`.
   On failure: `warning()` naming the scenario and the exe's message, omit
   it from `$hourplot`, record failure in `result$log`.
9. Delete `scenario_dir` (`unlink(recursive = TRUE)`) regardless of outcome.

After all requested scenarios: if every one failed, `stop()` with an
aggregate message (all failures are in `$log` at that point). Otherwise,
parse body-part dimensions once from whichever scenario ran (and
succeeded) first - the allometry inputs don't vary by scenario in the
override table above, so this is scenario-invariant (documented as such).
Dimension parsing ports V5 lines 1517-1530 verbatim: `readLines('./OUTPUT')`,
`dimensions <- out[131:136]`, `masses <- out[129]`, fixed-column
`read.table(textConnection(...))` parses, same column renames
(`Front Legs`, `Rear Legs`, `6th Appendage (Tail/Proboscis)`).

Compute `target_rmr`: if `defaults$animal$usrmet == "Y"`, `trgt <-
defaults$animal$met`; else if `defaults$animal$class == "MAMMAL"`,
`trgt <- if (marsup == "N") (70 * mass^0.75) * (4.185/(24*3.6)) else
(2187 * mass^0.737) * (4.185/3600)` (V5's formula, mammal-only - ported
verbatim, not generalized to bird/reptile/other classes since V5 itself
doesn't handle them; `class != "MAMMAL" && usrmet == "N"` produces
`trgt = NA` with a `warning()`, not a fabricated formula).

## Return value

```r
structure(
  list(
    hourplot    = list(),  # named by scenario ID actually run+succeeded; each
                            # element is the full-column HOURPLOT.csv subset
    dimensions  = data.frame(),  # body-part dimensions, one parse
    target_rmr  = list(trgt = numeric(), err = numeric()),  # err = defaults$model_settings$err
    log         = data.frame()  # scenario, success, message, timestamp
  ),
  class = "metchamber_result"
)
```

## `plot.metchamber_result()`

```r
plot.metchamber_result <- function(x, ...)
```

Builds up to 2 ggplot2 line charts (mirrors V5's two `plot()` calls):
- **Variable core temp**: `standing_variable` vs `curled_variable`
  (MET(W) vs TAIR), only if both are present in `x$hourplot`.
- **Constant core temp**: `standing_constant` vs `curled_constant`, only if
  both present.

Each chart adds horizontal reference lines at `trgt * (1 ± err)` (upper/
lower RMR target, matching V5's red/blue `abline()`s). If neither pair is
complete (e.g. `scenarios` only requested one ID), `plot()` emits a
`message()` noting nothing could be paired and returns `invisible(NULL)`.
Otherwise it `print()`s whatever chart(s) it could build and
`invisible()`-returns a named list of the ggplot objects (`variable_temp`,
`constant_temp` - only the ones actually built) so the caller can
`ggsave()` them.

Adds `ggplot2` to `DESCRIPTION` `Imports` (package's first plotting
dependency - no existing convention to follow here).

## Out of scope

- Regenerating/parameterizing the temperature-ramp data itself (bundled
  as static reference data, per Bryan's call).
- Any scenario beyond the 4 canonical ones (e.g. arbitrary custom ramps) -
  `scenarios` only selects among the 4 fixed IDs.
- Writing `.tiff` plot files directly (V5 does; this function returns
  ggplot objects instead, caller decides how to save them).

## Documentation

Full roxygen2 block for `run_metabolic_chamber()` per `CLAUDE.md`
conventions (`@param` for all 6 arguments including the scenario-ID table
and the fixed-override table, `@return` describing the `metchamber_result`
shape, `@export`, `@seealso write_endotherm_inputs, get_endotherm_defaults,
run_endotherm_model`). `plot.metchamber_result()` documented as an S3
method (`@method plot metchamber_result`, `@export`).

## Verification plan

1. Byte-diff-style verification isn't directly applicable here (this
   function's output is parsed CSV/OUTPUT data, not a fixed-format file
   it writes) - instead, verify against a real exe run: run V5's actual mc
   section (with `shell()` calls intact) once in a scratch dir with the
   Female Bighorn Sheep defaults, capturing its 4 `output1..4` data frames,
   `Animal Model Dimensions.csv`, and `trgt`. Then call
   `run_metabolic_chamber()` with equivalent `defaults` + the real exe and
   confirm each scenario's `$hourplot[[...]]` matches V5's corresponding
   `outputN` data frame row-for-row (same TAIR/MET values), matching
   [[feedback-verbatim-port-verification]]'s "verify against real output,
   not by re-reading the code" methodology.
2. `testthat` integration test (mirrors `test-run_endotherm_model.R`'s
   `skip_on_os(c("linux", "mac"))` pattern, using the real exe fixture):
   run all 4 scenarios, confirm `$hourplot` has 4 elements with expected
   columns, `$dimensions` has 6 rows, `$log$success` all `TRUE`.
3. `testthat` unit tests without the real exe: a scenario subset (e.g. just
   `"standing_variable"`) produces a 1-element `$hourplot` and `plot()`
   on that result messages instead of erroring (no complete pair to plot).
4. `testthat`: a forced `run_endotherm_model()` failure (e.g. bad
   `exe_path`) warns and is recorded in `$log$success == FALSE` rather than
   stopping the whole function, as long as at least one other scenario
   succeeds.
