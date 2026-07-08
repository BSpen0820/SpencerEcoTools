# write_endotherm_inputs() design

## Context

`D:\Code\NicheMapperAlom_End_FileCode\endo_alomvars_auto_V3.R` is a standalone 844-line script Bryan uses to prepare inputs for the NicheMapR Endotherm model (`Endo2022a.exe`). Its first ~350 lines are a flat block of user-editable variables (animal physiology, fur, allometry, behavior, diet, etc.), followed by ~500 lines that assemble those variables into two fixed-format text files — `endo.dat` and `alomvars.dat` — via hand-tuned `paste()`/`format()`/`sQuote()` calls with specific tab/space padding, then (in the original script only) calls `shell("Endo2022a.exe")`.

This conversion brings that input-file preparation into `SpencerEcoTools` as a reusable, documented function, consistent with the rest of the package's data-prep/modeling pipeline (see `CLAUDE.md`). Running the executable, parsing its outputs, and generating `JULDAYS.dat` are deliberately excluded from this piece of work — they are separate, later functions.

Source reference materials (read during design, not modified):
- `endo_alomvars_auto_V3.R` — the script being ported
- `Endo input file notes.docx` / `Alomvars input file notes.docx` — parameter documentation (Paul Mathewson et al.)
- `endo.dat` / `alomvars.dat` — sample output files in the same folder
- `JULDAYS.DAT` — static input file, out of scope here

## Goal

Add `write_endotherm_inputs()` to the package (likely `R/Microclimf_Modeling.R`, alongside other model-input writers such as `write_tile()`), which builds `endo.dat` and `alomvars.dat` from grouped, documented, defaulted R arguments instead of a hand-edited script.

## Function signature

```r
write_endotherm_inputs(
  output_dir,
  model_settings = list(),
  animal         = list(),
  fur            = list(),
  physiology     = list(),
  diet           = list(),
  thermoreg      = list(),
  flying_digging = list(),
  nest_shelter   = list(),
  allometry      = list(),
  study_area     = NULL
)
```

Each `list()` argument is merged onto an internal default list (via `utils::modifyList()`) that reproduces the original script's example animal (a 15 kg Chacma Baboon). Callers override only the fields they need to change, e.g.:

```r
write_endotherm_inputs(
  output_dir = "working_dir",
  animal     = list(mass = 20, species = "Coyote", cp = 3060)
)
```

`study_area` is accepted for consistency with the rest of the package (it is recorded in the returned log) but does **not** prefix the output filenames — see "File naming" below.

### Argument group contents

**`model_settings`** — simulation-level switches (script lines 14-42):
`julnum, juldays, hrout, outout, microin, outfile, outunits, strht, geom, geomult, apnd, ventpct, inccond, frcmpr, usralom, actht, err, acthrs, minfrg, nrght, prdht, fasky, fagrd, faobj, usrnure, afrnt, bfrnt, aside, bside`

**`animal`** — whole-animal properties (lines 46-59):
`species, class, marsup, cp, mass, timdepmass, mass2, fatpct, timdepfat, fatpct2, subqfat, density, usrmet, met`

**`fur`** — fur/feather properties (lines 61-124):
- Whole-body defaults: `varfur, diad, diav, lend, lenv, depd, depv, dend, denv, refld, reflv`
- `parts` sublist used only when `varfur = 1`: `leg`, `head_neck`, `torso`, `tail`, each holding the same nine fields (`diad, diav, lend, lenv, depd, depv, dend, denv, refld, reflv`) as the whole-body defaults, mirroring the script's `*1`/`*2`/`*3`/`*4` suffixes
- Time-dependent torso fur: `tmdptorfur, torlend, torlenv, tordepd, tordepv`

**`physiology`** — core temperature and heat-exchange physiology (lines 126-148):
`tcreg, tcmin, tcmax, tchib, tmdptc, tcreg2, tctskdif, texptair, sknwet, maxsknwet, sweat, pilo, maxpilo, flshk, flshkmin, flshkmax, usrfurk, usrfurk2, radfurdep, o2max, o2min`

**`diet`** — diet, digestion, and daily activity/hibernation schedule (lines 150-172):
`gut, fech2o, urea, digef, act, repro, prtn, fat, carb, dry, diurn, noct, crep, hibrn, hibfrac, land, land2`

**`thermoreg`** — behavioral thermoregulation options (lines 174-201):
`burrow, nest, climb, shdseek, dive, wind, niteshd, dive2, shdact, shdpost, trord, burTR, wade, treeslp, slpcnpy, hudl, hudlnum, hudldrs, hudlvnt, tcconcur, tcinc, tcwtr, o2inc, sknwtinc, tcconcur2, piloinc`

> Note: the original script assigns `tcinc` twice (line 195 for sweating, line 200 for piloerection), and the second assignment silently overwrites the first before either is used. Both are `0.01` in the example, so this has no visible effect there, but it means the script only ever supports one `tcinc` value, not two independent ones. This function preserves that single-`tcinc` behavior for fidelity; anyone needing independent sweating/piloerection increments would need a follow-up change (not part of this task).

**`flying_digging`** — flight and burrowing/fossoriality options (lines 203-221):
`flight, fltmetab, fltvel, fltload, foss, dig, nodes, arb, buro2, burco2, burn2, soiltyp, burseg, burdep`

**`nest_shelter`** — nest/shelter geometry and use (lines 223-242):
`shelter, nestuse, nestthk, nestk, nestno, nestloc, nestnode, nestlength, outdiam, shltrefl, shltrans, shltdens, shltcp, nestheight, shltnodes, noderadius`

**`allometry`** — only used to build `alomvars.dat` (lines 246-362):
- `group, loco`
- `parts` sublist with six named entries — `head, neck, torso, front_leg, rear_leg, tail` — each holding `diav, diah, len, dorsfur, dorsfur_a, ventfur, ventfur_a, dens, geom` (mirrors the script's `*1`...`*6` suffixes)
- `tail_type` (the script's `typ`: `'T'` tail vs `'P'` proboscis)
- `absval, absdim, adjdim`
- per-part subcutaneous fat: `subq_head, subq_neck, subq_torso, subq_front_leg, subq_rear_leg, subq_tail`, plus `tmdpfat`
- inactive postures: `post1, post2, post3, post4, slpstrt, shdstrt, endpost`
- per-part minimum flesh conductivity: `akmin_head, akmin_neck, akmin_torso, akmin_front_leg, akmin_rear_leg, akmin_tail`, plus `VTleg, VT6th, Grd6th`
- leg-shading geometry: `torsover, torsoff`
- bird sleeping: `brdslp, brdslplg`
- countercurrent exchange: `Tlegred, T6thred, Tcred1, Tcred2, Tfrac1, Tfrac2, Tdif1, Tdif2, MinT, Tleginc, T6thinc`

## File writing behavior

- Row assembly (the `row1`...`row290` for `endo.dat` and `rowa1`...`rowa66` for `alomvars.dat`) is **ported verbatim** from the script — same `paste()`/`format(..., nsmall=...)`/`sQuote()` calls, same literal comment/header text, same tab and space padding — just reading from the merged argument lists instead of loose script variables. `Endo2022a.exe` is a compiled reader that most likely parses these fixed-format files by column position, so preserving exact spacing is the actual requirement here, not a stylistic nicety.
- Both files are **always written**, regardless of `model_settings$usralom` (matches the script's existing behavior — it does not conditionally skip `alomvars.dat` either).
- **File naming:** written as exactly `endo.dat` and `alomvars.dat` in `output_dir` — no `study_area` prefix — because `Endo2022a.exe` hard-codes these filenames in its working directory. This is a deliberate deviation from this package's usual `{study_area}_...` output naming convention, noted here so it isn't "fixed" by a future contributor unfamiliar with the constraint.
- `options(useFancyQuotes = FALSE)` is set before writing (as in the script) so `sQuote()` produces plain `'x'` quotes rather than curly quotes.
- Returns invisibly a log `data.frame` with columns `file_path, step, status, timestamp` (one row per file written), consistent with this package's "always return a log" convention.

## Out of scope (explicitly deferred)

- Running `Endo2022a.exe` (a separate future function will handle execution)
- Parsing `MONTH`/`YEAR`/`HOURPLOT`/`OUTPUT` results back into R
- Generating `JULDAYS.dat` (remains a manually-prepared input for now)
- Tiled/multi-period/HPC-array looping (`run_micro_big_nichemap()`'s pattern) — this function handles one parameter set / one output directory per call; any looping across species or scenarios is the caller's responsibility

## Documentation

Full roxygen2 block per `CLAUDE.md` conventions: `@param` for each of the nine grouped arguments (documenting the expected named fields and defaults within), `@return`, `@details` (mentioning the byte-format sensitivity and the `usralom`/always-write-both-files behavior), `@export`. `@seealso` linking to the (future) exe-runner function once it exists.

## Verification plan

1. Run the original `endo_alomvars_auto_V3.R` unmodified in a scratch working directory to produce reference `endo.dat` and `alomvars.dat` files (its baboon example).
2. Call `write_endotherm_inputs()` with all-default arguments (which should represent the same baboon example) writing to a separate scratch directory.
3. Diff the two sets of files (`diff` / `identical()` on file contents) — they must match byte-for-byte. Any discrepancy indicates a porting error in the row-assembly logic (padding, rounding, or a missed variable), not an intentional design difference.
4. Spot-check one or two non-default overrides (e.g., a different `mass` and `species`) to confirm the merged-list overriding works and still produces correctly formatted rows.
