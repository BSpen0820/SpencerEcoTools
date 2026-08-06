# get_endotherm_defaults() design

## Context

`write_endotherm_inputs()` (`R/Endotherm_DataPrep.R`) builds its nine grouped arguments (`model_settings, animal, fur, physiology, diet, thermoreg, flying_digging, nest_shelter, allometry`) by merging a caller-supplied `list()` onto internal `.default_*()` helpers via `utils::modifyList()`. Those helpers are unexported (`.`-prefixed, no `@export`), so the only way to see the Female Bighorn Sheep baseline values they encode is to open `R/Endotherm_DataPrep.R` directly — there is no `?help` page or REPL-inspectable object for them.

Bryan wants to view the full default parameter set as a normal R object, edit fields on it, and pass the result straight into `write_endotherm_inputs()`.

## Goal

Add one exported function, `get_endotherm_defaults()`, that returns the nine default groups bundled into a single named list, using the same names as `write_endotherm_inputs()`'s arguments — so the result can be inspected (`str()`), edited, and spliced back in with `do.call()`.

## Function signature

```r
get_endotherm_defaults(
  julnum  = 12,
  juldays = c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
)
```

Returns (invisibly is not needed here — this is a pure getter, not a file-writing/logging function):

```r
list(
  model_settings = list(...),
  animal         = list(...),
  fur            = list(...),
  physiology     = list(...),
  diet           = list(...),
  thermoreg      = list(...),
  flying_digging = list(...),
  nest_shelter   = list(...),
  allometry      = list(...)
)
```

## Behavior

- Calls the eight existing internal `.default_*()` helpers unchanged (`.default_model_settings()`, `.default_animal(julnum)`, `.default_fur(julnum)`, `.default_physiology(julnum)`, `.default_diet(julnum)`, `.default_thermoreg()`, `.default_flying_digging()`, `.default_nest_shelter()`, `.default_allometry()`) and bundles them into one list named to match `write_endotherm_inputs()`'s formals exactly.
- **julnum/juldays consistency:** `.default_model_settings()` is a zero-arg helper hardcoded to `julnum = 12` and a 12-element `juldays`. If a caller passes a different `julnum` without this function reconciling `model_settings`, the bundle would be internally inconsistent — `animal$mass2`/`fur$torlend`/etc. would be sized to the new `julnum` while `model_settings$julnum`/`juldays` stayed at their length-12 defaults, and `write_endotherm_inputs()`'s internal `.chk_vec_len()` checks (which read `julnum` from `model_settings$julnum`) would fail confusingly.

  To prevent that, `get_endotherm_defaults()`:
  1. Stops with a clear error if `length(juldays) != julnum` (mismatched arguments given by the caller).
  2. Overwrites `model_settings$julnum` and `model_settings$juldays` in the returned list with the function's own `julnum`/`juldays` arguments, so the bundle is internally consistent by construction regardless of what was passed.

- No file I/O, no log return — this is a plain getter, unlike the rest of the package's "always return a log" convention (which applies to functions that perform an action, not to querying defaults).

## Usage

```r
defaults <- get_endotherm_defaults()
defaults$animal$mass <- 60           # inspect and tweak in place
str(defaults$fur)                    # view a group

do.call(write_endotherm_inputs, c(list(output_dir = out_dir), defaults))
```

Because the returned list's names match `write_endotherm_inputs()`'s argument names exactly, `do.call()` works without any reshaping. Callers who only want to change one group can still pass it positionally/by name as before — `get_endotherm_defaults()` doesn't change `write_endotherm_inputs()`'s existing behavior at all.

## Out of scope

- Exporting the individual `.default_*()` helpers — kept internal per the package's `.`-prefix convention for internal helpers (`CLAUDE.md`).
- Any validation beyond the julnum/juldays length check — `write_endotherm_inputs()` already validates the rest at write time.

## Documentation

Full roxygen2 block per `CLAUDE.md` conventions: `@param` for `julnum` and `juldays` (noting the consistency behavior above), `@return` describing the nine-element list, `@export`, `@seealso write_endotherm_inputs`.

## Verification plan

1. `testthat`: `get_endotherm_defaults()`'s returned list has exactly the nine names `write_endotherm_inputs()` expects, in a shape `do.call()` can consume.
2. `testthat`: `do.call(write_endotherm_inputs, c(list(output_dir = tmp), get_endotherm_defaults()))` succeeds and writes `endo.dat`/`alomvars.dat` (round-trip with unmodified defaults).
3. `testthat`: `get_endotherm_defaults(julnum = 6, juldays = 1:12)` (mismatched lengths) errors.
4. `testthat`: `get_endotherm_defaults(julnum = 6, juldays = c(15,45,74,105,135,166))` succeeds and returns `animal$mass2` etc. of length 6, with `model_settings$julnum == 6`.
