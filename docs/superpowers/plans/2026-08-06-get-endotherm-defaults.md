# get_endotherm_defaults() Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add an exported `get_endotherm_defaults()` function that bundles the nine internal default-parameter groups used by `write_endotherm_inputs()` into one inspectable, editable R list, so a caller can view/tweak the defaults and pass them straight back in via `do.call()`.

**Architecture:** One new exported function in `R/Endotherm_DataPrep.R`, placed directly above `write_endotherm_inputs()` (before line 263's roxygen block). It calls the eight existing `.default_*()` helpers already defined at the top of that file (lines 10-256) and wraps them in a named list matching `write_endotherm_inputs()`'s argument names. No new files, no new dependencies.

**Tech Stack:** R, `testthat` 3e (existing test suite in `tests/testthat/`), roxygen2 for docs.

## Global Constraints

- Internal helpers stay `.`-prefixed, no `@export`, minimal comments (per `CLAUDE.md`) — this task adds one exported wrapper, not exports of the individual `.default_*()` helpers.
- Full roxygen2 block required for the exported function: `@param`, `@return`, `@details`, `@export`, `@seealso` (per `CLAUDE.md`).
- `stop()` for unrecoverable errors (per `CLAUDE.md`) — used here for the julnum/juldays length mismatch.
- Run `devtools::document()` after the roxygen changes (per `CLAUDE.md`).
- Spec: `docs/superpowers/specs/2026-08-06-get-endotherm-defaults-design.md`.

---

### Task 1: `get_endotherm_defaults()`

**Files:**
- Modify: `R/Endotherm_DataPrep.R` (insert new function after `.chk_vec_len()` at line 261, before the `write_endotherm_inputs()` roxygen block currently starting at line 263)
- Test: `tests/testthat/test-get_endotherm_defaults.R` (new file)

**Interfaces:**
- Consumes: existing internal helpers already defined earlier in `R/Endotherm_DataPrep.R` — `.default_model_settings()`, `.default_animal(julnum)`, `.default_fur(julnum)`, `.default_physiology(julnum)`, `.default_diet(julnum)`, `.default_thermoreg()`, `.default_flying_digging()`, `.default_nest_shelter()`, `.default_allometry()`. None of these take a `juldays` argument or return `julnum`/`juldays` fields except `.default_model_settings()`, which hardcodes `julnum = 12` and a 12-element `juldays`.
- Produces: `get_endotherm_defaults(julnum = 12, juldays = c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349))` — returns a plain (non-invisible) named `list()` with exactly these nine names, in this order: `model_settings, animal, fur, physiology, diet, thermoreg, flying_digging, nest_shelter, allometry`. `model_settings$julnum` and `model_settings$juldays` in the returned list are overwritten to equal the function's own `julnum`/`juldays` arguments (not `.default_model_settings()`'s hardcoded values). Errors via `stop()` if `length(juldays) != julnum`, with a message containing `"must have length"` (matching the existing `.chk_vec_len()` message style used elsewhere in this file).

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-get_endotherm_defaults.R`:

```r
test_that("get_endotherm_defaults returns the nine groups write_endotherm_inputs expects", {
  defaults <- get_endotherm_defaults()

  expect_named(
    defaults,
    c("model_settings", "animal", "fur", "physiology", "diet",
      "thermoreg", "flying_digging", "nest_shelter", "allometry")
  )
})

test_that("get_endotherm_defaults's model_settings matches the julnum/juldays arguments by default", {
  defaults <- get_endotherm_defaults()

  expect_identical(defaults$model_settings$julnum, 12)
  expect_identical(
    defaults$model_settings$juldays,
    c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
  )
  expect_length(defaults$animal$mass2, 12)
  expect_length(defaults$diet$digef, 12)
})

test_that("get_endotherm_defaults resizes julnum-dependent vectors for a custom julnum", {
  custom_days <- c(15, 45, 74, 105, 135, 166)
  defaults <- get_endotherm_defaults(julnum = 6, juldays = custom_days)

  expect_identical(defaults$model_settings$julnum, 6)
  expect_identical(defaults$model_settings$juldays, custom_days)
  expect_length(defaults$animal$mass2, 6)
  expect_length(defaults$fur$torlend, 6)
  expect_length(defaults$physiology$tcreg2, 6)
  expect_length(defaults$diet$digef, 6)
})

test_that("get_endotherm_defaults errors when juldays length doesn't match julnum", {
  expect_error(
    get_endotherm_defaults(julnum = 6, juldays = 1:12),
    "must have length"
  )
})

test_that("get_endotherm_defaults's output round-trips through write_endotherm_inputs via do.call", {
  tmp_dir <- tempfile("endo_defaults_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  defaults <- get_endotherm_defaults()
  log <- do.call(write_endotherm_inputs, c(list(output_dir = tmp_dir), defaults))

  expect_true(file.exists(file.path(tmp_dir, "endo.dat")))
  expect_true(file.exists(file.path(tmp_dir, "alomvars.dat")))
  expect_true(all(log$status == "success"))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-get_endotherm_defaults.R')"`
Expected: FAIL — `could not find function "get_endotherm_defaults"`

- [ ] **Step 3: Implement `get_endotherm_defaults()`**

Insert into `R/Endotherm_DataPrep.R` immediately after `.chk_vec_len()` (after line 261), before the `write_endotherm_inputs()` roxygen block:

```r
#' Get the default parameter set for write_endotherm_inputs()
#'
#' Returns the same internal default values that \code{write_endotherm_inputs()}
#' falls back to for any argument group left as \code{list()}, bundled into a
#' single named list so they can be inspected, edited, and passed back in.
#'
#' @param julnum Number of julian days in the model run. Defaults to \code{12}
#'   (one per month, matching the reference Female Bighorn Sheep example).
#'   Any julnum-dependent vector field in the returned \code{animal, fur,
#'   physiology, diet} groups (e.g. \code{animal$mass2}, \code{diet$digef})
#'   is sized to this value.
#' @param juldays Numeric vector of julian day numbers, length \code{julnum}.
#'   Defaults to the reference Female Bighorn Sheep example's 12 monthly
#'   midpoints. Written into the returned \code{model_settings$juldays} (and
#'   \code{julnum} into \code{model_settings$julnum}), overriding
#'   \code{write_endotherm_inputs()}'s own internal default for that group so
#'   the returned bundle stays internally consistent.
#'
#' @return A named \code{list()} with nine elements — \code{model_settings,
#'   animal, fur, physiology, diet, thermoreg, flying_digging, nest_shelter,
#'   allometry} — using the same names as \code{write_endotherm_inputs()}'s
#'   arguments, suitable for editing and passing back in with
#'   \code{do.call(write_endotherm_inputs, c(list(output_dir = ...), defaults))}.
#'
#' @details
#' \code{write_endotherm_inputs()} builds each of its nine argument groups by
#' merging a caller-supplied \code{list()} onto an internal, unexported
#' default. This function exposes those defaults directly so they don't have
#' to be read out of the package source to be inspected or modified.
#'
#' @examples
#' \dontrun{
#'   defaults <- get_endotherm_defaults()
#'   str(defaults$animal)
#'
#'   # Tweak a couple of fields, then write inputs using the edited bundle
#'   defaults$animal$mass <- 60
#'   defaults$animal$species <- "Male Bighorn"
#'   do.call(write_endotherm_inputs, c(list(output_dir = "working_dir"), defaults))
#' }
#'
#' @seealso write_endotherm_inputs
#' @export
get_endotherm_defaults <- function(julnum = 12,
                                    juldays = c(15, 45, 74, 105, 135, 166,
                                                196, 227, 258, 288, 319, 349)) {
  .chk_vec_len(juldays, julnum, "juldays")

  ms <- .default_model_settings()
  ms$julnum <- julnum
  ms$juldays <- juldays

  list(
    model_settings = ms,
    animal         = .default_animal(julnum),
    fur            = .default_fur(julnum),
    physiology     = .default_physiology(julnum),
    diet           = .default_diet(julnum),
    thermoreg      = .default_thermoreg(),
    flying_digging = .default_flying_digging(),
    nest_shelter   = .default_nest_shelter(),
    allometry      = .default_allometry()
  )
}
```

- [ ] **Step 4: Run `devtools::document()` to regenerate NAMESPACE/man**

Run: `Rscript -e "devtools::document()"`
Expected: Completes without error; `NAMESPACE` gains `export(get_endotherm_defaults)`; `man/get_endotherm_defaults.Rd` is created.

- [ ] **Step 5: Run the tests to verify they pass**

Run: `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-get_endotherm_defaults.R')"`
Expected: All 5 tests PASS.

- [ ] **Step 6: Run the full test suite to check for regressions**

Run: `Rscript -e "devtools::test()"`
Expected: No new failures (in particular `test-write_endotherm_inputs`-adjacent tests in `test-run_endotherm_model.R` and `test-write_juldays_dat.R` still pass — this task doesn't modify `write_endotherm_inputs()` or `.default_*()` itself).

- [ ] **Step 7: Commit**

```bash
git add R/Endotherm_DataPrep.R NAMESPACE man/get_endotherm_defaults.Rd tests/testthat/test-get_endotherm_defaults.R
git commit -m "feat: add get_endotherm_defaults() to inspect/edit write_endotherm_inputs() defaults"
```
