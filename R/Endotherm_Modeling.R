# Internal default parameter list for write_juldays_dat() ---------------------

.default_habitat_settings <- function(julnum) {
  list(
    startday  = 1,
    endday    = 365,
    absorp    = rep(0.8, julnum),
    shade_min = rep(15.6, julnum),
    shade_max = rep(16.6, julnum),
    surfwet   = rep(5, julnum),
    multihab  = "N"
  )
}

#' Write a NicheMapR Endotherm model JULDAYS.DAT input file
#'
#' Builds the fixed-format \code{JULDAYS.DAT} file required alongside
#' \code{endo.dat}/\code{alomvars.dat} (see \code{\link{write_endotherm_inputs}})
#' by the NicheMapR Endotherm model executable. Shares the same
#' \code{model_settings} list (specifically \code{julnum}/\code{juldays}) so the
#' day list in \code{JULDAYS.DAT} always matches the one written into
#' \code{endo.dat} — a mismatch between the two causes the exe's fixed-format
#' reader to misparse the file.
#'
#' @param output_dir Directory to write \code{JULDAYS.DAT} into. Written using
#'   this exact filename, because the exe hard-codes it in its working
#'   directory.
#' @param model_settings Named list with \code{julnum} and \code{juldays}
#'   (identical in meaning to the same-named argument of
#'   \code{\link{write_endotherm_inputs}} — pass the same list to both
#'   functions). \code{juldays} must have length \code{julnum}.
#' @param habitat_settings Named list of habitat/substrate settings:
#'   \code{startday, endday} (single integers, the overall simulation day-of-year
#'   bounds), \code{absorp} (substrate absorptivity, length \code{julnum}),
#'   \code{shade_min, shade_max} (minimum/maximum percent shade, each length
#'   \code{julnum}), \code{surfwet} (percent surface wet, length \code{julnum}),
#'   and \code{multihab} (\code{"Y"}/\code{"N"}, multiple habitats flag).
#'
#' @return Invisibly, a log \code{data.frame} with columns \code{file_path,
#'   step, status, timestamp}.
#'
#' @export
write_juldays_dat <- function(output_dir, model_settings = list(), habitat_settings = list()) {

  if (!dir.exists(output_dir))
    stop(sprintf("'output_dir' does not exist:\n  %s", output_dir))

  ms <- utils::modifyList(.default_model_settings(), model_settings)
  julnum <- ms$julnum
  .chk_vec_len(ms$juldays, julnum, "model_settings$juldays")

  hs <- utils::modifyList(.default_habitat_settings(julnum), habitat_settings)
  for (.v in list(list(hs$absorp, "habitat_settings$absorp"),
                  list(hs$shade_min, "habitat_settings$shade_min"),
                  list(hs$shade_max, "habitat_settings$shade_max"),
                  list(hs$surfwet, "habitat_settings$surfwet")))
    .chk_vec_len(.v[[1]], julnum, .v[[2]])

  row1  <- c("Julian Days Start Day End Day ", "\n")
  row2  <- c(" ---------- ------ -------- ", "\n")
  row3  <- c(paste(" ", julnum, hs$startday, hs$endday, ""), "\n")
  row4  <- c(" ", "\n")
  row5  <- c(" Julian Days ", "\n")
  row6  <- c(" ---------- ", "\n")
  row7  <- c(paste0(" ", paste(ms$juldays, collapse = " "), " "), "\n")
  row8  <- c(" ", "\n")
  row9  <- c(" Substrate Absorptivity ", "\n")
  row10 <- c(" ---------- ", "\n")
  row11 <- c(paste0(" ", paste(hs$absorp, collapse = " "), " "), "\n")
  row12 <- c(" ", "\n")
  row13 <- c(" Percent Shade ", "\n")
  row14 <- c(" ---------- ", "\n")
  row15 <- c(paste0(" ", paste(hs$shade_min, collapse = " "), " "), "\n")
  row16 <- c(paste0(" ", paste(hs$shade_max, collapse = " "), " "), "\n")
  row17 <- c(" ", "\n")
  row18 <- c(" Percent Surface Wet ", "\n")
  row19 <- c(" ---------- ", "\n")
  row20 <- c(paste0(" ", paste(hs$surfwet, collapse = " "), " "), "\n")
  row21 <- c(" ", "\n")
  row22 <- c(" Multiple habitats (Y/N) ", "\n")
  row23 <- c(" ---------- ", "\n")
  row24 <- c(paste0(" ", sQuote(hs$multihab, q = FALSE)), "\n")

  juldays_path <- file.path(output_dir, "JULDAYS.DAT")
  cat(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10,
      row11, row12, row13, row14, row15, row16, row17, row18, row19, row20,
      row21, row22, row23, row24,
      file = juldays_path, sep = "")

  log_df <- data.frame(
    file_path = juldays_path,
    step      = "write_juldays_dat",
    status    = "success",
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    stringsAsFactors = FALSE
  )
  invisible(log_df)
}

#' Split a total day count into consecutive equal-size chunks
#'
#' Used to break a multi-day simulation into consecutive blocks (each run as
#' one Endotherm model invocation, with \code{model_settings$julnum} set to
#' \code{chunk_size}) so that, e.g., animal mass can be updated between chunks
#' from one chunk's output before running the next.
#'
#' @param total_days Integer. Total number of days to cover. Must be evenly
#'   divisible by \code{chunk_size}.
#' @param chunk_size Integer. Number of days per chunk (i.e. the
#'   \code{julnum}/\code{juldays} length to use for each Endotherm model
#'   invocation).
#'
#' @return A list of integer vectors, each of length \code{chunk_size},
#'   giving the consecutive day-of-year numbers for that chunk. Together they
#'   cover \code{1:total_days} in order.
#'
#' @export
chunk_days <- function(total_days, chunk_size) {
  if (total_days <= 0 || chunk_size <= 0) {
    stop("total_days and chunk_size must both be positive")
  }
  if (total_days %% chunk_size != 0) {
    stop(sprintf(
      "total_days (%d) must be a multiple of chunk_size (%d)",
      total_days, chunk_size
    ))
  }
  n_chunks <- total_days %/% chunk_size
  lapply(seq_len(n_chunks), function(i) {
    start <- (i - 1L) * chunk_size + 1L
    start:(start + chunk_size - 1L)
  })
}

#' Read valid cell indices from a 1/0 mask raster
#'
#' Identifies which cells of a gridded domain have data to process (value
#' \code{1}) versus not (value \code{0}), for domains where not every cell in
#' the bounding extent was actually run (e.g. a sparse/tiled climate dataset).
#'
#' @param mask_path Path to a raster file readable by \code{terra::rast()}
#'   (e.g. \code{.tif}, \code{.nc}) with cell values of \code{0} or \code{1}.
#'
#' @return Integer vector of 1-indexed cell numbers (in \code{terra}'s native
#'   row-major cell order) where the mask value is \code{1}.
#'
#' @export
read_valid_cell_indices <- function(mask_path) {
  r <- terra::rast(mask_path)
  vals <- terra::values(r, mat = FALSE)
  which(vals == 1)
}

#' Distribute cells round-robin across a SLURM array
#'
#' Mirrors the \code{clust_array_arg}/\code{clust_array_size} SLURM convention
#' used by \code{\link{run_micro_big_nichemap}}: the full set of cells is
#' enumerated and distributed round-robin across \code{clust_array_size} array
#' tasks, and only the current task's subset is returned.
#'
#' @param valid_cell_indices Integer vector of cell indices to distribute
#'   (e.g. from \code{\link{read_valid_cell_indices}}).
#' @param clust_array_arg Integer. Value of \code{$SLURM_ARRAY_TASK_ID} for
#'   this node (1-based). \code{NULL} (default) returns every cell — use this
#'   when not running under a SLURM array.
#' @param clust_array_size Integer. Total number of array tasks. Required
#'   when \code{clust_array_arg} is set.
#'
#' @return An integer vector, the subset of \code{valid_cell_indices} assigned
#'   to this array task.
#'
#' @export
cells_for_array_task <- function(valid_cell_indices, clust_array_arg = NULL, clust_array_size = NULL) {
  if (!is.null(clust_array_arg) &&
      (!is.numeric(clust_array_arg) || length(clust_array_arg) != 1))
    stop("clust_array_arg must be a single numeric value or NULL")
  if (!is.null(clust_array_size) &&
      (!is.numeric(clust_array_size) || length(clust_array_size) != 1))
    stop("clust_array_size must be a single numeric value or NULL")
  if (!is.null(clust_array_arg) && is.null(clust_array_size))
    stop("clust_array_size must be provided when clust_array_arg is set")
  if (!is.null(clust_array_arg) &&
      (clust_array_arg < 1 || clust_array_arg > clust_array_size))
    stop("clust_array_arg must be between 1 and clust_array_size")

  node <- if (is.null(clust_array_size)) rep(1L, length(valid_cell_indices)) else
    rep(seq_len(clust_array_size), length.out = length(valid_cell_indices))

  if (is.null(clust_array_arg)) valid_cell_indices else valid_cell_indices[node == clust_array_arg]
}

#' Run the NicheMapR Endotherm model executable
#'
#' Invokes the compiled Endotherm model exe in \code{workspace_dir}, which
#' must already contain \code{alomvars.dat}, \code{endo.dat}, \code{JULDAYS.DAT},
#' and the microclimate driver CSVs (\code{metout.csv}, \code{shadmet.csv},
#' \code{soil.csv}, \code{shadsoil.csv}) it expects to read from its working
#' directory. On Windows the exe is run natively; elsewhere it is run under
#' Wine with a per-process \code{WINEPREFIX} (required because Wine prefixes
#' are not safe to share across concurrent processes).
#'
#' @param workspace_dir Directory containing the exe and its input files.
#' @param exe_name Character. Executable filename, expected to already be
#'   present in \code{workspace_dir}. Default \code{"Endo2022a.exe"}.
#' @param sysname Character, one of \code{Sys.info()[["sysname"]]}'s possible
#'   values. Determines native vs Wine invocation. Default detects the
#'   current OS.
#'
#' @return A list with elements \code{success} (logical) and \code{message}
#'   (character, the contents of \code{ErrorMsgs.dat}, or a description of
#'   why the run could not be evaluated).
#'
#' @export
run_endotherm_model <- function(workspace_dir, exe_name = "Endo2022a.exe",
                                 sysname = Sys.info()[["sysname"]]) {
  exe_path <- file.path(workspace_dir, exe_name)
  if (!file.exists(exe_path)) {
    return(list(success = FALSE, message = sprintf("exe not found at %s", exe_path)))
  }

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(workspace_dir)

  if (identical(sysname, "Windows")) {
    system2(exe_name, input = c("alomvars.dat", "endo.dat"), stdout = TRUE, stderr = TRUE)
  } else {
    # Wine refuses to create its config dir under a prefix whose nearest
    # existing ancestor it doesn't own - true for bare "/tmp" in non-root
    # containers (Docker/Apptainer commonly run as an unprivileged user
    # while /tmp itself stays root-owned). tempdir() is created by this R
    # session and so is always owned by the current user; pre-creating the
    # prefix dir under it (rather than letting Wine discover "/tmp" as the
    # nearest existing ancestor) avoids the ownership check entirely.
    wineprefix <- file.path(tempdir(), paste0("wineprefix_", Sys.getpid()))
    dir.create(wineprefix, recursive = TRUE, showWarnings = FALSE)
    system(
      paste0(
        "printf 'alomvars.dat\\nendo.dat\\n' | ",
        "WINEPREFIX=", wineprefix, " wine ", exe_name
      ),
      intern = TRUE
    )
  }

  error_msgs_path <- file.path(workspace_dir, "ErrorMsgs.dat")
  if (!file.exists(error_msgs_path)) {
    return(list(success = FALSE, message = "ErrorMsgs.dat was not produced"))
  }
  error_msgs <- readLines(error_msgs_path, warn = FALSE)
  list(
    success = any(grepl("Calculations completed\\.", error_msgs)),
    message = paste(error_msgs, collapse = " | ")
  )
}

# Internal helpers for run_metabolic_chamber() --------------------------------

.mc_scenario_ids <- c("standing_variable", "curled_variable",
                       "curled_constant", "standing_constant")

.mc_ramp_juldays <- c(15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)

.default_mc_overrides <- function() {
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
}

.mc_scenario_overrides <- function(scenario_id, endo_inputs) {
  active <- if (grepl("^standing", scenario_id)) "Y" else "N"

  posture <- if (grepl("^curled", scenario_id)) {
    list(post3 = "Y", post4 = "N", slpstrt = 3, shdstrt = 3, endpost = 3)
  } else {
    list()
  }

  temp <- if (grepl("constant$", scenario_id)) {
    tcreg <- endo_inputs$physiology$tcreg
    list(tcmax = tcreg + 0.1, tcmin = tcreg - 0.1)
  } else {
    list()
  }

  list(
    diet       = list(diurn = rep(active, 12), noct = rep(active, 12), crep = rep(active, 12)),
    physiology = temp,
    allometry  = posture
  )
}

.mc_target_rmr <- function(endo_inputs) {
  an <- endo_inputs$animal
  if (identical(an$usrmet, "Y")) {
    return(an$met)
  }
  if (!identical(an$class, "MAMMAL")) {
    warning(sprintf(
      "target RMR formula is only implemented for class 'MAMMAL' (got '%s'); target_rmr$trgt will be NA",
      an$class
    ))
    return(NA_real_)
  }
  if (identical(an$marsup, "Y")) {
    (2187 * an$mass ^ 0.737) * (4.185 / 3600)
  } else {
    (70 * an$mass ^ 0.75) * (4.185 / (24 * 3.6))
  }
}

.mc_parse_dimensions <- function(output_path) {
  out <- readLines(output_path, warn = FALSE)
  dimensions <- out[131:136]
  dim_table <- utils::read.table(textConnection(dimensions), header = FALSE)
  masses <- out[129]
  mass_table <- utils::read.table(textConnection(masses), header = FALSE)
  mass_table <- t(mass_table[11:16])
  dim_table <- dim_table[, c(1, 6:8)]
  dim_table$mass <- mass_table[, 1]
  colnames(dim_table) <- c("Body Part", "Vertical or Side-to-Side Diameter (m)",
                            "Horizontal or Front-to-Back Diameter (m)", "Length (m)", "Mass (kg)")
  dim_table[4, 1] <- "Front Legs"
  dim_table[5, 1] <- "Rear Legs"
  dim_table[6, 1] <- "6th Appendage (Tail/Proboscis)"
  dim_table
}

#' Run NicheMapR Endotherm metabolic chamber calibration scenarios
#'
#' Exposes an animal model built from \code{endo_inputs} to a synthetic,
#' hand-crafted temperature ramp (\code{\link{metchamber_metout}}/
#' \code{\link{metchamber_soil}}) across up to 4 canonical scenarios -
#' \{standing, curled\} x \{variable, constant core temperature\} - the way a
#' real metabolic chamber experiment would, and returns the resulting
#' metabolic-rate-vs-temperature data. Each scenario is built independently
#' in its own temporary directory via \code{\link{write_endotherm_inputs}}/
#' \code{\link{write_juldays_dat}}/\code{\link{run_endotherm_model}} - unlike
#' the source script this is ported from, it never reads or overwrites any
#' real \code{endo.dat}/\code{alomvars.dat} already on disk.
#'
#' @param endo_inputs Named list in the same 9-group shape
#'   \code{\link{write_endotherm_inputs}}/\code{\link{get_endotherm_defaults}}
#'   use (\code{model_settings, animal, fur, physiology, diet, thermoreg,
#'   flying_digging, nest_shelter, allometry}) - the animal's real parameters.
#'   \code{model_settings$julnum}/\code{juldays} are always overridden to
#'   \code{12}/the bundled ramp's julian days, regardless of what's supplied
#'   here.
#' @param exe_path Full path to the Endotherm model executable (any
#'   version/filename).
#' @param scenarios Character vector, subset/reorder of
#'   \code{c("standing_variable", "curled_variable", "curled_constant",
#'   "standing_constant")}. Default runs all 4.
#' @param mc_overrides Optional named list, same 9-group shape, merged over
#'   the fixed metabolic-chamber override table (disabled sweating/panting/
#'   diving/burrowing/flying, etc. - see \code{@details}) before each
#'   scenario's own overrides are applied. Changing these changes what
#'   "metabolic chamber simulation" means scientifically for this run.
#' @param save_dir Optional directory. If given, each scenario's
#'   \code{endo.dat}/\code{alomvars.dat} (as actually run) is copied there as
#'   \code{{scenario}_endo.dat}/\code{{scenario}_alomvars.dat}.
#' @param sysname Passed through to \code{\link{run_endotherm_model}}
#'   (Windows vs. Wine invocation). Default detects the current OS.
#'
#' @return An object of class \code{"metchamber_result"}: a list with
#'   \code{hourplot} (named list of data frames, one per scenario that
#'   actually succeeded - the relevant hours of \code{HOURPLOT.csv}),
#'   \code{dimensions} (data frame, body-part dimensions parsed from one
#'   successful scenario's \code{OUTPUT} file - scenario-invariant),
#'   \code{target_rmr} (list with \code{trgt} and \code{err}, for plotting
#'   reference lines), and \code{log} (data frame: \code{scenario, success,
#'   message, timestamp}, one row per requested scenario).
#'
#' @details
#' Every scenario always applies this fixed override table on top of
#' \code{endo_inputs} (before \code{mc_overrides} or scenario-specific
#' fields): \code{model_settings} \code{outout/microin/outfile = "Y"/"CSV"/"CSV"},
#' \code{strht = "N"}; \code{physiology} \code{sweat/pilo = "N"};
#' \code{diet} \code{act = rep(1.0, 12)}, \code{repro = rep(0.0, 12)};
#' \code{thermoreg} \code{burrow/nest/climb/shdseek/dive/wind/niteshd/dive2 =
#' "N"}, \code{shdact = "Y"}, \code{shdpost = "S"}, \code{treeslp/hudl/
#' tcconcur/tcconcur2 = "N"}; \code{flying_digging}
#' \code{flight/foss/dig/arb = "N"}. If a scenario's exe run fails, a
#' \code{warning()} is issued and that scenario is omitted from
#' \code{$hourplot} (recorded as a failure in \code{$log}); the function only
#' \code{stop()}s if every requested scenario fails.
#'
#' @seealso \code{\link{write_endotherm_inputs}}, \code{\link{get_endotherm_defaults}},
#'   \code{\link{run_endotherm_model}}, \code{\link{plot.metchamber_result}}
#' @export
run_metabolic_chamber <- function(endo_inputs, exe_path,
                                   scenarios = .mc_scenario_ids,
                                   mc_overrides = list(),
                                   save_dir = NULL,
                                   sysname = Sys.info()[["sysname"]]) {

  if (!all(scenarios %in% .mc_scenario_ids))
    stop(sprintf("'scenarios' must be from: %s", paste(.mc_scenario_ids, collapse = ", ")))
  if (length(scenarios) == 0)
    stop(sprintf("'scenarios' must name at least one of: %s", paste(.mc_scenario_ids, collapse = ", ")))
  if (!file.exists(exe_path))
    stop(sprintf("'exe_path' does not exist:\n  %s", exe_path))
  if (!is.null(save_dir) && !dir.exists(save_dir))
    stop(sprintf("'save_dir' does not exist:\n  %s", save_dir))

  required_groups <- c("model_settings", "animal", "fur", "physiology", "diet",
                        "thermoreg", "flying_digging", "nest_shelter", "allometry")
  missing_groups <- setdiff(required_groups, names(endo_inputs))
  if (length(missing_groups) > 0)
    stop(sprintf("'endo_inputs' is missing required group(s): %s", paste(missing_groups, collapse = ", ")))

  if (!is.null(endo_inputs$model_settings$julnum) && endo_inputs$model_settings$julnum != 12) {
    message(sprintf(
      "run_metabolic_chamber() always uses julnum = 12 (tied to the bundled temperature-ramp data) - overriding endo_inputs$model_settings$julnum (%d). If endo_inputs' other per-julnum vectors (e.g. animal$mass2) were sized to a different julnum, rebuild endo_inputs with julnum = 12 (e.g. get_endotherm_defaults(julnum = 12)) to avoid a length-mismatch error below.",
      endo_inputs$model_settings$julnum
    ))
  }

  exe_name <- basename(exe_path)
  fixed_overrides <- utils::modifyList(.default_mc_overrides(), mc_overrides)
  target_rmr_trgt <- .mc_target_rmr(endo_inputs)  # compute once, before the loop - warns early if class isn't MAMMAL

  hourplot <- list()
  log_rows <- list()
  dimensions <- NULL
  scenario_dirs <- character(0)
  on.exit(unlink(scenario_dirs, recursive = TRUE), add = TRUE)

  for (scenario_id in scenarios) {
    scenario_dir <- tempfile(paste0("mc_", scenario_id, "_"))
    dir.create(scenario_dir)
    scenario_dirs <- c(scenario_dirs, scenario_dir)

    scen_overrides <- .mc_scenario_overrides(scenario_id, endo_inputs)

    merged <- endo_inputs
    for (grp in names(fixed_overrides))
      merged[[grp]] <- utils::modifyList(merged[[grp]], fixed_overrides[[grp]])
    for (grp in names(scen_overrides))
      merged[[grp]] <- utils::modifyList(merged[[grp]], scen_overrides[[grp]])
    merged$model_settings <- utils::modifyList(
      merged$model_settings, list(julnum = 12, juldays = .mc_ramp_juldays)
    )

    write_endotherm_inputs(
      output_dir     = scenario_dir,
      model_settings = merged$model_settings,
      animal         = merged$animal,
      fur            = merged$fur,
      physiology     = merged$physiology,
      diet           = merged$diet,
      thermoreg      = merged$thermoreg,
      flying_digging = merged$flying_digging,
      nest_shelter   = merged$nest_shelter,
      allometry      = merged$allometry
    )

    write_juldays_dat(
      scenario_dir,
      model_settings   = list(julnum = 12, juldays = .mc_ramp_juldays),
      habitat_settings = list(startday = 1, endday = 365,
                               absorp = rep(0.8, 12), shade_min = rep(0, 12),
                               shade_max = rep(100, 12), surfwet = rep(5, 12),
                               multihab = "N")
    )

    utils::write.csv(metchamber_metout, file.path(scenario_dir, "metout.csv"), row.names = FALSE)
    utils::write.csv(metchamber_metout, file.path(scenario_dir, "shadmet.csv"), row.names = FALSE)
    utils::write.csv(metchamber_soil, file.path(scenario_dir, "soil.csv"), row.names = FALSE)
    utils::write.csv(metchamber_soil, file.path(scenario_dir, "shadsoil.csv"), row.names = FALSE)

    if (!is.null(save_dir)) {
      file.copy(file.path(scenario_dir, "endo.dat"),
                file.path(save_dir, sprintf("%s_endo.dat", scenario_id)), overwrite = TRUE)
      file.copy(file.path(scenario_dir, "alomvars.dat"),
                file.path(save_dir, sprintf("%s_alomvars.dat", scenario_id)), overwrite = TRUE)
    }

    file.copy(exe_path, file.path(scenario_dir, exe_name))

    run_result <- run_endotherm_model(scenario_dir, exe_name = exe_name, sysname = sysname)

    log_rows[[scenario_id]] <- data.frame(
      scenario  = scenario_id,
      success   = run_result$success,
      message   = run_result$message,
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      stringsAsFactors = FALSE
    )

    if (!run_result$success) {
      warning(sprintf("run_metabolic_chamber: scenario '%s' failed: %s", scenario_id, run_result$message))
      next
    }

    hourplot_path <- file.path(scenario_dir, "HOURPLOT.csv")
    if (!file.exists(hourplot_path)) {
      warning(sprintf(
        "run_metabolic_chamber: scenario '%s' succeeded but HOURPLOT.csv was not produced", scenario_id
      ))
      next
    }
    hp <- utils::read.csv(hourplot_path, skip = 1)
    hourplot[[scenario_id]] <- hp[c(2:23, 27:48, 52:73, 77:96), ]

    if (is.null(dimensions)) {
      dim_result <- tryCatch(
        .mc_parse_dimensions(file.path(scenario_dir, "OUTPUT")),
        error = function(e) {
          warning(sprintf(
            "run_metabolic_chamber: could not parse body-part dimensions from scenario '%s': %s",
            scenario_id, conditionMessage(e)
          ))
          NULL
        }
      )
      if (!is.null(dim_result)) dimensions <- dim_result
    }
  }

  if (length(hourplot) == 0) {
    failures <- vapply(log_rows, function(r) sprintf("%s: %s", r$scenario, r$message), character(1))
    stop(sprintf(
      "run_metabolic_chamber: every requested scenario failed:\n  %s",
      paste(failures, collapse = "\n  ")
    ))
  }

  log_df <- do.call(rbind, log_rows)
  rownames(log_df) <- NULL

  structure(
    list(
      hourplot   = hourplot,
      dimensions = dimensions,
      target_rmr = list(trgt = target_rmr_trgt, err = endo_inputs$model_settings$err),
      log        = log_df
    ),
    class = "metchamber_result"
  )
}

.mc_comparison_plot <- function(standing_hp, curled_hp, target_rmr, title) {
  standing_hp$Posture <- "Standing"
  curled_hp$Posture <- "Curled"
  combined <- rbind(standing_hp, curled_hp)

  ymax <- max(combined[["MET.W."]], na.rm = TRUE) * 1.05

  ggplot2::ggplot(
    combined,
    ggplot2::aes(x = .data[["TAIR"]], y = .data[["MET.W."]], linetype = .data[["Posture"]])
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_hline(yintercept = target_rmr$trgt * (1 + target_rmr$err),
                         color = "red", linetype = "dashed") +
    ggplot2::geom_hline(yintercept = target_rmr$trgt * (1 - target_rmr$err),
                         color = "blue", linetype = "dashed") +
    ggplot2::coord_cartesian(ylim = c(0, ymax)) +
    ggplot2::labs(title = title, x = "Temperature (C)", y = "Metabolic Rate (W)") +
    ggplot2::theme_minimal()
}

#' Plot metabolic chamber calibration results
#'
#' Builds up to two comparison charts (predicted metabolic rate vs. air
#' temperature, standing vs. curled posture) from a
#' \code{\link{run_metabolic_chamber}} result: one for the variable-core-temp
#' scenarios, one for the constant-core-temp scenarios. Each includes
#' reference lines at the animal's target resting metabolic rate +/- its
#' error margin. A chart is only built if both scenarios in its pair are
#' present in \code{x$hourplot}.
#'
#' @param x A \code{"metchamber_result"} object, as returned by
#'   \code{\link{run_metabolic_chamber}}.
#' @param ... Unused, present for S3 generic compatibility.
#'
#' @return Invisibly, a named list of the ggplot2 objects actually built
#'   (\code{variable_temp}, \code{constant_temp} - only the ones with a
#'   complete scenario pair), so they can be further modified or saved with
#'   \code{ggplot2::ggsave()}. Returns \code{invisible(NULL)} (with a
#'   \code{message()}) if no complete pair is available.
#'
#' @method plot metchamber_result
#' @export
plot.metchamber_result <- function(x, ...) {
  plots <- list()

  if (all(c("standing_variable", "curled_variable") %in% names(x$hourplot))) {
    plots$variable_temp <- .mc_comparison_plot(
      x$hourplot$standing_variable, x$hourplot$curled_variable,
      x$target_rmr, "Metabolic Chamber: Variable Core Temperature"
    )
  }
  if (all(c("standing_constant", "curled_constant") %in% names(x$hourplot))) {
    plots$constant_temp <- .mc_comparison_plot(
      x$hourplot$standing_constant, x$hourplot$curled_constant,
      x$target_rmr, "Metabolic Chamber: Constant Core Temperature"
    )
  }

  if (length(plots) == 0) {
    message("plot.metchamber_result: no complete standing/curled scenario pair available to plot")
    return(invisible(NULL))
  }

  for (p in plots) print(p)
  invisible(plots)
}
