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
    wineprefix <- paste0("/tmp/wineprefix_", Sys.getpid())
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
