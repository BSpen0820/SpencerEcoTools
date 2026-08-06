test_that(".default_mc_overrides returns the fixed 5-group override table", {
  ov <- .default_mc_overrides()
  expect_named(ov, c("model_settings", "physiology", "diet", "thermoreg", "flying_digging"))
  expect_equal(ov$model_settings$outout, "Y")
  expect_equal(ov$model_settings$microin, "CSV")
  expect_equal(ov$model_settings$outfile, "CSV")
  expect_equal(ov$model_settings$strht, "N")
  expect_equal(ov$physiology$sweat, "N")
  expect_equal(ov$physiology$pilo, "N")
  expect_equal(ov$diet$act, rep(1.0, 12))
  expect_equal(ov$diet$repro, rep(0.0, 12))
  expect_equal(ov$thermoreg$shdact, "Y")
  expect_equal(ov$thermoreg$shdpost, "S")
  expect_equal(ov$flying_digging$flight, "N")
})

test_that(".mc_scenario_overrides sets standing scenarios active all day with real posture/temp", {
  endo_inputs <- get_endotherm_defaults()
  ov <- .mc_scenario_overrides("standing_variable", endo_inputs)
  expect_equal(ov$diet$diurn, rep("Y", 12))
  expect_equal(ov$diet$noct, rep("Y", 12))
  expect_equal(ov$diet$crep, rep("Y", 12))
  expect_equal(ov$physiology, list())
  expect_equal(ov$allometry, list())
})

test_that(".mc_scenario_overrides sets curled scenarios inactive all day with forced posture 3", {
  endo_inputs <- get_endotherm_defaults()
  ov <- .mc_scenario_overrides("curled_variable", endo_inputs)
  expect_equal(ov$diet$diurn, rep("N", 12))
  expect_equal(ov$diet$noct, rep("N", 12))
  expect_equal(ov$diet$crep, rep("N", 12))
  expect_equal(ov$allometry$post3, "Y")
  expect_equal(ov$allometry$post4, "N")
  expect_equal(ov$allometry$slpstrt, 3)
  expect_equal(ov$allometry$shdstrt, 3)
  expect_equal(ov$allometry$endpost, 3)
  expect_equal(ov$physiology, list())
})

test_that(".mc_scenario_overrides narrows core temp to tcreg +/- 0.1 for constant scenarios", {
  endo_inputs <- get_endotherm_defaults()
  tcreg <- endo_inputs$physiology$tcreg
  ov <- .mc_scenario_overrides("curled_constant", endo_inputs)
  expect_equal(ov$physiology$tcmax, tcreg + 0.1)
  expect_equal(ov$physiology$tcmin, tcreg - 0.1)

  ov2 <- .mc_scenario_overrides("standing_constant", endo_inputs)
  expect_equal(ov2$physiology$tcmax, tcreg + 0.1)
  expect_equal(ov2$physiology$tcmin, tcreg - 0.1)
  expect_equal(ov2$allometry, list())
})

test_that(".mc_scenario_overrides leaves tcmax/tcmin untouched for variable scenarios", {
  endo_inputs <- get_endotherm_defaults()
  ov <- .mc_scenario_overrides("standing_variable", endo_inputs)
  expect_null(ov$physiology$tcmax)
  expect_null(ov$physiology$tcmin)
})

test_that(".mc_target_rmr uses the user-supplied metabolic rate when usrmet is Y", {
  endo_inputs <- get_endotherm_defaults()
  endo_inputs$animal$usrmet <- "Y"
  endo_inputs$animal$met <- 42
  expect_equal(.mc_target_rmr(endo_inputs), 42)
})

test_that(".mc_target_rmr computes the allometric formula for a non-marsupial mammal", {
  endo_inputs <- get_endotherm_defaults()
  endo_inputs$animal$usrmet <- "N"
  endo_inputs$animal$class <- "MAMMAL"
  endo_inputs$animal$marsup <- "N"
  endo_inputs$animal$mass <- 56.6
  expect_equal(.mc_target_rmr(endo_inputs), (70 * 56.6 ^ 0.75) * (4.185 / (24 * 3.6)))
})

test_that(".mc_target_rmr computes the marsupial formula variant", {
  endo_inputs <- get_endotherm_defaults()
  endo_inputs$animal$usrmet <- "N"
  endo_inputs$animal$class <- "MAMMAL"
  endo_inputs$animal$marsup <- "Y"
  endo_inputs$animal$mass <- 10
  expect_equal(.mc_target_rmr(endo_inputs), (2187 * 10 ^ 0.737) * (4.185 / 3600))
})

test_that(".mc_target_rmr warns and returns NA for a non-MAMMAL class", {
  endo_inputs <- get_endotherm_defaults()
  endo_inputs$animal$usrmet <- "N"
  endo_inputs$animal$class <- "BIRDIE"
  expect_warning(result <- .mc_target_rmr(endo_inputs), "MAMMAL")
  expect_true(is.na(result))
})

test_that(".mc_parse_dimensions extracts a 6-row body-part dimension table from a fake OUTPUT file", {
  tmp <- tempfile("OUTPUT_")
  lines <- rep("", 136)
  # Column 1 = label (ignored/overwritten for rows 4-6), columns 6:8 = the
  # three dimensions .mc_parse_dimensions keeps (dim_table[, c(1, 6:8)]).
  lines[131] <- "Head 0 0 0 0 0.10 0.11 0.12"
  lines[132] <- "Neck 0 0 0 0 0.20 0.21 0.22"
  lines[133] <- "Torso 0 0 0 0 0.30 0.31 0.32"
  lines[134] <- "Leg1 0 0 0 0 0.40 0.41 0.42"
  lines[135] <- "Leg2 0 0 0 0 0.50 0.51 0.52"
  lines[136] <- "Tail 0 0 0 0 0.60 0.61 0.62"
  # Column 1 = ignored, columns 11:16 = the six masses (t(mass_table[11:16])).
  lines[129] <- "x x x x x x x x x x 1.1 2.2 3.3 4.4 5.5 6.6"
  writeLines(lines, tmp)
  on.exit(unlink(tmp))

  dims <- .mc_parse_dimensions(tmp)

  expect_equal(nrow(dims), 6)
  expect_named(dims, c("Body Part", "Vertical or Side-to-Side Diameter (m)",
                        "Horizontal or Front-to-Back Diameter (m)", "Length (m)", "Mass (kg)"))
  expect_equal(dims[["Body Part"]], c("Head", "Neck", "Torso", "Front Legs",
                                       "Rear Legs", "6th Appendage (Tail/Proboscis)"))
  expect_equal(dims[["Vertical or Side-to-Side Diameter (m)"]], c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60))
  expect_equal(dims[["Mass (kg)"]], c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6))
})
