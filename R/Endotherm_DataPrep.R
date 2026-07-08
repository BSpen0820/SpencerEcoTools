# Internal default parameter lists for write_endotherm_inputs() ---------------
# All defaults reproduce a Female Bighorn Sheep example, extracted from a real
# NicheMapR Endotherm run: D:/Code/PhD/Side Projects/SheepMigrationLoss/
# Endotherm Model/Sheep_MetabolicChamber/{endo.dat,alomvars.dat}.
# Fields with no equivalent in that reference (the huddle/tree-sleep block,
# which was absent from that file; a couple of fields hard-coded as literals
# in this script's row-writer, e.g. fur "tran" and the endo.dat "DEPEND"
# value) retain their prior (Chacma Baboon example) defaults.

.default_model_settings <- function() {
  list(
    julnum   = 12,
    juldays  = c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349),
    hrout    = "Y",
    outout   = "Y",
    microin  = "CSV",
    outfile  = "CSV",
    outunits = "KJ",
    strht    = "N",
    geom     = 1,
    geomult  = 3,
    apnd     = "4APNDG",
    ventpct  = 0.0,
    inccond  = "N",
    frcmpr   = 0.5,
    usralom  = "Y",
    actht    = "Y",
    err      = 0.01,
    acthrs   = 2,
    minfrg   = 1.0,
    nrght    = 0.8,
    prdht    = 0.25,
    fasky    = 0.5,
    fagrd    = 0.3,
    faobj    = 0.0,
    usrnure  = "N",
    afrnt    = 0.37,
    bfrnt    = 0.66,
    aside    = 0.42,
    bside    = 0.75
  )
}

.default_animal <- function(julnum) {
  list(
    species    = "Female Bighorn",
    class      = "MAMMAL",
    marsup     = "N",
    cp         = 3073,
    mass       = 56.6,
    timdepmass = 0,
    mass2      = rep(56.6, julnum),
    fatpct     = 18.4,
    timdepfat  = 0,
    fatpct2    = rep(18.4, julnum),
    subqfat    = "Y",
    density    = 1033.6,
    usrmet     = "Y",
    met        = 90.4
  )
}

.default_fur <- function(julnum) {
  leg       <- list(diad = 98.7,  diav = 105.7, lend = 15.1, lenv = 13.9, depd = 7.0,  depv = 3.9,  dend = 592, denv = 814, refld = 0.425, reflv = 0.472)
  head_neck <- list(diad = 208.3, diav = 251.7, lend = 38.1, lenv = 38.1, depd = 40.6, depv = 33.1, dend = 717, denv = 563, refld = 0.534, reflv = 0.46)
  torso     <- list(diad = 201.0, diav = 201.0, lend = 37.1, lenv = 32.8, depd = 24.4, depv = 23.1, dend = 410, denv = 317, refld = 0.571, reflv = 0.573)
  tail      <- list(diad = 0,     diav = 0,     lend = 0,    lenv = 0,    depd = 0,    depv = 0,    dend = 0,   denv = 0,   refld = 0,     reflv = 0)
  # varfur = 0 (single-lump) fallback uses the torso values as the whole-body default.
  c(torso,
    list(
      varfur     = 1,
      parts      = list(leg = leg, head_neck = head_neck, torso = torso, tail = tail),
      tmdptorfur = 0,
      torlend    = rep(torso$lend, julnum),
      torlenv    = rep(torso$lenv, julnum),
      tordepd    = rep(torso$depd, julnum),
      tordepv    = rep(torso$depv, julnum)
    ))
}

.default_physiology <- function(julnum) {
  list(
    tcreg     = 38.6,
    tcmin     = 35.7,
    tcmax     = 40.5,
    tchib     = 37.5,
    tmdptc    = 0,
    tcreg2    = rep(38.6, julnum),
    tctskdif  = 0.5,
    texptair  = 6.3,
    sknwet    = 0.0,
    maxsknwet = 9.0,
    sweat     = "Y",
    pilo      = "Y",
    maxpilo   = 50.0,
    flshk     = 1.0,
    flshkmin  = 0.4,
    flshkmax  = 2.8,
    usrfurk   = "N",
    usrfurk2  = 0.04,
    radfurdep = 1.0,
    o2max     = 25,
    o2min     = 17
  )
}

.default_diet <- function(julnum) {
  list(
    gut     = 2.75,
    fech2o  = 0.66,
    urea    = 0.18,
    digef   = rep(0.7579, julnum),
    act     = rep(1.00, julnum),
    repro   = rep(0, julnum),
    prtn    = rep(0.123, julnum),
    fat     = rep(0.0, julnum),
    carb    = rep(0.877, julnum),
    dry     = rep(0.443, julnum),
    diurn   = rep("Y", julnum),
    noct    = rep("Y", julnum),
    crep    = rep("Y", julnum),
    hibrn   = rep("N", julnum),
    hibfrac = rep(0, julnum),
    land    = rep("L", julnum),
    land2   = rep("L", julnum)
  )
}

.default_thermoreg <- function() {
  list(
    burrow    = "N",
    nest      = "N",
    climb     = "N",
    shdseek   = "N",
    dive      = "N",
    wind      = "N",
    niteshd   = "N",
    dive2     = "N",
    shdact    = "N",
    shdpost   = "S",
    trord     = "N",
    burTR     = 1,
    wade      = "N",
    treeslp   = "Y",
    slpcnpy   = 85.0,
    hudl      = "Y",
    hudlnum   = 3,
    hudldrs   = 0.4,
    hudlvnt   = 0.4,
    tcconcur  = "N",
    tcinc     = 0.01,
    tcwtr     = 38.5,
    o2inc     = 1.0,
    sknwtinc  = 1.0,
    tcconcur2 = "N",
    piloinc   = 0.01
  )
}

.default_flying_digging <- function() {
  list(
    flight   = "N",
    fltmetab = 10.57,
    fltvel   = 13,
    fltload  = 0,
    foss     = "N",
    dig      = "N",
    nodes    = 1,
    arb      = "N",
    buro2    = 20.95,
    burco2   = 0.03,
    burn2    = 79.02,
    soiltyp  = 1,
    burseg   = 0,
    burdep   = 0
  )
}

.default_nest_shelter <- function() {
  list(
    shelter    = "NONE",
    nestuse    = "N",
    nestthk    = 0.0,
    nestk      = 0.0,
    nestno     = 0,
    nestloc    = "A",
    nestnode   = 1,
    nestlength = 0,
    outdiam    = 0,
    shltrefl   = 0,
    shltrans   = "N",
    shltdens   = 0,
    shltcp     = 0,
    nestheight = 0,
    shltnodes  = 0,
    noderadius = 0
  )
}

.default_allometry <- function() {
  list(
    group = "mammal",
    loco  = "quadped",
    parts = list(
      head      = list(diav = 30.08, diah = 11.28, len = 36.14, dorsfur = 40.6, dorsfur_a = 40.6, ventfur = 33.1, ventfur_a = 33.1, dens = 1033.6, geom = 5),
      neck      = list(diav = 24.47, diah = 13.12, len = 24.03, dorsfur = 40.6, dorsfur_a = 40.6, ventfur = 33.1, ventfur_a = 33.1, dens = 1033.6, geom = 3),
      torso     = list(diav = 56.57, diah = 24.98, len = 97.88, dorsfur = 24.4, dorsfur_a = 24.4, ventfur = 23.1, ventfur_a = 23.1, dens = 1033.6, geom = 3),
      front_leg = list(diav = 5.79,  diah = 9.05,  len = 59.34, dorsfur = 7.0,  dorsfur_a = 7.0,  ventfur = 3.9,  ventfur_a = 3.9,  dens = 1033.6, geom = 3),
      rear_leg  = list(diav = 6.91,  diah = 12.45, len = 62.53, dorsfur = 7.0,  dorsfur_a = 7.0,  ventfur = 3.9,  ventfur_a = 3.9,  dens = 1033.6, geom = 3),
      tail      = list(diav = 0,     diah = 0,     len = 0,     dorsfur = 0,    dorsfur_a = 0,    ventfur = 0,    ventfur_a = 0,    dens = 0,      geom = 0)
    ),
    tail_type      = "P",
    absval         = 111.63,
    absdim         = 1,
    adjdim         = 1,
    subq_head      = "N",
    subq_neck      = "N",
    subq_torso     = "Y",
    subq_front_leg = "N",
    subq_rear_leg  = "N",
    subq_tail      = "N",
    tmdpfat        = 1,
    post1          = "N",
    post2          = "N",
    post3          = "Y",
    post4          = "N",
    slpstrt        = 3,
    shdstrt        = 3,
    endpost        = 3,
    akmin_head      = 0.4,
    akmin_neck      = 0.4,
    akmin_torso     = 2.0,
    akmin_front_leg = 0.4,
    akmin_rear_leg  = 0.4,
    akmin_tail      = 0.4,
    VTleg    = "N",
    VT6th    = "N",
    Grd6th   = "N",
    torsover = 0.0,
    torsoff  = 0.0,
    brdslp   = "N",
    brdslplg = 2,
    Tlegred  = "N",
    T6thred  = "N",
    Tcred1   = "D",
    Tcred2   = "D",
    Tfrac1   = 0.3,
    Tfrac2   = 0.3,
    Tdif1    = 1.0,
    Tdif2    = 1.0,
    MinT     = 2.5,
    Tleginc  = "N",
    T6thinc  = "N"
  )
}

.chk_vec_len <- function(x, n, name) {
  if (length(x) != n)
    stop(sprintf("'%s' must have length %d (model_settings$julnum), got %d", name, n, length(x)))
}

#' Write NicheMapR Endotherm model input files (endo.dat, alomvars.dat)
#'
#' Builds the fixed-format \code{endo.dat} and \code{alomvars.dat} input files
#' required by the NicheMapR Endotherm model executable (\code{Endo2022a.exe}),
#' from grouped, defaulted, documented R arguments instead of a hand-edited
#' script. This is a direct port of \code{endo_alomvars_auto_V3.R}'s user-input
#' block and file-writing logic (Paul Mathewson, Megan Fitzpatrick, Warren
#' Porter) into a reusable package function.
#'
#' @param output_dir Directory to write \code{endo.dat} and \code{alomvars.dat}
#'   into. Written using these exact filenames (no \code{study_area} prefix),
#'   because \code{Endo2022a.exe} hard-codes these names in its working
#'   directory.
#' @param model_settings Named list of simulation-level settings: \code{julnum,
#'   juldays, hrout, outout, microin, outfile, outunits, strht, geom, geomult,
#'   apnd, ventpct, inccond, frcmpr, usralom, actht, err, acthrs, minfrg,
#'   nrght, prdht, fasky, fagrd, faobj, usrnure, afrnt, bfrnt, aside, bside}.
#'   \code{juldays} must have length \code{julnum}.
#' @param animal Named list of whole-animal properties: \code{species, class,
#'   marsup, cp, mass, timdepmass, mass2, fatpct, timdepfat, fatpct2, subqfat,
#'   density, usrmet, met}.
#' @param fur Named list of fur/feather properties: whole-body defaults
#'   (\code{varfur, diad, diav, lend, lenv, depd, depv, dend, denv, refld,
#'   reflv}), a \code{parts} sub-list (\code{leg, head_neck, torso, tail}, each
#'   with the same nine fields) used when \code{varfur = 1}, and
#'   time-dependent torso fur (\code{tmdptorfur, torlend, torlenv, tordepd,
#'   tordepv}).
#' @param physiology Named list of core-temperature and heat-exchange
#'   physiology: \code{tcreg, tcmin, tcmax, tchib, tmdptc, tcreg2, tctskdif,
#'   texptair, sknwet, maxsknwet, sweat, pilo, maxpilo, flshk, flshkmin,
#'   flshkmax, usrfurk, usrfurk2, radfurdep, o2max, o2min}.
#' @param diet Named list of diet, digestion, and daily activity/hibernation
#'   schedule: \code{gut, fech2o, urea, digef, act, repro, prtn, fat, carb,
#'   dry, diurn, noct, crep, hibrn, hibfrac, land, land2}. The vector fields
#'   must have length \code{model_settings$julnum}.
#' @param thermoreg Named list of behavioral thermoregulation options:
#'   \code{burrow, nest, climb, shdseek, dive, wind, niteshd, dive2, shdact,
#'   shdpost, trord, burTR, wade, treeslp, slpcnpy, hudl, hudlnum, hudldrs,
#'   hudlvnt, tcconcur, tcinc, tcwtr, o2inc, sknwtinc, tcconcur2, piloinc}.
#' @param flying_digging Named list of flight and burrowing/fossoriality
#'   options: \code{flight, fltmetab, fltvel, fltload, foss, dig, nodes, arb,
#'   buro2, burco2, burn2, soiltyp, burseg, burdep}.
#' @param nest_shelter Named list of nest/shelter geometry and use:
#'   \code{shelter, nestuse, nestthk, nestk, nestno, nestloc, nestnode,
#'   nestlength, outdiam, shltrefl, shltrans, shltdens, shltcp, nestheight,
#'   shltnodes, noderadius}.
#' @param allometry Named list used only to build \code{alomvars.dat}:
#'   \code{group, loco}, a \code{parts} sub-list (\code{head, neck, torso,
#'   front_leg, rear_leg, tail}, each with \code{diav, diah, len, dorsfur,
#'   dorsfur_a, ventfur, ventfur_a, dens, geom}), \code{tail_type, absval,
#'   absdim, adjdim}, per-part subcutaneous fat flags (\code{subq_head,
#'   subq_neck, subq_torso, subq_front_leg, subq_rear_leg, subq_tail,
#'   tmdpfat}), inactive postures (\code{post1, post2, post3, post4, slpstrt,
#'   shdstrt, endpost}), per-part minimum flesh conductivity
#'   (\code{akmin_head, akmin_neck, akmin_torso, akmin_front_leg,
#'   akmin_rear_leg, akmin_tail, VTleg, VT6th, Grd6th}), leg-shading geometry
#'   (\code{torsover, torsoff}), bird sleeping (\code{brdslp, brdslplg}), and
#'   countercurrent exchange (\code{Tlegred, T6thred, Tcred1, Tcred2, Tfrac1,
#'   Tfrac2, Tdif1, Tdif2, MinT, Tleginc, T6thinc}). Always written
#'   regardless of \code{model_settings$usralom}, matching the source
#'   script's behavior.
#' @param study_area Optional string recorded in the returned log only; it
#'   does not prefix the output filenames (see \code{output_dir}).
#'
#' @return Invisibly, a log \code{data.frame} with columns \code{file_path,
#'   step, status, timestamp}, one row per file written.
#'
#' @details
#' The row-by-row text assembly mirrors the source script's \code{paste()}/
#' \code{format()}/\code{sQuote()} calls exactly, including tab/space padding,
#' because \code{Endo2022a.exe} is a compiled reader that is presumed to parse
#' these fixed-format files by column position. Two quirks of the source
#' script are preserved for fidelity rather than silently fixed:
#' \itemize{
#'   \item \code{thermoreg$tcinc} is used for both the sweating and
#'     piloerection concurrent-temperature-change increments (the source
#'     script assigns two same-named variables, so only one value ever
#'     reaches the file).
#'   \item In \code{alomvars.dat}'s 6th-appendage (tail/proboscis) row, the
#'     locomotion type (\code{allometry$loco}) is written a second time in
#'     the column documented as tail-vs-proboscis type; \code{allometry$tail_type}
#'     is accepted but not currently written anywhere, matching the source
#'     script exactly.
#' }
#' Running \code{Endo2022a.exe}, parsing its outputs, and generating
#' \code{JULDAYS.dat} are out of scope for this function.
#'
#' @export
write_endotherm_inputs <- function(output_dir,
                                    model_settings = list(),
                                    animal         = list(),
                                    fur            = list(),
                                    physiology     = list(),
                                    diet           = list(),
                                    thermoreg      = list(),
                                    flying_digging = list(),
                                    nest_shelter   = list(),
                                    allometry      = list(),
                                    study_area     = NULL) {

  if (!dir.exists(output_dir))
    stop(sprintf("'output_dir' does not exist:\n  %s", output_dir))

  ms <- utils::modifyList(.default_model_settings(), model_settings)
  julnum <- ms$julnum
  .chk_vec_len(ms$juldays, julnum, "model_settings$juldays")

  an <- utils::modifyList(.default_animal(julnum), animal)
  fr <- utils::modifyList(.default_fur(julnum), fur)
  ph <- utils::modifyList(.default_physiology(julnum), physiology)
  di <- utils::modifyList(.default_diet(julnum), diet)
  tr <- utils::modifyList(.default_thermoreg(), thermoreg)
  fd <- utils::modifyList(.default_flying_digging(), flying_digging)
  ns <- utils::modifyList(.default_nest_shelter(), nest_shelter)
  al <- utils::modifyList(.default_allometry(), allometry)

  for (.v in list(list(an$mass2, "animal$mass2"), list(an$fatpct2, "animal$fatpct2"),
                  list(ph$tcreg2, "physiology$tcreg2"),
                  list(fr$torlend, "fur$torlend"), list(fr$torlenv, "fur$torlenv"),
                  list(fr$tordepd, "fur$tordepd"), list(fr$tordepv, "fur$tordepv"),
                  list(di$digef, "diet$digef"), list(di$act, "diet$act"),
                  list(di$repro, "diet$repro"), list(di$prtn, "diet$prtn"),
                  list(di$fat, "diet$fat"), list(di$carb, "diet$carb"),
                  list(di$dry, "diet$dry"), list(di$diurn, "diet$diurn"),
                  list(di$noct, "diet$noct"), list(di$crep, "diet$crep"),
                  list(di$hibrn, "diet$hibrn"), list(di$hibfrac, "diet$hibfrac"),
                  list(di$land, "diet$land"), list(di$land2, "diet$land2")))
    .chk_vec_len(.v[[1]], julnum, .v[[2]])

  old_fancy <- getOption("useFancyQuotes")
  options(useFancyQuotes = FALSE)
  on.exit(options(useFancyQuotes = old_fancy), add = TRUE)

  fur_leg  <- if (fr$varfur == 0) fr else fr$parts$leg
  fur_hn   <- if (fr$varfur == 0) fr else fr$parts$head_neck
  fur_trs  <- if (fr$varfur == 0) fr else fr$parts$torso
  fur_tail <- if (fr$varfur == 0) fr else fr$parts$tail

  # NOTE: the "legs" row uses different spacing (two spaces before depd/denv)
  # than the head&neck/torso/tail rows (tab before depd, one space before
  # denv) in the source script. Preserved verbatim as two separate builders.
  .fur_row_leg <- function(p) {
    c(format(round(p$diad, 1), nsmall = 1), " ", format(round(p$diav, 1), nsmall = 1), " ",
      format(round(p$lend, 1), nsmall = 1), " ", format(round(p$lenv, 1), nsmall = 1), "  ",
      format(round(p$depd, 1), nsmall = 1), " ", format(round(p$depv, 1), nsmall = 1), "    ",
      format(round(p$dend, 1), nsmall = 1), "  ", format(round(p$denv, 1), nsmall = 1), "  ",
      format(round(p$reflv, 3), nsmall = 2), "  ", format(round(p$refld, 3), nsmall = 2), "\t   ", 0.08, "\n")
  }
  .fur_row_other <- function(p) {
    c(format(round(p$diad, 1), nsmall = 1), " ", format(round(p$diav, 1), nsmall = 1), " ",
      format(round(p$lend, 1), nsmall = 1), " ", format(round(p$lenv, 1), nsmall = 1), "\t",
      format(round(p$depd, 1), nsmall = 1), " ", format(round(p$depv, 1), nsmall = 1), "    ",
      format(round(p$dend, 1), nsmall = 1), " ", format(round(p$denv, 1), nsmall = 1), "  ",
      format(round(p$reflv, 3), nsmall = 2), "  ", format(round(p$refld, 3), nsmall = 2), "\t   ", 0.08, "\n")
  }

  # --- Begin building the endo.dat file --------------------------------------
  row1  <- c("Name simulation & input file to run: 'endoprop','endotime', and 'endosens' are choices. Input for Endo2011a.", "\n")
  row2  <- c("endoprop = no variation in animal parameters with time; read only this file", "\n")
  row3  <- c("\"endotime = time dependent variables, e.g. core temp., food type available, etc.; \"", "\n")
  row4  <- c("endosens = sensitivity analysis for different body sizes for given climate regime;", "\n")
  row5  <- c("2ND VARIABLE: Hourly output (y/n)? Do not use for GIS-type calc's unless need hourly output", "\n")
  row6  <- c("Hourly output = 'y' creates files 'HOURPLOT.OUT' and 'ACTHOURS.OUT'.3RD VAR = 'y'->file OUTPUT printed", "\n")
  row7  <- c("\"4TH VAR: IF 2.0<DEPEND<3.0 = total metab.in W;if DEPEND=2.0, then metab. in W/kg;\"", "\n")
  row8  <- c("5TH-7TH variables: 'ECTHRM' vs 'NDTHRM' (Ectotherm vs. Endotherm)=known [Tc-Met rate known; solve for Tc] vs", "\n")
  row9  <- c("\"[fixed Tc, solve for Met(no flight) OR [fixed Met, solve for Tc (flight). USE 'NDTHRM' IF FLIGHT = 'Y'\"", "\n")
  row10 <- c("\"If 'ECTHRM', then read slope & intercept of ln(ml O2/g/h) = slope*Tc + intercept, ELSE USE ZERO FOR SLOPE AND INTERCEPT IF NDTHRM\"", "\n")
  row11 <- c("8th Variable: What type of microclimate input files are read in. 'CSV' if using Micro2011; 'OUT' if using Micro2010", "\n")
  row12 <- c("9th Variable: What type of output files from endotherm model: 'CSV' or 'OUT'", "\n")
  row13 <- c("10th Variable: What units for metabolic requirements in MONTH and YEAR output files: 'JL', 'KJ', or 'MJ'  *Joules, Kilojoules, or Megajoules*", "\n")
  row14 <- c("-----------------------------------------------------------------------------", "\n")
  row15 <- paste("'ENDOTIME' '", ms$hrout, "' '", ms$outout, "' 1.0 'NDTHRM' 0 0 '", ms$microin, "'  '", ms$outfile, "' '", ms$outunits, "'", sep = "", "\n")
  row16 <- c("", "\n")
  row17 <- c("Do a transient in   If a transient, is it   Consider stored  Specific   Class of animal (6 letters): 'MAMMAL',\"\t", "\n")
  row18 <- c("addition to steady  for the animal (1.)  heat in energy   Heat       'BIRDIE','REPTIL','AMPHIB','INSECT'\"\t\t  ", "\n")
  row19 <- c("state (y/n)         or nest/shelter (0.)?   balance?(Y/n)    (J/KgC)    mammal,bird,reptile,amphibian,insect)\"\t   MARSUPIAL?", "\n")
  row20 <- c("--------            -------------------     -------------    -------    ----------------------------------------    ----------", "\n")
  row21 <- paste("'N'                  1                      '", ms$strht, "'               ", an$cp, "       '", an$class, "'                                    '", an$marsup, "'", sep = "", "\n")
  row22 <- c("", "\n")
  row23 <- paste("Animal species = ", an$species, sep = "", "\n")
  row24 <- c("Animal Variables           Dep var form: 2.0<DEPEND<3.0 = total metab.in W", "\n")
  row25 <- c("ALLOMETRIC properties ", "\n")
  row26 <- c("Geometric properties\t\t     Whole body(torso if apndgs) Geom Mult      '0APNDG'if no appendages-IN CAPS!!\t\t", "\n")
  row27 <- c("Max       Fat mass   Is fat subcut?    Geometric approx.(integer)  Ellips:(A:B)   '2APNDG'if 2 appndg (e.g.bird)   % ventral area      Include\t\t  dec % fur   Animal \t    User supplied\t", "\n")
  row28 <- c("\"weight   as % body  (If so, affects     1=cyl,2=spher,             Cyl:(L:Rskin)  '4APNDG'if 4 appndg (e.g.mammal) contacting substr.  conduction\t  compression density \t    allometry?", "\n")
  row29 <- c("\"(kg)     mass(%)    heat loss)(Y/N)     4=ellipsoid                                  use complx geom's              (decimal 100%=1.O) w/ sub? (Y/N)      for condct  kg/m3(932.9)    (Y/N)", "\n")
  row30 <- c("------    ----       -------------      ----------------------     ---------      ----------------                 ----------\t         ------------        -----\t  --------     --------------", "\n")
  row31 <- c(format(round(an$mass, 2), nsmall = 2), "\t   ", format(round(an$fatpct, 2), nsmall = 2), "\t\t  ", sQuote(an$subqfat), "\t\t\t", ms$geom, "\t\t   ", format(round(ms$geomult, 2), nsmall = 2), "\t\t    ", sQuote(ms$apnd), "\t\t\t\t", format(round(ms$ventpct, 2), nsmall = 2), "\t\t   ", sQuote(ms$inccond), "\t\t     ", format(round(ms$frcmpr, 2), nsmall = 2), "\t", format(round(an$density, 2), nsmall = 2), "\t\t", sQuote(ms$usralom), "\t\t", "\n")
  row32 <- c("", "\n")
  row33 <- c("User-Specified Metabolic Options", "\n")
  row34 <- c("User supplied  \t\tAssume activity heat\tdec % variance from  Increasing act hrs: 0=If ACTIV=Y     Minimum forage rate       dec % of energy for      dec % of energy for ", "\n")
  row35 <- c("metabolic rate\t\tcontribs to thrmreg\texpected met rate    1=If QMETAB>QMIN; 2=If act mults\t(Enter as multiple        activity released as     production released as  ", "\n")
  row36 <- c("(Y/N)      Met rate (W)     (Y/N)\t\tto trigger thrmreg    balance; 3=Other min forage rate\t of basal metabolism)     heat that can affect Tc  heat that can affect Tc   ", "\n")
  row37 <- c("----       ----------   -------------------      ----------------    --------------------------------\t---------------------      -----------------------   -------------------", "\n")
  row38 <- c(sQuote(an$usrmet), "\t     ", format(round(an$met, 2), nsmall = 2), "\t\t", sQuote(ms$actht), "\t\t\t", format(round(ms$err, 2), nsmall = 2), "\t\t\t", ms$acthrs, "\t\t\t\t", format(round(ms$minfrg, 2), nsmall = 2), "\t\t\t", format(round(ms$nrght, 2), nsmall = 2), "\t\t\t", format(round(ms$prdht, 2), nsmall = 2), "\n")
  row39 <- c("", "\n")
  row40 <- c("Fur Properties - LEGS   Note to user *** use same values for all if modeling as a single lump\t\t\t\t", "\n")
  row41 <- c("Hair dia  Hair length   Fur depth   Hair dens. fur        fur       fur      \t\t\t\t", "\n")
  row42 <- c("(um)      (mm)         (mm)         (1/cm2)    front_refl back_refl tran  \t\t\t\t", "\n")
  row43 <- c("dsl vntl  dsl  vntl     dsl vnt      dsl vntl  nd         nd        nd  \t\t\t\t", "\n")
  row44 <- c("----  ---  ---- -----   --- ---      ---- ----   ----    ----      ---- \t\t\t\t", "\n")
  row45 <- .fur_row_leg(fur_leg)
  row46 <- c("\t\t\t\t", "\n")
  row47 <- c("Fur Properties - HEAD & NECK           \t\t\t\t", "\n")
  row48 <- c("Hair dia  Hair length   Fur depth   Hair dens. fur        fur          fur     \t\t\t\t", "\n")
  row49 <- c("(um)      (mm)         (mm)         (1/cm2)    dorsl_refl ventrl_refl  tran   ", "\n")
  row50 <- c("dsl vntl  dsl  vntl     dsl vnt      dsl vntl   nd         nd          nd    nd means NOT %, but decimal ratio! Max. value = 1.00", "\n")
  row51 <- c("---- ---  ---- -----   ---  ---    ----  ----   ----      -------     ----   ", "\n")
  row52 <- .fur_row_other(fur_hn)
  row53 <- c("", "\n")
  row54 <- c("Fur Properties - TORSO           ", "\n")
  row55 <- c("Hair dia  Hair length   Fur depth   Hair dens.  fur         fur         fur      ", "\n")
  row56 <- c("(um)      (mm)         (mm)         (1/cm2)     dorsl_refl ventrl_refl tran   ", "\n")
  row57 <- c("dsl vntl  dsl  vntl     dsl vnt      dsl vntl   nd         nd          nd      ", "\n")
  row58 <- c("---- ---  ---- -----   --- ---      ---- ----   ----       -----       ----    ", "\n")
  row59 <- .fur_row_other(fur_trs)
  row60 <- c("", "\n")
  row61 <- c("Fur Properties - TAIL           ", "\n")
  row62 <- c("Hair dia  Hair length   Fur depth   Hair dens.  fur         fur         fur      ", "\n")
  row63 <- c("(um)      (mm)         (mm)         (1/cm2)     dorsl_refl ventrl_refl tran   ", "\n")
  row64 <- c("dsl vntl  dsl  vntl     dsl vnt      dsl vntl   nd         nd          nd      ", "\n")
  row65 <- c("---- ---  ---- -----   --- ---      ---- ----   ----       -----       ----    ", "\n")
  row66 <- .fur_row_other(fur_tail)
  row67 <- c("", "\n")
  row68 <- c("Hair/Feather length (mm) - dorsal", "\n")
  row69 <- c("This allows for TORSO insulation due to fur/feathers to change seasonally.", "\n")
  row70 <- c("Not SI data.", "\n")
  row71 <- c("------------------------------------------------------------------------------------------------------------- ", "\n")
  row72 <- if (fr$tmdptorfur == 0) c(paste(rep(fur_trs$lend, julnum), collapse = " "), "\n") else c(paste(fr$torlend, collapse = " "), "\n")
  row73 <- c("", "\n")
  row74 <- c("Hair/Feather length (mm) - ventral", "\n")
  row75 <- c("This allows for TORSO insulation due to fur/feathers to change seasonally.", "\n")
  row76 <- c("Not SI data.", "\n")
  row77 <- c("------------------------------------------------------------------------------------------------------------- ", "\n")
  row78 <- if (fr$tmdptorfur == 0) c(paste(rep(fur_trs$lenv, julnum), collapse = " "), "\n") else c(paste(fr$torlenv, collapse = " "), "\n")
  row79 <- c("", "\n")
  row80 <- c("Pelt/plumage depth (mm)- dorsal", "\n")
  row81 <- c("This allows for TORSO insulation due to fur/feathers to change seasonally.", "\n")
  row82 <- c("Not SI data.", "\n")
  row83 <- c("------------------------------------------------------------------------------------------------------------- ", "\n")
  row84 <- if (fr$tmdptorfur == 0) c(paste(rep(fur_trs$depd, julnum), collapse = " "), "\n") else c(paste(fr$tordepd, collapse = " "), "\n")
  row85 <- c("", "\n")
  row86 <- c("Pelt/plumage depth (mm)- ventral", "\n")
  row87 <- c("This allows for TORSO insulation due to fur/feathers to change seasonally.", "\n")
  row88 <- c("Not SI data.", "\n")
  row89 <- c("------------------------------------------------------------------------------------------------------------- ", "\n")
  row90 <- if (fr$tmdptorfur == 0) c(paste(rep(fur_trs$depv, julnum), collapse = " "), "\n") else c(paste(fr$tordepv, collapse = " "), "\n")
  row91 <- c(" ", "\n")
  row92 <- c("PHYSIOLOGICAL properties - temperature and water loss from metabolism & skin\t\t\t\t\t\tFlesh thermal                       User supplied  fur           Depth in fur for radiant exchange", "\n")
  row93 <- c("Core     Core   Core    Min. diff   Texpir-     % skin wet  Max % skin    Sweat OK?\tPiloerect?  Max pilo     conductivity (0.412 - 2.8 W/mC)    thermal conductivity (W/mC)? 1.0= fur surface;", "\n")
  row94 <- c("regul_T  max T  min T   Tc-Tskin(C) Tair (C)    (sweat)     wet (sweat)   (Y/N)\t\t(Y/N)\t    Pct (%)      Start    Minimum  Maximum           (Y/N)    Value               0.5=halfway b/w fur and skin", "\n")
  row95 <- c("----     -----  ----   ----------   -------     ----------  -----------   ---------    ----------   --------\t -------  -------  -------\t      -----    -----               ---------------", "\n")
  row96 <- c(format(round(ph$tcreg, 1), nsmall = 1), "\t", format(round(ph$tcmin, 1), nsmall = 1), "\t", format(round(ph$tcmax, 1), nsmall = 1), "\t   ", format(round(ph$tctskdif, 1), nsmall = 1), "\t     ", format(round(ph$texptair, 1), nsmall = 1),
             "\t ", format(round(ph$sknwet, 1), nsmall = 1), "\t\t", format(round(ph$maxsknwet, 1), nsmall = 1), "\t    ", sQuote(ph$sweat), "\t\t", sQuote(ph$pilo), "\t    ", format(round(ph$maxpilo, 1), nsmall = 1), "\t  ", format(round(ph$flshk, 1), nsmall = 1), "\t    ", format(round(ph$flshkmin, 1), nsmall = 1),
             "\t    ", format(round(ph$flshkmax, 1), nsmall = 1), "\t\t\t", sQuote(ph$usrfurk), "\t", format(round(ph$usrfurk2, 1), nsmall = 1), "\t\t\t", format(round(ph$radfurdep, 1), nsmall = 1), "\n")
  row97  <- c("", "\n")
  row98  <- c("PHYSIOLOGICAL properties - lungs and gut:  dig. eff.", "\n")
  row99  <- c("O2 extraction      O2 extraction     Gut passage  Fecal water  Urea in", "\n")
  row100 <- c("efficiency max(%)  efficiency min(%) time (days)  (dec. %)     urine (dec. %)", "\n")
  row101 <- c("------------       ---------------- ----------   ----------   --------------", "\n")
  row102 <- c(format(round(ph$o2max, 1), nsmall = 1), "\t\t\t", format(round(ph$o2min, 1), nsmall = 1), "\t     ", format(round(di$gut, 2), nsmall = 2), "\t     ", format(round(di$fech2o, 2), nsmall = 2), "\t     ", format(round(di$urea, 2), nsmall = 2), "\n")
  row103 <- c("", "\n")
  row104 <- c("PHYSIOLOGICAL properties - monthly values: Core temperature regulated this month", "\n")
  row105 <- c("This can be used to simulate hibernation or other unusual activity timing.", "\n")
  row106 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row107 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row108 <- if (ph$tmdptc == 0) c(paste(rep(ph$tcreg, julnum), collapse = " "), "\n") else c(paste(ph$tcreg2, collapse = " "), "\n")
  row109 <- c("", "\n")
  row110 <- c("Physiological properties - Digestive efficiencies - monthly values:", "\n")
  row111 <- c("(decimal %)    \"", "\n")
  row112 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row113 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row114 <- c(paste(di$digef, collapse = " "), "\n")
  row115 <- c("", "\n")
  row116 <- c("PHYSIOLOGICAL properties - monthly values: Times basal for activity energy & food for it (1-7)", "\n")
  row117 <- c("This can be used to simulate phenology of food available and reproduction timing.", "\n")
  row118 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row119 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row120 <- c(paste(di$act, collapse = " "), "\n")
  row121 <- c("", "\n")
  row122 <- c("PHYSIOLOGICAL properties - monthly values: Times basal for disc. nrg food intake (0-7)", "\n")
  row123 <- c("Can use to simulate phenology of food available and reproduction timing.0=no reprod effort", "\n")
  row124 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row125 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row126 <- c(paste(di$repro, collapse = " "), "\n")
  row127 <- c("", "\n")
  row128 <- c("FOOD properties -  % protein (decimal %): monthly values:", "\n")
  row129 <- c(" 100%=1.00 Nectar except breeding months, then same as creeper (April-May): \"", "\n")
  row130 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row131 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row132 <- c(paste(di$prtn, collapse = " "), "\n")
  row133 <- c("", "\n")
  row134 <- c("FOOD properties -  % fat: monthly values", "\n")
  row135 <- c("(decimal:  100% is 1.00)", "\n")
  row136 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row137 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row138 <- c(paste(di$fat, collapse = " "), "\n")
  row139 <- c("", "\n")
  row140 <- c("FOOD properties - % carbohydrate: monthly values", "\n")
  row141 <- c("(decimal format) 100% carbohydrate = 1.00 decimal", "\n")
  row142 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row143 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row144 <- c(paste(di$carb, collapse = " "), "\n")
  row145 <- c("", "\n")
  row146 <- c("FOOD properties - monthly values: % dry matter", "\n")
  row147 <- c("(decimal)(0.25 green veg.;0.75 seed humid stor.; 0.9219 dry seed)", "\n")
  row148 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row149 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row150 <- c(paste(di$dry, collapse = " "), "\n")
  row151 <- c("", "\n")
  row152 <- c("BEHAVIORAL properties - monthly values: Diurnal?  ALL BEHAVIOR OPTIONS MUST BE IN CAPS!!!", "\n")
  row153 <- c("(Y/N) (Diurnal value for Diurnal in BEHAV.DATA should be the January value here)", "\n")
  row154 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row155 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row156 <- c(paste(sQuote(di$diurn), collapse = " "), "\n")
  row157 <- c("", "\n")
  row158 <- c("BEHAVIORAL properties - monthly values: Nocturnal?", "\n")
  row159 <- c("(Y/N) (Nocturnal value for Nocturnal in BEHAV.DATA should be the January value here)", "\n")
  row160 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row161 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row162 <- c(paste(sQuote(di$noct), collapse = " "), "\n")
  row163 <- c("", "\n")
  row164 <- c("BEHAVIORAL properties - monthly values: Crepuscular?", "\n")
  row165 <- c("(Y/N)", "\n")
  row166 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row167 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row168 <- c(paste(sQuote(di$crep), collapse = " "), "\n")
  row169 <- c("", "\n")
  row170 <- c("BEHAVIORAL properties - monthly values: Hibernate?", "\n")
  row171 <- c("(Y/N)", "\n")
  row172 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row173 <- c(" --------------------------------------------------------------------------", "\n")
  row174 <- c(paste(sQuote(di$hibrn), collapse = " "), "\n")
  row175 <- c("", "\n")
  row176 <- c("Fraction of day hibernating,", "\n")
  row177 <- c("if hibernating", "\n")
  row178 <- c("(0.0 - 1.0)", "\n")
  row179 <- c("--------------------------------------------------------------------------", "\n")
  row180 <- c(paste(di$hibfrac, collapse = " "), "\n")
  row181 <- c("", "\n")
  row182 <- c("BEHAVIORAL properties - monthly values:\t\t\t\t\t\t****NOTE: use 'W' to model an animal floating on the water. To model a fully submerged animal", "\n")
  row183 <- c("Active on Land (L) or Water (W) for each simulation day?\t\t\t  that spends some time on land and some time in the water, use 'L' here and then use", "\n")
  row184 <- c(paste(ms$juldays, collapse = ". "), "\t\t\t  the dive option and dive table to model the time in the water", "\n")
  row185 <- c("--------------------------------------------------------", "\n")
  row186 <- c(paste(di$land, collapse = " "), "\n")
  row187 <- c("", "\n")
  row188 <- c("BEHAVIORAL properties - monthly values:", "\n")
  row189 <- c("Inactive on Land (L) or Water (W) for each simulation day(?)", "\n")
  row190 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row191 <- c("--------------------------------------------------------", "\n")
  row192 <- c(paste(di$land2, collapse = " "), "\n")
  row193 <- c("", "\n")
  row194 <- c("Time dependent changes in mass (kg)", "\n")
  row195 <- c("This allows for seasonal changes to to fat loss or gain or growth", "\n")
  row196 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row197 <- c("--------------------------------------------------------", "\n")
  row198 <- if (an$timdepmass == 0) c(paste(rep(an$mass, julnum), collapse = " "), "\n") else c(paste(an$mass2, collapse = " "), "\n")
  row199 <- c("", "\n")
  row200 <- c("% body fat composition", "\n")
  row201 <- c("This allows for insulation due to fat to change seasonally.", "\n")
  row202 <- c("Whether it is subcutaneous or body fat is determined by the user in the 3rd data line, 3rd data element above.", "\n")
  row203 <- c("-------------------------------------------------------------------------------------------------------------", "\n")
  row204 <- if (an$timdepfat == 0) c(paste(rep(an$fatpct, julnum), collapse = " "), "\n") else c(paste(an$fatpct2, collapse = " "), "\n")
  row205 <- c("", "\n")
  row206 <- c("Hibernation body temperature (if hibernate), else set to lowest body temperature.", "\n")
  row207 <- c("This allows for seasonal changes in body temperature.", "\n")
  row208 <- c("Whether it is subcutaneous or body fat is determined by the user in the 3rd data line, 3rd data element above.", "\n")
  row209 <- c("-------------------------------------------------------------------------------------------------------------", "\n")
  row210 <- c(paste(rep(ph$tchib, julnum), collapse = " "), "\n")
  row211 <- c("", "\n")
  row212 <- c("BEHAVIORAL: Use nest Climb    Ground shade Dive      Seek Wind\t Night shade?      Dive?\tActive in\t      TR order:\t Burrow/Nest TR use option:", "\n")
  row213 <- c("Burrow OK?  for TR?  to cool? seeking OK?  to cool?  Protection? (Cold protection) Dive option\tshade in day?\t      Behav 1st?   1= both hot & cold", "\n")
  row214 <- c("(Y/N)       (Y/N)    (Y/N)    (Y/N)        (Y/N)     (Y/N)\t (Y/N)            (Y/N)\t\t(Y/N); (S)tand/(L)ie\t(Y/N)\t   2= only hot 3= only cold\tWade (Y/N)?", "\n")
  row215 <- c("-------    -------   -----    ----------   --------  -----------  -------------    ---------\t-----/-------\t      ---------\t-------------------------\t----------", "\n")
  row216 <- c(sQuote(tr$burrow), "\t    ", sQuote(tr$nest), "\t     ", sQuote(tr$climb), "\t  ", sQuote(tr$shdseek), "\t   ", sQuote(tr$dive), "\t\t", sQuote(tr$wind), "\t\t", sQuote(tr$niteshd), "\t\t", sQuote(tr$dive2), "\t", sQuote(tr$shdact), "\t", sQuote(tr$shdpost), "\t\t  ", sQuote(tr$trord),
              "\t\t", tr$burTR, "\t\t\t", sQuote(tr$wade), "\n")
  row217 <- c("", "\n")
  row218 <- c("THRMREG:    Tree sleep  Huddle    # animals  Huddled      Huddled    Concurrent  Tc increase  Tc trigger  O2 extref  SkinW       Concurrent     Tc decrease   Piloerect   ", "\n")
  row219 <- c("Tree sleep?  shade     at night? huddled?  drsl contct vntrl contct  Tc increase?  increment\twtr loss   increment   increment  Tc decrease?   increment      increment", "\n")
  row220 <- c("(Y/N)       (%)        (Y/N)     (intgr)    (dec. %)    (dec. %)       (Y/N)       (deg. C)\t(deg. C)    (%)        (%)          (Y/N)         (deg. C)      (dec. %)", "\n")
  row221 <- c("-------     -------     -----    ---------  --------    --------     ----------    ---------     ------    -------    --------     ----------     ----------    --------", "\n")
  row222 <- c(sQuote(tr$treeslp), "\t    ", format(round(tr$slpcnpy, 1), nsmall = 1), "\t ", sQuote(tr$hudl), "\t   ", tr$hudlnum, "\t    ", format(round(tr$hudldrs, 2), nsmall = 2),
              "\t ", format(round(tr$hudlvnt, 2), nsmall = 2), "\t\t", sQuote(tr$tcconcur), "\t    ", format(round(tr$tcinc, 2), nsmall = 2), "\t  ", format(round(tr$tcwtr, 2), nsmall = 2), "\t    ", format(round(tr$o2inc, 1), nsmall = 1),
              "\t      ", format(round(tr$sknwtinc, 1), nsmall = 1), "\t\t", sQuote(tr$tcconcur2), "\t    ", format(round(tr$tcinc, 2), nsmall = 2), "\t   ", format(round(tr$piloinc, 2), nsmall = 2), "\n")
  row223 <- c("", "\n")
  row224 <- c("BEHAVIORAL  Flight variables             Flight  If flight=yes, If animal is pollinating", "\n")
  row225 <- c("Flight OK? If flight = yes, then specify Velocity  insect, average flight load (g) may be", "\n")
  row226 <- c("(Y/N)      flight metab (W) specified to (m/s)  correct for flight metabolism", "\n")
  row227 <- c("-------    -----------------------------   ---   ----------------------------------", "\n")
  row228 <- c(sQuote(fd$flight), "\t\t\t", format(round(fd$fltmetab, 2), nsmall = 2), "\t\t  ", format(round(fd$fltvel, 2), nsmall = 2), "\t\t    ", format(round(fd$fltload, 2), nsmall = 2), "\n")
  row229 <- c("", "\n")
  row230 <- c("Fossorial(Below    Digger\tIf fossorial,       Arboreal \t  Shelter/Nest type: CYLN=HOLLOW FULL CYL;    NEST/SHELTER USE WHEN", "\n")
  row231 <- c("grd EXCLUSIVELY)?  Dig burrows? OK node#s=2-10;  exclusively?  HFCL=HOLLOW HALF CYLINDER; SPHR=HOLLOW SPHERE  INACTIVE (NOT HIBERNATING)?", "\n")
  row232 <- c("  (Y/N)           (Y/N)\t\telse use 1\t    (Y/N)         FLAT,CUPP,CYLN,HFCL,SPHR,DOME,NONE)            (Y/N)", "\n")
  row233 <- c("----------       ----------\t-------------    --------\t  -------------------------------------           --------", "\n")
  row234 <- c(sQuote(fd$foss), "\t\t  ", sQuote(fd$dig), "\t\t  ", fd$nodes, "\t\t   ", sQuote(fd$arb), "\t\t\t", sQuote(ns$shelter), "\t\t\t\t\t ", sQuote(ns$nestuse), "\n")
  row235 <- c("", "\n")
  row236 <- c("CONFIGURATION FACTORS FOR DIFFUSE IR    User-supplied", "\n")
  row237 <- c("Fasky   Fagrd  Fabush/near object \tNu-Re Correlation\tFront\t Side", "\n")
  row238 <- c("0-0.5   0-0.5  0-0.5   \t\t\tCoefficients? (Y/N)     a     b       a      b", "\n")
  row239 <- c("-----   -----  -----  \t\t\t-------------------    ----  ----   ----  ----", "\n")
  row240 <- c(format(round(ms$fasky, 1), nsmall = 1), "\t", format(round(ms$fagrd, 1), nsmall = 1), "\t ", format(round(ms$faobj, 1), nsmall = 1), "\t\t\t\t", sQuote(ms$usrnure), "\t       ", format(round(ms$afrnt, 2), nsmall = 2), "  ", format(round(ms$bfrnt, 2), nsmall = 2),
              "   ", format(round(ms$aside, 2), nsmall = 2), "  ", format(round(ms$bside, 2), nsmall = 2), "\n")
  row241 <- c("", "\n")
  row242 <- c("NEST properties: if nest THICKNESS = 0.0, no nest is assumed\"\t   Shelter/nest # animals in nest\t   If nest/rest place above ground = 'A'\t\tIf nest is below ground,'B',then how deep", "\n")
  row243 <- c("Nest wall            Nest wall (wood: 0.10-0.35;sheep wool:0.05)  to adjust # present for xtra\t\t   If nest/rest place below ground = 'B'\t\tis the nest, i.e., what is the node number (2-10)?", "\n")
  row244 <- c("thickness (m)        thermal conductivity (W/m-C)\t           heat production. If <1, no nest calc.   If nest='B',set 'Burrow OK'='N',so stay @ 1 depth\tExample: node 6 is at 20 cm. (see bottom line below)", "\n")
  row245 <- c("---------------      ----------------------------\t    \t --------------------------------------    ------------------------------\t\t\t\t---------------------------------------------", "\n")
  row246 <- c(format(round(ns$nestthk, 2), nsmall = 2), "\t\t\t", format(round(ns$nestk, 2), nsmall = 2), "\t\t\t\t\t\t\t", format(round(ns$nestno, 0), nsmall = 0), "\t\t\t\t\t\t", sQuote(ns$nestloc), "\t\t\t\t\t\t\t", format(round(ns$nestnode, 0), nsmall = 0), "\n")
  row247 <- c("", "\n")
  row248 <- c("AIR (Burrow gas properties; atm values in parens) NOTE:THESE MUST SUM TO 100.0%", "\n")
  row249 <- c(" % O2     %CO2      %N2", "\n")
  row250 <- c("(20.95%)  (0.03%)  (79.02%) = standard atmosphere", "\n")
  row251 <- c("--------  -------  --------", "\n")
  row252 <- c(format(round(fd$buro2, 2), nsmall = 2), "\t   ", format(round(fd$burco2, 2), nsmall = 2), "    ", format(round(fd$burn2, 2), nsmall = 2), "\n")
  row253 <- c("", "\n")
  row254 <- c("SOIL,BURROW properties\t\t\tSegment\t        Burrow", "\n")
  row255 <- c("Soil type; Finesand=1 sandyloam=2 \tlength/day\tdepth", "\n")
  row256 <- c("gravelly sand=3 clay=4\t\t\t(m)\t        (m)", "\n")
  row257 <- c("-----------------------------------\t----------\t------", "\n")
  row258 <- c(fd$soiltyp, "\t\t\t\t\t", format(round(fd$burseg, 2), nsmall = 2), "\t\t", format(round(fd$burdep, 2), nsmall = 2), "\n")
  row259 <- c("", "\n")
  row260 <- c("Shelter/Nest properties  (Hominid Paleoshelter)\t\t\t     Shelter transient?", "\n")
  row261 <- c("Used when nest type is not 'NONE' and multiplier >= 1          \t     Is there a tree/log or large shelter", "\n")
  row262 <- c("Length(m) Outer diameter(m) Solar reflectivity(decimal: 1.0 = 100%)  that needs to be run as a transient (Y/N)", "\n")
  row263 <- c("--------- ----------------- ------------------------------------     ------------------------------------------", "\n")
  row264 <- c(format(round(ns$nestlength, 2), nsmall = 2), "\t\t", format(round(ns$outdiam, 2), nsmall = 2), "\t\t\t", format(round(ns$shltrefl, 2), nsmall = 2), "\t\t\t\t\t\t", sQuote(ns$shltrans), "\n")
  row265 <- c("", "\n")
  row266 <- c("Density\t      Specific\tNest height rel. to ground surf.(m)", "\n")
  row267 <- c("nest material Heat      + = below surface,", "\n")
  row268 <- c("(kg/m3)\t      (J/kg-C)  - = above surface", "\n")
  row269 <- c("--------    -----------\t---------------", "\n")
  row270 <- c(format(round(ns$shltdens, 2), nsmall = 2), "\t     ", format(round(ns$shltcp, 2), nsmall = 2), "\t      ", ns$nestheight, "\n")
  row271 <- c("", "\n")
  row272 <- c("TRANSIENT for SHELTER OR ANIMAL(geometry specified above by shelter/nest type)", "\n")
  row273 <- c("Nodes start from the geometrical center of the hollow", "\n")
  row274 <- c("Number of nodes starting at the inner radius of the shelter to the outer surface", "\n")
  row275 <- c("---------------------------------------------------------", "\n")
  row276 <- c(ns$shltnodes, "\n")
  row277 <- c("", "\n")
  row278 <- c("Node locations (m) measured from the geometrical center. Last outer node should be the outer surface.", "\n")
  row279 <- c("Radial dimension of first node should be 0 for an animal, larger than the radius of the animal for a shelter transient.", "\n")
  row280 <- c("For example: for a shelter, if number of nodes = 5: Radii might be 0.03 0.035 0.04 0.05 0.10 ***NOTE: Total thickness must be same as nest wall thickness above***", "\n")
  row281 <- c("-----------------------------------------------------", "\n")
  row282 <- c(ns$noderadius, "\n")
  row283 <- c("", "\n")
  row284 <- c("# These are comments and not read.: logMR = 0.031*T - 2.27\t\t\t\t(Eq. 1)", "\n")
  row285 <- c("# where T = temperature (in  C) and MR is in mlCO2/hr", "\n")
  row286 <- c("", "\n")
  row287 <- c("# My rederivation for O2 instead of CO2 for Tsetse fly assuming protein diet (RQ = 0.8) lnMR = 0.0714*T - 2.561", "\n")
  row288 <- c("# where T = temperature (in C) and MR is in mlO2/g-h", "\n")
  row289 <- c("", "\n")
  row290 <- c("# Eucalyptus properties like those of hickory, oak")

  endo_path <- file.path(output_dir, "endo.dat")
  cat(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10,
      row11, row12, row13, row14, row15, row16, row17, row18, row19, row20,
      row21, row22, row23, row24, row25, row26, row27, row28, row29, row30,
      row31, row32, row33, row34, row35, row36, row37, row38, row39, row40,
      row41, row42, row43, row44, row45, row46, row47, row48, row49, row50,
      row51, row52, row53, row54, row55, row56, row57, row58, row59, row60,
      row61, row62, row63, row64, row65, row66, row67, row68, row69, row70,
      row71, row72, row73, row74, row75, row76, row77, row78, row79, row80,
      row81, row82, row83, row84, row85, row86, row87, row88, row89, row90,
      row91, row92, row93, row94, row95, row96, row97, row98, row99, row100,
      row101, row102, row103, row104, row105, row106, row107, row108, row109, row110,
      row111, row112, row113, row114, row115, row116, row117, row118, row119, row120,
      row121, row122, row123, row124, row125, row126, row127, row128, row129, row130,
      row131, row132, row133, row134, row135, row136, row137, row138, row139, row140,
      row141, row142, row143, row144, row145, row146, row147, row148, row149, row150,
      row151, row152, row153, row154, row155, row156, row157, row158, row159, row160,
      row161, row162, row163, row164, row165, row166, row167, row168, row169, row170,
      row171, row172, row173, row174, row175, row176, row177, row178, row179, row180,
      row181, row182, row183, row184, row185, row186, row187, row188, row189, row190,
      row191, row192, row193, row194, row195, row196, row197, row198, row199, row200,
      row201, row202, row203, row204, row205, row206, row207, row208, row209, row210,
      row211, row212, row213, row214, row215, row216, row217, row218, row219, row220,
      row221, row222, row223, row224, row225, row226, row227, row228, row229, row230,
      row231, row232, row233, row234, row235, row236, row237, row238, row239, row240,
      row241, row242, row243, row244, row245, row246, row247, row248, row249, row250,
      row251, row252, row253, row254, row255, row256, row257, row258, row259, row260,
      row261, row262, row263, row264, row265, row266, row267, row268, row269, row270,
      row271, row272, row273, row274, row275, row276, row277, row278, row279, row280,
      row281, row282, row283, row284, row285, row286, row287, row288, row289, row290,
      file = endo_path, sep = "")

  # --- Begin building the alomvars.dat file -----------------------------------
  hd  <- al$parts$head
  nk  <- al$parts$neck
  trs <- al$parts$torso
  fl  <- al$parts$front_leg
  rl  <- al$parts$rear_leg
  tl  <- al$parts$tail

  .part_row <- function(p) {
    c(format(round(p$diav, 2), nsmall = 2), "\t\t\t", format(round(p$diah, 2), nsmall = 2), "\t\t ", format(round(p$len, 2), nsmall = 2), "    ", format(round(p$dorsfur, 1), nsmall = 1), "     ", format(round(p$dorsfur_a, 1), nsmall = 1), "\t\t",
      format(round(p$ventfur, 1), nsmall = 1), "    ", format(round(p$ventfur_a, 1), nsmall = 1), "\t", format(round(p$dens, 1), nsmall = 1), "\t\t", format(round(p$geom, 0), nsmall = 0))
  }

  rowa1  <- c(paste("Animal species = ", an$species, sep = ""), "\n")
  rowa2  <- c("Allometry input from taxidermy specimen & sedated individual", "\n")
  rowa3  <- c("Assume head long axis in horizontal plane; dorsal = up = vertical: ventral = belly/bottom  ALL PHOTOGRAPH length units in CM **********.", "\n")
  rowa4  <- c("", "\n")
  rowa5  <- c("Vertebrate/invertebrate group                            Locomotion type", "\n")
  rowa6  <- c("'mammal','birdie','reptil','amphib','insect','btrfly'    bipedal/quadped", "\n")
  rowa7  <- c("----------------------------------------------------     ---------------", "\n")
  rowa8  <- c(sQuote(al$group), "\t\t\t\t\t\t", sQuote(al$loco), "\n")
  rowa9  <- c("", "\n")
  rowa10 <- c("HEAD  Geometry types allowed: cylinder(1),sphere(2),ellipsoidal cyl.(3), ellipsoid (4), truncated cone(5). For cone the vert diam entry is large base diam. Horiz diam entry is the \"snout\" diameter.", "\n")
  rowa11 <- c("Dia.vertical(distal)  Dia.horiz(proximal)  Length  Fur depth(mm)-Midorsl  Midventral       Density(kg/m^3)         Geometry ", "\n")
  rowa12 <- c("----------------      -------------------  ------  --------//---------     -----//-----   --------------       ---------", "\n")
  rowa13 <- c(format(round(hd$diav, 2), nsmall = 2), "\t\t\t\t", format(round(hd$diah, 2), nsmall = 2), "\t   ", format(round(hd$len, 2), nsmall = 2), "    ", format(round(hd$dorsfur, 1), nsmall = 1), "    ", format(round(hd$dorsfur_a, 1), nsmall = 1), "\t  ",
              format(round(hd$ventfur, 1), nsmall = 1), "    ", format(round(hd$ventfur_a, 1), nsmall = 1), "\t\t", format(round(hd$dens, 1), nsmall = 1), "\t\t  ", format(round(hd$geom, 0), nsmall = 0), "\n")
  rowa14 <- c("", "\n")
  rowa15 <- c("NECK long axis in horizontal plane: same definitions as above: USER -ONLY 2 CHOICES FOR ALL other body parts\t\t\t****NOTE for the fur depths: the first entry is the \"reference\" fur depth that the photo allometry and flesh", "\n")
  rowa16 <- c("Dia.vertical  Dia. horizont  Length  Fur depth(mm)-Midorsl Fur-vntrl(mm) Density(kg/m^3) Geometry cyl(1), elips cyl(3)      dimensions will be based on. The second entry is for users who want to change fur depth without changing", "\n")
  rowa17 <- c("------------  -------------  ------  ------//---------  \t----//----    --------------  ---------\t\t\t\t\t\t\tbody dimensions.**********", "\n")
  rowa18 <- c(format(round(nk$diav, 2), nsmall = 2), "\t\t", format(round(nk$diah, 2), nsmall = 2), "\t    ", format(round(nk$len, 2), nsmall = 2), "    ", format(round(nk$dorsfur, 1), nsmall = 1), "   ", format(round(nk$dorsfur_a, 1), nsmall = 1), "\t\t",
              format(round(nk$ventfur, 1), nsmall = 1), "   ", format(round(nk$ventfur_a, 1), nsmall = 1), "\t    ", format(round(nk$dens, 1), nsmall = 1), "\t   ", format(round(nk$geom, 0), nsmall = 0), "\n")
  rowa19 <- c("", "\n")
  rowa20 <- c("TORSO: assume long axis in horizontal plane: same definitions as above", "\n")
  rowa21 <- c("Dia.vertical  Dia. horizont  Length  Fur depth(mm)-drsl    \tFur-vntrl(mm)  Density(kg/m^3) Geometry cyl(1), elips cyl(3)", "\n")
  rowa22 <- c("------------  -------------  ------  --------//---------        ----//------    --------------  ---------", "\n")
  rowa23 <- c(format(round(trs$diav, 2), nsmall = 2), "\t\t", format(round(trs$diah, 2), nsmall = 2), "\t     ", format(round(trs$len, 2), nsmall = 2), "    ", format(round(trs$dorsfur, 1), nsmall = 1), "    ", format(round(trs$dorsfur_a, 1), nsmall = 1), "\t\t",
              format(round(trs$ventfur, 1), nsmall = 1), "    ", format(round(trs$ventfur_a, 1), nsmall = 1), "\t    ", format(round(trs$dens, 1), nsmall = 1), "\t\t", format(round(trs$geom, 0), nsmall = 0), "\n")
  rowa24 <- c("", "\n")
  rowa25 <- c("FRONT LEGS: diameters = sideways, front-back                   If 'birdie', set front leg Geometry = 0.", "\n")
  rowa26 <- c("Dia.sideways  Dia. front-back  Length  Fur depth(mm)-drsl  Fur-Midventral Density(kg/m^3) Geometry cyl(1), ellips cyl(3)", "\n")
  rowa27 <- c("------------  -------------    ------  -------//----------  -----//----- -------------  ---------", "\n")
  rowa28 <- c(format(round(fl$diav, 2), nsmall = 2), "\t\t", format(round(fl$diah, 2), nsmall = 2), "\t\t", format(round(fl$len, 2), nsmall = 2), "    ", format(round(fl$dorsfur, 1), nsmall = 1), "   ", format(round(fl$dorsfur_a, 1), nsmall = 1), "\t  ",
              format(round(fl$ventfur, 1), nsmall = 1), "    ", format(round(fl$ventfur_a, 1), nsmall = 1), "\t  ", format(round(fl$dens, 1), nsmall = 1), "\t   ", format(round(fl$geom, 0), nsmall = 0), "\n")
  rowa29 <- c("", "\n")
  rowa30 <- c("BACK LEGS: diameters = sideways, front-back", "\n")
  rowa31 <- c("Dia.sideways  Dia. front-back  Length  Fur depth(mm)-drsl  Fur-Midventral Density(kg/m^3) Geometry cyl(1), ellips cyl(3)", "\n")
  rowa32 <- c("------------  -------------    ------  --------//---------  ----//------ --------------  ---------", "\n")
  rowa33 <- c(format(round(rl$diav, 2), nsmall = 2), "\t\t", format(round(rl$diah, 2), nsmall = 2), "\t\t", format(round(rl$len, 2), nsmall = 2), "    ", format(round(rl$dorsfur, 1), nsmall = 1), "    ", format(round(rl$dorsfur_a, 1), nsmall = 1), "\t  ",
              format(round(rl$ventfur, 1), nsmall = 1), "    ", format(round(rl$ventfur_a, 1), nsmall = 1), "\t  ", format(round(rl$dens, 1), nsmall = 1), "\t   ", format(round(rl$geom, 0), nsmall = 0), "\n")
  rowa34 <- c("", "\n")
  rowa35 <- c("TAIL/ADDITIONAL APPENDAGE: assume long axis in horizontal plane: same definitions as with torso above   Geom: cyl(1),elips      Is 6th part a tail   **Use either if not modeling", "\n")
  rowa36 <- c("Dia.vert (proximal)  Dia. horiz (distal)  Length  Fur depth(mm)-drsl    Fur-vntrl(mm)  Density(kg/m^3)  cyl(3),trunc cone(5)    (T) or proboscis (P) **a 6th appendage", "\n")
  rowa37 <- c("------------        -------------         ------  --------//---------  -----//-----     --------------  ---------               --------------------", "\n")
  rowa38 <- c(format(round(tl$diav, 2), nsmall = 2), "\t\t\t", format(round(tl$diah, 2), nsmall = 2), "\t\t ", format(round(tl$len, 2), nsmall = 2), "    ", format(round(tl$dorsfur, 1), nsmall = 1), "     ", format(round(tl$dorsfur_a, 1), nsmall = 1), "\t\t",
              format(round(tl$ventfur, 1), nsmall = 1), "    ", format(round(tl$ventfur_a, 1), nsmall = 1), "\t", format(round(tl$dens, 1), nsmall = 1), "\t\t", format(round(tl$geom, 0), nsmall = 0), "\t\t\t", sQuote(al$loco), "\n")
  rowa39 <- c("", "\n")
  rowa40 <- c("Absolute measurement (cm)   Shoulder hyt(1);Torso diam (2); Tot length1 (no tail)(3)\tADJUST INITIAL DIMENSIONS", "\n")
  rowa41 <- c("(Real physical dimension)\tTot length2 (incl.tail)(4); Bipedal (5)                      (0=no adjustment; 1=adjust radially; 2= adjust all dimensions) ", "\n")
  rowa42 <- c("------------------  ---------------------------------------------------------   ----------------------------------------------------------------", "\n")
  rowa43 <- c(format(round(al$absval, 2), nsmall = 2), "\t\t\t\t\t", format(round(al$absdim, 0), nsmall = 0), "\t\t\t\t\t\t", format(round(al$adjdim, 0), nsmall = 0), "\n")
  rowa44 <- c("", "\n")
  rowa45 <- c("Subcutaneous Fat on Body Parts (Y/N)\t\t\t\t\t Where does time-dependent mass change come from? ", "\n")
  rowa46 <- c("Head\tNeck\tTorso\tFront Legs\tBack Legs  Tail\t\t 0 = all parts proportionally; 1= torso only (INTEGER!)", "\n")
  rowa47 <- c("----\t----\t-----\t----------\t---------  ----\t\t -------------------------------------------------", "\n")
  rowa48 <- c(sQuote(al$subq_head), "\t", sQuote(al$subq_neck), "\t", sQuote(al$subq_torso), "\t  ", sQuote(al$subq_front_leg), "\t\t", sQuote(al$subq_rear_leg), "\t   ", sQuote(al$subq_tail), "\t\t\t", format(round(al$tmdpfat, 0), nsmall = 0), "\n")
  rowa49 <- c("", "\n")
  rowa50 <- c("Post1  Post2  Post3  Post4     Start Sleep Posture    Start Shade Posture   End Inactive Posture   **Note: Posture 1 = all body parts still modeled, but all in contact with ground", "\n")
  rowa51 <- c("(Y/N)  (Y/N)  (Y/N)  (Y/N)\t\t     (1-4)\t\t\t\t   (1-4)\t\t\t\t(1-4)  Post 2 = legs lumped into torso, head/neck still held up. Post 3 = legs lumped", "\n")
  rowa52 <- c("-----  -----  -----  -----     -------------------    -------------------   --------------------\t\t   into torso, head/neck in contact with ground. Posture 4 = single lump.", "\n")
  rowa53 <- c(sQuote(al$post1), "\t", sQuote(al$post2), "\t", sQuote(al$post3), "\t", sQuote(al$post4), "\t\t", format(round(al$slpstrt, 0), nsmall = 0), "\t\t\t", format(round(al$shdstrt, 0), nsmall = 0), "\t\t\t", format(round(al$endpost, 0), nsmall = 0), "\n")
  rowa54 <- c("", "\n")
  rowa55 <- c("MinFlshK MinFlshK MinFlshK  MinFlshK  MinFlshK  MinFlshK    Variable core temp? (Y/N)      Torso overhang (for leg shade) - horizontal distance between       Leg vertical offset (for leg shade) - vertical distance      Bird sleep            Bird sleep on  NOTE: these last 2 variables only kick in if it's a bird.  ", "\n")
  rowa56 <- c("Head     Neck\t  Torso\t    Front Leg.Rear Leg  Tail     Legs  6APNDG 6APDNG on ground?    widest point on torso and lateral edge of leg (from front view)      between torso and top of top of variable-core-temp       leg  standing?(Y/N)     1 or 2 legs?", "\n")
  rowa57 <- c("----    -----\t  -----\t   ---------   --------  ------ ----- ------ -----------------   --------------------------------------------------------------     -------------------------------------------------------         ------------      -------------", "\n")
  rowa58 <- c(format(round(al$akmin_head, 2), nsmall = 2), "\t", format(round(al$akmin_neck, 2), nsmall = 2), "\t  ", format(round(al$akmin_torso, 2), nsmall = 2), "\t    ", format(round(al$akmin_front_leg, 2), nsmall = 2), "\t     ", format(round(al$akmin_rear_leg, 2), nsmall = 2), "\t", format(round(al$akmin_tail, 2), nsmall = 2), "\t",
              sQuote(al$VTleg), "\t", sQuote(al$VT6th), "\t", sQuote(al$Grd6th), "\t\t\t", format(round(al$torsover, 2), nsmall = 2), "\t\t\t\t\t\t\t\t", format(round(al$torsoff, 2), nsmall = 2), "\t\t\t\t\t\t\t", sQuote(al$brdslp), "\t\t\t", format(round(al$brdslplg, 0), nsmall = 0), "\n")
  rowa59 <- c("", "\n")
  rowa60 <- c("Tc reduced (Y/N)?       Tc=fraction or difference    Fraction (0-1): 1= core,                                        Let leg temp            Let 6th app. temp", "\n")
  rowa61 <- c("Legs     6th Appendage     from local temp (D/F)      0.5 = halfway,  0=ground      Difference     Min Temp (C)    increase if hot? (Y/N)    increase if hot? (Y/N)", "\n")
  rowa62 <- c("------   -------------    ------------//---------      ----------//-----------   ------//------    ------------     ------------------       -------------------", "\n")
  rowa63 <- c(sQuote(al$Tlegred), "\t\t", sQuote(al$T6thred), "\t\t", sQuote(al$Tcred1), "\t   ", sQuote(al$Tcred2), "\t\t", format(round(al$Tfrac1, 2), nsmall = 2), "\t   ", format(round(al$Tfrac2, 2), nsmall = 2), "\t\t",
              format(round(al$Tdif1, 2), nsmall = 2), "\t", format(round(al$Tdif2, 2), nsmall = 2), "\t\t", format(round(al$MinT, 2), nsmall = 2), "\t\t   ", sQuote(al$Tleginc), "\t\t\t", sQuote(al$T6thinc), "\n")
  rowa64 <- c("", "\n")
  rowa65 <- c("**Note: be sure that the minimum and maximum flesh themal conductivity values here do not conflict with endo.dat inputs for overall min/max values.", "\n")
  rowa66 <- c("**Note: be sure to include a minimum flesh thermal K for all body parts regardless of whether they are being modeled.", "\n")

  alomvars_path <- file.path(output_dir, "alomvars.dat")
  cat(rowa1, rowa2, rowa3, rowa4, rowa5, rowa6, rowa7, rowa8, rowa9, rowa10,
      rowa11, rowa12, rowa13, rowa14, rowa15, rowa16, rowa17, rowa18, rowa19, rowa20,
      rowa21, rowa22, rowa23, rowa24, rowa25, rowa26, rowa27, rowa28, rowa29, rowa30,
      rowa31, rowa32, rowa33, rowa34, rowa35, rowa36, rowa37, rowa38, rowa39, rowa40,
      rowa41, rowa42, rowa43, rowa44, rowa45, rowa46, rowa47, rowa48, rowa49, rowa50,
      rowa51, rowa52, rowa53, rowa54, rowa55, rowa56, rowa57, rowa58, rowa59, rowa60,
      rowa61, rowa62, rowa63, rowa64, rowa65, rowa66,
      file = alomvars_path, sep = "")

  log_df <- data.frame(
    file_path = c(endo_path, alomvars_path),
    step      = c("write_endo_dat", "write_alomvars_dat"),
    status    = c("success", "success"),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    stringsAsFactors = FALSE
  )
  if (!is.null(study_area)) log_df$study_area <- study_area

  invisible(log_df)
}
