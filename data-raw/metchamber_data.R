# Regenerates data/metchamber_metout.rda and data/metchamber_soil.rda from the
# hand-crafted metabolic-chamber calibration files NicheMapR's maintainers
# provided alongside endo_alomvars_auto_V5.R. Per that folder's
# "Metabolic Chamber Instructions.docx": these files simulate a chamber by
# ramping air/ground/sky temperature ~1 degC warmer each hour across the
# first several of 12 nominal "days", holding wind/humidity/solar constant.
# Not observational data - do not regenerate or "fix" the duplicate TALOC
# column names below; downstream code (run_metabolic_chamber()) relies on
# reproducing read.csv()'s exact auto-deduped names.

mc_dir <- "D:/Code/Niche Mapper/NicheMap_UpdatedExe/Endo R Input_noexecutable/Metabolic Chamber Files"

metchamber_metout <- read.csv(file.path(mc_dir, "metout.csv"))
metchamber_soil   <- read.csv(file.path(mc_dir, "soil.csv"))

save(metchamber_metout, file = "data/metchamber_metout.rda", compress = "bzip2")
save(metchamber_soil,   file = "data/metchamber_soil.rda",   compress = "bzip2")
