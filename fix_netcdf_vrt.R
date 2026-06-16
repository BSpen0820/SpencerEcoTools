
devtools::load_all('D:\\Code\\PhD\\R_Packages\\SpencerEcoTools', quiet = TRUE)
library(terra)
library(rhdf5)
library(ncdf4)

mout <- readRDS("./base/mout_slow_snow.rds")
tme  <- mout$tme
nr <- dim(mout$Tz)[1]; nc <- dim(mout$Tz)[2]; nt <- length(tme)

mout_vars <- names(SpencerEcoTools:::.mout_meta)

dtm_full <- rast(nrow = nr, ncol = nc,
                 xmin = -6,  xmax = -6  + nc   * 0.0027,
                 ymin = 49,  ymax = 49  + nr   * 0.0027,
                 crs  = "EPSG:4326")

nr_half  <- nr %/% 2L
split_y  <- ymax(dtm_full) - nr_half * res(dtm_full)[2]
dtm_top  <- crop(dtm_full, ext(xmin(dtm_full), xmax(dtm_full), split_y, ymax(dtm_full)))
dtm_bot  <- crop(dtm_full, ext(xmin(dtm_full), xmax(dtm_full), ymin(dtm_full), split_y))

mout_top <- lapply(mout_vars, function(v) mout[[v]][seq_len(nr_half), , , drop = FALSE])
names(mout_top) <- mout_vars; mout_top$tme <- tme
mout_bot <- lapply(mout_vars, function(v) mout[[v]][seq(nr_half + 1L, nr), , , drop = FALSE])
names(mout_bot) <- mout_vars; mout_bot$tme <- tme

str(mout_top)

write_tile(mout_top, "./base/mout_top.nc", dtm = dtm_top, file_fmt = "nc")
write_tile(mout_bot, "./base/mout_bot.nc", dtm = dtm_bot, file_fmt = "nc")

rm(list = ls())

plot(rast('./base/mout_top.nc', subds = 'Tz'))

library(sf)

gdal_paths <- c(
  "NETCDF:\"./base/mout_bot.nc\":Tz",
  "NETCDF:\"./base/mout_top.nc\":Tz"
)

# Force GDAL to build it anyway. It will ignore the dimension names 
# and use the spatial extents it reads from the metadata.
sf::gdal_utils(
  util = "buildvrt",
  source = gdal_paths,
  destination = "temp_mosaic.vrt"
)

# Load into terra instantly
v_mosaic <- terra::rast("temp_mosaic.vrt")

