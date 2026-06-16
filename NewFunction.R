
setwd('D:\\Code\\PhD\\Chapter 3 - Tradeoffs\\Landscape Thermal Modeling')

## ---- libraries ------------------------------------------------------------
library(microclimfPara)
library(terra)
library(tidyverse)
library(rhdf5)
library(sf)

devtools::load_all('D:/Code/PhD/R_Packages/SpencerEcoTools')

# HPC Option
# terraOptions(
#   threads = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "16")),
#   memfrac = 0.7,
#   tempdir = "~/terra_temp"
# )

## Define Date Range and read in static dem -----------------------------------------


moose <- st_read('./Data/Moose MCPs/MooseGPSLocs.shp')
moose$t_<- as.POSIXct(moose$t_, tz = "UTC")

dates <- unique(moose$t_)
dates <- dates[month(dates) %in% 3:8 & day(dates) == 1]
dates <- as.Date(dates)
dates_df <- tibble(dates = dates) %>% 
  group_by(year(dates)) %>% 
  summarise(Start_Dates = min(dates), End_Dates = max(dates)) %>% 
  select(-1)

dem_fine <- rast('./Data/DEM/DEM_GLO30.tif')
dem_coarse <- rast('./Data/DEM/DEM_800m.tif')

rm(moose)

dates <- as.Date(t(dates_df[1,])[,1])

tiles <- create_tiles(coarse_dem = dem_coarse, fine_dem = dem_fine, dates = dates,
                      snow_modeling = T, return_tiles_rast = T, output_path = './tilemap.tif')




run_micro_big_nichemap <- function(tiles, #tile object from create_tiles
                               clim, #either dir if iteratively through time or single climarray from package_climate() or file path to single climarray
                               dates, #eiter dataframe of dates or vector of start and end date
                               dtm_fine, #fine dem, what the microclimate resolution will be, or file path to it
                               dtm_coarse, #coarse dem, what current climate (clim) resolution is, or file path to it
                               vegp, #either dir if iteratively or vegp object from package_veg_soil or file path to single file
                               soilc, #either dir if iteratively or soilc object from package_veg_soil or file path to single file
                               output_dir, #output folder, will create a subdir for micropoints, smod, and mout
                               reqhgt       = 2, #height in which microclimate will be needed, should always be positive
                               zref         = 2, #height in which coarse climate variables were measured except wind
                               windhgt      = 10, #height in which wind is measured since typically higher
                               matemp       = NA, #mean annual temp if provided, to refine below ground temps
                               snow = FALSE, #should snow modeling be done?
                               snowenv = "Taiga", #optional parameter used if snow = T to specify snow environment, see microclimf for details
                               Dynreqhgt = FALSE, #optional parameter used if snow = T to specify if reqhgt should move as snow accumulates
                               altcorrect = 0, #which altcorrect method should be used, see microclimf for details
                               parallel = FALSE, #should snow modeling and microclimate modeling use parallel processing
                               ncores = 2, #option parameter used if parallel = T
                               study_area   = NULL, #create a subdir with study area, and add study area to final names of output files
                               file_fmt = c("h5", "nc"), #write to hf file or nc file
                               compression = 4L, #compression level 0-9 only
                               ...){ #hidden optional parameters for slurm processing, will divide tile work across nodes automatically, independent of parallel processing as nodes can still parallel process
  
  
  #dubug, deactivate before running
  
  tiles = tiles #tile object from create_tiles
  clim = './Data/Weather/GMU1_Pkg/GMU1_Climate_20200301_to_20200801.RDS'  #either dir if iteratively through time or single climarray from package_climate()
  dates = dates #eiter dataframe of dates or vector of start and end date
  dtm_fine = dem_fine #fine dem, what the microclimate resolution will be
  dtm_coarse = dem_coarse #coarse dem, what current climate (clim) resolution is
  vegp = './Data/VegPara/GMU1_VegPara_20200301_to_20200801.RDS' #either dir if iteratively or vegp object from package_veg_soil
  soilc = './Data/SoilPara/GMU1_SoilPara_20200301_to_20200801.RDS' #either dir if iteratively or soilc object from package_veg_soil
  output_dir = "./Microclim_out" #output folder, will create a subdir for micropoints, smod, and mout
  reqhgt = 2 #height in which microclimate will be needed, should always be positive
  zref = 2 #height in which coarse climate variables were measured except wind
  windhgt = 10 #height in which wind is measured since typically higher
  matemp = NA #mean annual temp if provided, to refine below ground temps, otherwised averaged from clim
  snow = TRUE #should snow modeling be done?
  snowenv = "Alpine" #optional parameter used if snow = T to specify snow environment, see microclimf for details
  Dynreqhgt = TRUE
  altcorrect = 0 #which altcorrect method should be used, see microclimf for details
  parallel = FALSE #should snow modeling and microclimate modeling use parallel processing
  ncores = 2 #option parameter used if parallel = T
  study_area = 'GMU1'
  file_fmt = "h5"
  compression = 4L
  clust_array_arg = NULL
  clust_array_size = NULL
  
  #... slurm checks for hidden optional parameters ----------
  
  dots <- list(...)
  allowed <- c("clust_array_arg", "clust_array_size")
  unknown <- setdiff(names(dots), allowed)
  if (length(unknown) > 0) {
    stop("Unknown argument(s): ", paste(unknown, collapse = ", "))
  }
  
  clust_array_arg  <- dots$clust_array_arg
  clust_array_size <- dots$clust_array_size
  
  # Validate cluster array arguments
  if (!is.null(clust_array_arg) &&
      (!is.numeric(clust_array_arg) || length(clust_array_arg) != 1)) {
    stop("clust_array_arg must be a single numeric value or NULL")
  }
  if (!is.null(clust_array_size) &&
      (!is.numeric(clust_array_size) || length(clust_array_size) != 1)) {
    stop("clust_array_size must be a single numeric value or NULL")
  }
  if (!is.null(clust_array_arg) && is.null(clust_array_size)) {
    stop("clust_array_size must be provided when clust_array_arg is set")
  }
  if (!is.null(clust_array_arg) &&
      (clust_array_arg < 1 || clust_array_arg > clust_array_size)) {
    stop("clust_array_arg must be between 1 and clust_array_size")
  }
  
  if(reqhgt <= 0)stop('reqhgt must be positive for the function')
  
  # slurm check end ----------
  
  
  sdepth    <- c(0, 1.5, 5, 10, 15, 20, 30, 50, 100, 200) / -100   # Static needed soil depths for nichemapper
  allheights <- c(reqhgt, sdepth)
  
  #Ok create terrain features first before cropping to avoid edge effects
  
  slope <- terrain(dtm_fine, v = "slope")
  aspect <- terrain(dtm_fine, v = "aspect")
  twi <- microclimf:::.topidx(dtm_fine)

  hor <- array(NA,dim=c(dim(dtm_fine)[1:2], 24))
  for (i in 1:24) hor[,,i] <- microclimf:::.horizon(dtm_fine, (i-1) * 15)

  msl<-tan(apply(atan(hor),c(1,2),mean))
  svfa<-0.5*cos(2*msl)+0.5
  wsa<-microclimf:::.windsheltera(dtm_fine, 2, 1)
  
  rm(msl)
  gc()
  
  tiles_task <- data.frame(tiles = 1:(max(values(tiles$tiles_rast))))
  tiles_task$node <- if(is.null(clust_array_size)) {1} else rep(1:clust_array_size, length = nrow(tiles_task))
  tiles_task <- if(is.null(clust_array_arg)) {tiles_task} else tiles_task[tiles_task$node == clust_array_arg, ]
  
  for(i in seq_len(nrow(tiles_task))){
    
    i <- 1
    
    tile_proc <- tiles$tiles_proc[i,]
    tile_core <- tiles$tiles_core[i,]
    
    dem_coarse_tile <- crop(dem_coarse, ext(tile_proc))
    dem_fine_tile <- crop(dem_fine, dem_coarse_tile, snap = 'out')
    
    dem_fine_core <- crop(dem_fine, ext(tile_core))
    
    # Crop terrain Features
    slope_tile <- crop(slope, dem_fine_tile)
    aspect_tile <- crop(aspect, dem_fine_tile)
    twi_tile <- crop(twi, dem_fine_tile)
    
    crop_cells_numbs_crop <- cellFromXY(dem_fine, xyFromCell(dem_fine_tile, 
                                                        cell = 1:(nrow(dem_fine_tile)*ncol(dem_fine_tile))))
    crop_rowcol_buff <- rowColFromCell(dem_fine, crop_cells_numbs_crop)
    
    hor_tile <- hor[unique(crop_rowcol_buff[,1]), unique(crop_rowcol_buff[,2]), , drop = FALSE]
    svfa_tile <- svfa[unique(crop_rowcol_buff[,1]), unique(crop_rowcol_buff[,2]),  drop = FALSE]
    wsa_tile <- wsa[unique(crop_rowcol_buff[,1]), unique(crop_rowcol_buff[,2]), , drop = FALSE]
    
    gc()
    
    dates_seq <- if(is.null(attr(dates, 'data.frame'))) {1} else {nrow(dates)}
    
    for(d in 1:dates_seq){
      
      d <- 1
      
      # Crop soil
      soilc.d  <- read_rds(soilc)
      soil.crop <- lapply(soilc.d, function(x) wrap(crop(unwrap(x), dem_fine_tile)))
      attr(soil.crop,"class") <- attr(soilc.d, "class")
      rm(soilc.d); gc()
      
      vegp.d     <- read_rds(vegp)
      veg.crop  <- lapply(vegp.d, function(x) wrap(crop(unwrap(x), dem_fine_tile)))
      attr(veg.crop, "class") <- attr(vegp, "class")
      rm(vegp.d); gc()
      
      climdatag <- read_rds(clim)
      clim.crop <- lapply(climdatag, function(x) wrap(crop(unwrap(x), dem_coarse_tile)))
      rm(climdatag); gc()
      
      # note need to add functionality where dates can be vector or dataframe
      tme <- as.POSIXlt(seq(as.POSIXct(sprintf("%s 00:00:00", dates[1]), tz = "UTC"), 
                            as.POSIXct(sprintf("%s 23:00:00", dates[2]), tz = "UTC"), by = "1 hour"))
      
      # Create output directory for this period
      out_parts <- c(output_dir)
      
      period_label <- sprintf("%s_to_%s",
                              format(dates[1], "%Y%m%d"),
                              format(dates[2],   "%Y%m%d"))
      
      if (!is.null(study_area)) out_parts <- c(out_parts, study_area)
      out_parts <- c(out_parts, "Micropoint_Models", period_label)
      out_dir   <- do.call(file.path, as.list(out_parts))
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      
      for(h in allheights){
        
        h <- allheights[1]
        
        pointa <- microclimfPara::runpointmodela(climarrayr = clim.crop, tme = tme, reqhgt = h,
                                                 dtm = dem_fine_tile, vegp = veg.crop, soilc = soil.crop,
                                                 matemp = matemp, zref = zref, windhgt = windhgt)
        
        outf <- file.path(out_dir,
                          sprintf("%s_Grdhght_MicropointModel_%s.RDS", h, period_label))
        readr::write_rds(pointa, outf)
  
      }
      
      if(snow == T) {
        
        outf <- file.path(out_dir,
                          sprintf("%s_Grdhght_MicropointModel_%s.RDS", allheights[1], period_label))
        pointa <- read_rds(outf)
        
        smod <- microclimfPara::runsnowmodel(weather = clim.crop, micropoint = pointa, vegp = veg.crop, soilc = soil.crop,
                             dtm = dem_fine_tile, dtmc = dem_coarse_tile, tme = tme, altcorrect = altcorrect,
                             snowenv = snowenv, method = 'slow', zref = zref, windhgt = windhgt, parallel = parallel,
                             ncores = ncores)
        
        out_parts <- c(output_dir)
        
        period_label <- sprintf("%s_to_%s",
                                format(dates[1], "%Y%m%d"),
                                format(dates[2],   "%Y%m%d"))
        
        if (!is.null(study_area)) out_parts <- c(out_parts, study_area)
        out_parts <- c(out_parts, "Snow_Models", period_label)
        out_dir   <- do.call(file.path, as.list(out_parts))
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        
        outf <- file.path(out_dir,
                          sprintf("%s_SnowModel_%s.RDS", study_area, period_label))
        
        smod.trim <- trim_tile_buffer(smod, dem_proc = dem_fine_tile, dem_core = dem_fine_core)
        
        if(file_fmt == "h5"){
          write_tile(smod.trim, out_path = outf, tme = tme, compression = compression, file_fmt = file_fmt)
        } else {
          write_tile(smod.trim, out_path = outf, tme = tme, compression = compression, file_fmt = file_fmt,
                     dtm = dem_fine_core)
        }
        
        rm(smod.trim)
        
      }
      
      out_parts.mout <- c(output_dir)
      
      period_label <- sprintf("%s_to_%s",
                              format(dates[1], "%Y%m%d"),
                              format(dates[2],   "%Y%m%d"))
      
      if (!is.null(study_area)) out_parts.mout <- c(out_parts.mout, study_area)
      out_parts.mout <- c(out_parts.mout, "Microclim_Models", period_label)
      out_dir.mout   <- do.call(file.path, as.list(out_parts.mout))
      dir.create(out_dir.mout, recursive = TRUE, showWarnings = FALSE)
    
      for(h in allheights){
        
        h <- allheights[1]
        
        out_parts <- c(output_dir)
        
        period_label <- sprintf("%s_to_%s",
                                format(dates[1], "%Y%m%d"),
                                format(dates[2],   "%Y%m%d"))
        
        if (!is.null(study_area)) out_parts <- c(out_parts, study_area)
        out_parts <- c(out_parts, "Micropoint_Models", period_label)
        out_dir   <- do.call(file.path, as.list(out_parts))
        outf.pointa <- file.path(out_dir,
                          sprintf("%s_Grdhght_MicropointModel_%s.RDS", h, period_label))
        
        
        pointa <- read_rds(outf)
        
        if(snow == T) {
          mout <- microclimfPara::runmicro(micropoint = pointa, reqhgt = h, vegp = veg.crop, soilc = soil.crop,
                                         dtm = dem_fine_tile, dtmc = dem_coarse_tile, altcorrect = altcorrect,
                                         snow = snow, snowmod = smod, runchecks = TRUE, slr = slope_tile,
                                         apr = aspect_tile, hor = hor_tile, twi = twi_tile, wsa = wsa_tile,
                                         svf = svfa_tile, Dynreqhgt = Dynreqhgt, parallel = parallel, ncores = ncores)
        } else {
          mout <- microclimfPara::runmicro(micropoint = pointa, reqhgt = h, vegp = veg.crop, soilc = soil.crop,
                                           dtm = dem_fine_tile, dtmc = dem_coarse_tile, altcorrect = altcorrect,
                                           snow = snow, runchecks = TRUE, slr = slope_tile,
                                           apr = aspect_tile, hor = hor_tile, twi = twi_tile, wsa = wsa_tile,
                                           svf = svfa_tile, Dynreqhgt = Dynreqhgt, parallel = parallel, ncores = ncores)
          
        }
        
        
        outf <- file.path(out_dir,
                          sprintf("%s_Grdhght_MicroclimModel_%s.RDS", h, period_label))
        
        mout.trim <- trim_tile_buffer(mout, dem_proc = dem_fine_tile, dem_core = dem_fine_core)
        
        if(file_fmt == "h5"){
          write_tile(mout.trim, out_path = outf, compression = compression, file_fmt = file_fmt)
        } else {
          write_tile(mout.trim, out_path = outf, compression = compression, file_fmt = file_fmt,
                     dtm = dem_fine_core)
        }
        
      }
      
      
    }
  }
}


