#' HLS Fmask Bit Description Table
#'
#' A reference data frame describing the bit positions and values used in the
#' Fmask quality band of the NASA Harmonized Landsat Sentinel-2 (HLS) V2.0
#' product. Used internally by download_hls() for cloud and quality masking.
#'
#' @format A data frame with 16 rows and 4 columns:
#' \describe{
#'   \item{Mask_name}{Name of the mask category}
#'   \item{Bit_Position}{Bit position in the Fmask band}
#'   \item{BitValue}{Bit value corresponding to the description}
#'   \item{description}{Human readable description of the bit value}
#' }
#' @source \url{https://lpdaac.usgs.gov/documents/1698/HLS_User_Guide_V2.pdf}
"fmask_bits"

#' MODIS LAI/FPAR QC Bit Description Table
#'
#' A reference data frame describing the bit positions and values used in the
#' FparLai_QC quality band of the MODIS MCD15A3H LAI/FPAR product. Used
#' internally by download_modis_lai() for quality masking.
#'
#' @format A data frame with 15 rows and 4 columns:
#' \describe{
#'   \item{Mask_name}{Name of the mask category}
#'   \item{Bit_Position}{Bit position in the FparLai_QC band}
#'   \item{BitValue}{Bit value corresponding to the description}
#'   \item{description}{Human readable description of the bit value}
#' }
#' @source \url{https://lpdaac.usgs.gov/documents/624/MOD15_User_Guide_V6.pdf}
"FparLAI_QC"

#' AORC Variable Metadata
#'
#' Lookup table describing meteorological variables available in the
#' Analysis of Record for Calibration (AORC) forcing dataset, including
#' variable labels used in AORC files and their associated units.
#'
#' This dataset can be used to identify required climate variables when
#' preparing AORC data for use in microclimate modeling workflows.
#'
#' @format A data frame with 9 rows and 3 variables:
#' \describe{
#'   \item{ClimateVar}{Human-readable climate variable name.}
#'   \item{VarLabel}{Variable name used in AORC data products.}
#'   \item{Units}{Measurement units of the variable.}
#' }
#'
#' @details
#' Variables included are:
#' \itemize{
#'   \item Total precipitation
#'   \item Air temperature
#'   \item Specific humidity
#'   \item Downward longwave radiation
#'   \item Downward shortwave radiation
#'   \item Atmospheric pressure
#'   \item East-west wind component (U)
#'   \item South-north wind component (V)
#'   \item Diffuse radiation
#' }
#'
#' @source NOAA Analysis of Record for Calibration (AORC)
"AORC_meterodf"

#' Microclim Meteorological Variable Metadata
#'
#' Lookup table describing meteorological variables and naming conventions
#' used by the microclimate modeling functions in this package.
#'
#' This dataset provides standardized variable names and units expected by
#' Microclim-compatible forcing datasets.
#'
#' @format A data frame with 9 rows and 3 variables:
#' \describe{
#'   \item{ClimateVar}{Human-readable climate variable name.}
#'   \item{VarLabel}{Standardized variable name used by Microclim.}
#'   \item{Units}{Measurement units of the variable.}
#' }
#'
#' @details
#' Variables included are:
#' \itemize{
#'   \item Total precipitation
#'   \item Air temperature
#'   \item Relative humidity
#'   \item Downward longwave radiation
#'   \item Downward shortwave radiation
#'   \item Atmospheric pressure
#'   \item Wind speed
#'   \item Wind direction
#'   \item Diffuse radiation
#' }
#'
#' @seealso
#' \code{\link{AORC_meterodf}}
"Microclim_meterodf"

#' Remotely Sourced Data in the Microclimate Modeling Pipeline
#'
#' This is a documentation-only page (no associated data object) summarizing
#' every externally sourced dataset used across the preprocessing pipeline in
#' `R/Microclimf_DataPrep.R`, for use as a supplementary methods reference
#' when citing data sources in a manuscript.
#'
#' @section Data sources:
#'
#' | Data Type | Title / Product | Variable(s) Used | Unit | Reference |
#' | --- | --- | --- | --- | --- |
#' | Elevation | Copernicus GLO-30 DEM | Elevation | meters | European Space Agency, Sinergise (2021). *Copernicus Global Digital Elevation Model*. Distributed by OpenTopography. \doi{10.5069/G9028PQB} |
#' | Surface reflectance (primary) | Harmonized Landsat Sentinel-2 (HLS) v2.0 - HLSS30 & HLSL30 | Red, green, blue, NIR surface reflectance | unitless (reflectance, 0-1) | Masek, J., Ju, J., Roger, J., Skakun, S., Vermote, E., Claverie, M., Dungan, J., Yin, Z., Freitag, B., & Justice, C. (2021). *HLS Sentinel-2 Multi-spectral Instrument Surface Reflectance Daily Global 30 m v2.0 (HLSS30)* and *HLS Landsat Operational Land Imager Surface Reflectance Daily Global 30 m v2.0 (HLSL30)*. NASA EOSDIS Land Processes DAAC. \doi{10.5067/HLS/HLSS30.002}; \doi{10.5067/HLS/HLSL30.002} |
#' | Surface reflectance (alternative) | Copernicus Sentinel-2 MSI Level-2A Surface Reflectance (Harmonized) | Red, green, blue, NIR surface reflectance | unitless (reflectance, 0-1) | European Space Agency (2022). *Sentinel-2 MSI Level-2A BOA Reflectance, Harmonized*. Copernicus Programme. \url{https://sentinel.esa.int/web/sentinel/user-guides/sentinel-2-msi} |
#' | Cloud/shadow mask | Cloud Score+ (S2_HARMONIZED) | Cloud/shadow quality score (`cs`, `cs_cdf`) | unitless (0-1 probability) | Pasquarella, V. J., Brown, C. F., Czerwinski, W., & Rucklidge, W. J. (2023). Comprehensive quality assessment of optical satellite imagery using weakly supervised video learning. *CVPR Workshops (EarthVision)*. Dataset: Google Earth Engine `GOOGLE/CLOUD_SCORE_PLUS/V1/S2_HARMONIZED` |
#' | Leaf Area Index | MODIS MCD15A3H v061 | LAI | m^2^ m^-2^ | Myneni, R., Knyazikhin, Y., & Park, T. (2021). *MODIS/Terra+Aqua Leaf Area Index/FPAR 4-Day L4 Global 500 m SIN Grid V061*. NASA EOSDIS Land Processes DAAC. \doi{10.5067/MODIS/MCD15A3H.061} |
#' | Albedo | MODIS MCD43A3 v061 | Black-sky/white-sky albedo | unitless (fraction, 0-1) | Schaaf, C., & Wang, Z. (2021). *MODIS/Terra+Aqua BRDF/Albedo Daily L3 Global 500 m SIN Grid V061*. NASA EOSDIS Land Processes DAAC. \doi{10.5067/MODIS/MCD43A3.061} |
#' | Soil physical properties | SoilGrids 2.0 | Bulk density, clay, sand, silt fraction | cg cm^-3^ (bulk density); g kg^-1^ (clay/sand/silt) | Poggio, L., de Sousa, L. M., Batjes, N. H., Heuvelink, G. B. M., Kempen, B., Ribeiro, E., & Rossiter, D. (2021). SoilGrids 2.0: producing soil information for the globe with quantified spatial uncertainty. *SOIL*, 7, 217-240. \doi{10.5194/soil-7-217-2021} |
#' | Hourly climate forcing | NOAA Analysis of Record for Calibration (AORC) v1.1 | Precipitation, downward longwave/shortwave radiation, surface pressure, specific humidity, 2 m air temperature, 10 m U/V wind components | mm hr^-1^; W m^-2^; Pa; kg kg^-1^; degC (converted from K); m s^-1^ | NOAA Office of Water Prediction (2021). *Analysis of Record for Calibration (AORC) v1.1*. NOAA National Water Model, distributed via AWS Open Data. \url{https://registry.opendata.aws/noaa-nws-aorc/} |
#' | Land cover | National Land Cover Database (NLCD) | Land cover class | categorical | Dewitz, J. (2021). *National Land Cover Database (NLCD) 2019 Products*. U.S. Geological Survey data release. \doi{10.5066/P9KZCM54} |
#' | Vegetation height | LANDFIRE Existing Vegetation Height (EVH) | Vegetation height class | categorical -> meters | LANDFIRE (2022). *Existing Vegetation Height, LANDFIRE 2.2.0*. U.S. Department of the Interior, Geological Survey; U.S. Department of Agriculture, Forest Service. \url{https://www.landfire.gov} |
#'
#' @section Notes:
#' HLS and Sentinel-2 are alternates in the pipeline (`download_hls()` vs
#' `download_s2()`), not both required for a given run - cite whichever
#' produced the final imagery. Cloud Score+ is a mask rather than a modeled
#' variable, but determines which pixels survive into the reflectance
#' composites and is worth citing on that basis. AORC access dates matter for
#' citation purposes since NOAA periodically updates the Zarr store version.
#'
#' @seealso
#' \code{\link{AORC_meterodf}}, \code{\link{Microclim_meterodf}}
#' @name data_sources
NULL
