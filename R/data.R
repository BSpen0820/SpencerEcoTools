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
