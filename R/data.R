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

