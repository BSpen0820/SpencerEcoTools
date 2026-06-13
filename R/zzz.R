ee <- NULL

.onLoad <- function(libname, pkgname) {
  ee <<- reticulate::import("ee", delay_load = TRUE)
}
