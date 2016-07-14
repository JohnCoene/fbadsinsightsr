# startUpMessage -------------------------------
.onAttach <- function(libname, pkgname = "fbAdsInsightsR") {
  packageStartupMessage("Run ?fbAdsInsightsR to get started")
}
.onUnload <- function (libpath) {
  library.dynam.unload("fbAdsInsightsR", libpath)
}