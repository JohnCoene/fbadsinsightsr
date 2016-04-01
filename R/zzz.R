# startUpMessage -------------------------------
.onAttach <- function(libname, pkgname = "fbAdsInsightsR") {
  packageStartupMessage("Welcome to the fbAdsInsightsR package.", "\n\n",
                        "All documentation is openly available at: ",
                        "bitbucket.org/JohnCheetah/fbadsinsightsrdocs/src")
}