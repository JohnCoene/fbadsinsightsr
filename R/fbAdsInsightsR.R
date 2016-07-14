#' fbAdsInsightsR
#' 
#' Set of functions to collect data from Facebook Insights API.
#' 
#' @section Functions:
#' 
#' OAuth
#' \itemize{
#' \item \code{\link{fbAuthenticate}}
#' }
#' Get-family
#' \itemize{
#' \item \code{\link{getAccount}}
#' \item \code{\link{getCampaign}}
#' \item \code{\link{getAdset}}
#' \item \code{\link{getAd}}
#' \item \code{\link{getAny}}
#' \item \code{\link{getActivity}}
#' \item \code{\link{getCreative}}
#' \item \code{\link{getLabCampaigns}}
#' \item \code{\link{getLabCreatives}}
#' \item \code{\link{getLabAdsets}}
#' \item \code{\link{getLabAds}}
#' \item \code{\link{getEstReach}}
#' }
#' List-family
#' \itemize{
#' \item \code{\link{listBusinesses}}
#' \item \code{\link{listAccounts}}
#' \item \code{\link{listVideos}}
#' \item \code{\link{listImages}}
#' \item \code{\link{listCreatives}}
#' \item \code{\link{listApps}}
#' \item \code{\link{listAdApps}}
#' \item \code{\link{listLabels}}
#' }
#' Grab-family
#' \itemize{
#' \item \code{\link{grabCampaigns}}
#' \item \code{\link{grabAdsets}}
#' \item \code{\link{grabAds}}
#' \item \code{\link{grabCreatives}} (.deprecated, use \code{\link{listCreatives}})
#' }
#' Check-family
#' \itemize{
#' \item \code{\link{checkTarget}} (.deprecated, use \code{\link{checkTargetSentence}})
#' \item \code{\link{checkStatus}}
#' \item \code{\link{checkTargetTree}}
#' \item \code{\link{checkTargetSentence}}
#' \item \code{\link{checkUsers}}
#' }
#' Update-family
#' \itemize{
#' \item \code{\link{updateCampaign}}
#' \item \code{\link{updateAdset}}
#' \item \code{\link{updateAd}}
#' }
#' Create-family
#' \itemize{
#' \item \code{\link{createLabels}}
#' }
#' Helpers
#' \itemize{
#' \item \code{\link{findFields}}
#' \item \code{\link{findParams}}
#' }
#' 
#' @examples 
#' \dontrun{
#' # run authentication with your app details
#' TK <- fbAuthenticate(app.id = "1234567890123456", 
#'                      app.secret = "16xx79321xx0130x2x10a08x3e2x80xx", 
#'                      scope = "ads_management")
#'                           
#' # list facebook advertising accounts you have access to
#' accounts <- listAccounts(id = "me", token = TK)
#' 
#' # grab list of ads under random account
#' ads <- grabAds(sample(accounts$id, 1), TK)
#' 
#' # get data on random ad
#' set.seed(19880525)
#' ad_data <- getAd(ad.id = sample(ads$id, 1), token = TK)
#' 
#' # get daily performance data for last month with a summary
#' my_preset <- findParams("date.preset")[grep("last_month", 
#'                                             findParams("date.preset"))]
#' ad_data <- getAd(ad.id = sample(ads$id, 1), token = TK, n = 1000, 
#'                  date.preset = my_preset, time.increment = 1)
#'                  
#' # get ad performance data by country 
#' ad_data <- getAd(ad.id = sample(ads$id, 1), token = TK,
#'                  breakdowns = "country")
#'                  
#' # get ad performance data by age and gender 
#' ad_data <- getAd(ad.id = sample(ads$id, 1), token = TK,
#'                  breakdowns = c("age", "gender"))
#'                  
#' # list creatives
#' creatives <- listCreatives(sample(accounts$id, 1), TK)
#' 
#' # check targeting of adset
#' adsets <- grabAdsets(sample(accounts$id, 1), TK)
#' (target <- checkTargetSentence(sample(adsets$id, 1), TK))
#' 
#' # check targeting from account
#' targeting <- checkTargetTree(sample(accounts$id, 1), TK)
#' 
#' # get labels
#' labels <- listLabels(accounts$id[1], TK)
#' 
#' # get ads that match ANY label
#' ad_labels <- getLabAds(accounts$id[1], labels = labs, operator = "ANY", 
#'                        token = TK)
#' 
#' # get campaigns that match ALL labels                        
#' ad_labels <- getLabAds(accounts$id[1], labels = labs, operator = "ALL", 
#'                        token = TK)
#'                        
#' (apps <- listApps(accounts$id[1], TK)) # list apps
#' (apps <- listAdApps(accounts$id[1], TK)) # list advertiseable apps
#' }
#' 
#' @keywords internal
#' 
#' @useDynLib fbAdsInsightsR
#' @importFrom Rcpp sourceCpp
#' 
#' @docType package
#' @name fbAdsInsightsR
NULL