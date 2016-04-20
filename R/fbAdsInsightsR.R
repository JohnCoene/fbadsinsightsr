#' fbAdsInsightsR
#' 
#' Set of functions to collect data from Facebook Insights API.
#' 
#' @section Functions:
#' 
#' \itemize{
#' \item \code{\link{fbAuthenticate}}
#' \item \code{\link{getAccount}}
#' \item \code{\link{getCampaign}}
#' \item \code{\link{getAdset}}
#' \item \code{\link{getAd}}
#' \item \code{\link{getAny}}
#' \item \code{\link{getActivity}}
#' \item \code{\link{listBusinesses}}
#' \item \code{\link{listAccounts}}
#' \item \code{\link{listVideos}}
#' \item \code{\link{listImages}}
#' \item \code{\link{listCreatives}}
#' \item \code{\link{findFields}}
#' \item \code{\link{findParams}}
#' \item \code{\link{grabCampaigns}}
#' \item \code{\link{grabAdsets}}
#' \item \code{\link{grabAds}}
#' \item \code{\link{grabCreatives}} (.deprecated, use listCreatives)
#' \item \code{\link{checkTarget}}
#' \item \code{\link{checkStatus}}
#' }
#' 
#' @examples 
#' \dontrun{
#' # run authentication with your app details
#' fbOAuth <- fbAuthenticate(app.id = "1234567890123456", 
#'                           app.secret = "16xx79321xx0130x2x10a08x3e2x80xx", 
#'                           scope = "ads_management")
#'                           
#' # list facebook advertising accounts you have access to
#' accounts <- listAccounts(id = "me", token = fbOAuth)
#' 
#' # grab list of ads under random account
#' ads <- grabAds(sample(accounts$id, 1), fbOAuth)
#' 
#' # get data on random ad
#' set.seed(19880525)
#' ad_data <- getAd(ad.id = sample(ads$id, 1), token = fbOAuth)
#' 
#' # get daily performance data for last month with a summary
#' my_preset <- findParams("date.preset")[grep("last_month", 
#'                                             findParams("date.preset"))]
#' ad_data <- getAd(ad.id = sample(ads$id, 1), token = fbOAuth, n = 1000, 
#'                  date.preset = my_preset, time.increment = 1)
#'                  
#' # get ad performance data by country 
#' ad_data <- getAd(ad.id = sample(ads$id, 1), token = fbOAuth,
#'                  breakdowns = "country")
#'                  
#' # get ad performance data by age and gender 
#' ad_data <- getAd(ad.id = sample(ads$id, 1), token = fbOAuth,
#'                  breakdowns = c("age", "gender"))
#'                  
#' # list creatives
#' creatives <- listCreatives(sample(accounts$id, 1), fbOAuth)
#' 
#' # check targeting of adset
#' adsets <- grabAdsets(sample(accounts$id, 1), fbOAuth)
#' (target <- checkTarget(sample(adsets$id, 1), fbOAuth))
#' }
#' 
#' @docType package
#' @name fbAdsInsightsR
NULL