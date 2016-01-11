#' findCampaigns
#' 
#' @description Fetches the list of campaigns under a given account.
#' 
#' @inheritParams findAds
#' 
#' @examples 
#' \dontrun{
#' camp <- findCampaigns(account.id = "123456789", token = "XXXXXX")
#' 
#' # use info for query
#' dat <- getCampaign(campaign.id = sample(camp$id, 1), date.preset = "last_week",
#'  token = "XXXXX")
#' }
#'
#' @seealso \code{\link{getCampaign}}
#' 
#' @author John Coene <john.coene@@cmcm.com>
#' 
#' @export
findCampaigns <- function(id, token, fields = "default", ..., 
                          n = 100,
                          verbose = FALSE){

  fb_data <- findObjects(id = id, token = token, fields = fields, ..., 
                         n = n, verbose = verbose, object = "campaigns")
  
  return(fb_data)
  
}