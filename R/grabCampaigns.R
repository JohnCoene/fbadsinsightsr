#' grabCampaigns
#' 
#' @description Fetches the list of campaigns under a given account.
#' 
#' @inheritParams grabAds
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
#' @author John Coene \email{john.coene@@cmcm.com}
#' 
#' @export
grabCampaigns <- function(id, token, fields = "default", ..., 
                          n = 100,
                          verbose = FALSE){
  
  # check that id is that of account
  if(!length(id[grep("act_", id)]) || nchar(as.character(id)) != 19){
    stop("must be account.id (starting with act_) and followed by 15 digits")
  }
  
  fb_data <- findObjects(id = id, token = token, fields = fields, ..., 
                         n = n, verbose = verbose, object = "campaigns",
                         FUN = "grabCampaigns")
  
  if (nrow(fb_data) == 0) warning(paste("No data."), call. = FALSE)
  
  return(fb_data)
  
}