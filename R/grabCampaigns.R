#' Grab list of campaigns
#' 
#' @description Fetches the list of campaigns under a given account.
#' 
#' @inheritParams grabAds
#' 
#' @examples 
#' \dontrun{
#' camps <- grabCampaigns(id = "act_123456789012345", token = "XXXXXX")
#' }
#'
#' @seealso \code{\link{getCampaign}}
#' 
#' @author John Coene \email{jcoenep@@gmail.com}
#' 
#' @export
grabCampaigns <- function(id, token, fields = "default", ..., n = 100, 
                          verbose = FALSE, limit = 100){
  
  # check that id is that of account
  if(!length(id[grep("act_", id)]) || nchar(as.character(id)) > 20){
    stop("must be id (starting with act_) and followed by digits")
  }
  
  fb_data <- findObjects(id = id, token = token, fields = fields, ..., 
                         n = n, verbose = verbose, object = "campaigns",
                         FUN = "grabCampaigns", limit = limit)
  
  if (nrow(fb_data) == 0) warning(paste("No data."), call. = FALSE)
  
  return(fb_data)
  
}