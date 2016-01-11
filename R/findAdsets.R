#' findAdsets
#' 
#' @description Fetches the list of adsets under a given account or campaign.
#' 
#' @inheritParams findAds
#' 
#' @examples 
#' \dontrun{
#' adsets <- findAdsets(account.id = "act_123456789123456", token = "XXXXXX")
#' 
#' # use info for query
#' dat <- getAdset(adset.id = sample(adsets$id, 1), date.preset = "lifetime",
#'  token = "XXXXX")
#' 
#' }
#'
#' @seealso \code{\link{getAdset}}
#' 
#' @author John Coene <john.coene@@cmcm.com>
#' 
#' @export
findAdsets <- function (id, token, fields = "default", ..., n = 100,
                        verbose = FALSE) {
  
  fb_data <- findObjects(id = id, token = token, fields = fields, ..., 
                         n = n, verbose = verbose, object = "adsets")
  
  return(fb_data)
  
}