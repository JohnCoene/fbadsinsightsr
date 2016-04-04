#' grabAdsets
#' 
#' @description Fetches the list of adsets under a given account or campaign.
#' 
#' @inheritParams grabAds
#' 
#' @examples 
#' \dontrun{
#' adsets <- grabAdsets(id = "act_123456789012345", token = "XXXXXX")
#' 
#' }
#'
#' @seealso \code{\link{getAdset}}
#' 
#' @author John Coene \email{john.coene@@cmcm.com}
#' 
#' @export
grabAdsets <- function (id, token, fields = "default", ..., n = 100,
                        verbose = FALSE, limit = 100) {
  
  fb_data <- findObjects(id = id, token = token, fields = fields, ..., 
                         n = n, verbose = verbose, object = "adsets",
                         FUN = "grabAdsets", limit = limit)
  
  if (nrow(fb_data) == 0) warning(paste("No data."), call. = FALSE)
  
  return(fb_data)
  
}