#' List broad targeting categories
#' 
#' @description Fetches the list of broad targeting categories under 
#' a given account.
#' 
#' @inheritParams listCreatives
#' 
#' @examples 
#' \dontrun{
#' tgt <- listBroadTargeting(id = "act_123456789012345", token = "XXXXXX")
#' }
#' 
#' @author John Coene \email{jcoenep@@gmail.com}
#' 
#' @export
listBroadTargeting <- function (id, token, fields = "default", n = 100, 
                        verbose = FALSE, limit = 100) {
  
  fb_data <- findObjects(id = id, token = token, fields = fields, 
                         n = n, verbose = verbose, 
                         object = "broadtargetingcategories",
                         FUN = "listBroadTargeting", limit = limit)
  
  if (nrow(fb_data) == 0) warning(paste("No data."), call. = FALSE)
  
  return (fb_data)
  
}