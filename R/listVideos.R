#' List videos
#' 
#' @description Fetches the list of videos under a given account.
#' 
#' @inheritParams listCreatives
#' 
#' @examples 
#' \dontrun{
#' vids <- listVideos(id = "act_123456789012345", token = "XXXXXX")
#' }
#' 
#' @author John Coene \email{john.coene@@cmcm.com}
#' 
#' @export
listVideos <- function (id, token, fields = "default", n = 100, 
                        verbose = FALSE, limit = 100) {
  
  fb_data <- findObjects(id = id, token = token, fields = fields, 
                         n = n, verbose = verbose, object = "advideos",
                         FUN = "listVideos", limit = limit)
  
  if (nrow(fb_data) == 0) warning(paste("No data."), call. = FALSE)
  
  return (fb_data)
  
}