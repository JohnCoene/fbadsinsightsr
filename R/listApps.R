#' List Applications
#' 
#' @description Fetches the list of applications under a given account.
#' 
#' @param id
#'  The id of the object you want to grab from (Required), see 
#' \href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.
#' @param token
#'  A valid token as returned by \code{\link{fbAuthenticate}} 
#' or a short-term token from 
#' \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param n
#'  Number of results to retrieve, defaults to \code{100}. 
#' When you make an API request, you will usually not receive all of the 
#' results of that request in a single response. This is because some 
#' responses could contain thousands of objects so most responses are 
#' paginated by default.
#' @param verbose
#'  Defaults to \code{FALSE} if \code{TRUE} will print information on the 
#'  queries in the console.
#' @param limit Number of results requested at each API call, defaults to 
#' \code{100}.
#' 
#' @examples 
#' \dontrun{
#' (apps <- listApps(id = "act_123456789012345", token = "XXXXXX"))
#' }
#' 
#' @author John Coene \email{jcoenep@@gmail.com}
#' 
#' @export
listApps <- function (id, token, n = 100, verbose = FALSE, limit = 100) {
  
  fb_data <- findObjects(id = id, token = token, fields = NULL, 
                         n = n, verbose = verbose, object = "applications",
                         FUN = NULL, limit = limit)
  
  if (nrow(fb_data) == 0) warning(paste("No data."), call. = FALSE)
  
  return (fb_data)
  
}