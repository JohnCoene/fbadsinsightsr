#' listBusinesses
#' 
#' @description Fetches all businesses under the authenticated user. 
#' Requires access to Business Managers' API.
#' 
#' @param token A valid token as returned by \code{\link{fbAuthenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param id user ID, defaults to \code{me}.
#' @param n Number of results to retrieve, defaults to \code{100}. When you make an API request, you will usually not receive all of the results of that request in a single response. This is because some responses could contain thousands of objects so most responses are paginated by default. \code{previous} fetches the previous page of response (after the initial query) similarly \code{next} fetches the next page and \code{NULL} does not paginate (only makes one query).
#' @param verbose Defaults to \code{FALSE} if \code{TRUE} will print information on the query in the console.
#' @param limit Number of results requested at each API call, defaults to 100.
#'
#' @details Returns data.frame of account IDs and status. requires \code{ads_management} permission!
#' 
#' @examples 
#' \dontrun{
#' # get account ids
#' accs <- listBusinesses(token = "XXXXXX")
#' }
#' 
#' @author John Coene \email{john.coene@@cmcm.com}
#' 
#' @export
listBusinesses <- function(token, id = "me", n = 100, verbose = FALSE, 
                           limit = 100) {
  
  # check inputs
  if (missing(token)){
    stop("Missing token")
  }
  
  fb_data <- findObjects(id = id, token = token, fields = "default", 
                         n = n, verbose = verbose, object = "businesses",
                         FUN = "listAccounts", limit = limit)
  
  if (nrow(fb_data) == 0) warning(paste("No data."), call. = FALSE)
  
  return (fb_data)
}