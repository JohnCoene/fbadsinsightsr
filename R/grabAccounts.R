#' grabAccounts
#' 
#' @description Fetches all accounts under one business ID. 
#' All accounts to which the user has access to that is.
#' 
#' @param id Your business.facebook.com ID or your user id.
#' @param token A valid token as returned by \code{\link{fbAuthenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param n Number of results to retrieve, defaults to \code{100}. When you make an API request, you will usually not receive all of the results of that request in a single response. This is because some responses could contain thousands of objects so most responses are paginated by default. \code{previous} fetches the previous page of response (after the initial query) similarly \code{next} fetches the next page and \code{NULL} does not paginate (only makes one query).
#' @param verbose Defaults to \code{FALSE} if \code{TRUE} will print information on the query in the console.
#'
#' @details Returns data.frame of account IDs and status. requires \code{ads_management} permission!
#' 
#' @examples 
#' \dontrun{
#' # get account ids
#' act <- findAccounts(id = "me", token = "XXXXXXXXXXX")
#' 
#' # get information on account
#' data <- getAccount(account.id = act[2,2], token = "XXXXXXXXXXX")
#' }
#' 
#' @seealso \code{\link{getAccount}}
#' 
#' @author John Coene <john.coene@@cmcm.com>
#' 
#' @export
grabAccounts <- function(id, token, n = 100, verbose = FALSE,
                         fields = "default") {
  
  # check inputs
  if(missing(id)){
    stop("Missing account.id")
  } else if (missing(token)){
    stop("Missing token")
  }
  
  
  fb_data <- findObjects(id = id, token = token, fields = fields, 
                         n = n, verbose = verbose, object = "adaccounts",
                         FUN = "grabAccounts")
  
  return (fb_data)
}