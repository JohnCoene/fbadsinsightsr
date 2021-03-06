#' Grab list of accounts
#' 
#' @description Fetches all accounts under one business ID. 
#' All accounts to which the user has access to that is.
#' 
#' @param id Your business.facebook.com ID or your user id.
#' @param token A valid token as returned by \code{\link{fbAuthenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param n Number of results to retrieve, defaults to \code{100}. When you make an API request, you will usually not receive all of the results of that request in a single response. This is because some responses could contain thousands of objects so most responses are paginated by default. \code{previous} fetches the previous page of response (after the initial query) similarly \code{next} fetches the next page and \code{NULL} does not paginate (only makes one query).
#' @param fields Defaults to \code{default} which returns name and id of accounts. See \code{\link{findFields}} to find all available fields.
#' @param verbose Defaults to \code{FALSE} if \code{TRUE} will print information on the query in the console.
#' @param limit Number of results requested at each API call, defaults to 100.
#'
#' @details Returns data.frame of account IDs and status. requires \code{ads_management} permission!
#' 
#' @examples 
#' \dontrun{
#' # get account ids
#' accs <- grabAccounts(id = "act_123456789012345", token = "XXXXXX")
#' }
#' 
#' @seealso \code{\link{getAccount}}
#' 
#' @author John Coene \email{jcoenep@@gmail.com}
#' 
#' @export
grabAccounts <- function(id, token, n = 100, fields = "default",
                         verbose = FALSE, limit = 100) {
  
  .Deprecated("listAccounts")
  
  # check inputs
  if(missing(id)){
    stop("Missing account.id")
  } else if (missing(token)){
    stop("Missing token")
  }
  
  fb_data <- findObjects(id = id, token = token, fields = fields, 
                         n = n, verbose = verbose, object = "adaccounts",
                         FUN = "listAccounts", limit = limit)
  
  if (nrow(fb_data) == 0) warning(paste("No data."), call. = FALSE)
  
  return (fb_data)
}