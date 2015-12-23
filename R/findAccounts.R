#' findAccounts
#' 
#' @description Fetches all accounts under one business ID. All accounts to which the user has access to that is.
#' 
#' @param id Your business.facebook.com ID or your user id.
#' @param token A valid token as returned by \code{\link{fbAuthenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param n Number of results to retrieve, defaults to \code{100}. When you make an API request, you will usually not receive all of the results of that request in a single response. This is because some responses could contain thousands of objects so most responses are paginated by default. \code{previous} fetches the previous page of response (after the initial query) similarly \code{next} fetches the next page and \code{NULL} does not paginate (only makes one query).
#' 
#' @details Returns data.frame of account IDs.
#' 
#' @examples 
#' \dontrun{
#' # get account ids
#' act <- findAccounts(id = "me", token = "XXXXXXXXXXX")
#' 
#' # get information on account
#' info <- findInfo(account.id = act[2,2], token = "XXXXXXXXXXX")
#' }
#' 
#' @seealso \code{\link{findInfo}}
#' 
#' @author John Coene <john.coene@@cmcm.com>
#' 
#' @export
findAccounts <- function(id, token, n = 100) {
  
  # check inputs
  if(missing(id)){
    stop("Missing account.id")
  } else if (missing(token)){
    stop("Missing token")
  }
  
  # check token
  token <- checkToken(token)
  
  url <- paste0("https://graph.facebook.com/v2.5/", id, "/adaccounts?fields=",
                "&access_token=", token)
  
  # call api
  response <- httr::GET(url)
  
  # parse to list
  json <- rjson::fromJSON(rawToChar(response$content))
  
  # check if query successful 
  if(length(json$error$message)){
    stop(paste("this is likely due to account.id or token. Error Message returned: ",
               json$error$message))
  }
  
  # parse
  data <- parseJSON(json)
  
  data <- paginate(data = data, json = json, verbose = verbose, n = n)
  
  # verbose
  if (verbose == TRUE) {
    cat(paste(n, "results requested, API returned", nrow(data)))
  } 
  
  return(data)
}