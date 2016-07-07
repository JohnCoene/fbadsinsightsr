#' Check object target tree
#' 
#' @description Retrieves the targeting description of a specific object.
#' 
#' @param id ID of object to retrieve.
#' @param token A valid token as returned by \code{\link{fbAuthenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param limit.type Limit the type of audience to retrieve, defaults to \code{NULL}. See \code{\link{findParams}}
#' @param n Number of results to retrieve, defaults to \code{100}. When you make an API request, you will usually not receive all of the results of that request in a single response. This is because some responses could contain thousands of objects so most responses are paginated by default. \code{previous} fetches the previous page of response (after the initial query) similarly \code{next} fetches the next page and \code{NULL} does not paginate (only makes one query).
#' @param limit
#'  Number of results requested at each API call, defaults to 100.
#'  Sometimes useful to bring it down if many results (\code{n}) are required as the 
#'  API might otherwise return \code{error_code: 1} or in other words an
#'   "Unknown error".
#' 
#' @details Corresponds to this API call; \url{https://developers.facebook.com/docs/marketing-api/targeting-description/v2.6} 
#' 
#' @examples 
#' \dontrun{
#' # get information on account
#' ads <- grabAds(account.id = "act_123456789012345", token = "XXXXXXXXXXX")
#' 
#' # fetch targeting description of random ad
#' checkTargetTree(id = sample(ads$id, 1), token = "XXXXXXXXXXX")
#' }
#' 
#' @seealso \code{\link{checkTargetSentence}}
#' 
#' @author John Coene \email{john.coene@@cmcm.com}
#' 
#' @export
checkTargetTree <- function(id, token, limit.type = NULL, limit = 100, n = 100){
  
  # check inputs
  if(missing(id)){
    stop("Missing id")
  } else if (missing(token)){
    stop("Missing token")
  }
  
  # check token
  token <- checkToken(token = token)
  
  testParam("limit.type", limit.type)
  
  # build url
  url <- paste0("https://graph.facebook.com/v2.6/",
                id, "/targetingbrowse?limit=", limit, 
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
  
  # check if data returned
  if (length(json$data)) {
    
    # parse
    dat <- do.call(plyr::"rbind.fill", lapply(json$data, as.data.frame))
    
  } else if (!length(json$data)) {
    
    # create empty data.frame to return
    dat <- data.frame()
    
  }
  
  if (nrow(dat) == 0) warning(paste("No data."), call. = FALSE)
  
  return(dat)
  
}