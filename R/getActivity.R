#' getActivity
#' 
#' @description Get activities on an ad account.
#' 
#' @param token
#' A valid token as returned by \code{\link{fbAuthenticate}} or a 
#' short-term token from 
#' \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param account.id The id of the account you want to retrieve (Required),
#'  see \href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.
#' @param since date \code{YYYY-MM-DD}
#' @param until date \code{YYYY-MM-DD}, defaults to now
#' @param n
#'  Number of results to retrieve, defaults to \code{100}. 
#'  When you make an API request, you will usually not receive all of the 
#'  results of that request in a single response. 
#'  This is because some responses could contain thousands of objects so 
#'  most responses are paginated by default.
#' @param limit
#'  Number of results requested at each API call, defaults to \code{100}.
#'  Sometimes useful to bring it down if many results (\code{n}) are required as the 
#'  API might otherwise return \code{error_code: 1} or in other words an
#'   "Unknown error".
#' @param fields 
#' There are numerous valid fields defaults to 
#' (\code{default}) which returns the most popular ones. See 
#' \code{\link{findFields}} 
#'   
#' @examples 
#' \dontrun{
#' # run authentication with your app details
#' fbOAuth <- fbAuthenticate(app.id = "1234567890123456", 
#'                           app.secret = "76xx79121xx0130x2x10a08x3e2x80xx", 
#'                           scope = "ads_management")
#' 
#' # get account ids
#' acc <- grabAccounts(id = "me", token = "XXXXXXXXXXX")
#' 
#' # get activity
#' act <- getActivity(token = fbOAuth, account.id = sample(act, 1), n = 200)
#' }
#' 
#' @author John Coene \email{john.coene@@cmcm.com}
#' 
#' @seealso \code{\link{fbAuthenticate}}, \code{\link{grabAccounts}}
#' 
#' @export
getActivity <- function(token, account.id, fields = "default", since = NULL, 
                          until = NULL, n = 100, limit = 100){
  
  # check token
  token <- checkToken(token)
  
  # check time
  if(length(since)) {
    since_check <- as.Date(since, format= "%Y-%m-%d")
    
    if(class(since_check) == "try-error" || is.na(since_check)) {
      stop("since must be YYYY-MM-DD")
    }
    
    since <- paste0("&since=", since)
  }
  
  if(length(until)) {
    until_check <- as.Date(until, format= "%Y-%m-%d")
    
    if (class(until_check) == "try-error" || is.na(until_check)) {
      stop("until must be YYYY-MM-DD")
    }
    
    until = paste0("&until=", until)
  }
  
  # create fields
  if(fields[1] == "default") {
    fields <- findFields("getActivity")
  } 
  
  if(class(fields) != "character") {
    stop("Fields must be a character vector")
  } else { 
    # test if fields correct
    testParam("fields", fields, "getActivity")
    
    # createFields
    fields <- createFields(fields)
  }
  
  # build uri
  uri <- paste0("https://graph.facebook.com/v2.5/", account.id,
                "/activities?fields=", fields,"&limit=", limit, 
                since, until, "&access_token=", token)
  
  # fetch init
  response <- httr::GET(uri)
  
  # parse to list
  json <- rjson::fromJSON(rawToChar(response$content, multiple = FALSE))
  
  if(length(json$data) >= 1) {
    
    bind <- parseLog(json, account.id)
    
    # paginate
    # loop if url is present and
    while(nrow(bind) < n && length(json$paging$`next`)){
      
      response <- httr::GET(json$paging$`next`)
      
      json <- rjson::fromJSON(rawToChar(response$content))
      
      if(length(json$data) >= 1) {
        
        bind2 <- parseLog(json, account.id)
        
        bind <- plyr::rbind.fill(bind, bind2)
        
        # sleep 0.5 second between queries
        Sys.sleep(0.5)
      }
      
    }
    
  } else {
    
    warning("No data")
    bind <- data.frame()
    
  }
  
  
  return(bind)
}