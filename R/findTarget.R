#' findTarget
#' 
#' @description Retrieves the targeting description of a specific ad or adset.
#' 
#' @param id Either a adset ID or an ad ID.
#' @param n Number of results to retrieve, defaults to \code{100}. When you make an API request, you will usually not receive all of the results of that request in a single response. This is because some responses could contain thousands of objects so most responses are paginated by default. \code{previous} fetches the previous page of response (after the initial query) similarly \code{next} fetches the next page and \code{NULL} does not paginate (only makes one query).
#' @param token A valid token as returned by \code{\link{fbAuthenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param verbose Defaults to \code{FALSE} if \code{TRUE} will print information on the query in the console.
#' 
#' @return Returns a \code{data.frame} containing targeting sentences as columns.
#' 
#' @export
findTarget <- function(id, n = 100, token){
  
  # check inputs
  if(missing(id)){
    stop("Missing account.id")
  } else if (missing(token)){
    stop("Missing token")
  }
  
  # check token
  token <- checkToken(token = token)
  
  # build url
  url <- paste0("https://graph.facebook.com/v2.5/",
                id, "/targetingsentencelines?access_token=", token)
  
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
  
  i <- 1
  
  # Paginate
  while (nrow(data) < n & 
         !is.null(json$paging$`next`)) {
    # GET
    response <- httr::GET(json$paging$`next`)
    
    # parse
    json <- rjson::fromJSON(rawToChar(response$content))
    
    # bind
    data <- plyr::rbind.fill(data, parseJSON(json))
    
    # pause between queries if more than 2 to avoid lengthy calls
    i <- i + 1
    
    if(i >= 3) {
      Sys.sleep(2)
    }
  }
  
  return(data)
}