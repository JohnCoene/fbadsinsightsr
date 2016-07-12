#' Get ad estimated reach
#' 
#' @description Retrieves given ad expected reach
#' 
#' @param id The id of the object you want to retrieve (Required),
#'  see \href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.
#' @param token
#'  A valid token as returned by \code{\link{fbAuthenticate}} or a 
#'  short-term token from 
#'  \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param optimize_for What you are optimizing for. This API generates min, 
#' median, and max bid estimates related to the optimization goal, defaults to 
#' \code{APP_INSTALLS}, see official documentation or \code{\link{findParams}} 
#' for valid values
#' 
#' @examples 
#' \dontrun{
#' # run authentication with your app details
#' TK <- fbAuthenticate(app.id = "1234567890123456", 
#'                      app.secret = "16xx79321xx0130x2x10a08x3e2x80xx")
#' 
#' # get all accounts users (me) has access to
#' accounts <- listAccounts("me", TK, n = 999999)
#' ads <- listAds(sample(accounts$id, 1), TK)
#' 
#' reach <- getEstReach(samples(ads$id, 1), TK)
#' }
#' 
#' @seealso \code{\link{listLabels}}
#' 
#' @author John Coene \email{john.coene@@cmcm.com}
#' 
#' @export
getEstReach <- function(id, token, optimize_for = "APP_INSTALLS"){
  
  # check inputs
  if(missing(id)){
    stop("Missing id")
  } else if (missing(token)){
    stop("Missing token")
  } 
  
  testParam("optimize_for", optimize_for)
  
  # build url
  uri <- paste0("https://graph.facebook.com/v2.6/",
                id, "/reachestimate?optimize_for=", optimize_for, 
                "&access_token=", token)
  
  # call api
  response <- httr::GET(uri)
  
  json <- rjson::fromJSON(rawToChar(response$content))
  
  # check if query successful 
  if(length(json$error$message)){
    stop(paste("this is likely due to account.id or token.",
               "Error Message returned: ",
               json$error$message))
  }
  
  # check if data returned
  if (length(json$data)) {
    
    # parse
    dat <- do.call(plyr::"rbind.fill", 
                   lapply(json$data$bid_estimations, as.data.frame))
    
  } else if (!length(json$data)) {
    
    # create empty data.frame to return
    dat <- data.frame()
    
  }
  
  if (nrow(dat) == 0) warning(paste("No data."), call. = FALSE)
  
  return(dat)
  
}