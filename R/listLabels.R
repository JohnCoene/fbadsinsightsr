#' List labels
#' 
#' @description Retrieves ad labels under given account.
#' 
#' @param account.id ID of the account to retrieve labels from.
#' @param token A valid token as returned by \code{\link{fbAuthenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' 
#' @examples 
#' \dontrun{
#' # get all accounts
#' accounts <- listAccounts("me", token = "XXXXXXXXXXX")
#' 
#' # get labels of random account
#' labs <- listLabels(account.id = sample(accounts$id, 1), token = "XXXXXXXXXXX")
#' }
#' 
#' @seealso \code{\link{getLabCampaigns}}
#' 
#' @author John Coene \email{john.coene@@cmcm.com}
#' 
#' @export
listLabels <- function(account.id, token){
  
  # check inputs
  if(missing(account.id)){
    stop("Missing account.id")
  } else if (missing(token)){
    stop("Missing token")
  }
  
  # check token
  token <- checkToken(token = token)
  
  # build url
  url <- paste0("https://graph.facebook.com/v2.7/",
                account.id, "/adlabels", 
                "?access_token=", token)
  
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
    
  } else if (!length(json$targetingsentencelines)) {
    
    # create empty data.frame to return
    dat <- data.frame()
    
  }
  
  if (nrow(dat) == 0) warning(paste("No labels."), call. = FALSE)
  
  return(dat)
  
}