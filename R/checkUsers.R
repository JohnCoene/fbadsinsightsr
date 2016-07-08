#' Check users
#' 
#' @description Retrieves the users having access to the account
#' 
#' @param id ID of object to retrieve.
#' @param token A valid token as returned by \code{\link{fbAuthenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#'
#' @examples 
#' \dontrun{
#' # get information on account
#' ads <- grabAds(account.id = "act_123456789012345", token = "XXXXXXXXXXX")
#' }
#' 
#' @seealso \code{\link{checkTargetTree}}
#' 
#' @author John Coene \email{john.coene@@cmcm.com}
#' 
#' @export
checkUsers <- function(id, token){
  
  # check inputs
  if(missing(id)){
    stop("Missing id")
  } else if (missing(token)){
    stop("Missing token")
  }
  
  # check token
  token <- checkToken(token = token)
  
  # build url
  url <- paste0("https://graph.facebook.com/v2.6/",
                id, "/users?", 
                "access_token=", token)
  
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
    
    perm <- data.frame(permissions = c(1, 2, 3, 4, 5, 7, 9, 10),
                       permission = c("ACCOUNT_ADMIN", "ADMANAGER_READ", 
                                      "ADMANAGER_WRITE", "BILLING_READ",
                                      "BILLING_WRITE", "REPORTS", 
                                      "DIRECT_MANAGED", "DIRECT_MANAGED"),
                       stringsAsFactors = FALSE)
    
    dat <- plyr::join(dat, perm, by = c("permissions"), type = "left")
    dat$permissions <- NULL
    
    roles <- data.frame(role = c(1001, 1002, 1003, 1004),
                        roles = c("Administrator", "Advertiser", 
                                  "Analyst", "Sales"))
    
    dat <- plyr::join(dat, roles, by = c("role"), type = "left")
    dat$role <- NULL
    
  } else if (!length(json$data)) {
    
    # create empty data.frame to return
    dat <- data.frame()
    
  }
  
  if (nrow(dat) == 0) warning(paste("No data."), call. = FALSE)
  
  return(dat)
  
}