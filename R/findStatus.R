#' findStatus
#' 
#' @description Returns effective as well as configured status and created time, name, account id of object requested (campaign, adset or ad).
#' 
#' @param id The id of the campaign, adset or ad you want to retrieve
#'  (Required), see 
#'  \href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.
#' @param token A valid token as returned by \code{\link{fbAuthenticate}}
#'  or a short-term token from 
#'  \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param fields default (\code{"effective_status"}) 
#' returns the most popular ones. Run \code{\link{findFields}} 
#' to see all valid fields.
#'
#' @details 
#' \itemize{
#' \item \code{configured_status}: The configured status of the object (campaign, adset or ad). If object is campaign and this status is \code{PAUSED}, all its active ad sets and ads will be paused and have an effective status \code{CAMPAIGN_PAUSED}.
#' \item \code{effective_status} The effective status of this campaign/adset/ad; \code{enum {ACTIVE, PAUSED, DELETED, PENDING_REVIEW, DISAPPROVED, PREAPPROVED, PENDING_BILLING_INFO, CAMPAIGN_PAUSED, ARCHIVED, ADSET_PAUSED}}
#' }
#' 
#' @examples 
#' \dontrun{
#' get information on account
#' ads <- findAds(id = "act_123456789012345", token = "XXXXXXXXXXX")
#' 
#' # take random ad.id
#' rand_id <- sample(ads$id, 1)
#' 
#' # getStatus
#' (status <- findStatus(id = rand_id, token = "XXXXXXXXXX"))
#' }
#' 
#' @seealso \code{\link{findAds}}
#' 
#' @author John Coene <john.coene@@cmcm.com>
#' 
#' @export
findStatus <- function(id, token, fields = "effective_status") {
  
  # check inputs
  if(missing(id)){
    stop("Missing id")
  } else if (missing(token)){
    stop("Missing token")
  } else if (length(id[grep("act", id)])) {
    stop("id must be that of a campaign, adset or ad")
  }
  
  # check token verison
  token <- checkToken(token)
  
  # create fields
  if(fields[1] == "default") {
    fields <- findFields("findStatus")
  } 
  
  if(class(fields) != "character") {
    stop("Fields must be a character vector")
  } else { 
    # test if fields correct
    testParam("fields", fields, "findStatus")
    
    # createFields
    fields <- createFields(fields)
  }
  
  # build url
  url <- paste0("https://graph.facebook.com/v2.5/",
                id, "?fields=",
                fields,
                "&access_token=", token)
  
  # call api
  response <- httr::GET(url)
  
  # parse json to list
  json <- rjson::fromJSON(rawToChar(response$content))
  
  # check if query successful 
  if(length(json$error$message)){
    stop(paste("this is likely due to id or token. Error Message returned: ",
               json$error$message))
  } else if (length(json$data) == 0) {
    
    # to df
    dat <- t(as.data.frame(unlist(json)))
    
    # rename
    row.names(dat) <- c(1:nrow(dat))
  }
  
  return(dat)
  
}