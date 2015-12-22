#' findInfo
#' 
#' @description Fetches the list of campaigns, adsets and ads under a given account.
#' 
#' @param account.id Your ad account id, starting by "act_" and followed by 15 digits (Required), see \href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.
#' @param token A valid token as returned by \code{\link{fbAuthenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' 
#' @details Essentially consists of three separate API calls thus may take a few seconds; \code{Sys.sleep} of 3 seconds between each query.
#' One call is made for each object, ads, adsets and campaigns respectively.
#' 
#' @return Returns a list of four data.frames.
#' \itemize{
#' \item ads - campaign id, adset id, id, name and bid amount of each ad (rows)
#' \item adsets - id, name and campaign_id of each adset (rows)
#' \item campaigns - id and name of all campaigns
#' \item summary - includes all of the above in one neat \code{data.frame} using ads as rows.
#' }
#' 
#' See list structure (\code{str}) or \code{names}
#' 
#' @examples 
#' \dontrun{
#' lst <- getInfo(account.id = "act_123456789123456", token = "XXXXXX")
#' }
#' 
#' @seealso \code{\link{getAny}}
#' 
#' @author John Coene <john.coene@@cmcmc.com>
#' 
#' @export
findInfo <- function(account.id, token) {
  
  # check inputs
  if(missing(account.id)){
    stop("Missing account.id")
  } else if (missing(token)){
    stop("Missing token")
  }
  
  # build urls
  ads.url <- paste0("https://graph.facebook.com/v2.5/",
                    account.id, "/ads?fields=campaign_id%2Cadset_id%2Cid%2Cname%2Cbid_amount%2Cbid_type&access_token=",
                    token)
  
  adset.url <- paste0("https://graph.facebook.com/v2.5/",
                      account.id, "/adsets?fields=id%2Cname%2Ccampaign_id&access_token=",
                      token)
  
  camp.url <- paste0("https://graph.facebook.com/v2.5/",
                     account.id, "/campaigns?fields=id%2Cname&access_token=",
                     token)
  
  # vector of urls to query
  urls <- c(ads.url, adset.url, camp.url)
  
  # initiate list
  lst <- list()
  
  for(i in 1:length(urls)){
    # call api
    response <- httr::GET(urls[i])
    
    # parse to list
    json <- rjson::fromJSON(rawToChar(response$content))
    
    # check if query successful 
    if(length(json$error$message)){
      stop(paste("this is likely due to account.id or token. Error Message returned: ",
                 json$error$message))
    } else {
      
      # parse
      lst[[i]] <- parseJSON(json = json)
      
    }
    
    # wait 3 second before next API call
    Sys.sleep(3)
    
  }
  
  # rename lst
  names(lst) <- c("ads", "adsets", "campaigns")
  
  # create summary table
  data <- merge(lst$ads, lst$adsets, by.x = "adset_id", by.y = "id")
  data <- merge(data, lst$campaigns, by.x = "campaign_id.x", by.y = "id")
  
  # delete last duplicate columns
  data[,8] <- NULL
  
  # rename
  names(data) <- c("campaign_id", "adset_id", "ad_id", "ad_name",
                   "bid_amount", "bid_type", "adset_name", "campaign_name")
  
  lst$summary <- data
  
  return (lst)
  
}