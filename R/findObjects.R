#' findObjects
#' 
#' @description Fetches the list of campaigns, adsets and ads under a given account.
#' 
#' @param account.id Your ad account id, starting by "act_" and followed by 15 digits (Required), see \href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.
#' @param n Number of results (ads, adset and campaigns respectively) to retrieve, defaults to \code{100}. When you make an API request, you will usually not receive all of the results of that request in a single response. This is because some responses could contain thousands of objects so most responses are paginated by default. \code{previous} fetches the previous page of response (after the initial query) similarly \code{next} fetches the next page and \code{NULL} does not paginate (only makes one query).
#' @param token A valid token as returned by \code{\link{fbAuthenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' 
#' @details Essentially consists of three separate API calls thus may take a few seconds; \code{Sys.sleep} of 0.5 seconds between each query.
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
#' lst <- findObjects(account.id = "act_123456789123456", token = "XXXXXX")
#' 
#' # inspect structure of feedback
#' str(lst)
#' 
#' # show names of data.frames within list
#' names(lst)
#' 
#' # use info for query
#' data <- getAdset(adset.id = sample(lst$adsets$id, 1), date.preset = "last_week", token = "XXXXX")
#' 
#' }
#'
#' @seealso \code{\link{getAdset}}
#' 
#' @author John Coene <john.coene@@cmcm.com>
#' 
#' @export
findObjects <- function(account.id, token, n = 100) {
  
  # check inputs
  if(missing(account.id)){
    stop("Missing account.id")
  } else if (missing(token)){
    stop("Missing token")
  } else if (length(grep("act_", account.id)) == 0){
    stop("must be account.id starting with act_")
  }
  
  # check token
  token <- checkToken(token)
  
  # build urls
  ads.url <- paste0("https://graph.facebook.com/v2.5/",
                    account.id, "/ads?fields=campaign_id%2C", 
                    "adset_id%2Cid%2Cname%2Cbid_amount%2Cbid_type",
                    "&access_token=",
                    token)
  
  adset.url <- paste0("https://graph.facebook.com/v2.5/",
                      account.id, "/adsets?fields=id%2Cname%2C",
                      "campaign_id&access_token=",
                      token)
  
  camp.url <- paste0("https://graph.facebook.com/v2.5/",
                     account.id, "/campaigns?fields=id%2Cname",
                     "&access_token=",
                     token)
  
  # vector of urls to query
  urls <- c(ads.url, adset.url, camp.url)
  
  # initiate list
  lst <- list()
  
  # set verbose to FALSE
  verbose <- FALSE
  
  for(i in 1:length(urls)){
    # call api
    response <- httr::GET(urls[i])
    
    # parse to list
    json <- rjson::fromJSON(rawToChar(response$content))
    
    # check if query successful 
    if(length(json$error$message)){
      stop(paste("this is likely due to account.id or token.",
                 "Error Message returned: ",
                 json$error$message))
    } else {
      
      # parse
      lst[[i]] <- toDF(response)
      
      # paginate
      lst[[i]] <- paginate(json, data = lst[[i]], verbose = verbose, n = n)
      
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
  
  # verbose
  if (verbose == TRUE) {
    cat(paste(n, "results requested, API returned", nrow(data), "rows", "\n"))
  } 
  
  return (lst)
  
}