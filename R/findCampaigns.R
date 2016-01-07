#' findCampaigns
#' 
#' @description Fetches the list of campaigns under a given account.
#' 
#' @param account.id Your ad account id, starting by "act_" and followed by 15 digits (Required), see \href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.
#' @param token A valid token as returned by \code{\link{fbAuthenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param n Number of results to retrieve, defaults to \code{100}. When you make an API request, you will usually not receive all of the results of that request in a single response. This is because some responses could contain thousands of objects so most responses are paginated by default. \code{previous} fetches the previous page of response (after the initial query) similarly \code{next} fetches the next page and \code{NULL} does not paginate (only makes one query).
#' @param verbose Defaults to \code{FALSE} if \code{TRUE} will print information on the query in the console.
#' 
#' @examples 
#' \dontrun{
#' camp <- findCampaigns(account.id = "123456789", token = "XXXXXX")
#' 
#' # use info for query
#' dat <- getCampaign(campaign.id = sample(camp$id, 1), date.preset = "last_week",
#'  token = "XXXXX")
#' }
#'
#' @seealso \code{\link{getCampaign}}
#' 
#' @author John Coene <john.coene@@cmcm.com>
#' 
#' @export
findCampaigns <- function(account.id, token, n = 100, verbose = FALSE){
  # check inputs
  if(missing(account.id)){
    stop("Missing account.id")
  } else if (missing(token)){
    stop("Missing token")
  }
  
  # check token
  token <- checkToken(token)
  
  # build url
  url <- paste0("https://graph.facebook.com/v2.5/",
                id, "/adcreatives?fields=image_url%2Cname&access_token=",
                token)
  
  # call api
  response <- httr::GET(url)
  
  # parse to list
  json <- rjson::fromJSON(rawToChar(response$content))
  
  # check if query successful 
  if(length(json$error$message)){
    stop(paste("this is likely due to id or token. Error Message returned: ",
               json$error$message))
  } else if (length(json$data) == 0) {
    warning(paste("No data."))
    
    # make empt data.frame
    data <- data.frame()
  } else {
    
    # parse
    data <- toDF(response)
    
    #paginate
    data <- paginate(data = data, json = json, verbose = verbose, n = n)
    
    # verbose
    if (verbose == TRUE) {
      cat(paste(n, "results requested, API returned", nrow(data)))
    } 
    
    return (data)
  }
}