#' getCreative
#' 
#' @description Fetches URL(s) name(s) and more about creatives used in an ad.
#' 
#' @param id The id of the ad you want to retrieve (Required), see \href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.
#' @param token A valid token as returned by \code{\link{fbAuthenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param n Number of results to retrieve, defaults to \code{100}. When you make an API request, you will usually not receive all of the results of that request in a single response. This is because some responses could contain thousands of objects so most responses are paginated by default. \code{previous} fetches the previous page of response (after the initial query) similarly \code{next} fetches the next page and \code{NULL} does not paginate (only makes one query).
#' @param verbose Defaults to \code{FALSE} if \code{TRUE} will print information on the query in the console.
#' 
#' @details Call \href{https://developers.facebook.com/docs/marketing-api/reference/adgroup/adcreatives/}{adcreatives}.
#' 
#' @return Returns IDs and URLs of all images contained in object (single row if ad.id is passed as arguement)
#' 
#' @examples 
#' \dontrun{
#' # authenticate
#' fbOAuth <- fbAuthenticate(app.id = "1234567890123456", 
#'                           app.secret = "16xx79321xx0130x2x10a08x3e2x30xx", 
#'                           scope = "ads_management")
#' # get account ids
#' act <- findAccounts(id = "me", token = fbOAuth)
#' 
#' # get all creatives in ad account
#' img_acc <- getCreative(id = act[2,2], token = fbOAuth)
#' 
#' # get campaigns, adsets and ads IDs from account
#' obj <- findObjects(account.id = act[2,2], token = token = fbOAuth)
#' 
#' # get ad creatives
#' crea_ad <- getCreative(id = sample(obj$ads$id, 1), token = fbOAuth)
#' }
#' 
#' @author John Coene <john.coene@@cmcm.com>
#' 
#' @seealso \code{\link{fbAuthenticate}}, \code{\link{findAccounts}}, \code{\link{findObjects}}
#' 
#' @export
getCreative <- function(id, token, n = 100, verbose = FALSE){
  
  # check inputs
  if(missing(id)){
    stop("Missing id")
  } else if (missing(token)){
    stop("Missing token")
  } 

  # check token verison
  token <- checkToken(token)
  
  
  url <- paste0("https://graph.facebook.com/v2.5/",
                id, "/adcreatives?fields=",
                "id%2Cadlabels%2Cbody%2Ccall_to_action_type",
                "%2Cimage_hash%2Cimage_url",
                "%2Cinstagram_actor_id%2Cinstagram_permalink_url",
                "%2Cinstagram_story_id%2Clink_url%2Cname",
                "%2Cobject_id%2Cobject_url%2Cobject_story_id",
                "%2Cobject_type",
                "%2Cproduct_set_id%2Crun_status%2Ctemplate_url",
                "%2Cthumbnail_url%2Ctitle%2Curl_tags",
                "%2Capplink_treatment"
                , "&access_token=",
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
    warning(paste("No creative."))
    
    # make empt data.frame
    dat <- data.frame()
  } else {
    
    # parse
    dat <- toDF(response)
    
    #paginate
    dat <- paginate(data = dat, json = json, verbose = verbose, n = n)
    
    # verbose
    if (verbose == TRUE) {
      cat(paste(n, "results requested, API returned", nrow(dat), "rows", "\n"))
    } 
    
  }
  
  return(dat)
}

