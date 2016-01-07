#' getImage
#' 
#' @description Fetches URL(s) name(s) and ID(s) of images used for an ad, an adset, a campaign or all the images in an account.
#' 
#' @param id The id of the object you want to retrieve (Required), see \href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}. Can be account id, campaign id, adset id or an ad id.
#' @param token A valid token as returned by \code{\link{fbAuthenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param time.range time range must be \code{c(since = 'YYYY-MM-DD', until='YYYY-MM-DD')}
#' @param date.preset Represents a relative time range (Optional). This field is ignored if \code{time.range} is specified. Run \code{\link{findDatePreset}} to see all valid presets. @param time.range time range must be \code{c(since = 'YYYY-MM-DD', until='YYYY-MM-DD')}
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
#'                           app.secret = "16xx79321xx0130x2x10a08x3e2x80xx", 
#'                           scope = "ads_management")
#' # get account ids
#' act <- findAccounts(id = "me", token = fbOAuth)
#' 
#' # get all images in ad account
#' img_acc <- getImage(id = act[2,2], token = fbOAuth)
#' 
#' # get campaigns, adsets and ads IDs from account
#' info <- findObjects(account.id = act[2,2], token = token = fbOAuth)
#' 
#' # get all images in random campaign
#' img_camp <- getImage(id = sample(info$summary$campaign_id, 1), token = fbOAuth)
#' 
#' # get images used in an adset
#' img_adset <- getImage(sid = sample(info$adsset$id, 1), token = fbOAuth)
#' 
#' # get ad image
#' img_ad <- getImage(id = sample(info$ads$id, 1), token = fbOAuth)
#' }
#' 
#' @author John Coene <john.coene@@cmcm.com>
#' 
#' @seealso \code{\link{fbAuthenticate}}, \code{\link{findAccounts}}, \code{\link{findObjects}}
#'
#' @export
getImage <- function(id, token, time.range = NULL,
                     date.preset = NULL, n = 100,
                     verbose = FALSE){
  
  # check inputs
  if(missing(id)){
    stop("Missing id")
  } else if (missing(token)){
    stop("Missing token")
  } 
  
  # make date.preset NULL if time.range specified
  if (length(date.preset) == 1 && length(time.range) >= 1) {
    date.preset <- NULL
    warning("date.preset is ignored as time.range is specified")
  }
  
  if (length(date.preset) == 1) {
    
    # test
    testParam("date_preset", date.preset)
    
    date.preset <- paste0("&date_preset=", date.preset)
  } else if (length(date.preset) > 1) {
    stop("date.preset can only hold one of the following values: today, yesterday, last_3_days, this_week, last_week, last_7_days, last_14_days, last_28_days, last_30_days, last_90_days, this_month, last_month, this_quarter, last_3_months, lifetime")
  }
  
  # time range
  if(length(time.range) == 2) {
    
    date_check <- as.Date(time.range[2], format= "%Y-%m-%d")
    
    # further checks
    if(names(time.range)[1] != "since"){
      stop("time.range must be - c(since = 'YYYY-MM-DD', until='YYYY-MM-DD')")
    } else if (class(date_check) == "try-error" || is.na(date_check)){
      stop("Wrong date format. Must be YYYY-MM-DD")
    }
    
    time.range <- paste0("&since=", time.range[1], "&", "until=", time.range[2])
    
  } else if (length(time.range) > 2) {
    stop("time.range must be - c(since = 'YYYY-MM-DD', until='YYYY-MM-DD')")
  }
  
  # check token verison
  token <- checkToken(token)
  
  url <- paste0("https://graph.facebook.com/v2.5/",
                id, "/adcreatives?fields=",
                "image_url%2Cname",
                time.range, date.preset,
                "&access_token=",
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
    warning(paste("No image"))
  }
  
  # parse
  data <- toDF(response)
  
  # paginate
  data <- paginate(data = data, json = json, verbose = verbose, n = n)
  
  # verbose
  if (verbose == TRUE) {
    cat(paste(n, "results requested, API returned", nrow(data)))
  } 
  
  return(data)
}