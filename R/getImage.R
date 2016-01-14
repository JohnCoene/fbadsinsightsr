#' getImage
#' 
#' @description Fetches URL(s) name(s) and more about images used in an account.
#' 
#' @param account.id Your ad account id, starting by "act_" and followed by 15 digits (Required), see \href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.
#' @param fields 
#' Defaults to \code{default} which returns the most popular ones. 
#' Run \code{\link{findFields}} to see al valid fields.
#'  to see all valid fields.
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
#'                           app.secret = "16xx79321xx0130x2x10a08x3e2x80xx", 
#'                           scope = "ads_management")
#' # get account ids
#' act <- grabAccounts(id = "me", token = fbOAuth)
#' 
#' # get all images in ad account
#' img_acc <- getImage(id = act[2,2], token = fbOAuth)
#' }
#' 
#' @author John Coene <john.coene@@cmcm.com>
#' 
#' @seealso \code{\link{fbAuthenticate}}, \code{\link{grabAccounts}}
#'
#' @export
getImage <- function(account.id, token, fields = "default", n = 100, 
                     verbose = FALSE){
  
  # check inputs
  if(missing(account.id)){
    stop("Missing id")
  } else if (missing(token)){
    stop("Missing token")
  } else if (length(account.id[grep("act_", account.id)]) == 0){
    stop("Must be account id. starting with act_")
  }
  
  # create field
  if(class(fields) != "character") {
    stop("Fields must be a character vector", 
         call. = TRUE)
  } else { 
    # make default
    if(fields[1] == "default") fields <- c("id", "name", "url")
    
    # test if fields correct
    testParam("fields", fields, "getImage")
    
    # createFields
    fields <- createFields(fields)
  }
  
  # check token verison
  token <- checkToken(token)
  
  uri <- paste0("https://graph.facebook.com/v2.5/",
                account.id, "/adimages?fields=",
                fields,
                "&access_token=",
                token)
  
  # call api
  response <- httr::GET(uri)
  
  # construct data
  fb_data <- constructFbAdsData(response)
  
  # parse data
  fb_data <- digest(fb_data)
  
  # paginate
  fb_data <- paginate(fb_data, n = n, verbose = verbose)
  
  # verbose
  if (verbose == TRUE) {
    cat(paste(n, "results requested, API returned", nrow(fb_data$data),
              "rows", "\n"))
  }
  
  # converge
  fb_data <- converge(fb_data)
  
  return(fb_data)
}