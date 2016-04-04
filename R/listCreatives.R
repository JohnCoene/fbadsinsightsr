#' listCreatives
#' 
#' @description Fetches URL(s) name(s) and more about creatives used in an ad.
#' 
#' @param id
#'  The id of the object you want to grab from (Required), see 
#' \href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.
#' @param token
#'  A valid token as returned by \code{\link{fbAuthenticate}} 
#' or a short-term token from 
#' \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param n
#'  Number of results to retrieve, defaults to \code{100}. 
#' When you make an API request, you will usually not receive all of the 
#' results of that request in a single response. This is because some 
#' responses could contain thousands of objects so most responses are 
#' paginated by default.
#' @param fields Defaults to \code{default} (\code{id} and \code{name}). 
#' See \code{findFields}
#' @param verbose
#'  Defaults to \code{FALSE} if \code{TRUE} will print information on the 
#'  queries in the console.
#' @param limit Number of results requested at each API call, defaults to 
#' \code{100}.
#' 
#' @details Calls 
#' \href{https://developers.facebook.com/docs/marketing-api/reference/adgroup/adcreatives/}{adcreatives}.
#' 
#' @return Returns IDs and URLs of all images contained in object 
#' (single row if ad.id is passed as arguement)
#' 
#' @examples 
#' \dontrun{
#' # authenticate
#' fbOAuth <- fbAuthenticate(app.id = "1234567890123456", 
#'                           app.secret = "16xx79321xx0130x2x10a08x3e2x30xx", 
#'                           scope = "ads_management")
#' # get account ids
#' act <- grabAccounts(id = "me", token = fbOAuth)
#' 
#' # get all creatives in ad account
#' img_acc <- listCreatives(id = act[2,2], token = fbOAuth)
#' 
#' # get campaigns, adsets and ads IDs from account
#' ads <- grabAds(account.id = act[2,2], token = token = fbOAuth)
#' 
#' # get ad creatives
#' crea_ad <- listCreatives(id = sample(ads$id, 1), token = fbOAuth)
#' }
#' 
#' @author John Coene \email{john.coene@@cmcm.com}
#' 
#' @seealso \code{\link{fbAuthenticate}}, \code{\link{grabAccounts}},
#'  \code{\link{grabAds}}
#' 
#' @export
listCreatives <- function(id, token, n = 100, fields = "default", 
                          verbose = FALSE, limit = 100){
  
  # check inputs
  if(missing(id)){
    stop("Missing id")
  } else if (missing(token)){
    stop("Missing token")
  } 
  
  # check token verison
  token <- checkToken(token)
  
  # create fields
  if(fields[1] == "default") {
    fields <- c("id%2Cname")
  } else {
    if(class(fields) != "character") {
      stop("Fields must be a character vector", 
           call. = FALSE)
    } else { 
      # test if fields correct
      testParam("fields", fields, "listCreatives")
      
      # createFields
      fields <- createFields(fields)
    }
  }
  
  uri <- paste0("https://graph.facebook.com/v2.5/",
                id, "/adcreatives?fields=",
                fields,
                "&limit=", limit, "&access_token=",
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
  
  if (nrow(fb_data) == 0) warning(paste("No data."), call. = FALSE)
  
  return(fb_data)
}

