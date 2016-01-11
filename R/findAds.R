#' findAds
#' 
#' @description Fetches the list of ads under a given account, campaign or adset.
#' 
#' @param id Your ad account id, starting by "act_" and followed by 15 digits (Required), see \href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.
#' @param token A valid token as returned by \code{\link{fbAuthenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param fields default (\code{"default"}) returns the most popular ones. Run \code{\link{findFields}} to see all valid fields.
#' @param n Number of results to retrieve, defaults to \code{100}. When you make an API request, you will usually not receive all of the results of that request in a single response. This is because some responses could contain thousands of objects so most responses are paginated by default. \code{previous} fetches the previous page of response (after the initial query) similarly \code{next} fetches the next page and \code{NULL} does not paginate (only makes one query).
#' @param verbose Defaults to \code{FALSE} if \code{TRUE} will print information on the query in the console.
#' @param ... additional parameters to pass to insights.
#' 
#' @examples 
#' \dontrun{
#' ads <- findAds(account.id = adsets$id[1], token = "XXXXXX")
#' 
#' # use info for query
#' dat <- getAd(ad.id = sample(ads$id, 1), token = "XXXXXX")
#' }
#'
#' @seealso \code{\link{getAd}}
#' 
#' @author John Coene <john.coene@@cmcm.com>
#' 
#' @export
findAds <- function (id, token, fields = "default", ..., n = 100,
                     verbose = FALSE) {
  
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
    fields <- c("creative", "name")
  } 
  
  if(class(fields) != "character") {
    stop("Fields must be a character vector")
  } else { 
    # test if fields correct
    testParam("fields", fields, "findAds")
    
    # createFields
    fields <- createFields(fields)
  }
  
  args <- unlist(list(...))
  # create fields
  if(length(args)) {
    # test if fields correct
    testParam("fields", args, "getAny")
    
    # createFields
    args <- createFields(args)
  } else {
    args <- NULL
  }
  
  if (length(args)){
    # build url
    url <- paste0("https://graph.facebook.com/v2.5/",
                  id, "/ads?fields=",
                  fields,
                  "%2Cinsights{", args, "}",
                  "&access_token=",
                  token) 
  } else {
    # build url
    url <- paste0("https://graph.facebook.com/v2.5/",
                  id, "/ads?fields=",
                  fields,
                  "&access_token=",
                  token)
  }
  
  # call api
  response <- httr::GET(url)
  
  # call api
  response <- httr::GET(url)
  
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
    
  return (dat)
  
}