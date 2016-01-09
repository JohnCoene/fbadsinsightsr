#' findAdsets
#' 
#' @description Fetches the list of adsets under a given account or campaign.
#' 
#' @param id Your ad account id, starting by "act_" and followed by 15 digits (Required), see \href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.
#' @param token A valid token as returned by \code{\link{fbAuthenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param fields default (\code{"default"}) returns the most popular ones. Run \code{\link{findFields}} to see all valid fields.
#' @param n Number of results to retrieve, defaults to \code{100}. When you make an API request, you will usually not receive all of the results of that request in a single response. This is because some responses could contain thousands of objects so most responses are paginated by default. \code{previous} fetches the previous page of response (after the initial query) similarly \code{next} fetches the next page and \code{NULL} does not paginate (only makes one query).
#' @param verbose Defaults to \code{FALSE} if \code{TRUE} will print information on the query in the console.
#' 
#' @examples 
#' \dontrun{
#' adsets <- findAdsets(account.id = "act_123456789123456", token = "XXXXXX")
#' 
#' # use info for query
#' dat <- getAdset(adset.id = sample(adsets$id, 1), date.preset = "lifetime",
#'  token = "XXXXX")
#' 
#' }
#'
#' @seealso \code{\link{getAdset}}
#' 
#' @author John Coene <john.coene@@cmcm.com>
#' 
#' @export
findAdsets <- function (id, token, fields = "default", n = 100,
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
    fields <- findFields("findAds")
  } 
  
  if(class(fields) != "character") {
    stop("Fields must be a character vector")
  } else { 
    # test if fields correct
    testParam("fields", fields, "findAds")
    
    # createFields
    fields <- createFields(fields)
  }
  
  # build url
  url <- paste0("https://graph.facebook.com/v2.5/",
                id, "/adsets?fields=",
                fields,
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
    warning(paste("No data."))
    
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
    
  return (dat)
  
}