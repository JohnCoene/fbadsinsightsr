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
#' adsets <- findAdsets(account.id = "act_123456789123456", token = "XXXXXX")
#' 
#' ads <- findAds(account.id = adsets$id[1], token = "XXXXXX")
#' 
#' # use info for query
#' dat <- getAd(ad.id = sample(ads$id, 1), token = "XXXXXX")
#' }
#'
#' @seealso \code{\link{getAd}}, \code{\link{findAdsets}}
#' 
#' @author John Coene <john.coene@@cmcm.com>
#' 
#' @export
findAds <- function (id, token, fields = "default", n = 100,
                     verbose = FALSE, ...) {
  
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
  
  # build url
  url <- paste0("https://graph.facebook.com/v2.5/",
                id, "/ads?fields=",
                fields,
                "%2Cinsights{", args, "}",
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
    
    # make empty data.frame to return and avoid error
    dat <- data.frame()
  } else {
    
    # check if data present in JSON
    if(length(json$data)){
      
      # extract names
      # find which nested list has largest number of variables
      lg <- vector()
      
      # loop through lists
      for(i in 1:length(json$data)){
        
        # get length
        lg[i] <- length(json$data[[i]])
        
        # identify longest (that's what she said)
        index <- which.max(lg)
      }
      
      # use variable names of largest list
      names <- names(json$data[[index]])
    }
    
    if(length(names[which(names == "insights")])) {
      
      insights_lst <- list()
      
      for(i in 1:length(json$data)){
        
        # extract insights
        insights_lst[[i]] <- json$data[[i]]$insights$data[[1]]
        
        # remove from initial json
        json$data[[i]]$insights <- NULL
      }
      
      # name list data for toDF formula
      insights_json <- list(data = insights_lst)
      
      # remove for performances or set to NULL, less likely to cause errors
      insights_lst <- NULL
      
      # toDF json WITHOUT insights
      base_df <- toDF(json)
      
      # toDF INSIGHTS json
      ins_df <- toDF(insights_json)
      
      # rename to distinguish between variables
      ins_names <- names(ins_df)
      names(ins_df) <- paste0("insights_", ins_names)
      
      # bind
      dat <- cbind.data.frame(base_df, ins_df)
    } else {
      dat <- toDF(json)
    }
    
    # verbose
    if (verbose == TRUE) {
      cat(paste(n, "results requested, API returned", nrow(dat), "rows", "\n"))
    } 
    
  }
    
  return (dat)
  
}