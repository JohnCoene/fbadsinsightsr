#' Get campaigns by labels
#' 
#' @description Retrieves campaigns by labels under given account.
#' 
#' @param account.id ID of the account to retrieve labels from.
#' @param labels IDs of labels to retrieve.
#' @param fields Fields (variables) to retrieve, see \code{\link{findFields}} 
#' for valid values, defaults to \code{id} and \code{name}.
#' @param token A valid token as returned by \code{\link{fbAuthenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param n Number of results to return
#' @param verbose
#'  Defaults to \code{FALSE} if \code{TRUE} will print information on the 
#'  queries in the console.
#' 
#' @examples 
#' \dontrun{
#' # run authentication with your app details
#' TK <- fbAuthenticate(app.id = "1234567890123456", 
#'                      app.secret = "16xx79321xx0130x2x10a08x3e2x80xx")
#' 
#' # get all accounts users (me) has access to
#' accounts <- listAccounts("me", TK, n = 999999)
#' 
#' set.seed(19880525) # set seed for sample() reproducability
#' 
#' # get labels of random account
#' labs <- getLabels(account.id = sample(accounts$id, 1), token = TK)
#' 
#' # get campaigns by labels
#' camps <- getLabCampaigns(sample(accounts$id, 1), labels = labs$id, 
#'                          token = TK)
#' }
#' 
#' @seealso \code{\link{getLabels}}
#' 
#' @author John Coene \email{john.coene@@cmcm.com}
#' 
#' @export
getLabCampaigns <- function(account.id, labels, fields = c("id", "name"), 
                            token, n = 100, verbose = FALSE){
  
  # check inputs
  if(missing(account.id)){
    stop("Missing account.id")
  } else if (missing(token)){
    stop("Missing token")
  } else if (missing(labels)){
    stop("Missing labels")
  }
  
  if(length(fields)){
    testParam("fields", fields, fct = "getLabCampaigns")
    
    # createFields
    fields <- createFields(fields)
  }
  
  # check token
  token <- checkToken(token = token)
  
  labels <- paste0("[%22", paste0(labels, collapse = "%22%2C%20%22"), "%22]")
  
  # build url
  uri <- paste0("https://graph.facebook.com/v2.6/",
                account.id, "/campaignsbylabels?fields=", fields, 
                "&ad_label_ids=", labels , "&access_token=", token)
  
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