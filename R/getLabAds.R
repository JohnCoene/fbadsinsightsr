#' Get ads by labels
#' 
#' @description Retrieves ads by labels under given account.
#' 
#' @inheritParams getLabCampaigns
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
#' labs <- listLabels(account.id = sample(accounts$id, 1), token = TK)
#' 
#' # get ads by labels - 2 selected at random
#' ads <- listLabAds(sample(accounts$id, 1), labels = sample(labs$id, 2), 
#'                   token = TK)
#' }
#' 
#' @seealso \code{\link{listLabels}}
#' 
#' @author John Coene \email{john.coene@@cmcm.com}
#' 
#' @export
getLabAds <- function(account.id, labels, fields = c("id", "name"), 
                      operator = "ANY", token, n = 100, verbose = FALSE){
  
  # check inputs
  if(missing(account.id)){
    stop("Missing account.id")
  } else if (missing(token)){
    stop("Missing token")
  } else if (missing(labels)){
    stop("Missing labels")
  }
  
  if(toupper(operator) %in% c("ANY", "ALL") == FALSE) stop("Wrong operator")
  
  if(length(fields)){
    testParam("fields", fields, fct = "getLabCampaigns")
    
    # createFields
    fields <- createFields(fields)
  }
  
  # check token
  token <- checkToken(token = token)
  
  labels <- paste0("[%22", paste0(labels, collapse = "%22%2C%20%22"), "%22]")
  
  # build url
  uri <- paste0("https://graph.facebook.com/v2.8/",
                account.id, "/adsbylabels?fields=", fields, 
                "&ad_label_ids=", labels , "&operator=", operator, 
                "&access_token=", token)
  
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
    message(paste(n, "results requested, API returned", nrow(fb_data$data), 
                  "rows"))
  }
  
  # converge
  fb_data <- converge(fb_data)
  
  if (nrow(fb_data) == 0) warning(paste("No data."), call. = FALSE)
  
  return(fb_data)
  
}