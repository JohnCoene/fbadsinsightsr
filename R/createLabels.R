#' Create labels
#' 
#' @description Create labels in given account
#' 
#' @param account.id ID of the account to retrieve labels from.
#' @param labels Name of labels to create
#' @param token A valid token as returned by \code{\link{fbAuthenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' 
#' @return Vector of label IDs created
#' 
#' @examples 
#' \dontrun{
#' # run authentication with your app details
#' TK <- fbAuthenticate(app.id = "1234567890123456", 
#'                      app.secret = "16xx79321xx0130x2x10a08x3e2x80xx")
#'                      
#' # get 100 accounts users (me) has access to
#' accounts <- listAccounts("me", TK)
#' 
#' ids <- createLabels(accs$id[1], labels = c("first label", "second label"),
#'                     token = TK)
#' }
#' 
#' @seealso \code{\link{getLabels}}
#' 
#' @author John Coene \email{john.coene@@cmcm.com}
#' 
#' @export
createLabels <- function(account.id, labels, token){
  
  # check inputs
  if(missing(account.id)){
    stop("Missing account.id")
  } else if (missing(token)){
    stop("Missing token")
  } else if(missing(labels)) {
    stop("Missing labels")
  }
  
  # check token
  token <- checkToken(token = token)
  
  res <- lapply(labels, function(x){
    httr::POST(url = paste0("https://graph.facebook.com/v2.6/", 
                            account.id, "/adlabels"), 
               body = list(name = x, access_token = token))
  })
  
  stop <- lapply(res, function(x) httr::stop_for_status(x))
  
  content <- lapply(res, function(x) httr::content(x))
  
  return(unlist(content))
}