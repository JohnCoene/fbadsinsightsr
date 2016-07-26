#' Update adset
#' 
#' @description Update a specific adset
#' 
#' @param id ID of object to update.
#' @param settings \code{list} of settings to update
#' @param token A valid token as returned by \code{\link{fbAuthenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' 
#' @return Boolean, success \code{TRUE} or \code{FALSE}
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
#' ids <- createLabels(accs$id[1], labels = "my label", token = TK)
#' 
#' adset_ids <- grabAdsets(accs$id[1], TK) # retrieve adsets
#'
#' # collapse ids
#' ids <- paste0("'", paste0(ids, collapse = "', '"), "'")
#'                   
#' # create settings to update - ad label                   
#' my_settings <- list(adlabels = paste0("[{id:", ids,"}]")) 
#' 
#' updateAdset(adset_ids[1], my_settings, TK)
#' }
#' 
#' @author John Coene \email{john.coene@@cmcm.com}
#' 
#' @export
updateAdset <- function(id, settings, token){
  
  # check inputs
  if(missing(id)){
    stop("Missing id")
  } else if (missing(token)){
    stop("Missing token")
  } else if(missing(settings)) {
    stop("Missing settings")
  }
  
  # check token
  token <- checkToken(token = token)
  
  # append token
  settings <- append(settings, token)
  names(settings)[length(settings)] <- "access_token"
  
  resp <- httr::POST(url = paste0("https://graph.facebook.com/v2.7/", id), 
                     body = settings, encode = "json")
  
  httr::stop_for_status(resp)
  
  content <- httr::content(resp)
  
  return(unlist(content))
}