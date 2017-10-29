#' Update ad
#' 
#' @description Update a specific ad
#' 
#' @inheritParams updateAdset
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
#' # collapse ids
#' ids <- paste0("'", paste0(ids, collapse = "', '"), "'")
#' 
#' ad_ids <- grabAds(accs$id[1], TK) # retrieve adsets
#'                   
#' # create settings to update - ad label                   
#' my_settings <- list(adlabels = paste0("[{id:'", ids,"'}]")) 
#' 
#' updateAd(ad_ids[1], my_settings, TK)
#' }
#' 
#' @author John Coene \email{jcoenep@@gmail.com}
#' 
#' @export
updateAd <- function(id, settings, token){
  
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
  
  resp <- httr::POST(url = paste0("https://graph.facebook.com/v2.8/", id), 
                     body = settings, encode = "json")
  
  httr::stop_for_status(resp)
  
  content <- httr::content(resp)
  
  return(unlist(content))
}