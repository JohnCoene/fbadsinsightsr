#' Create an audience
#' 
#' @description Create an empty audience
#' 
#' @param token A valid token as returned by \code{\link{fbAuthenticate}} 
#' or a short-term token from 
#' \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param account.id The id of the account you want to retrieve (Required),
#'  see \href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.
#' @param description Description of the audience
#' @param name Name of the audience
#' @param subtype Type of custom audience, derived from original data source.
#' See \code{\link{findParams}}.
#' 
#' @details To create a custom audience you'll first need to create a 
#' blank audience. Then, you'll want to add people to the blank audience you j
#' ust created by updating the users edge of the audience using 
#' \code{\link{fillAudience}}.
#' 
#' @seealso \code{\link{fillAudience}}
#' 
#' @author John Coene \email{john.coene@@cmcm.com}
#' 
#' @export
createAudience <- function(token, account.id, description, name, 
                           subtype = "CUSTOM"){
  
  if(missing(account.id)){
    stop("Missing account.id", call. = FALSE)
  } 
  if (missing(token)){
    stop("Missing token", call. = FALSE)
  } 
  if (missing(description)) {
    stop("Missing description", call. = FALSE)
  }
  if (missing(name)) {
    stop("Missing name", call. = FALSE)
  }
  
  testParam("subtype", subtype, "createAudience")
  
  uri <- paste0('https://graph.facebook.com/v2.6/',
                account.id, '/customaudiences?')
  
  resp <- httr::POST(url = uri, body = list(
    name = URLencode(name),
    description = URLencode(description),
    subtype = subtype,
    access_token = token
  ))
  
  return(uri)
  
}