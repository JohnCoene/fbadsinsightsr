#' Get creative content
#' 
#' @description Get creative content details and meta-data
#' 
#' @param object.story.id Object story id, can be obtained with 
#' \code{\link{listCreatives}}
#' @param token
#'  A valid token as returned by \code{\link{fbAuthenticate}} or a 
#'  short-term token from 
#'  \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param fields Variables to retrieve, see \code{\link{findFields}}
#' 
getCreative <- function(object.story.id, token, 
                        fields = c("name", "id", "message")){
  
  if(missing(object.story.id)){
    stop("Missing object.story.id", call. = FALSE)
  }
  if (missing(token)){
    stop("Missing token", call. = FALSE)
  } 
  
  testParam("fields", fields, "getCreative")
  
  # check token verison
  token <- checkToken(token)
  
  # build url
  uri <- paste0('https://graph.facebook.com/v2.5/',
                object.story.id, '?fields=', createFields(fields),
                '&access_token=', token)
  
  if(length(uri) > 1){
    stop("multiple ids supplied", call. = FALSE)
  }
  
  # call api
  response <- httr::GET(uri)
  
  # parse
  content <- httr::content(response)
  df <- as.data.frame(content)
  
  if (nrow(df) == 0) warning(paste("No data."), call. = FALSE)
  
  return(df)
}