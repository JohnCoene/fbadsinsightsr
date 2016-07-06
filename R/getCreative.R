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
#' @examples 
#' \dontrun{
#' # run authentication with your app details
#' fbOAuth <- fbAuthenticate(app.id = "1234567890123456", 
#'                           app.secret = "76xx79121xx0130x2x10a08x3e2x80xx", 
#'                           scope = "ads_management")
#' ls <- listCreatives(id = "act_1231313", token = fbOAuth)
#' (creative <- getCreative(object.story.id = sample(ls$object_story_id, 1), 
#'                          token = fbOAuth))
#' }
#' 
#' @author John Coene \email{jcoenep@@gmail.com}
#' 
#' @export
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
  uri <- paste0('https://graph.facebook.com/v2.6/',
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