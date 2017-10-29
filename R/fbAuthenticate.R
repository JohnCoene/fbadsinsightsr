#' Authenticate
#' 
#' @description 
#' Authenticate through Facebook Graph API with your facebook application which you can create at \href{https://developers.facebook.com/apps}{https://developers.facebook.com/apps}
#' 
#' @param app.id ID of your facebook app. \href{https://developers.facebook.com/apps}{See facebook apps}
#' @param app.secret App secret of facebook app. \href{https://developers.facebook.com/apps}{See facebook apps}
#' @param scope General permissions, either \code{ads_management} or \code{ads_read}, defaults to the former, \href{https://developers.facebook.com/docs/facebook-login/permissions}{see permissions}.
#' @param app.name Name of your app, optional.
#' 
#' @details 
#' \strong{httpuv}: Consider loading the httpuv package to avoid issues, 
#' \code{install.packages("httpuv");library(httpuv)}
#' 
#' There are two different ways of making authenticated requests. One is to obtain
#' a temporary access token from \url{https://developers.facebook.com/tools/explorer}, 
#' the other is to use a long-term token provided by this very function. 
#' 
#' Either way you will have to create an app; go to \url{https://developers.facebook.com/apps}, register as a developer
#' and create a new app. You will also need a verified Facebook account.
#' 
#' To get the short-term token:
#' 
#' \enumerate{
#' \item Go to facebook's Graph API Explorer: \url{https://developers.facebook.com/tools/explorer}
#' \item Select your app in the top right corner (defaults to "Graph API Explorer")
#' \item Then in the dropdown underneath hit "Get Token" and select "Get User Access Token"
#' \item In the popup navigate to "Extended Permissions" and select "ads_management" and/or "ads_read"
#' \item Click "Get Access Token" and allow the app in the following popup
#' \item Copy and Save the token in your R sesssion.
#' }
#' However, this token has a 2-hour lifetime by default and thus
#' needs to be renewed frequently. The alternative is to create an OAuth token using this function and your "App ID" and "App Secret"
#' 
#' @examples  
#' \dontrun{
#' # run authentication with your app details
#' fbOAuth <- fbAuthenticate(app.id = "1234567890123456", 
#'                           app.secret = "16xx79321xx0130x2x10a08x3e2x80xx", 
#'                           scope = "ads_management")
#' print(fbOAuth)
#' <Token>
#' <oauth_endpoint>
#'  authorize: https://www.facebook.com/dialog/oauth
#'  access:    https://graph.facebook.com/oauth/access_token
#'<oauth_app> optional
#'  key:    123456789012345
#'  secret: <hidden>
#'  <credentials> access_token, expires
#' ---
#' }
#' 
#' @author John coene \email{jcoenep@@gmail.com}
#' @export
fbAuthenticate <- function (app.id, app.secret, scope = "ads_management",
                             app.name = "optional") {
  
  # check scope
  scopeCheck(scope)
  
  # create endpoint
  myapp <- httr::oauth_app(app.name, app.id, app.secret)
  
  # 3. Get OAuth credentials
  facebook_token <- httr::oauth2.0_token(
    httr::oauth_endpoints("facebook"),
    myapp, scope = scope,
    type = "application/x-www-form-urlencoded"
  )
  
  return(facebook_token)
}