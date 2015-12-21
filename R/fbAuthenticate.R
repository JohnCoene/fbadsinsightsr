#' fbAuthenticate
#' 
#' @description 
#' Authenticate through Facebook Graph API with your facebook application which you can create at \href{https://developers.facebook.com/apps}{https://developers.facebook.com/apps}
#' 
#' @param app.id ID of your facebook app. \href{https://developers.facebook.com/apps}{See facebook apps}
#' @param app.secret App secret of facebook app. \href{https://developers.facebook.com/apps}{See facebook apps}
#' @param scope General permissions, either "ads_management" or "ads_read", defaults to the latter, \href{https://developers.facebook.com/docs/facebook-login/permissions}{see permissions}.
#' @param app.name Name of your app, optional.
#' 
#' @details 
#' There are two different ways of making authenticated requests. One is to obtain
#' a temporary access token from \url{https://developers.facebook.com/tools/explorer},
#' which can be used as argument in any of the functions in this package.
#'
#' However, this token has a 2-hour lifetime by default and after it expires and thus
#' needs to be renewed frequently. The second alternative is to create an OAuth token. The 
#' process to create it is a bit more tedious. It is divided in three steps.
#' 
#' First, go to \url{https://developers.facebook.com/apps}, register as a developer
#' and create a new app. You will also need a verified Facebook account.
#' After that, click in "Show" under "App Secret" to find your 'App ID' and 'App Secret'.
#' \enumerate{
#' 
#' \item Go to facebook's Graph API Explorer: \url{https://developers.facebook.com/tools/explorer}
#' \item Select "Graph API Explorer" in the top right corner
#' \item Then in the dropdown underneath hit "Get Token" and select "Get User Access Token"
#' \item In the popup navigate to "Extended Permissions" and select "ads_management" and "ads_read"
#' \item Click "Get Access Token" and allow the app in the following popup
#' \item Copy and Save the token in your R sesssion.
#' }
#' 1. Go to facebook's Graph API Explorer: \url{https://developers.facebook.com/tools/explorer}
#' 2. Select "Graph API Explorer" in the top right corner
#' 3. Then in the dropdown underneath hit "Get Token" and select "Get User Access Token"
#' 4. In the popup navigate to "Extended Permissions" and select "ads_management" and "ads_read"
#' 5. Click "Get Access Token" and allow the app in the following popup
#' 6. Copy and Save the token in your R sesssion.
#' 
#' @examples  
#' \dontrun{
#' # run authentication with your app details
#' fbOAuth <- fb_authenticate(app.id = "1234567890123456", 
#'                            app.secret = "76xx79121xx0130x2x10a08d3e2x80xx", 
#'                            scope = "ads_management")
#' print(fbOAuth)
#' <Token>
#' <oauth_endpoint>
#'  authorize: https://www.facebook.com/dialog/oauth
#'  access:    https://graph.facebook.com/oauth/access_token
#'<oauth_app> optional
#'  key:    1234567890123456
#'  secret: <hidden>
#'  <credentials> access_token, expires
#' ---
#' }
#' 
#' @author John coene \email{john.coene@@cmcm.com}
#' @export
fbAuthenticate <- function (app.id, app.secret, scope = "ads_read",
                             app.name = "optional") {
  
  # check inputs
  if (class(app.secret) != "character" & class(app.secret) != "factor") {
    stop("Invalid app.secret: must be of class character or factor")
  }
  
  # check scope
  scopeCheck(scope)
  
  # create endpoint
  facebook <- httr::oauth_endpoint(authorize = "https://www.facebook.com/dialog/oauth",
                             access = "https://graph.facebook.com/oauth/access_token")	
  
  # create app
  myapp <- httr::oauth_app(app.name, app.id, app.secret)
  
  # check httr version
  if (packageVersion('httr') <= "0.2"){
    facebook_token <- httr::oauth2.0_token(facebook, myapp,
                                     scope=scope, type = "application/x-www-form-urlencoded")
    fb_oauth <- httr::sign_oauth2.0(facebook_token$access_token) 
    if (httr::GET("https://graph.facebook.com/me", config=fb_oauth)$status==200){
      message("Authentication successful.")
    }
  } else if (packageVersion('httr') > "0.2" & packageVersion('httr') <= "0.6.1"){
    fb_oauth <- httr::oauth2.0_token(facebook, myapp,
                               scope=scope, type = "application/x-www-form-urlencoded", cache=FALSE)	
    if (httr::GET("https://graph.facebook.com/me", config(token=fb_oauth))$status==200){
      message("Authentication successful.")
    }	
  } else if (packageVersion('httr') > "0.6.1"){
    Sys.setenv("HTTR_SERVER_PORT" = "1410/")
    fb_oauth <- httr::oauth2.0_token(facebook, myapp,
                               scope=scope, type = "application/x-www-form-urlencoded", cache=FALSE)		
    if (httr::GET("https://graph.facebook.com/me", config(token=fb_oauth))$status==200){
      message("Authentication successful.")
    }	
  }
  
  return(fb_oauth)
}