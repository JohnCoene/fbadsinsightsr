#' get_account
#' 
#' @description Get insights on advertising performance on ad account. See \href{https://developers.facebook.com/docs/marketing-api/reference/ad-account/insights/}{documentation} for more information
#' 
#' @param account_id Your ad account id, starting by "act_" and followed by 15 digits, see \href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.
#' @param fields There are in total 73 valid fields for this function. Run \code{\link{find_fields}} to print see all valid fields.
#' @param action_attribution_windows Determines what is the attribution window for the actions. For example, \code{c("28d_click")} means the API returns all actions that happened 28 days after someone clicked on the ad (Optional).
#' @param action_breakdowns How to break down action results. Supports more than one breakdowns (Optional).
#' @param action_report_time Not Yet Implemented
#' @param breakdowns Not Yet Implemented
#' @param date_preset Not Yet Implemented
#' @param level Not Yet Implemented
#' @param time_increment Not Yet Implemented
#' @param time_range Not Yet Implemented
#' @param token A valid token as returned by \code{\link{fb_authenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' 
#' @export
#' 
#' @author John Coene <john.coene@@cmcm.com>
get_account <- function(account_id, fields = "default",
                        action_attribution_windows = NULL,
                        action_breakdowns = NULL, action_report_time,
                        breakdowns, date_preset, level, 
                        time_increment, time_range, token) {
  
  # check inputs
  if(missing(account_id)){
    stop("Missing account_id")
  } else if (missing(token)){
    stop("Missing token")
  }
  
  # create fields
  if(fields[1] == "default") {
    fields <- NULL
  } else {
    if(class(fields) != "character") {
      stop("Fields must be a character vector")
    } else {
      test_param("fields", fields)
      fields <- create_fields(fields)
    }
  }
  
  # action_attribution_windows
  if (length(action_attribution_windows)) {
    
    # test
    test_param("action_attribution_windows", action_attribution_windows)
    
    # build action_attribution_windows
    action_attribution_windows <- paste0("&action_attribution_windows=", to_libcrul(action_attribution_windows))
  }
  
  # action_breakdowns
  if (length(action_breakdowns)) {
    
    # test
    test_param("action_breakdowns", action_breakdowns)
    
    # build action_attribution_windows
    action_breakdowns <- paste0("&action_breakdowns=", to_libcrul(action_breakdowns))
  }
  
  base_url <- "https://graph.facebook.com/v2.5/"
  
  url <- paste0(base_url, account_id, "/insights?fields=",fields,
                action_attribution_windows, action_breakdowns,
                "&access_token=", token)
  
  return(url)
  
}

