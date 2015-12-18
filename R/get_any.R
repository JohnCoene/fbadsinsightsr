#' get_any
#' 
#' @description Get insights on advertising performances of any ad-related objects; \href{https://developers.facebook.com/docs/marketing-api/reference/ad-account/insights/}{documentation}.
#' 
#' @param id Your ad account id OR campaign id OR adset id OR ad id (Required), see \href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.
#' @param fields There are in total 73 valid fields; default (\code{NULL}) returns the most popular ones. Run \code{\link{find_fields}} to see all valid fields.
#' @param action_attribution_windows Determines what is the attribution window for the actions. For example, \code{c("28d_click")} means the API returns all actions that happened 28 days after someone clicked on the ad (Optional). See details below for valid values.
#' @param action_breakdowns How to break down action results. Supports more than one breakdowns (Optional). Run \code{\link{find_action_breakdowns}} to see all valid action breakdowns.
#' @param action_report_time Determines the report time of action stats. For example, if a person saw the ad on Jan 1st but converted on Jan 2nd, when you query the API with \code{action_report_time="impression"}, you will see a conversion on Jan 1st. When you query the API with \code{action_report_time="conversion"}, you will see a conversion on Jan 2nd (Optional).
#' @param breakdowns How to break down the result. Does not support more than one breakdown, except \code{c("age", "gender")} and \code{"impression_device", "placement"}. The option \code{impression_device} cannot be used by itself (Optional). Run \code{\link{find_breakdowns}} to see all valid breakdowns.
#' @param date_preset Represents a relative time range (Optional). This field is ignored if \code{time_range} or \code{time_ranges} is specified. Run \code{\link{find_date_preset}} to see all valid presets.
#' @param level Represents the level of result (Optional). Must be one  of \code{ad}, \code{adset}, \code{campaign}, \code{account}.
#' @param time_increment If it is an integer, it is the number of days from 1 to 90. After you pick a reporting period by using \code{time_range} or \code{date_preset}, you may choose to have the results for the whole period, or have results for smaller time slices. If "all_days" is used, it means one result set for the whole period. If "monthly" is used, you will get one result set for each calendar month in the given period. Or you can have one result set for each N-day period specified by this param.
#' @param time_range time range must be \code{c(since = 'YYYY-MM-DD', until='YYYY-MM-DD')}
#' @param paginate Not yet implemented
#' @param token A valid token as returned by \code{\link{fb_authenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' 
#' @details This function refers to the following API call \url{https://developers.facebook.com/docs/marketing-api/reference/ad-account/insights/},
#' it is strongly encouraged to have a look a the latter link.
#' only the following parameters are not available \code{default_summary}, \code{filtering}, 
#' \code{summary}, \code{sort} and \code{time_ranges}.
#' 
#' Valid \code{action_attribution_windows}:
#' \itemize{
#' \item 1d_view
#' \item 7d_view
#' \item 28d_view
#' \item 1d_click
#' \item 7d_click
#' \item 28d_click
#' }
#' 
#' @export
#' 
#' @seealso \code{\link{fb_authenticate}} 
#' 
#' @author John Coene <john.coene@@cmcm.com>
get_any <- function(id, fields = "default",
                        action_attribution_windows = NULL,
                        action_breakdowns = NULL, action_report_time = NULL,
                        breakdowns = NULL, date_preset = NULL, level = NULL, 
                        time_increment = NULL, time_range = NULL, 
                        paginate = "next", token) {
  
  # check inputs
  if(missing(id)){
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
      # test if fields correct
      test_param("fields", fields)
      
      # create_fields
      fields <- create_fields(fields)
    }
  }
  
  # action_attribution_windows
  if (length(action_attribution_windows)) {
    
    # test if correct
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
  
  # action_report_time
  if (length(action_report_time) == 1) {
    
    # test
    test_param("action_report_time", action_report_time)
    
    action_report_time <- paste0("&action_report_time=", action_report_time)
  } else if (length(action_report_time) > 1) {
    stop("action_report_time can only hold one value: impression OR conversion")
  }
  
  # breakdowns
  breakdowns <- build_breakdowns(breakdowns = breakdowns)
  
  # date_preset
  
  # make date_preset NULL if time_range specified
  if (length(date_preset) == 1 && length(time_range) >= 1) {
    date_preset <- NULL
    warning("date_preset is ignored as time_range is specified")
  }
  
  if (length(date_preset) == 1) {
    
    # test
    test_param("date_preset", date_preset)
    
    date_preset <- paste0("&date_preset=", date_preset)
  } else if (length(date_preset) > 1) {
    stop("date_preset can only hold one of the following values: today, yesterday, last_3_days, this_week, last_week, last_7_days, last_14_days, last_28_days, last_30_days, last_90_days, this_month, last_month, this_quarter, last_3_months, lifetime")
  }
  
  # level
  if (length(level) == 1) {
    
    # test
    test_param("level", level)
    
    level <- paste0("&level=", level)
  } else if (length(level) > 1) {
    stop("level can only hold one of the following values: ad, adset, campaign, account")
  }  
  
  #time increment
  if(length(time_increment) == 1) {
    
    if(is.null(time_range) && is.null(date_preset)) {
      stop("time_increment must be used with either date_preset OR time_range")
    }
    
    # test if interger/numeric or character is passed
    if(class(time_increment) == "numeric" || class(time_increment) == "interger"){
      if(time_increment < 1 || time_increment > 90) {
        stop("If integer is passed to time_increment (number of days) must be between 1 and 90.")
      } else {
        time_increment <- paste0("&time_increment=", time_increment)
      }
    } else {
      # test
      test_param("time_increment", time_increment)
      
      time_increment <- paste0("&time_increment=", time_increment)
    }
    
  } else if (length(time_increment) > 1) {
    stop("time_increment can only hold one of the following values: monthly OR all_days")
  }
  
  # time range
  if(length(time_range) == 2) {
    
    date_check <- as.Date(time_range[2], format= "%Y-%m-%d")
    
    # further checks
    if(names(time_range)[1] != "since"){
      stop("time_range must be - c(since = 'YYYY-MM-DD', until='YYYY-MM-DD')")
    } else if (class(date_check) == "try-error" || is.na(date_check)){
      stop("Wrong date format. Must be YYYY-MM-DD")
    }
    
    time_range <- paste0("&since=", time_range[1], "&", "until=", time_range[2])
    
  } else if (length(time_range) > 2) {
    stop("time_range must be - c(since = 'YYYY-MM-DD', until='YYYY-MM-DD')")
  }
  
  base_url <- "https://graph.facebook.com/v2.5/"
  
  # check token verison
  token <- check_token(token)
  
  url <- paste0(base_url, id, "/insights?fields=",fields,
                action_attribution_windows, action_breakdowns,
                action_report_time, breakdowns, date_preset, level,
                time_increment, time_range, "&access_token=", token)
  
  # call api
  response <- GET(url)
  
  # parse to list
  json <- rjson::fromJSON(rawToChar(response$content))
  
  # check if query successful 
  if(length(json$error$message)){
    stop(paste("this is likely due to account_id or token. Error Message returned: ",
               json$error$message))
  }
  
  # parse
  data <- parse_json(json)
  
  return(data)
  
}

