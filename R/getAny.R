#' getAny
#' 
#' @description Get insights on advertising performances of an ad, an adset, a campaign or even an account. This function may relace any other GET function in this package.
#' 
#' @param id The id of the object you want to retrieve (Required), see \href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.
#' @param fields There are in total 73 valid fields; default (\code{NULL}) returns the most popular ones. Run \code{\link{findFields}} to see all valid fields.
#' @param action.attribution.windows Determines what is the attribution window for the actions. For example, \code{c("28d_click")} means the API returns all actions that happened 28 days after someone clicked on the ad (Optional). See details below for valid values.
#' @param action.breakdowns How to break down action results. Supports more than one breakdowns (Optional). Run \code{\link{findActionBreakdowns}} to see all valid action breakdowns.
#' @param action.report.time Determines the report time of action stats. For example, if a person saw the ad on Jan 1st but converted on Jan 2nd, when you query the API with \code{action.report.time="impression"}, you will see a conversion on Jan 1st. When you query the API with \code{action.report.time="conversion"}, you will see a conversion on Jan 2nd (Optional).
#' @param breakdowns How to break down the result. Does not support more than one breakdown, except \code{c("age", "gender")} and \code{"impression_device", "placement"}. The option \code{impression_device} cannot be used by itself (Optional). Run \code{\link{findBreakdowns}} to see all valid breakdowns.
#' @param date.preset Represents a relative time range (Optional). This field is ignored if \code{time.range} is specified. Run \code{\link{findDatePreset}} to see all valid presets.
#' @param level Represents the level of result (Optional). Must be one  of \code{ad}, \code{adset}, \code{campaign}, \code{account}.
#' @param time.increment If it is an integer, it is the number of days from 1 to 90. After you pick a reporting period by using \code{time.range} or \code{date.preset}, you may choose to have the results for the whole period, or have results for smaller time slices. If "all_days" is used, it means one result set for the whole period. If "monthly" is used, you will get one result set for each calendar month in the given period. Or you can have one result set for each N-day period specified by this param.
#' @param time.range time range must be \code{c(since = 'YYYY-MM-DD', until='YYYY-MM-DD')}
#' @param n Number of results to retrieve, defaults to \code{100}. When you make an API request, you will usually not receive all of the results of that request in a single response. This is because some responses could contain thousands of objects so most responses are paginated by default. \code{previous} fetches the previous page of response (after the initial query) similarly \code{next} fetches the next page and \code{NULL} does not paginate (only makes one query).
#' @param token A valid token as returned by \code{\link{fbAuthenticate}} or a short-term token from \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param verbose Defaults to \code{FALSE} if \code{TRUE} will print information on the query in the console.
#' @param simplify Depreacted. 
#' 
#' @details This function refers to the following API call \url{https://developers.facebook.com/docs/marketing-api/reference/ad-account/insights/},
#' it is strongly encouraged to have a look a the latter link.
#' only the following parameters are not available \code{default_summary}, \code{filtering}, 
#' \code{summary}, \code{sort} and \code{time_ranges}.
#' 
#' Valid \code{action.attribution.windows}:
#' \itemize{
#' \item 1d_view
#' \item 7d_view
#' \item 28d_view
#' \item 1d_click
#' \item 7d_click
#' \item 28d_click
#' }
#' 
#' \code{getAny} may replace any other GET function. Indeed all functions share the same parameters and fields, the data that is returned solely depend on the object id the functions is fed. See examples.
#' 
#' @examples 
#' \dontrun{
#' # run authentication with your app details
#' fbOAuth <- fbAuthenticate(app.id = "1234567890123456", 
#'                           app.secret = "76xx79121xx0130x2x10a08x3e2x80xx", 
#'                           scope = "ads_management")
#'                           
#' # get information on account
#' info <- findInfo(account.id = "act_123456789012345", token = fbOAuth)
#' 
#' # get date.preset
#' date <- findDatePreset()[grep("quarter", findDatePreset())]
#' 
#' # fetch Ad data
#' dat <- getAny(id = sample(info$ad$id, 1), token = fbOAuth, date.preset = date)
#' 
#' # fetch Adset data
#' dat <- getAny(id = sample(info$adsets$id, 1), token = fbOAuth, date.preset = date)
#' 
#' # fetch Campaign data
#' dat <- getAny(id = sample(info$campaigns$id, 1), token = fbOAuth, date.preset = date)
#' 
#' # fetch Account data
#' dat <- getAny(id = "act_123456789012345", token = fbOAuth, date.preset = date)
#' }
#' 
#' @export
#' 
#' @seealso \code{\link{fbAuthenticate}} 
#' 
#' @author John Coene <john.coene@@cmcm.com>
getAny <- function(id, fields = "default",
                   action.attribution.windows = NULL,
                   action.breakdowns = NULL, action.report.time = NULL,
                   breakdowns = NULL, date.preset = NULL, level = NULL, 
                   time.increment = NULL, time.range = NULL, 
                   n = 100, token, verbose = FALSE,
                   simplify = FALSE) {
  
  
  # check if region and action_carousel
  for (i in 1:length(fields)) {
    if(!is.null(breakdowns) && breakdowns == "region"){
      if(fields[i] == "action_carousel_card_id" ||
         fields [i] == "action_carousel_card_name"){
        stop("region cannot be used with action_carousel_card_id or action_carousel_card_name")
      }
    }
  }
  
  # check inputs
  if(missing(id)){
    stop("Missing id")
  } else if (missing(token)){
    stop("Missing token")
  }
  
  # simplify
  if(simplify == TRUE){
    warning("simplify has been deprecated. see README")
  }
  
  # create fields
  if(fields[1] == "default") {
    fields <- NULL
  } else {
    if(class(fields) != "character") {
      stop("Fields must be a character vector")
    } else { 
      # test if fields correct
      testParam("fields", fields)
      
      # createFields
      fields <- createFields(fields)
    }
  }
  
  # action.attribution.windows
  if (length(action.attribution.windows)) {
    
    # test if correct
    testParam("action_attribution_windows", action.attribution.windows)
    
    # build action.attribution.windows
    action.attribution.windows <- paste0("&action_attribution_windows=", toHTTP(action.attribution.windows))
  }
  
  # action.breakdowns
  if (length(action.breakdowns)) {
    
    # test
    testParam("action_breakdowns", action.breakdowns)
    
    # build action.breakdowns
    action.breakdowns <- paste0("&action_breakdowns=", toHTTP(action.breakdowns))
  }
  
  # action.report.time
  if (length(action.report.time) == 1) {
    
    # test
    testParam("action_report_time", action.report.time)
    
    action.report.time <- paste0("&action_report_time=", action.report.time)
  } else if (length(action.report.time) > 1) {
    stop("action.report.time can only hold one value: impression OR conversion")
  }
  
  # breakdowns
  breakdowns <- buildBreakdowns(breakdowns = breakdowns)
  
  # date.preset
  
  # make date.preset NULL if time.range specified
  if (length(date.preset) == 1 && length(time.range) >= 1) {
    date.preset <- NULL
    warning("date.preset is ignored as time.range is specified")
  }
  
  if (length(date.preset) == 1) {
    
    # test
    testParam("date_preset", date.preset)
    
    date.preset <- paste0("&date_preset=", date.preset)
  } else if (length(date.preset) > 1) {
    stop("date.preset can only hold one of the following values: today, yesterday, last_3_days, this_week, last_week, last_7_days, last_14_days, last_28_days, last_30_days, last_90_days, this_month, last_month, this_quarter, last_3_months, lifetime")
  }
  
  # level
  if (length(level) == 1) {
    
    # test
    testParam("level", level)
    
    level <- paste0("&level=", level)
  } else if (length(level) > 1) {
    stop("level can only hold one of the following values: ad, adset, campaign, account")
  }  
  
  #time increment
  if(length(time.increment) == 1) {
    
    if(is.null(time.range) && is.null(date.preset)) {
      stop("time.increment must be used with either date.preset OR time.range")
    }
    
    # test if interger/numeric or character is passed
    if(class(time.increment) == "numeric" || class(time.increment) == "interger"){
      if(time.increment < 1 || time.increment > 90) {
        stop("If integer is passed to time.increment (number of days) must be between 1 and 90.")
      } else {
        time.increment <- paste0("&time_increment=", time.increment)
      }
    } else {
      # test
      testParam("time_increment", time.increment)
      
      time.increment <- paste0("&time_increment=", time.increment)
    }
    
  } else if (length(time.increment) > 1) {
    stop("time.increment can only hold one of the following values: monthly OR all_days")
  }
  
  # time range
  if(length(time.range) == 2) {
    
    date_check <- as.Date(time.range[2], format= "%Y-%m-%d")
    
    # further checks
    if(names(time.range)[1] != "since"){
      stop("time.range must be - c(since = 'YYYY-MM-DD', until='YYYY-MM-DD')")
    } else if (class(date_check) == "try-error" || is.na(date_check)){
      stop("Wrong date format. Must be YYYY-MM-DD")
    }
    
    time.range <- paste0("&since=", time.range[1], "&", "until=", time.range[2])
    
  } else if (length(time.range) > 2) {
    stop("time.range must be - c(since = 'YYYY-MM-DD', until='YYYY-MM-DD')")
  }
  
  # check token verison
  token <- checkToken(token)
  
  url <- paste0("https://graph.facebook.com/v2.5/",
                id, "/insights?fields=",fields,
                action.attribution.windows, action.breakdowns,
                action.report.time, breakdowns, date.preset, level,
                time.increment, time.range, "&access_token=", token)
  
  # call api
  response <- httr::GET(url)
  
  # parse to list
  json <- rjson::fromJSON(rawToChar(response$content))
  
  # check if query successful 
  if(length(json$error$message)){
    stop(paste("this is likely due to id or token. Error Message returned: ",
               json$error$message))
  } else if (length(json$data) == 0) {
    warning(paste("No data."))
    
    # make empt data.frame
    data <- data.frame()
  } else {
    
    # parse
    data <- toDF(response)
    
    #paginate
    data <- paginate(data = data, json = json, verbose = verbose, n = n)
    
    # verbose
    if (verbose == TRUE) {
      cat(paste(n, "results requested, API returned", nrow(data)))
    } 
    
    
  }
  
  return(data)
  
}