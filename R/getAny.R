#' Get data on any object
#' 
#' @description Get insights on advertising performances of an ad, an adset, a campaign or even an account. This function may relace any other GET function in this package.
#' 
#' @param id The id of the object you want to retrieve (Required),
#'  see \href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.
#' @param fields 
#' There are in total 73 valid fields defaults to 
#' (\code{default}) which returns the most popular ones. 
#' Run \code{\link{findFields}} to see all valid fields.
#' @param action.attribution.windows 
#' Determines what is the attribution window for the actions.
#'  For example, \code{c("28d_click")} means the API returns all actions
#'   that happened 28 days after someone clicked on the ad (Optional).
#'    See details below for valid values.
#' @param action.breakdowns 
#' How to break down action results. Supports more than one breakdowns 
#' (Optional). Run \code{\link{findParams}} to see all 
#' valid action breakdowns.
#' @param action.report.time 
#' Determines the report time of action stats. 
#' For example, if a person saw the ad on Jan 1st but converted on Jan 2nd,
#'  when you query the API with \code{action.report.time="impression"}, 
#'  you will see a conversion on Jan 1st. When you query the API with 
#'  \code{action.report.time="conversion"}, you will see a conversion 
#'  on Jan 2nd (Optional).
#' @param breakdowns
#'  How to break down the result. Does not support more than one breakdown,
#'   except \code{c("age", "gender")} and 
#'   \code{"impression_device", "placement"}. The option 
#'   \code{impression_device} cannot be used by itself (Optional). Run 
#'   \code{\link{findParams}} to see all valid breakdowns.
#' @param date.preset
#'  Represents a relative time range (Optional). This field is ignored if 
#'  \code{time.range} is specified. Run \code{\link{findParams}} 
#'  to see all valid presets.
#' @param level
#'  Represents the level of result (Optional). Must be one  of \code{ad}, 
#'  \code{adset}, \code{campaign}, \code{account}.
#' @param time.increment
#'  If it is an integer, it is the number of days from 1 to 90. 
#'  After you pick a reporting period by using \code{time.range} or 
#'  \code{date.preset}, you may choose to have the results for the whole
#'   period, or have results for smaller time slices. If "all_days" is used,
#'    it means one result set for the whole period. If "monthly" is used, 
#'    you will get one result set for each calendar month in the given period.
#'     Or you can have one result set for each N-day period specified by this
#'      param.
#' @param time.range
#'  time range must be \code{c(since = 'YYYY-MM-DD', until='YYYY-MM-DD')}
#' @param n
#'  Number of results to retrieve, defaults to \code{100}. 
#'  When you make an API request, you will usually not receive all of the 
#'  results of that request in a single response. 
#'  This is because some responses could contain thousands of objects so 
#'  most responses are paginated by default.
#' @param token
#'  A valid token as returned by \code{\link{fbAuthenticate}} or a 
#'  short-term token from 
#'  \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param summary
#'  Default value: \code{FALSE}
#'  Determine whether to return a summary section with the same fields as 
#'  specified by fields will be included in the summary section. If 
#'  \code{TRUE} the data structure returned will be of type list 
#'  (see @return below)
#' @param verbose
#'  Defaults to \code{FALSE} if \code{TRUE} will print information on the 
#'  queries in the console.
#' @param limit
#'  Number of results requested at each API call, defaults to 100.
#'  Sometimes useful to bring it down if many results (\code{n}) are required as the 
#'  API might otherwise return \code{error_code: 1} or in other words an
#'   "Unknown error".
#' 
#' @details This function refers to the following API call \url{https://developers.facebook.com/docs/marketing-api/reference/ad-account/insights/},
#' it is strongly encouraged to have a look a the latter link.
#' only the following parameters are not available \code{default_summary}, 
#' \code{filtering}, \code{summary}, \code{sort} and \code{time_ranges}.
#' 
#' \code{getAny} may replace any other GET function. Indeed all functions share the same parameters and fields, the data that is returned solely depend on the object id the functions is fed. See examples.
#' 
#' @return Note that if summary is \code{TRUE} the function returns a 
#' \code{list}
#' 
#' @examples 
#' \dontrun{
#' # run authentication with your app details
#' fbOAuth <- fbAuthenticate(app.id = "1234567890123456", 
#'                           app.secret = "76xx79121xx0130x2x10a08x3e2x80xx", 
#'                           scope = "ads_management")
#'                           
#' # get ads on account
#' ads <- grabAds(id = "act_1014262775316748", token = TK)
#' 
#' # get adsets on account
#' adsets <- grabAdsets(id = "act_1014262775316748", token = TK)
#' 
#' # get ads on account
#' camp <- grabCampaigns(id = "act_1014262775316748", token = TK)
#' 
#' # get date.preset (this quarter)
#' date <- findParams("date.preset")[grep("quarter", findParams("date.preset"))]
#' 
#' # fetch Ad data
#' dat <- getAny(id = sample(ads$id, 1), token = TK, date.preset = date)
#' 
#' # fetch Adset data
#' dat <- getAny(id = sample(adsets$id, 1), token = TK, date.preset = date)
#' 
#' # fetch Campaign data
#' dat <- getAny(id = sample(campaigns$id, 1), token = TK, date.preset = date)
#' 
#' # fetch Account data
#' dat <- getAny(id = "act_1014262775316748", token = TK, date.preset = date)
#' }
#' 
#' @export
#' 
#' @seealso \code{\link{fbAuthenticate}}, \code{\link{findParams}},
#' \code{\link{grabAds}}, \code{\link{grabAdsets}}, 
#' \code{\link{grabCampaigns}} 
#' 
#' @author John Coene \email{jcoenep@@gmail.com}
getAny <- function(id, token, fields = "default", n = 100, 
                   action.attribution.windows, action.breakdowns, 
                   action.report.time, breakdowns, date.preset, level, 
                   time.increment, time.range, summary = FALSE,
                   verbose = FALSE, limit = 100) {
  
  # check arguments
  if(missing(action.attribution.windows)) action.attribution.windows <- NULL
  if(missing(action.breakdowns)) action.breakdowns <- NULL
  if(missing(action.report.time)) action.report.time <- NULL
  if(missing(breakdowns)) breakdowns <- NULL
  if(missing(date.preset)) date.preset <- NULL
  if(missing(level)) level <- NULL
  if(missing(time.increment)) time.increment <- NULL
  if(missing(time.range)) time.range <- NULL
  
  # check inputs
  if(missing(id)){
    stop("Missing id", call. = FALSE)
  }
  if (missing(token)){
    stop("Missing token", call. = FALSE)
  } 
  
  # check if region and action_carousel
  for (i in 1:length(fields)) {
    if(!is.null(breakdowns) && breakdowns == "region"){
      if(fields[i] == "action_carousel_card_id" ||
         fields[i] == "action_carousel_card_name"){
        stop(paste0("region cannot be used with action_carousel_card_id",
                    "or action_carousel_card_name"),
             call. = FALSE)
      }
    }
  }
  
  # create fields
  if(fields[1] == "default") {
    fields <- NULL
  } else {
    if(class(fields) != "character") {
      stop("Fields must be a character vector", 
           call. = FALSE)
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
    action.attribution.windows <- paste0("&action_attribution_windows=",
                                         toHTTP(action.attribution.windows))
  }
  
  # action.breakdowns
  if (length(action.breakdowns)) {
    
    # test
    testParam("action_breakdowns", action.breakdowns)
    
    # build action.breakdowns
    action.breakdowns <- paste0("&action_breakdowns=", 
                                toHTTP(action.breakdowns))
  }
  
  # action.report.time
  if (length(action.report.time) == 1) {
    
    # test
    testParam("action_report_time", action.report.time)
    
    action.report.time <- paste0("&action_report_time=", action.report.time)
  } else if (length(action.report.time) > 1) {
    stop(paste0("action.report.time can only hold one value:", 
                "impression OR conversion"),
         call. = FALSE)
  }
  
  # breakdowns
  if(length(breakdowns)){
    breakdowns <- buildBreakdowns(breakdowns = breakdowns, f = testParam)
  }
  
  # date.preset
  
  # make date.preset NULL if time.range specified
  if (length(date.preset) == 1 && length(time.range) >= 1) {
    date.preset <- NULL
    warning("date.preset is ignored as time.range is specified",
            call. = FALSE)
  }
  
  if (length(date.preset) == 1) {
    
    # test
    testParam("date_preset", date.preset)
    
    date.preset <- paste0("&date_preset=", date.preset)
  } else if (length(date.preset) > 1) {
    stop(paste0("date.preset can only hold one of the following values: today,", 
                " yesterday, last_3_days, this_week, last_week, last_7_days,",
                " last_14_days, last_28_days, last_30_days, last_90_days, ",
                " this_month, last_month, this_quarter, last_3_months,",
                " lifetime"),
         call. = FALSE)
  }
  
  # level
  if (length(level) == 1) {
    
    # test
    testParam("level", level)
    
    level <- paste0("&level=", level)
  } else if (length(level) > 1) {
    stop(paste0("level can only hold one of the following values: ad, adset,",
                " campaign, account"), call. = FALSE)
  }  
  
  #time increment
  if(length(time.increment) == 1) {
    
    if(is.null(time.range) && is.null(date.preset)) {
      stop("time.increment must be used with either date.preset OR time.range",
           call. = FALSE)
    }
    
    # test if interger/numeric or character is passed
    if(class(time.increment) == "numeric" || class(time.increment) == "interger"){
      if(time.increment < 1 || time.increment > 90) {
        stop(paste0("If integer is passed to time.increment (number of days)",
                    " must be between 1 and 90. See @param."), call. = FALSE)
      } else {
        time.increment <- paste0("&time_increment=", time.increment)
      }
    } else {
      # test
      testParam("time_increment", time.increment)
      
      time.increment <- paste0("&time_increment=", time.increment)
    }
    
  } else if (length(time.increment) > 1) {
    stop(paste0("time.increment can only hold one of the following values:",
                " monthly OR all_days"), call. = FALSE)
  }
  
  # time range
  if(length(time.range) == 2) {
    
    date_check <- as.Date(time.range[2], format= "%Y-%m-%d")
    
    # further checks
    if(names(time.range)[1] != "since"){
      stop("time.range must be - c(since = 'YYYY-MM-DD', until = 'YYYY-MM-DD')",
           call. = FALSE)
    } else if (class(date_check) == "try-error" || is.na(date_check)){
      stop("Wrong date format. Must be YYYY-MM-DD", call. = FALSE)
    }
    
    time.range <- paste0("&time_range={'since':'", time.range[1], 
                         "','until':'", time.range[2], "'}")
    
  } else if (length(time.range) > 2) {
    stop("time.range must be - c(since = 'YYYY-MM-DD', until='YYYY-MM-DD')",
         call. = FALSE)
  }
  
  # summary
  summary <- paste0("&default_summary=", summary)
  
  # check token verison
  token <- checkToken(token)
  
  # build url
  uri <- paste0('https://graph.facebook.com/v2.8/',
                id, '/insights?fields=',fields,
                action.attribution.windows, action.breakdowns,
                action.report.time, breakdowns, date.preset, level,
                time.increment, time.range, summary,
                '&limit=', limit,'&access_token=', token)
  
  if(length(uri) > 1){
    stop("multiple ids supplied", call. = FALSE)
  }
  
  # call api
  response <- httr::GET(uri)
  
  # construct data
  fb_data <- constructFbAdsData(response)
  
  # parse data
  fb_data <- digest(fb_data)
  
  # paginate
  fb_data <- paginate(fb_data, n = n, verbose = verbose)
  
  # verbose
  if (verbose == TRUE) {
    message(paste(n, "results requested, API returned", nrow(fb_data$data), 
                  "rows"))
  }
  
  # converge
  fb_data <- converge(fb_data)
  
  if(class(fb_data) == "data.frame"){
    if (nrow(fb_data) == 0) warning(paste("No data."), call. = FALSE)
  }

  return(fb_data)
  
}