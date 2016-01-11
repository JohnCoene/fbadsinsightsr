#' getCampaign
#' 
#' @description Get insights on advertising performances of an ad. See \href{https://developers.facebook.com/docs/marketing-api/reference/ad-campaign-group/insights/}{documentation} for more information
#' 
#' @param campaign.id The id of the campaign you want to retrieve insights about (Required), see \href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.
#' \href{https://www.facebook.com/business/help/1492627900875762}{how to find yours}.
#' @param fields 
#' There are in total 73 valid fields; default 
#' (\code{NULL}) returns the most popular ones. Run \code{\link{findFields}}
#'  to see all valid fields.
#' @param action.attribution.windows 
#' Determines what is the attribution window for the actions.
#'  For example, \code{c("28d_click")} means the API returns all actions
#'   that happened 28 days after someone clicked on the ad (Optional).
#'    See details below for valid values.
#' @param action.breakdowns 
#' How to break down action results. Supports more than one breakdowns 
#' (Optional). Run \code{\link{findActionBreakdowns}} to see all 
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
#'   \code{\link{findBreakdowns}} to see all valid breakdowns.
#' @param date.preset
#'  Represents a relative time range (Optional). This field is ignored if 
#'  \code{time.range} is specified. Run \code{\link{findDatePreset}} 
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
#'  most responses are paginated by default. \code{previous} fetches the 
#'  previous page of response (after the initial query) similarly 
#'  \code{next} fetches the next page and \code{NULL} does not paginate 
#'  (only makes one query).
#' @param token
#'  A valid token as returned by \code{\link{fbAuthenticate}} or a 
#'  short-term token from 
#'  \href{https://developers.facebook.com/tools/explorer}{facebook Graph API Explorer}.
#' @param verbose
#'  Defaults to \code{FALSE} if \code{TRUE} will print information on the 
#'  queries in the console.
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
#' # take random ad.id
#' set.seed(123)
#' rand_id <- sample(info$campaigns$id, 1)
#' 
#' # get date.preset
#' date <- findDatePreset()[grep("quarter", findDatePreset())]
#' 
#' # fetch Ad data for this quarter
#' data <- getCampaign(campaign.id = rand_id, token = fbOAuth, date.preset = date)
#' }
#' 
#' @export
#' 
#' @seealso \code{\link{fbAuthenticate}} 
#' 
#' @author John Coene <john.coene@@cmcm.com>
getCampaign <- function(campaign.id, token, fields = "default", n = 100, 
                  action.attribution.windows, action.breakdowns, 
                  action.report.time, breakdowns, date.preset, level, 
                  time.increment, time.range, verbose = FALSE) {
  
  # check arguments
  if(missing(action.attribution.windows)) action.attribution.windows <- NULL
  if(missing(action.breakdowns)) action.breakdowns <- NULL
  if(missing(action.report.time)) action.report.time <- NULL
  if(missing(breakdowns)) breakdowns <- NULL
  if(missing(date.preset)) date.preset <- NULL
  if(missing(level)) level <- NULL
  if(missing(time.increment)) time.increment <- NULL
  if(missing(time.range)) time.range <- NULL
  
  fb_data <- getAny(id = campaign.id, token = token, fields = "default",
                    action.attribution.windows = action.attribution.windows,
                    action.breakdowns = action.breakdowns,
                    action.report.time = action.report.time,
                    breakdowns = breakdowns, date.preset = date.preset,
                    level = level, time.increment = time.increment, 
                    time.range = time.range, 
                    n = n, verbose = verbose)
  
  return(fb_data)
  
}