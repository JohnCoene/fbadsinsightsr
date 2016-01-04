#' findDatePreset
#' 
#' @description Returns all valid values for \code{date_preset} which can be used in the get-family functions. No input, see usage.
#' 
#' @examples 
#' \dontrun{
#' # get information on account
#' info <- findInfo(account.id = "act_123456789012345", token = "XXXXXXXXXXX")
#' 
#' # take random ad.id
#' set.seed(123)
#' rand_id <- sample(info$campaigns$id, 1)
#' 
#' # get date.preset
#' date <- findDatePreset()[9]
#' 
#' # fetch Campaign data for the last 30 days broken down by days (rather than aggregate)
#' data <- getAny(id = rand_id, token = "XXXXXXXXXXX", date.preset = date, time.increment = 1)
#' }
#' 
#' @seealso \code{\link{getAny}}
#' 
#' @export
#' 
#' @author John Coene <john.coene@@cmcm.com>
findDatePreset <- function() {
  
  #list
  date_preset <- c("today", "yesterday", "last_3_days", "this_week", "last_week", "last_7_days", "last_14_days", "last_28_days", "last_30_days", "last_90_days", "this_month", "last_month", "this_quarter", "last_3_months", "lifetime")
  return(date_preset)
}