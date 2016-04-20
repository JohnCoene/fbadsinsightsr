#' Find valid parameters
#' 
#' @description Helper function to check valid values for parameters of 
#' GET-family functions (i.e.: \code{\link{getAny}}).
#' 
#' @param params parameter(s) to check
#' 
#' @details 
#' Function applies to parameters of the following GET-family functions 
#' (i.e.: \code{\link{getAny}}) parameters:
#' \itemize{
#' \item \code{level}
#' \item \code{breakdowns}
#' \item \code{date.preset}
#' \item \code{time.increment}
#' \item \code{ation.breakdowns}
#' \item \code{action.report.time}
#' \item \code{action.attribution.windows}
#' \item \code{subtype}
#' }
#' 
#' @return If one \code{params} is passed the function returns a 
#' \code{vector} otherwise a \code{list} is returned.
#' 
#' @examples 
#' \dontrun{
#' 
#' # check all valid fields
#' parameters <- findParams()
#' 
#' # check valid breakdowns
#' val_br <- findParams("breakdowns")
#' 
#' # use parameter in GET-family function
#' dat <- getAny(id = "act_123456789012345", token = "XXXXXXXXXXXXX",
#'               breakdowns = val_br[1])
#' 
#' }
#' 
#' @seealso \code{\link{getAny}}
#' 
#' @author John Coene \email{john.coene@@cmcm.com}
#' 
#' @export
findParams <- function(params = c("level", "breakdowns", "date.preset",
                                  "action.breakdowns", "action.report.time",
                                  "action.attribution.windows", 
                                  "time.increment")){
  
  loop <- function(param) {
    # check input
    if(missing(param)){
      stop("missing param. See @details")
    }
    
    if(param == "action.breakdowns") {
      
      param <- c("action_carousel_card_id", 
                 "action_carousel_card_name",
                 "action_destination", "action_device", 
                 "action_target_id",
                 "action_type", "action_video_type")
      
    } else if (param == "action.attribution.windows") {
      
      param <- c("1d_view", "7d_view", "28d_view", "1d_click", "7d_click",
                 "28d_click")
      
    } else if(param == "breakdowns"){
      
      param <- c("age", "country", "gender", "frequency_value",
                 "hourly_stats_aggregated_by_advertiser_time_zone", 
                 "hourly_stats_aggregated_by_audience_time_zone",
                 "impression_device", "place_page_id", "placement",
                 "placement_merge_rhc", "product_id", "region")
      
    } else if (param == "date.preset"){
      
      param <- c("today", "yesterday", "last_3_days",
                 "this_week", "last_week", "last_7_days", 
                 "last_14_days", "last_28_days", "last_30_days",
                 "last_90_days", "this_month", "last_month", 
                 "this_quarter", "last_3_months", "lifetime")
      
    } else if (param == "action.report.time"){
      
      param <- c("impression", "conversion")
      
    } else if (param == "level"){
      
      param <- c("ad", "adset", "campaign", "account")
      
    } else if (param == "time.increment"){
      
      param <- c("monthly", "all_days")
      
    } else if (param == "subtype") {
      
      param <- c("CUSTOM", "WEBSITE", "APP", "CLAIM", "PARTNER", "MANAGED", 
                 "VIDEO", "LOOKALIKE", "ENGAGEMENT", "DATA_SET", 
                 "BAG_OF_ACCOUNTS")
      
    } else {
      
      stop("wrong param. See @details", call. = FALSE)
      
    }
    
    return(param)
  }
  
  # loop through parameters
  parameters <- lapply(params, loop)
  
  # unlist if one params passed
  if(length(parameters) == 1) {
    
    parameters <- unlist(parameters)
    
    # else name list
  } else {
    
    names(parameters) <- params
    
  }
  
  return(parameters)
}
