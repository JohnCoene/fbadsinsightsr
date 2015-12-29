#' findBreakdowns
#' 
#' @description Returns all valid \code{breakdwowns} which can be used in the get-family functions.. No input, see usage.
#' 
#' @examples 
#' \dontrun{
#' # get information on account
#' info <- findInfo(account.id = "act_123456789012345", token = "XXXXXXXXXXX")
#' 
#' # take random adset.id
#' set.seed(123)
#' rand_id <- sample(info$adset$id, 1)
#' 
#' # fetch Adset data broken down by age and gender
#' data <- getAny(id = rand_id, token = "XXXXXXXXXXX", 
#'                breakdowns = c("age", "gender"))
#' }
#' 
#' @seealso \code{\link{getAny}}, \code{\link{findInfo}}, \code{\link{findDatePreset}}
#' 
#' @export
#' 
#' @author John Coene <john.coene@@cmcmc.com>
findBreakdowns <- function() {
  
  # list
  breakdowns <- c("age", "country", "gender", "frequency_value", "hourly_stats_aggregated_by_advertiser_time_zone", "hourly_stats_aggregated_by_audience_time_zone", "impression_device", "place_page_id", "placement", "placement_merge_rhc", "product_id", "region")
  
  # sort
  breakdowns <- sort(breakdowns)
  
  return(breakdowns)
}