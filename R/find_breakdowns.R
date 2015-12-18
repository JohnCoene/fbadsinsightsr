#' find_breakdowns
#' 
#' @description Returns all valid \code{breakdwowns} which can be used in the get-family functions.. No input, see usage.
#' 
#' @seealso \code{\link{get_account}}
#' 
#' @export
#' 
#' @author John Coene <john.coene@@cmcmc.com>
find_breakdowns <- function() {
  breakdowns <- c("age", "country", "gender", "frequency_value", "hourly_stats_aggregated_by_advertiser_time_zone", "hourly_stats_aggregated_by_audience_time_zone", "impression_device", "place_page_id", "placement", "placement_merge_rhc", "product_id", "region")
  return(breakdowns)
}