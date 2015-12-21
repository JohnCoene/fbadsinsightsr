#' findActionBreakdowns
#' 
#' @description Returns all valid \code{action_breakdowns} which can be used in the get-family functions. No input, see usage.
#' 
#' @seealso \code{\link{getAny}}
#' 
#' @export
#' 
#' @author John Coene <john.coene@@cmcmc.com>
findActionBreakdowns <- function() {
  action_breakdowns <- c("action_carousel_card_id", "action_carousel_card_name", "action_destination", "action_device", "action_target_id", "action_type", "action_video_type")
  return(action_breakdowns)
}