#' findActionBreakdowns
#' 
#' @description Returns all valid \code{action_breakdowns} which can be used in the get-family functions. No input, see usage.
#' 
#' @examples 
#' \dontrun{
#' # get information on account
#' info <- findInfo(account.id = "act_123456789012345", token = "XXXXXXXXXXX")
#' 
#' # take random ad.id
#' set.seed(123)
#' rand_id <- sample(info$ads$id, 1)
#' 
#' # carousel-related action_breakdowns
#' carousel <- findActionBreakdowns()[grep("carousel", findActionBreakdowns())]
#' 
#' # fetch Ad data broken down by actions related to carousel
#' data <- getAny(id = rand_id, token = "XXXXXXXXXXX", action.breakdowns = carousel)
#' }
#' 
#' @seealso \code{\link{getAny}}, \code{\link{getInfo}}
#' 
#' @export
#' 
#' @author John Coene <john.coene@@cmcmc.com>
findActionBreakdowns <- function() {
  
  # list
  action_breakdowns <- c("action_carousel_card_id", "action_carousel_card_name", "action_destination", "action_device", "action_target_id", "action_type", "action_video_type")
  
  # sort
  action_breakdowns <- sort(action_breakdowns)
  return(action_breakdowns)
}