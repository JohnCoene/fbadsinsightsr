#' find_date_preset
#' 
#' @description Returns all valid values for \code{date_preset} which can be used in the get-family functions. No input, see usage.
#' 
#' @seealso \code{\link{get_account}}
#' 
#' @export
#' 
#' @author John Coene <john.coene@@cmcmc.com>
find_date_preset <- function() {
  date_preset <- c("today", "yesterday", "last_3_days", "this_week", "last_week", "last_7_days", "last_14_days", "last_28_days", "last_30_days", "last_90_days", "this_month", "last_month", "this_quarter", "last_3_months", "lifetime")
  return(date_preset)
}