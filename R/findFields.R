#' findFields
#' 
#' @description Returns all valid \code{fields} which can be used in 
#' the GET-family functions.
#' See details below for more information.
#' 
#' @param fct Name of the function you need the fields for
#' 
#' @details fields vary from \code{date} to \code{cpm}
#' Applies to the following functions (\code{fct} argument, i.e.: 
#' \code{fct = "whatStatus"})
#' \itemize{
#' \item \code{\link{getAny}}
#' \item \code{\link{getAccount}}
#' \item \code{\link{getAdset}}
#' \item \code{\link{getAd}}
#' \item \code{\link{getImage}}
#' \item \code{\link{getCreative}}
#' \item \code{\link{grabCampaigns}}
#' \item \code{\link{grabAdsets}}
#' \item \code{\link{grabAds}}
#' \item \code{\link{whatStatus}}
#' }
#' 
#' @examples 
#' \dontrun{
#' # video-related metrics
#' videos <- findFields("getAccount")[grep("video", findFields("getAccount"))]
#' 
#' # get account data on fields of interest
#' data <- getAccount(account.id = "act_123456789012345", token = "XXXXXXXXXXXX", 
#'                    fields = videos)
#' }
#' 
#' @seealso \code{\link{getAccount}}
#' 
#' @export
#' 
#' @author John Coene <john.coene@@cmcm.com>
findFields <- function(fct ="getAny") {
  
  # assign value to fct if missing
  if(missing(fct)){
    fct <- "getAny"
  }
  
  if(fct == "getAny" || fct == "getAccount" || fct == "getCampaign" ||
     fct == "getAdset" || fct == "getAds") {
    
    #list
    fields <- c("date_start", "date_stop", "account_id", "account_name", "ad_id",
                "ad_name", "buying_type", "campaign_id", "campaign_name", 
                "adset_id", "adset_name", "action_carousel_card_id", 
                "action_carousel_card_name", "actions", "unique_actions",
                "total_actions", "total_unique_actions", "action_values", 
                "total_action_value", "impressions", "social_impressions",
                "social_clicks", "unique_impressions", 
                "unique_social_impressions",
                "unique_clicks", "unique_social_clicks", "spend", "frequency", 
                "social_spend", "deeplink_clicks", "app_store_clicks", 
                "website_clicks", "cost_per_inline_post_engagement", 
                "inline_link_clicks", "cost_per_inline_link_click", 
                "inline_post_engagement", "call_to_action_clicks", 
                "newsfeed_avg_position", "newsfeed_impressions", 
                "newsfeed_clicks", "reach", "social_reach", "ctr",
                "unique_ctr", "unique_link_clicks_ctr", "cpm", "cpp",
                "cost_per_total_action", "cost_per_action_type", 
                "cost_per_unique_click", "cost_per_10_sec_video_view", 
                "cost_per_unique_action_type", "relevance_score", "website_ctr",
                "video_avg_sec_watched_actions", "video_avg_pct_watched_actions",
                "video_p25_watched_actions", "video_p50_watched_actions", 
                "video_p75_watched_actions", "video_p95_watched_actions", 
                "video_p100_watched_actions", "video_complete_watched_actions",
                "video_10_sec_watched_actions", "video_15_sec_watched_actions",
                "video_30_sec_watched_actions", "estimated_ad_recallers",
                "estimated_ad_recallers_lower_bound", 
                "estimated_ad_recallers_upper_bound", 
                "estimated_ad_recall_rate", 
                "estimated_ad_recall_rate_lower_bound", 
                "estimated_ad_recall_rate_upper_bound", 
                "cost_per_estimated_ad_recallers", "place_page_name")
    
  } else if(fct == "grabCampaigns" || fct == "grabAdsets" || 
            fct == "grabAds"){
    fields <- c("id", "name", 
                "account_id", "adset", "adset_id", "adlabels",
                "bid_amount", "bid_info", "bid_type", "configured_status",
                "effective_status", "created_time", "update_time",
                "creative", "campaign_id")
    
  } else if (fct == "whatStatus"){
    
    #build fields
    fields <- c("effective_status", "configured_status", "created_time",
                "name", "account_id")
  } else if (fct == "getImage") {
    fields <- c("id", "name", "account_id", "created_time", "creatives",
                "hash", "height", "width", "original_height", "original_width",
                "permalink_url", "status", "updated_time", "url", "url128")
  } else if (fct == "getCreative") {
    fields <- c("id", "adlabels", "body", "call_to_action_type", "image_hash",
                "image_url", "instagram_actor_id", "instagram_permalink_url",
                "instagram_story_id", "link_url", "name", "object_id", 
                "object_url", "object_story_id", "object_type", 
                "product_set_id", "run_status", "template_url", 
                "thumbnail_url", "title", "url_tags", "applink_treatment")
  } else {
    stop("wrong fct argument. See details.")
  }
  
  # sort
  fields <- sort(fields)
  return(fields)
}