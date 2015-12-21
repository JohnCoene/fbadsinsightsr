#' findFields
#' 
#' @description Returns all 73 valid \code{fields} which can be used in the get-family functions. No input, see details for more information.
#' 
#' @details fields vary from \code{age} to \code{cpm} for the full list please see the online official \href{https://developers.facebook.com/docs/marketing-api/reference/ads-insights/}{documentation}
#' 
#' @seealso \code{\link{getAny}}
#' 
#' @export
#' 
#' @author John Coene <john.coene@@cmcmc.com>
findFields <- function() {
  
  #list
  fields <- c("date_start", "date_stop", "account_id", "account_name", "ad_id",
              "ad_name", "buying_type", "campaign_id", "campaign_name", 
              "adset_id", "adset_name", "action_carousel_card_id", 
              "action_carousel_card_name", "actions", "unique_actions",
              "total_actions", "total_unique_actions", "action_values", 
              "total_action_value", "impressions", "social_impressions",
              "social_clicks", "unique_impressions", "unique_social_impressions",
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
              "estimated_ad_recallers_upper_bound", "estimated_ad_recall_rate", 
              "estimated_ad_recall_rate_lower_bound", 
              "estimated_ad_recall_rate_upper_bound", 
              "cost_per_estimated_ad_recallers", "place_page_name")
  
  # sort
  fields <- sort(fields)
  return(fields)
}