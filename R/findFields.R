#' Find valid fields
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
#' \item \code{\link{getActivity}}
#' \item \code{\link{getCreative}}
#' \item \code{\link{grabCampaigns}}
#' \item \code{\link{grabAdsets}}
#' \item \code{\link{grabAds}}
#' \item \code{\link{checkStatus}}
#' \item \code{\link{listAccounts}}
#' \item \code{\link{listCreatives}}
#' \item \code{\link{listVideos}}
#' \item \code{\link{listImages}}
#' \item \code{\link{listBroadTargeting}}
#' \item \code{\link{getLabCampaigns}}
#' \item \code{\link{getLabAdsets}}
#' \item \code{\link{getLabAds}}
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
#' @author John Coene \email{john.coene@@cmcm.com}
findFields <- function(fct ="getAny") {
  
  # assign value to fct if missing
  if(missing(fct)){
    fct <- "getAny"
  }
  
  if(fct == "getAny" || fct == "getAccount" || fct == "getCampaign" ||
     fct == "getAdset" || fct == "getAd") {
    
    #list
    fields <- c("date_start", "date_stop", "account_id", "account_name", "ad_id",
                "ad_name", "buying_type", "campaign_id", "campaign_name", 
                "adset_id", "adset_name", "action_carousel_card_id", 
                "action_carousel_card_name", "actions", "unique_actions",
                "total_actions", "total_unique_actions", "action_values", 
                "total_action_value", "impressions", "social_impressions",
                "social_clicks", "unique_impressions", 
                "unique_social_impressions", "clicks",
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
    
  } else if (fct == "checkStatus"){
    
    #build fields
    fields <- c("effective_status", "configured_status", "created_time",
                "name", "account_id")
    
  } else if (fct == "listCreatives" || fct == "grabCreatives") {
    
    fields <- c("id", "adlabels", "body", "call_to_action_type", "image_hash",
                "image_url", "instagram_actor_id", "instagram_permalink_url",
                "instagram_story_id", "link_url", "name", "object_id", 
                "object_url", "object_story_id", "object_type", 
                "product_set_id", "run_status", "template_url", 
                "thumbnail_url", "title", "url_tags", "applink_treatment")
    
  } else if (fct == "listAccounts" || fct == "grabAccounts") {
    
    fields <- c("id", "account_groups", "account_id", "account_status",
                "age", "agency_client_declaration", "business_city",
                "business_country_code", "business_name", "business_state",
                "business_street", "business_street2", "business_zip",
                "capabilities", "created_time", "currency", "disable_reason",
                "end_advertiser", "end_advertiser_name",
                "failed_delivery_checks", "funding_source",
                "funding_source_details", "has_migrated_permissions",
                "io_number", "is_notifications_enabled",
                "is_personal", "is_prepay_account", "is_tax_id_required",
                "line_numbers", "media_agency", "min_campaign_group_spend_cap",
                "min_daily_budget", "name", "owner", 
                "offsite_pixels_tos_accepted", "partner", "tax_id",
                "tax_id_status", "tax_id_type", "timezone_id", "timezone_name",
                "timezone_offset_hours_utc", "rf_spec", "tos_accepted",
                "user_role", "vertical_name", "amount_spent", "spend_cap",
                "balance", "business", "owner_business", "last_used_time",
                "asset_score")
    
  } else if (fct == "getActivity") {
    
    fields <- c("actor_id", "actor_name", "application_id", "application_name", 
                "date_time_in_timezone", "event_time", "event_type",
                "object_id", "object_name", "translated_event_type", 
                "extra_data")
    
  } else if (fct == "listImages") {
    
    fields <- c("account_id", "created_time", "creatives", "hash", "height",
                "id", "name", "original_height", "original_width")
    
  } else if (fct == "listVideos") {
    
    fields <- c("backdated_time", "backdated_time_granularity", 
                "content_category", "created_time", "description", "embed_html", 
                "emeddable", "event", "format")
    
  } else if(fct == "listBroadTargeting") {
    
    fields <- c("category_description", "id", "name", "parent_category", "size", 
                "source", "type", "type_name")
    
  } else if (fct == "getCreative") {
    
    fields <- c("message", "caption", "name", "created_time", "description", 
                "is_popular", "is_instagram_eligible", "is_hidden", 
                "is_expired", "is_app_share", "instagram_eligibility", 
                "promotion_status", "status_type", "is_spherical", 
                "subscribed", "timeline_visibility", "type", "updated_time", 
                "full_picture", "icon", "is_published", "id")
    
  } else if(fct == "getLabCampaigns" || fct == "getLabAdsets" || 
            fct == "getLabAds") {
    
    fields <- c("account_id", "adlabels", "buying_type", "configured_status", 
                "created_time", "effective_status", "id", "name", "objective",
                "can_use_spend_cap")
    
  } else {
    
    stop("wrong fct argument. See details.")
    
  }
  
  # sort
  fields <- sort(fields)
  return(fields)
}