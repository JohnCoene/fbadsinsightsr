# query_API
# 
# @description queries API (GET) see \code{httr::GET}
# 
# @param url url to call
# @param token either from \href{https://developers.facebook.com/tools/explorer/}{Facebook Graph API Explorer} or from \code{fb_authenticate}
# 
# @seealso \code{\link{fb_authenticate}}
#
# @author John Coene <john.coene@cmcm.com>
# 
# @keywords internal
query_API <- function(url, token){
  if (class(token)[1]=="config"){
    url.data <- GET(url, config=token)
  }
  if (class(token)[1]=="Token2.0"){
    url.data <- GET(url, config(token=token))
  }	
  if (class(token)[1]=="character"){
    url.data <- GET(url)
  }
  if (class(token)[1]!="character" & class(token)[1]!="config" & class(token)[1]!="Token2.0"){
    stop("Error in access token. See help for details.")
  }
  content <- rjson::fromJSON(rawToChar(url.data$content))
  if (length(content$error)>0){
    stop(content$error$message)
  }	
  return(content)
}

# scope_check
# 
# @description Checks if scopes matches available facebook permissions, href{https://developers.facebook.com/docs/facebook-login/permissions}{see documentation}
# 
# @param scope A \code{vector} of scopes (permissions), used in \code{\link{fb_authenticate}}
# 
# @seealso \code{\link{fb_authenticate}}
# 
# @author John coene \email{john.coene@@cmcm.com}  
# 
# @keywords internal
scope_check <- function(scope) {
  
  # input check
  
  # define valid scopes
  scopes <- c("public_profile", "user_friends", "email", "user_about_me", 
              "user_actions.books", "user_actions.fitness", 
              "user_actions.music", "user_actions.news", "user_actions.video", 
              "user_actions:{app_namespace}", "user_birthday", 
              "user_education_history", "user_events", 
              "user_games_activity", "user_hometown", "user_likes", 
              "user_location", "user_managed_groups", "user_photos",
              "user_posts", "user_relationships", "user_relationship_details", 
              "user_religion_politics", "user_tagged_places", "user_videos",
              "user_website", "user_work_history", "read_custom_friendlists",
              "read_insights", "read_audience_network_insights", 
              "read_page_mailboxes", "manage_pages", "publish_pages",
              "publish_actions", "rsvp_event", "pages_show_list", 
              "pages_manage_cta", "ads_read", "ads_management")
  
  for (i in 1:length(scope)) {
    test <- scopes[which(scopes == scope[i])]
    if (length(test) == 0) {
      scope_error <- scope[i]
      stop (paste0("Wrong scope: ", scope_error, " is not a correct permission. See ?fb_authenticate details"))
    }
  }
}

# user_data
# 
# @description Function used to parse user data gathered from \code{\link{get_user}} function.
# 
# @inheritParams parse_data
# 
# @seealso \code{\link{get_user}}
# 
# @author John coene \email{john.coene@@cmcm.com}
# 
# @keywords internal
user_data <- function(response) {
  
  # unlist 
  df <- as.data.frame(unlist(response$data),
                      stringsAsFactors = FALSE)
  
  # rename
  names(df) <- c("info")
  
  for (i in 1:nrow(df)) {
    if(df$info[i] == "1") {
      df$info[i] <- "ACCOUNT_ADMIN"
    } else if (df$info[i] == "2") {
      df$info[i] <- "ADMANAGER_READ"
    } else if (df$info[i] == "3") {
      df$info[i] <- "ADMANAGER_WRITE"
    }else if (df$info[i] == "4") {
      df$info[i] <- "BILLING_READ"
    } else if (df$info[i] == "5") {
      df$info[i] <- "ADMANAGER_WRITE"
    } else if (df$info[i] == "7") {
      df$info[i] <- "REPORTS"
    } else if (df$info[i] == "9" | df$info[i] == "10") {
      df$info[i] <- "OTHER"
    } else if (df$info[i] == "1001") {
      df$info[i] <- "Administrator access"
    } else if (df$info[i] == "1002") {
      df$info[i] <- "Advertiser (ad manager) access"
    } else if (df$info[i] == "1003") {
      df$info[i] <- "Analyst access"
    } else if (df$info[i] == "1004") {
      df$info[i] <- "Direct sales access"
    } 
  }
  return(df)
}

# parse_data
# 
# @description parses data 
# 
# @param response object of class 'response'
# 
# @details Uses methods similar to that used when parsing JSON files. 
# Tough responses are technically of class \code{list} these lists are originally parsed from JSON responses using \code{rjson} package.
# Please \code{\link{query_API}} function for more information on the JSON parse.
# 
# @seealso \code{\link{query_API}}
# 
# @author John coene \email{john.coene@@cmcm.com}
# 
# @keywords internal
parse_data <- function(response){
  response <- lapply(response$data, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  data <- do.call("rbind", response)
  
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  
  return (data)
}

# create_fields
create_fields <- function(fields = NULL){
  
  # check input presence
  if(is.null(fields)) {
    fields <- ""
  }
  
  # check input class
  if (class(fields) == "character") {
    
  } else {
    stop("fields must be character value or vector")
  }
  
  # add %2C
  for (i in 1:length(fields)) {
    if (i != length(fields)) {
      fields[i] <- paste0(fields[i], "%2C")
    } 
  }
  
  # collapse
  fields <- paste(fields, sep="", collapse = "")
  
  return(fields)
}


# build url
build_url <- function(root_node, edge, fields, params, token) {
  base_url <- "https://graph.facebook.com/v2.5/"
  
  fields <- create_fields(fields)
  params <- create_params(params)
  
  url <- paste0(base_url, root_node, "/", edge, "?fields=",fields,
                params, "access_token=", token)
  
  return(url)
}

# to_libcurl
to_libcrul <- function(params = NULL){
  
  if (length(params) == 0) {
    
    params <- ""
    
  } else {
    
    # add %22 at either end of each lst element
    params <- paste0("%22", params, "%22")
    
    for (i in 1:length(params)) {
      if (i != length(params)) {
        # add %2C BETWEEN lst element
        params[i] <- paste0(params[i], "%2C%20")
      } 
    }
    
    # collapse
    params <- paste(params, sep="", collapse = "")
    
    # add brackets
    params <- paste0("[", params, "]")
    
  }
  
  return (params)

}


# test_param
test_param <- function (params, param_vector) {
  
  if (params == "action_attribution_windows") {
    options <- c("1d_view", "7d_view", "28d_view", "1d_click", "7d_click", 
                 "28d_click", "default")
  } else if (params == "action_breakdowns") {
    options <- find_action_breakdowns()
  } else if (params == "fields") {
    options <- find_fields()
  } else if (params == "action_report_time") {
    options <- c("impression", "conversion")
  } else if (params == "breakdowns") {
    options <- find_breakdowns()
  } else if (params == "date_preset") {
    options <- find_date_preset()
  } else if (params == "level") {
    options <- c("ad", "adset", "campaign", "account") 
  } else if (params == "time_increment") {
    options <- c("monthly", "all_days")
  }
  
  for (i in 1:length(param_vector)) {
    test <- options[which(options == param_vector[i])]
    if (length(test) == 0) {
      param_vector_error <- param_vector[i]
      
      # collapse options to print
      options_print <- paste(options, sep = ",", collapse = ", ")
      
      # print error
      stop (paste0("Wrong ", params, " parameter specified '", param_vector_error, "'", " is not valid. All valid values are: ", options_print))
    }
  }

}

# check_token
#  
check_token <- function(token){
  
  # check token class
  if (class(token)[1]=="Token2.0"){
    token <- oauth$credentials$access_token
  }	else if (class(token)[1]=="character"){
    token <- token
  }
  
  return(token)
}

# breakdowns
build_breakdowns <- function(breakdowns) {
  # breakdowns
  if (length(breakdowns) >= 1 & length(breakdowns) <= 2) {
    
    # test
    test_param("breakdowns", breakdowns)
    
    if(length(breakdowns) == 2 && breakdowns == c("age", "gender")) {
      breakdowns <- paste0("&action_breakdowns=", to_libcrul(breakdowns))
    } else if (length(breakdowns) == 2 && breakdowns == c("impression_device", "placement")) {
      breakdowns <- paste0("&action_breakdowns=", to_libcrul(breakdowns))
    } else if (length(breakdowns) == 1 && breakdowns == "impression_device") {
      stop("impression_device cannot be used on its own")
    } else if (length(breakdowns) == 1) {
      breakdowns <- paste0("&breakdowns=", breakdowns)
    } else {
      stop("Wrong breakdowns specified. See @param")
    }
    
  } else if (length(breakdowns) >= 3) {
    stop("Too many breakdowns specified. See @param")
  }
  
  return(breakdowns)
  
}


# parse_json
parse_json <- function(json) {
  list <- json$data
  
  df <- data.frame()
  
  # build rows
  for(k in 1:length(list)){
    col <- data.frame(row.names = 1)
    # build columns
    for (i in 1:length(list[[k]])) {
      if(length(list[[k]][[i]]) == 1) {
        vect <- unlist(list[[k]][i])[1]
        vect <- as.data.frame(vect)
        names(vect) <- names(list[[k]][i])
        col <- cbind.data.frame(col, vect)
      } else if (length(list[[k]][[i]]) > 1){
        for (j in 1:length(list[[k]][[i]])){
          vect <- unlist(list[[k]][[i]][j])[2]
          vect <- as.data.frame(vect)
          names(vect) <- unlist(list[[k]][[i]][j])[1]
          col <- cbind.data.frame(col, vect)
        }
      }
    }
    
    # combine
    df <- plyr::rbind.fill(df, col)
    col <- NULL
  }
  
  return(df)
}

                                                                                                                                    