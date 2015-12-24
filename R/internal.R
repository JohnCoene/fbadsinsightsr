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
scopeCheck <- function(scope) {
  
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

# create_fields
createFields <- function(fields = NULL){
  
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

# to_libcurl
toHTTP <- function(params = NULL){
  
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


# testParam
testParam <- function (params, param_vector) {
  
  if (params == "action_attribution_windows") {
    options <- c("1d_view", "7d_view", "28d_view", "1d_click", "7d_click", 
                 "28d_click", "default")
  } else if (params == "action_breakdowns") {
    options <- findActionBreakdowns()
  } else if (params == "fields") {
    options <- findFields()
  } else if (params == "action_report_time") {
    options <- c("impression", "conversion")
  } else if (params == "breakdowns") {
    options <- findBreakdowns()
  } else if (params == "date_preset") {
    options <- findDatePreset()
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
checkToken <- function(token){
  
  # check token class
  if (class(token)[1]=="Token2.0"){
    token <- token$credentials$access_token
  }	else if (class(token)[1]=="character"){
    token <- token
  }
  
  return(token)
}

# breakdowns
buildBreakdowns <- function(breakdowns) {
  # breakdowns
  if (length(breakdowns) >= 1 & length(breakdowns) <= 2) {
    
    # test
    testParam("breakdowns", breakdowns)
    
    if(length(breakdowns) == 2 && breakdowns == c("age", "gender")) {
      breakdowns <- paste0("&breakdowns=", toHTTP(breakdowns))
    } else if (length(breakdowns) == 2 && breakdowns == c("impression_device", "placement")) {
      breakdowns <- paste0("&breakdowns=", toHTTP(breakdowns))
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
parseJSON <- function(json) {
  
  df <- do.call(plyr::"rbind.fill", lapply(json$data, as.data.frame))
  
  return(df)
}


paginate <- function(json, data, verbose = FALSE, n = 100) {
  
  i <- 1
  
  # Paginate
  while (nrow(data) < n && 
         !is.null(json$paging$`next`)) {
    # GET
    response <- httr::GET(json$paging$`next`)
    
    # parse
    json <- rjson::fromJSON(rawToChar(response$content))
    
    # bind
    data <- plyr::rbind.fill(data, parseJSON(json))
    
    # verbose
    if (verbose == TRUE && i == 1) {
      cat(paste0(n, " results requested", "\n"))
      cat(paste(nrow(data), "results"), fill = TRUE, labels = paste0("Query #", i, ":"))
    } else if (verbose == TRUE && i != 1) {
      cat(paste(nrow(data), "results"), fill = TRUE, labels = paste0("Query #", i, ":"))
    }
    
    # pause between queries if more than 2 pages of data to avoid lengthy calls
    if(i >= 3) {
      Sys.sleep(3)
      
      # verbose
      if(verbose == TRUE && i == 3){
        cat(paste("## 3 seconds pause between queries from now onwards ##", "\n"))
      }
    }
    
    # iterate
    i <- i + 1
  }
  
  return(data)
}


simplifyDataframe <- function(data){
  
  # check input
  if(class(data) != "data.frame"){
    stop("data must be data.frame")
  }
  
  # go throug heach column
  for (i in 1:ncol(data)){
    # test if name includes
    test <- names(data)[i][grep("action_type", names(data)[i])]
    
    # test value
    value <- names(data)[i + 1][grep("value", names(data)[i + 1])]
    
    # if test positive & "value" found
    if (length(value)){
      # remove "action_type.action_type"
      name <- gsub("action_type.", "", test)
      
      # remove numbers
      name <- gsub(".[1-9]", "", name)
      
      # remove 
      action_type <- data[,i][which(!is.na(data[,i]))]
      
      # if action_type is unique re-frame
      if(length(action_type) == 1){
        # rename columns
        names(data)[i + 1] <- paste0(name, "_", action_type)
        
        # remove colum
        data[,i] <- NULL
      }
      
    }
    test <- NULL
    value <- NULL
  }
  
  return (data)
}