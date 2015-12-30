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

paginate <- function(json, data, verbose = FALSE, n = 100) {
  
  i <- 1
  
  # Paginate
  while (nrow(data) < n && 
         !is.null(json$paging$`next`)) {
    # GET
    response <- httr::GET(json$paging$`next`)
    
    # get json
    json <- rjson::fromJSON(rawToChar(response$content))
    
    # bind
    data <- plyr::rbind.fill(data, toDF(response))
    
    # verbose
    if (verbose == TRUE && i == 1) {
      cat(paste0(n, " results requested", "\n"))
      cat(paste(nrow(data), "results"), fill = TRUE, labels = paste0("Query #", i, ":"))
    } else if (verbose == TRUE && i != 1) {
      cat(paste(nrow(data), "results"), fill = TRUE, labels = paste0("Query #", i, ":"))
    }
    
    # pause between queries if more than 2 pages of data to avoid lengthy calls
    if(i >= 3) {
      Sys.sleep(0.5)
      
      # verbose
      if(verbose == TRUE && i == 3){
        cat(paste("## half second pause between queries from now onwards ##", "\n"))
      }
    }
    
    # iterate
    i <- i + 1
  }
  
  return(data)
}

toDF <- function(response){
  
  # parse json to list
  json <- rjson::fromJSON(rawToChar(response$content))
  
  # check if data present in JSON
  if(length(json$data)){
    
    # extract names
    # find which nested list has largest number of variables
    lg <- vector()
    
    # loop through lists
    for(i in 1:length(json$data)){
      
      # get length
      lg[i] <- length(json$data[[i]])
      
      # identify longest (that's what she said)
      j <- which.max(lg)
    }
    
    # use variable names of largest list
    names <- names(json$data[[j[1]]])
    
    # identify nested lists
    vars <- names[grep("^actions$|^unique_actions$|^cost_per_action_type$|^cost_per_unique_action_type$|^website_ctr$",
                       names)]
    
    # loop through vars to remove from json
    json2 <- json
    
    # remove vars from json2
    for(i in 1:length(vars)){
      for(j in 1:length(json$data)){
        json2$data[[j]][which(names(json2$data[[j]]) == vars[i])] <- NULL
      }
    }
    
    # json2 to data.frame
    base_df <- do.call(plyr::"rbind.fill", lapply(json2$data, as.data.frame))
    
    # declare row_df
    row_df <- data.frame()
    
    # check if vars observed
    if(length(vars)){
      # rebuild json
      for(i in 1:length(vars)){
        for(j in 1:length(json$data)){
          lst <- json$data[[j]][which(names(json$data[[j]]) == vars[i])]
          
          # check if variable has been found
          if(length(lst)) {
            # sublist to dataframe
            dat <- do.call(plyr::"rbind.fill",
                           lapply(lst[[1]], as.data.frame))
            
            # transpose
            # name rows
            rownames(dat) <- dat[,1]
            
            # remove first column
            dat[,1] <- NULL
            
            # transpose
            dat <- as.data.frame(t(dat))
            
            # rename
            names(dat) <- paste0(vars[i], "_", names(dat))
            
          } else { # if no lst found
            row_df <- rbind.data.frame(rep(NA, ncol(dat)))
            names(row_df) <- names(dat)
          }
          
          # bind
          row_df <- plyr::rbind.fill(row_df, dat)
        }
        
        base_df <- cbind.data.frame(base_df, row_df)
        row_df <- NULL
      }
    }
    
    
    # if no data in json
  } else if (length(json$data) <= 0) {
    base_df <- NULL
  }
  
  return(base_df)
}

# account status sort
accountStatus <- function(data) {
  
  # build status ref
  statuses <- data.frame(id = c(1, 2, 3, 7, 9, 100, 101, 102, 201, 202),
                         status = c("active", "disabled", "unsettled",
                                    "pending risk review", "in grace period",
                                    "pending closure", "closed", 
                                    "pending settlement", "any active",
                                    "any close"))
  
  # merge
  dat <- merge(data, statuses, by.x = "account_status", 
               by.y = "id", all.x = TRUE)
  
  # remove unwanted columns
  dat$account_status <- NULL
  
  return(dat)
}