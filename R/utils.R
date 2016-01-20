# startUpMessage -------------------------------
.onAttach <- function(libname, pkgname = "fbAdsInsightsR") {
  packageStartupMessage("Welcome to the fbAdsInsightsR package.", "\n\n",
                        "Please contact a contributor",
                        " if you encounter an issue.", "\n",
                        "All documentation is openly available at: ",
                        "bitbucket.org/JohnCheetah/fbadsinsightsrdocs/src")
}

# scope check -------------------------------
scopeCheck <- function(scope) {
  
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
      stop (paste0("Wrong scope: ", scope_error,
                   " is not a correct permission. See ?fb_authenticate details"),
            call. = FALSE)
    }
  }
}

# create_fields -------------------------------
createFields <- function(fields){
  
  # check input class
  if (class(fields) != "character") {
    stop("fields must be character value or vector", call. = FALSE)
  }
  
  # collapse
  fields <- paste0(fields,  collapse = "%2C")
  
  return(fields)
}

# to_http -------------------------------
toHTTP <- function(params = NULL){
  
  if (!length(params)) {
    
    params <- ""
    
  } else {
    
    params <- paste0("[",paste0("%22", params, "%22", collapse = "%2C%20"),
                     "]", collapse = "")
    
  }
  
  return (params)
  
}

# testParam -------------------------------
testParam <- function (params, param_vector, fct) {
  
  # set default
  if(missing(fct)){
    fct <- "getAny"
  }
  
  if (params == "action_attribution_windows") {
    options <- findParams("action.attribution.windows")
  } else if (params == "action_breakdowns") {
    options <- findParams("action.breakdowns")
  } else if (params == "fields") {
    options <- findFields(fct)
  } else if (params == "action_report_time") {
    options <- c("impression", "conversion")
  } else if (params == "breakdowns") {
    options <- findParams("breakdowns")
  } else if (params == "date_preset") {
    options <- findParams("date.preset")
  } else if (params == "level") {
    options <- c("ad", "adset", "campaign", "account") 
  } else if (params == "time_increment") {
    options <- findParams("time.increment")
  }
  
  for (i in 1:length(param_vector)) {
    test <- options[which(options == param_vector[i])]
    if (length(test) == 0) {
      param_vector_error <- param_vector[i]
      
      # collapse options to print
      options_print <- paste(options, collapse = ", ")
      
      # print error
      stop (paste0("Wrong ", params, " parameter specified '", 
                   param_vector_error, "'", 
                   " is not valid. See find-family functions",
                   " findFields() or findParams() respectively.",
                   " All valid values are: ", options_print),
            call. = FALSE)
    }
  }
  
}

# check_token -------------------------------
checkToken <- function(token){
  
  # check token class
  if (class(token)[1]=="Token2.0"){
    token <- token$credentials$access_token
  }	else if (class(token)[1]=="character"){
    token <- token
  }
  
  return(token)
}

# breakdowns -------------------------------
buildBreakdowns <- function(breakdowns) {
  # breakdowns
  if (length(breakdowns) >= 1 & length(breakdowns) <= 2) {
    
    # test
    testParam("breakdowns", breakdowns)
    
    if(length(breakdowns) == 2 && breakdowns == c("age", "gender")) {
      breakdowns <- paste0("&breakdowns=", toHTTP(breakdowns))
    } else if (length(breakdowns) == 2 && breakdowns == c("impression_device", 
                                                          "placement")) {
      breakdowns <- paste0("&breakdowns=", toHTTP(breakdowns))
    } else if (length(breakdowns) == 1 && breakdowns == "impression_device") {
      stop("impression_device cannot be used on its own", call. = FALSE)
    } else if (length(breakdowns) == 1) {
      breakdowns <- paste0("&breakdowns=", breakdowns)
    } else {
      stop("Wrong breakdowns specified. Run findParams('breakdowns')", call. = FALSE)
    }
    
  } else if (length(breakdowns) >= 3) {
    stop("Too many breakdowns specified. See @param", call. = FALSE)
  }
  
  return(breakdowns)
  
}

# generic ------------------------------
bindPages <- function(base, page) UseMethod("bindPages")

bindPages.fbAdsData <- function(base, page){
  
  # get tables
  df_names <- names(base)[grep("^data$|^insights$", names(base))]
  
  # append
  for(i in 1:length(df_names)){
    base[[df_names[i]]] <- plyr::rbind.fill(base[[df_names[i]]], 
                                            page[[df_names[i]]])
  }
  
  if (length(page$url)) { 
    base$url <- page$url 
  } else if (!length(page$url)) {
      base$url <- NULL
    }
  
  return(base)
}

# generic -------------------------------
paginate <- function(fbData, verbose, n) UseMethod("paginate")

paginate.fbAdsData <- function(fbData, verbose = FALSE, n = 100) {
  
  # get n
  i <- 1
  
  # verbose
  if(verbose == TRUE && nrow(fbData$data) > 0){
    cat(paste0(nrow(fbData$data), " results"), fill = TRUE, 
        labels = paste0("Query #", i))
  }
  
  # loop if url is present and
  while(nrow(fbData$data) < n && length(fbData$url)){
    
    # call next page
    response <- httr::GET(fbData$url)
    
    # construct repsonse
    fbDataPage <- constructFbAdsData(response)
    
    # digest new page
    fbDataPage <- digest(fbDataPage)
    
    # bind
    fbData <- bindPages(fbData, fbDataPage)
    
    # verbose
    if(verbose == TRUE){
      cat(paste0(nrow(fbData$data), " results"), fill = TRUE, 
          labels = paste0("Query #", i + 1))
    }
    
    # sleep 0.5 second between queries
    Sys.sleep(0.5)
    
    i <- i + 1
  }
  
  return(fbData)
}

# account status sort -------------------------------
accountStatus <- function(data) {
  
  # build status reference table for ;ater match
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

# constructor -------------------------------
constructFbAdsData <- function(response){
  
  # check inputs
  if(class(response) != "response") stop("input must be response",
                                         call. = FALSE)
  
  # parse to list
  json <- rjson::fromJSON(rawToChar(response$content))
  
  # check if data has been returned
  # check if query successful 
  if(length(json$error$message)){
    stop(paste("likely due to id or token. Error Message returned by API: ",
               json$error$message), call. = FALSE)
  } else if (!length(json$data)) {
    warning(paste("No data."), call. = FALSE)
    
    # make empty object to return and avoid error
    structure(list(data = list()), class = "fbAdsData")
    
  } else if(length(json$data)) {
    
    # identify if insights present
    json_vars <- subAdsDataNames(sub.ads.data = json)
    
    if (length(json_vars[grep("insights", json_vars)])){
      
      # declare list to append in loop
      insights_lst <- list()
      
      # extract insights from json and remove from original json
      for(i in 1:length(json$data)){
        
        # extract insights
        insights_lst[[i]] <- json$data[[i]]$insights$data[[1]]
        
        # remove from initial json
        json$data[[i]]$insights <- NULL
      }
      
      # name list data for toDF formula
      ins_json <- list(data = insights_lst)
      
      # build class object depending on uri
      if(length(json$paging$`next`)) {
        # build class object
        structure(list(data = json, insights = ins_json,
                       url = json$paging$`next`),
                  class = "fbAdsData")
      } else {
        
        # build
        structure(list(data = json, insights = ins_json),
                  class = "fbAdsData")
      }
      
    } else if (length(names(json)[grep("summary", names(json))])) {
      
      # name list data for toDF formula
      sum_json <- list(data = list(json$summary))
      
      # json data
      json$summary <- NULL
      
      # build class object depending on uri
      if(length(json$paging$`next`)) {
        # build class object
        structure(list(data = json, summary = json,
                       url = json$paging$`next`),
                  class = "fbAdsData")
      } else {
        
        # build
        structure(list(data = json, summary = sum_json),
                  class = "fbAdsData")
      }
      
    } else {
      
      # build class object depending on uri
      if(length(json$paging$`next`)) {
        
        # build class object
        structure(list(data = json, url = json$paging$`next`),
                  class = "fbAdsData")
        
      } else {
        
        # build
        structure(list(data = json), class = "fbAdsData")
        
      }
    }
  }
}

# json names -------------------------------
subAdsDataNames <- function(sub.ads.data){
  
  # find which nested list has largest number of variables
  n_vect <- vector()
  
  # loop through lists
  for(i in 1:length(sub.ads.data$data)){
    
    # get names
    n_vect <- append(n_vect, names(sub.ads.data$data[[i]]))
    
  }
  
  # use variable names of largest list
  names <- unique(n_vect)
  
  return(names)
}

# generic method to parse data ==============================
digest <- function(x) UseMethod("digest")

# digest fbAdsData -------------------------------
digest.fbAdsData <- function(fbAdsData){
  
  # initialise k 
  k = 1
  
  while (k <= length(fbAdsData)){
    
    # check fields
    if (names(fbAdsData)[[k]] == "url"){
      url <- fbAdsData$url
    } else if (names(fbAdsData)[[k]] == "data" || 
               names(fbAdsData)[[k]] == "insights" ||
               names(fbAdsData)[[k]] == "summary"){
      
      # check if data present in fbAdsData[[k]]
      if(length(fbAdsData[[k]]$data)){
        
        # extract names
        names <- subAdsDataNames(fbAdsData[[k]])
        
        # pattern to look for
        pat <- paste0("^actions$|^unique_actions$|^cost_per_action_type$|",
                      "^cost_per_unique_action_type$|^website_ctr$|",
                      "^cost_per_10_sec_video_view$|",
                      "^video_avg_sec_watched_actions$|",
                      "^video_avg_pct_watched_actions$|",
                      "^video_p25_watched_actions$|",
                      "^video_p50_watched_actions$|",
                      "^video_p75_watched_actions$|",
                      "^video_p95_watched_actions$|",
                      "^video_p100_watched_actions$|",
                      "^video_complete_watched_actions$|",
                      "^video_10_sec_watched_actions$|",
                      "^video_15_sec_watched_actions$|",
                      "^video_30_sec_watched_actions")
        
        # identify nested lists
        vars <- names[grep(pat, names)]
        
        # loop through vars to remove from fbAdsData[[k]]
        fbData2 <- fbAdsData[[k]]
        
        # if vars found extract
        if(length(vars)) {
          
          # remove vars from fbData2
          for(i in 1:length(vars)){
            for(j in 1:length(fbData2$data)){
              fbData2$data[[j]][which(names(fbData2$data[[j]]) == vars[i])] <- NULL
            }
          }
        } 
          
        # fbData2 to data.frame
        base_df <- do.call(plyr::"rbind.fill", lapply(fbData2$data, 
                                                        as.data.frame))
        
        # declare row_df
        row_df <- data.frame()
        
        # check if vars observed
        if(length(vars)){
          # rebuild fbAdsData[[k]]
          for(i in 1:length(vars)){
            for(j in 1:length(fbAdsData[[k]]$data)){
              lst <- fbAdsData[[k]]$data[[j]][which(names(fbAdsData[[k]]$data[[j]]) == vars[i])]
              
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
                
                # bind
                row_df <- plyr::rbind.fill(row_df, dat)
                
              } else { # if no lst found
                # create NA
                dat_na <- rbind.data.frame(rep(NA, 1))
                names(dat_na) <- "nan"
                
                # bind
                row_df <- plyr::rbind.fill(row_df, dat_na)
                dat_na <- NULL
              }
              
              
            }
            
            # bind columns
            base_df <- cbind.data.frame(base_df, row_df)
            row_df <- NULL
            
            # remove unknowns
            base_df$nan <- NULL
          }
        }
        
        
        # if no data in fbData
      } else if (!length(fbAdsData$data)) {
        base_df <- data.frame()
      }
      
      # replace list with data.frame
      fbAdsData[[k]] <- base_df
      
    }
    
    k <- k + 1
  }
  
  return (fbAdsData)
}

#digest list -------------------------------
digest.list <- function(json){
  
  # check if data present in JSON
  if(length(json$data)){
    
    # extract names
    names <- subAdsDataNames(json$data)
    
    # pattern to look for
    pat <- paste0("^actions$|^unique_actions$|^cost_per_action_type$|",
                  "^cost_per_unique_action_type$|^website_ctr$|",
                  "^cost_per_10_sec_video_view$|",
                  "^video_avg_sec_watched_actions$|",
                  "^video_avg_pct_watched_actions$|",
                  "^video_p25_watched_actions$|",
                  "^video_p50_watched_actions$|",
                  "^video_p75_watched_actions$|",
                  "^video_p95_watched_actions$|",
                  "^video_p100_watched_actions$|",
                  "^video_complete_watched_actions$|",
                  "^video_10_sec_watched_actions$|",
                  "^video_15_sec_watched_actions$|",
                  "^video_30_sec_watched_actions")
    
    # identify nested lists
    vars <- names[grep(pat, names)]
    
    # loop through vars to remove from json
    json2 <- json
    
    # remove vars from json2
    for(i in 1:length(vars)){
      for(j in 1:length(json2$data)){
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
            
            # bind
            row_df <- plyr::rbind.fill(row_df, dat)
            
          } else { # if no lst found
            # create NA
            dat_na <- rbind.data.frame(rep(NA, 1))
            names(dat_na) <- "nan"
            
            # bind
            row_df <- plyr::rbind.fill(row_df, dat_na)
            dat_na <- NULL
          }
          
          
        }
        
        base_df <- cbind.data.frame(base_df, row_df)
        row_df <- NULL
        
        # remove unknowns
        base_df$nan <- NULL
      }
    }
    
    # if no data in json
  } else if (!length(json$data)) {
    base_df <- data.frame()
  }
  
  return(base_df)
}

# converge generic --------------------
converge <- function(x) UseMethod("converge")

# define converge
converge.fbAdsData <- function(fbData){
  
  # remove url
  fbData$url <- NULL
  
  # get tables
  df_names <- names(fbData)[grep("^data$|^insights$|^summary$",
                                 names(fbData))]
  # merge (or not)
  if(length(df_names) > 1){
    
    if(length(df_names[grep("summary", df_names)])) {
      
      # return list with data and summary
      data <- as.list(fbData)
      
      # reset class to list
      class(data) <- "list"
      
      # return data and insights
    } else if (length(df_names[grep("^insights$", df_names)])) {
      
      # check rows to see if can be bound to data.frame
      if(nrow(fbData[["data"]]) ==
         nrow(fbData[["insights"]])){
        
        # add insights_ before var names of insights
        names(fbData[["insights"]]) <- paste0("insights_",
                                              names(fbData[["insights"]]))
        
        # bind
        data <- cbind.data.frame(fbData[["data"]], fbData[["insights"]])
        
        # else return a list
      } else {
        data <- as.list(fbData)
      }
      
    }
    
  } else {
    data <- fbData$data
  }

  return(data)
}

# findObjects -----------------------------------
findObjects <- function(id, token, fields = "default", ..., n = 100,
                        verbose = FALSE, object, FUN){
  
  # check inputs
  if(missing(id)){
    stop("Missing id", call. = FALSE)
  } else if (missing(token)){
    stop("Missing token", call. = FALSE)
  } else if (missing(object)){
    stop("missing object", call. = FALSE)
  }
  
  # check token verison
  token <- checkToken(token)
  
  # create fields
  if(fields[1] == "default") {
    fields <- c("name", "id")
  } 
  
  if(class(fields) != "character") {
    stop("Fields must be a character vector", call. = FALSE)
  } else { 
    # test if fields correct
    testParam("fields", fields, FUN)
    
    # createFields
    fields <- createFields(fields)
  }
  
  args <- unlist(list(...))
  # create fields
  if(length(args)) {
    # test if fields correct
    testParam("fields", args, "getAny")
    
    # createFields
    args <- createFields(args)
  } else {
    args <- NULL
  }
  
  if (length(args)){
    # build url
    url <- paste0("https://graph.facebook.com/v2.5/",
                  id, "/",object,"?fields=",
                  fields,
                  "%2Cinsights{", args, "}",
                  "&access_token=",
                  token) 
  } else {
    # build url
    url <- paste0("https://graph.facebook.com/v2.5/",
                  id, "/",object,"?fields=",
                  fields,
                  "&access_token=",
                  token)
  }
  
  # call api
  response <- httr::GET(url)
  
  # construct data
  fb_data <- constructFbAdsData(response)
  
  # parse data
  fb_data <- digest(fb_data)
  
  # paginate
  fb_data <- paginate(fb_data, n = n, verbose = verbose)
  
  # verbose
  if (verbose == TRUE) {
    cat(paste(n, "results requested, API returned", nrow(fb_data$data),
              "rows", "\n"))
  } 
  
  # converge
  fb_data <- converge(fb_data)
  
  return (fb_data)
}