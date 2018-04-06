#### HELPER SCRIPT FOR GAMBIA DATA ANALYSIS ####
## Civis Analytics
## Spring 2018
## author: Clara Wang

## --------------< Unanswered Questions Functions >-----------------

## returns a vector indicating TRUE/FALSE for whether a column has a value (TRUE) or is NA (FALSE) ##
## use to check number of questions answered
rowListEmpty <- function(row_index, df, columns){
  out <- !is.na(df[row_index, columns])
  return(out)
}


## --------------< Percent Match Functions >-----------------

## returns a count for the number of values that match across two rows in the same table ##
numMatches <- function(rownum, baserow, comparisonrows){
  num_matches <- sum(baserow == comparisonrows[rownum,])
  return(num_matches)
}

## returns the percentage of responses that are exact matches ##
calcPctMatchesOneRow <- function(index, df, cols){
  # split the data frame into a "base" row that's going to be compared to all the "compare" rows (remainder of rows in the df)
  base <- df[index, cols]
  compare <- df[-index, cols]
  
  # if there's nothing to compare the base row against, then return NA for the percent matches
  if(nrow(compare) == 0){
    return(NA)
  }
  
  # loop through each of the "compare" rows and calculate the number of responses that match between the "base" row and each "compare" row
  matches <- map(.x = 1:nrow(compare), .f = numMatches, baserow = base, comparisonrows = compare)
  
  # grab the highest number of matches when comparing the "base" row to the "compare" rows
  highest <- max(unlist(matches)) 
  
  # calculate the highest percent matches
  percent_match <- ((highest / length(cols)) * 100)
  
  return(percent_match)
}

## takes a dataframe and optionally, a list of columns
## returns a list of the max percent matches for each row
calcPctMatchesAllRows <- function(df, cols, id_col){
  iterations <- nrow(df)
  percent_matches <- map(.x = 1:iterations, .f = calcPctMatchesOneRow, df = df, cols = cols)
  
  # if there's an ID column specified, rename the output list with those IDs
  if(!is.null(id_col)){
    names(percent_matches) <- df[[id_col]]
  }
  
  return(percent_matches)
}

calcPctMatchesOneGroup <- function(i, groups, group_col, df, cols, id_col){
  current_group <- groups[i]
  
  subset_df <- df[df[[group_col]] == current_group,]
  
  percent_matches <- calcPctMatchesAllRows(df = subset_df, cols = cols, id_col = id_col)
  return(percent_matches)
}

calcPctMatchesAllGroups <- function(groups, group_col, df, cols, id_col){
  matches_by_group <- map(.x = 1:length(groups), .f = calcPctMatchesOneGroup, df = df, groups = groups, group_col = group_col, cols = cols, id_col = id_col)
  return(matches_by_group)
}

getPercentMatches <- function(df, cols = names(df), return_id = FALSE, groups = unique(df[[group_col]]), group_col = NULL, id_col = NULL){
  # replace NA's with string NAs
  # this will help prevent issues with checking for matches with NAs
  df <- df %>% replace(., is.na(.), "NONE")
  
  rowids <- row.names(df)
  
  if(!is.null(group_col)){
    out <- calcPctMatchesAllGroups(groups = groups, group_col = group_col, df = df, cols = cols, id_col = id_col)
    names(out) <- groups
    out <- melt(out) %>%
      rename("Group" = L1,
             "ID" = L2,
             "pct_matches" = value)
    
  }
  else{
    out <- calcPctMatchesAllRows(df = df, cols = cols, id_col = id_col)
    out <- data.frame("pct_matches" = unlist(out))
    out$ID <- row.names(out)
    
  }
  return(out)
}



## --------------< Recode vector or column in a DF to numeric values >-----------------
recodeToNumeric <- function(column, df = NA){
  if(!is.na(df)){
    if(is.numeric(df[[column]])) {
      out <- df[[column]]
      return(out)
    }
    
    # if all of the responses are secretly numeric, convert them to numeric
    else if(!any(is.na(as.numeric(df[[column]])))){
      out <- as.numeric(df[[column]])
      return(out)
    }
    
    # if not already numeric and not secretly numeric, will have to recode
    else {
      # change to factor
      torecode <- factor(df[[column]])
      
      # save original values
      orig_values <- levels(torecode)
      
      # recode values by changing levels
      levels(torecode) <- seq(1, length(unique(df[[column]])) - any(is.na(df[[column]])))  # subtract 1 if there are NAs in values, because those aren't levels
      
      # grab new values
      new_values <- levels(torecode)
      
      # vector with new values, where names are each of the original values
      names(new_values) <- orig_values
      
      # change column to numeric, now that values have been recoded
      out <- as.numeric(torecode)
      
      return(out)
    }
  } else {
    if(is.numeric(column)) {
      out <- column
      return(out)
    }
    
    # if all of the responses are secretly numeric, convert them to numeric
    else if(!any(is.na(as.numeric(column)))){
      out <- as.numeric(column)
      return(out)
    }
    
    # if not already numeric and not secretly numeric, will have to recode
    else {
      # change to factor
      torecode <- factor(column)
      
      # save original values
      orig_values <- levels(torecode)
      
      # recode values by changing levels
      levels(torecode) <- seq(1, length(unique(column)) - any(is.na(column)))  # subtract 1 if there are NAs in values, because those aren't levels
      
      # grab new values
      new_values <- levels(torecode)
      
      # vector with new values, where names are each of the original values
      names(new_values) <- orig_values
      
      # change column to numeric, now that values have been recoded
      out <- as.numeric(torecode)
      
      return(out)
    }
  }
}


## --------------< Compare Distributions >-----------------

compareDistribsOneInterviewer <- function(df, interviewer_id, col, id_col = "CODE_Enq_CONF"){
  interviewer <- df[df[[id_col]] == interviewer_id,]
  rest <- df[df[[id_col]] != interviewer_id,]
  
  plot <- ggplot() +
    geom_density(data = interviewer, aes_string(x = col), color = "red", fill = "red", alpha = 0.5) +
    geom_density(data = rest, aes_string(x = col), color = "blue", fill = "blue", alpha = 0.5) +
    labs(title = paste0("Red is interviewer #", interviewer_id))
  print(plot)
  
  if(nrow(interviewer) <= 1){
    print(paste0("Only one observation for interviewer #", interviewer_id, ". Can't do t-test."))
    ttest_p <- NA
  } else {
    ttest_p <- t.test(interviewer[[col]], rest[[col]])$p.value
  }
  return(list("p_val" = ttest_p, "plot" = plot))
}


compareDistribs <- function(df, col, interviewer_id_col = "CODE_Enq_CONF"){
  interviewers <- unique(df[[interviewer_id_col]])
  n_interviewers <- length(interviewers)
  
  out <- map(.x = interviewers[1:n_interviewers], .f = compareDistribsOneInterviewer,
             df = df, col = col)
  names(out) <- interviewers
  return(out)
}



## --------------< Bivariate Comparisons >-----------------

removeOneInterviewerCompare <- function(df, interviewer_id, col_y, col_x, interviewer_id_col, return){
  subset_df <- df[df[[interviewer_id_col]] != interviewer_id, c(col_y, col_x)]
  
  out_R2 <- NA 
  out_nulldeviance <- NA 
  out_residualdeviance <- NA
  out_cor <- NA
  
  if("logit" %in% return){
    # if the y variable isn't binary, can't run logit model
    if(!all(unique(subset_df[[col_y]]) == c(0, 1))){
      print("The y variable is not binary -- can't return logit model output.")
    } else {
      out_nulldeviance <- glm(subset_df[[col_y]] ~ subset_df[[col_x]], family = binomial(link = "logit"))$null.deviance
      out_residualdeviance <- glm(subset_df[[col_y]] ~ subset_df[[col_x]], family = binomial(link = "logit"))$deviance
    }
  } 
  if ("R2" %in% return) {
    out_R2 <- summary(lm(subset_df[[col_y]] ~ subset_df[[col_x]]))$r.squared
  } 
  if ("cor" %in% return) {
    out_cor <- cor(subset_df[[col_y]], subset_df[[col_x]], use = "complete.obs")
  }
  
  out <- list("R2" = out_R2, "null_deviance" = out_nulldeviance, "residual_deviance" = out_residualdeviance, "cor" = out_cor)
}

compareRemoveMetrics <- function(df, col_y, col_x, interviewer_id_col = "CODE_Enq_CONF", return = c("logit", "R2", "cor")){
  interview_ids <- unique(df[[interviewer_id_col]])
  n_interviewers <- length(interview_ids)

  out <- map(.x = interview_ids[1:n_interviewers], .f = removeOneInterviewerCompare,
             df = df, col_y = col_y, col_x = col_x, interviewer_id_col = interviewer_id_col, return = return)
  
  names(out) <- interview_ids
  
  out <- data.frame(unlist(out))
  names(out) <- "value"
  out$temp <- row.names(out)
  row.names(out) <- seq(1:nrow(out))
  out <- out %>%
    separate(temp, c("interviewer_id", "data"), "[.]") %>%
    select("interviewer_id", "data", "value")
  
  out_R2_all <- NA 
  out_nulldeviance_all <- NA 
  out_residualdeviance_all <- NA
  out_cor_all <- NA
  
  if("logit" %in% return){
    # if the y variable isn't binary, can't run logit model
    if(!all(unique(df[[col_y]]) == c(0, 1))){
      print("The y variable is not binary -- can't return logit model output.")
    } else {
      out_nulldeviance_all <- glm(df[[col_y]] ~ df[[col_x]], family = binomial(link = "logit"))$null.deviance
      out_residualdeviance_all <- glm(df[[col_y]] ~ df[[col_x]], family = binomial(link = "logit"))$deviance
    }
  } 
  if ("R2" %in% return) {
    out_R2_all <- summary(lm(df[[col_y]] ~ df[[col_x]]))$r.squared
  } 
  if ("cor" %in% return) {
    out_cor_all <- cor(df[[col_y]], df[[col_x]], use = "complete.obs")
  }
  
  out <- out %>%
    spread(data, value) %>%
    mutate("diff_R2" = R2 - out_R2_all,
           "diff_nulldeviance" = null_deviance - out_nulldeviance_all,
           "diff_residualdeviance" = residual_deviance - out_residualdeviance_all,
           "diff_cor" = cor -  out_cor_all)
  
  return(out)
}


## --------------< Plot Gambia Map Geospatial >-----------------

# plotting Gambia map with interview locations, by interview day
makeMap <- function(id, df, mapbackground){
  subset <- df[df["interviewer_id"] == id,]
  
  n_sizes <- subset %>%
    group_by(interviewer_id, interview_day) %>%
    summarise(n_interviews = n())
  
  map <- ggmap(mapbackground) +
    geom_point(data = subset, 
               aes(x = lon, y = lat, fill = interviewer_id, alpha = 0.8), 
               size = 3, shape = 21) +
    geom_text(data = n_sizes,
              aes(x = -17, y = 12.8, label = paste0("# Interviews: ", n_interviews)), 
              color = "white", size = 4, fontface = "bold", hjust = 0, vjust = 0) +
    scale_y_continuous(limits = c(12.75, 14.25)) +
    guides(fill = FALSE, alpha = FALSE, size = FALSE) +
    labs(title = paste0("Interviewer ID: ", id)) +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank()) +
    facet_wrap(~interview_day)
  
  return(map)
}


## --------------< Calculating Distance >-----------------

calcGeoDist <- function(df, point1, point2, latcol = "lat", loncol = "lon"){
  
  a <- c(df[[loncol]][df["rowid"] == point1], df[[latcol]][df["rowid"] == point1])
  b <- c(df[[loncol]][df["rowid"] == point2], df[[latcol]][df["rowid"] == point2])
  
  distance <- distm(a, b, fun = distVincentyEllipsoid)[1]
  return(distance)
}


calcGeoDistOnetoAll <- function(df, point1, latcol = "lat", loncol = "lon"){
  point2 <- point1 + 1
  distance_onetoall <- map(.x = point2:nrow(df),
                           .f = calcGeoDist,
                           df = df,
                           point1 = point1)
  return(distance_onetoall)
}

calcGeoDistOneGroupConsecutive <- function(df, group, groupcol, latcol = "lat", loncol = "lon"){
  subset <- df[df[groupcol] == group,] %>%
    arrange(interview_start) %>%
    mutate(rowid = 1:nrow(.))    # create a column to keep track of what row you're on within the subset
  
  # if hit an interviewer with only one interviewer in the dataset
  if(nrow(subset) <= 1){
    print("This interviewer only has one interview in the dataset. Cannot calculate distance between interviews.")
    out <- subset %>%
      mutate("dist_from_prev_interview" = NA,
             "id" = NA) %>%
      select(dist_from_prev_interview, id, latcol, loncol, superid)
    return(out)
  }
  
  distances <- map2(.x = 1:(nrow(subset) - 1),
                    .y = 2:(nrow(subset)),
                    .f = calcGeoDist,
                    df = subset)
  
  # add one to each row ID number so when match back to df, each value will be the distance from the previous interview
  matchnames <- subset$rowid + 1
  matchnames <- matchnames[1:(length(matchnames) - 1)]  # take off the last value, because output distances are one shorter than original df
  matchnames <- paste0(group, "_", matchnames)          # add group identifier
  
  names(distances) <- matchnames
  
  out <- data.frame("dist_from_prev_interview" = unlist(distances))
  out$id <- row.names(out)
  row.names(out) <- 1:nrow(out)
  
  out <- out %>%
    left_join(subset %>%
                mutate(id = paste0(group, "_", rowid)) %>%
                select(id, latcol, loncol, superid), by = "id")
  
  return(out)
}



## --------------< Maybe Garbage >-----------------


calcQuestionMeanVar <- function(df, question, i = 1, group_col = NULL, groups = unique(df[[group_col]]), output = "mean-var"){
  if(!is.null(group_col)){
    current_group <- groups[i]
    question_column <- df[[question]][df[[group_col]] == current_group]  # grab question column for specific group
  } else {
    question_column <- df[[question]]
  }
  
  # calculate mean of column responses
  mean_out <- mean(question_column, na.rm = TRUE)
  
  # calculate variance of column responses
  question_column_nona <- question_column %>% 
    ifelse(is.na(.), max(question_column) + 1, .)  # recode NAs to be one value higher than highest value in column
  
  var_out <- var(question_column_nona)
  
  # return specific output, based on user input
  if(output == "mean"){
    return(mean_out)
  } else if(output == "mean-var"){
    return(c(mean_out, var_out))
  } else if(output == "var"){
    return(var_out)
  }
}