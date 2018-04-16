#############  GAMBIA ANALYSIS - FINAL ####################
## Script to assess Gambia survey data; finalized
## Civis Analytics
## author: Clara Wang
## Spring 2018


## --------------< Set-up Workspace >-----------------
rm(list=ls())

dyn.load(paste0(system2('/usr/libexec/java_home', stdout = TRUE), '/jre/lib/server/libjvm.dylib'))
library(tidyverse)
library(readr)
library(mgcv)
library(readxl)
library(ggmap)
library(ggplot2)
library(purrr)
library(reshape2)
library(lubridate)
library(geosphere)
library(civis)
library(civis.deckR)

setwd("~/Desktop/Gambia/GambiaSurveyQC")

## load helper functions
source("helpers_Gambia.R")



## --------------< Read in Data >-----------------

## interviewer data
interviewer <- read_xlsx("interview_supervisors.xlsx", skip = 1, col_types = "text")
interviewer[nrow(interviewer), "ID"] <- "220012450_2"  # change duplicate ID to have _2 appended on

## survey data
#survey <- read_xlsx("gambia_batch1.xlsx", col_types = "text")  # first 237 responses
survey <- read_xlsx("gambia_batch2.xlsx", col_types = "text")  # second batch (includes responses from first batch -- 707 responses)

# recode surveys with wrong Interview IDs
survey <- survey %>%
  mutate(CODE_Enq_CONF = dplyr::case_when(CODE_Enq_CONF == "220012787" ~ "220018727",
                                          CODE_Enq_CONF == "221015265" ~ "220012878",
                                          CODE_Enq_CONF == "22013701" ~ "220013701",
                                          CODE_Enq_CONF == "2200148051" ~ "220012878",
                                          TRUE ~ as.character(CODE_Enq_CONF)))


## geospatial data
#geospatial <- read_xlsx("GPSbatch1.xlsx")  # first batch
geospatial <- read_xlsx("GPSbatch2.xlsx")  # second batch (includes responses from first batch)


## IDs that are in survey data but not in list of Interviewer IDs
bad_ids <- setdiff(survey$CODE_Enq_CONF, interviewer$ID)


## --------------< Munge and Join Data >-----------------

## df with all survey data joined with interviewer data
survey_interviewer_clean <- survey %>%
  mutate("interviewer_id" = CODE_Enq_CONF,
         "r_gender" = Q3,
         "r_education" = Q5,
         "r_yearborn" = Q4,
         "interview_start" = lubridate::ymd_hm(STIME),
         "interview_end" = interview_start + seconds(INTTIME),
         "interview_day" = lubridate::ymd(substr(STIME, 1, 8)),
         "interview_length" = as.numeric(INTTIME),
         "r_yearborn" = as.integer(r_yearborn),
         "superid" = 1:nrow(.)) %>%
  # join on interviewer data
  left_join(interviewer, by = c("interviewer_id" = "ID"))
 
## df with all geospatial data
geospatial_clean <- geospatial %>%
  transmute(interviewer_id = as.character(`INTERVIEWER ID`),
            interview_day = ymd(`DATE OF INTERVIEW`),  
            interview_day_string = as.character(interview_day),
            r_gender = as.character(GENDER),
            r_yearborn = `YEAR OF BIRTH`,
            lat = LAT,
            lon = LON,
            r_education = as.character(EDUCATION),
            checkdupes = seq(1:nrow(.)))

## df with all survey + interviewer data joined with geospatial data
all_clean <- survey_interviewer_clean %>%
  left_join(geospatial_clean, by = c("interviewer_id", "interview_day", "r_gender", "r_education", "r_yearborn")) %>%
  group_by(checkdupes) %>%
  mutate(geodupe = ifelse(n() > 1, 1, 0)) %>% ungroup()  # duplicates from joining on geospatial data

## df with geospatial data, joined with survey + interviewer data; duplicates removed
all_geospatial_clean_nodupes <- all_clean %>%
  filter(geodupe == 0 & !is.na(lat))



#############  ASSESS INTERVIEW QUALITY -- EASY TESTS  ####################

## -------------< 1. Calculate Percent Matches >----------------
# ***according to Kuriakose and Robbins, don't want to exceed 85% matches***

# grab the columns that we want to calculate the perent match for
question_cols <- c(grep("^Q[0-9]", names(survey_interviewer_clean), value = TRUE))  # assume all columns that are Q[digit] are questions that should be answered
question_cols <- question_cols[question_cols != "Q92"]  # remove this because doesn't need to match (interviewer ID)
question_cols <- question_cols[question_cols != "Q93"]  # remove this because doesn't need to match (interviewer ID)

# calculate the percent of questions that match across respondents, grouped by interviewer
grouped_pctmatch <- getPercentMatches(df = survey_interviewer_clean, cols = question_cols, group_col = "interviewer_id", id_col = "superid")
# calculate the percent of questions that match across all respondents
all_pctmatch <- getPercentMatches(df = survey_interviewer_clean, cols = question_cols, id_col = "superid")

# for interviewers with just one survey, can't get grouped_pctmatch value because no other survey response to compare to, so result is NA
# to remove NAs, coalesce the all_pctmatch value with the grouped_pctmatch value
pctmatch_analysis <- grouped_pctmatch %>%
  left_join(select(all_pctmatch, "allpctmatches" = pct_matches, "ID"), by = c("ID" = "ID")) %>%
  group_by(Group) %>%
  mutate("avg_pctmatch" = mean(pct_matches)) %>% ungroup() %>%
  mutate("avg_pctmatch_removeNAs" = coalesce(avg_pctmatch, allpctmatches)) %>%
  select("interviewer_id" = Group, "avg_pctmatch", "avg_pctmatch_removeNAs") %>% distinct() %>%
  View(.)


## -------------< 2. Length of Interview >----------------

length_of_interview_analysis <- survey_interviewer_clean %>%
  transmute("superid" = superid, 
            "interviewer_id" = interviewer_id,
            "interview_length" = interview_length,
            "overall_mean_inttime" = mean(interview_length, na.rm = TRUE),
            "overall_median_inttime" = median(interview_length, na.rm = TRUE),
            "onethird_overall_median_inttime" = overall_median_inttime / 3,
            "sd_inttime" = sd(interview_length, na.rm = TRUE),
            "diff_from_overall_mean" = interview_length - overall_mean_inttime, 
            "sd_from_overall_mean" = diff_from_overall_mean / sd_inttime,
            "less_onethird_median_inttime" = ifelse(interview_length < onethird_overall_median_inttime, 1, 0))

interviewers_speedy <- length_of_interview_analysis %>%
  filter(less_onethird_median_inttime == 1) %>%
  select(interviewer_id) %>% group_by(interviewer_id) %>%
  summarise(n = n()) %>% ungroup()
  

## --------------< 3. Distance Analysis >-----------------
# grabbing demos columns that seem to be the demographic info for survey respondents?
name_cols <- grep("NAME_", names(survey_interviewer_clean), value = TRUE)
age_cols <- grep("AGE_", names(survey_interviewer_clean), value = TRUE)
gender_cols <- grep("GENDER_", names(survey_interviewer_clean), value = TRUE)

want_cols <- c(name_cols, age_cols, gender_cols)

# calculate the time between interviews per interviewer per day
timediff <- survey_interviewer_clean %>%
  arrange(sort(survey_interviewer_clean$interview_start)) %>% 
  group_by(interviewer_id, interview_day) %>%
  mutate("previous_interview_end" = lag(interview_end, 1)) %>% ungroup() %>%
  mutate("secs_since_prev_interview" = difftime(interview_start, previous_interview_end, units = "secs")) %>%
  select("interviewer_id", "previous_interview_end", "interview_start", "interview_end", "secs_since_prev_interview", "interview_length", "Q89", want_cols) %>%
  arrange(interviewer_id, interview_start)

# Q86 -- rural/urban interview location

# calculate distance between consecutive interviews for each interviewer per day
distance_df <- data.frame("dist_from_prev_interview" = NA, "id" = NA, "lat" = NA, "lon" = NA, "superid" = NA)  # initialize empty df
# loop through each possible day, calculate distance between each interview for each interviewer in that day
for(date in unique(all_geospatial_clean_nodupes$interview_day)){
  subset_date <- all_geospatial_clean_nodupes[all_geospatial_clean_nodupes$interview_day == date,]
  
  temp <- map_df(.x = unique(subset_date$interviewer_id)[1:length(unique(subset_date$interviewer_id))],
                 .f = calcGeoDistOneGroupConsecutive,
                 groupcol = "interviewer_id",
                 df = subset_date)
  
  distance_df <- bind_rows(distance_df, temp)
}

# clean up the df to include more information about interview start time, end time, and length of interview
distance_analysis <- distance_df %>%
  full_join(all_geospatial_clean_nodupes) %>%
  select(superid, interviewer_id, interview_day, lat, lon, dist_from_prev_interview, interview_length, interview_start, interview_end, Q86) %>%
  filter(!is.na(superid)) %>%
  left_join(timediff, by = c("interviewer_id", "interview_start", "interview_end", "interview_length")) %>%
  arrange(interviewer_id, interview_start) 

# ggplot(distance_analysis) +
#   geom_density(aes(x = dist_from_prev_interview), alpha = 0.5) +
#   facet_wrap(~interviewer_id, scales = "free")


## interviewers with no change in distance
nochange_interviewers <- distance_analysis %>%
  group_by(interviewer_id) %>%
  filter(ifelse(min(dist_from_prev_interview, na.rm = TRUE) == 0, TRUE, FALSE))


nochange <- distance_analysis %>%
  filter(dist_from_prev_interview == 0)


#############  ASSESS INTERVIEW QUALITY -- HARD TESTS  ####################

## -------------< 4. Compare Average Response for Each Question, by Interviewer >----------------
# subset the questions to only those that are expected not to differ significantly across interviewers
# i.e. remove questions about location of interview, consent to interview, etc.
question_cols_subset <- question_cols[!(question_cols %in% c("Q95", "Q91", "Q90", "Q89", "Q88", "Q87", "Q86", "Q85", "Q1", "Q1_1", "Q4"))]

mean_analysis <- survey_interviewer_clean %>%
  # recode the subset question columns to numeric
  mutate_at(.funs = funs("recode" = recodeToNumeric), .vars = c(question_cols_subset)) %>% ungroup() %>%
  # grab just the interviewer_id column and the recoded question columns
  select(interviewer_id, paste0(question_cols_subset, "_recode")) %>%
  gather(question, value, -interviewer_id) %>%
  # for each interviewer, calculate the "average response" for each question
  group_by(question, interviewer_id) %>%
  summarise("interviewer_mean" = mean(value, na.rm = TRUE)) %>% ungroup() %>%
  # calculate the "average, average response" for each question
  group_by(question) %>%
  mutate("mean_interviewer_mean" = mean(interviewer_mean, na.rm = TRUE),        # the average interviewer average for each question
         "sd_of_means" = sd(interviewer_mean, na.rm = TRUE)) %>% ungroup() %>%  # the standard deviation of interviewer averages for each question
  mutate("sd_from_mean" = (interviewer_mean - mean_interviewer_mean) / sd_of_means) %>%  # calc number of standard deviations away from the mean an interviewer is for each question
  group_by(interviewer_id) %>%
  summarise("mean_sd_from_mean" = mean(sd_from_mean, na.rm = TRUE)) %>%  # calculate the average standard deviation from the mean across all questions for each interviewer
  left_join(survey_interviewer_clean %>%
              group_by(interviewer_id) %>%
              summarise(n = n()))


# -------------< 5. Time Analysis -- Time Between Surveys and Length of Interview >--------------
# grab interviewer IDs for interviewers who had next interview start in within a minute of previous interview
check <- timediff %>%
  filter(secs_since_prev_interview < 0) %>%
  select(interviewer_id) %>%
  distinct()

# grab all the interviews conducted by interviewers under suspicion (grabbed in "check")
# eyeball interview times and demographic column info about the respondents to check for weirdness
check2 <- timediff %>%
  filter(interviewer_id %in% check[[1]]) %>%
  arrange(interviewer_id, interview_start) %>%
  select("interviewer_id", "previous_interview_end", "interview_start", "interview_end", "secs_since_prev_interview", "interview_length", "Q89", want_cols) %>%
  View(.)

#### NOTES:
## Interviewer 220011442 has 2 interviews that overlap with one another
## Interview 220011442 has an interview that takes place within one miinute of the previous interview in a different geographic district

timediff_suspicious <- c("220011442")


# check difference in distribution of interview times across interviewers
time_check <- compareDistribs(survey_interviewer_clean, "interview_length")

# turn output into data frame with column indicating whether t-test is statistically significant or not
temp <- unlist(time_check)
temp <- temp[grepl("p_val", names(temp))]
temp <- data.frame("p_val" = unlist(temp))
temp$interviewer_id <- row.names(temp)
time_ttest_analysis <- temp %>% 
  separate(interviewer_id, c("interviewer_id", "remove"), "[.]") %>%
  select(interviewer_id, p_val) %>%
  mutate("signif" = ifelse(p_val <= 0.05, 1, 0)) %>%
  left_join(survey_interviewer_clean %>% 
              group_by(interviewer_id) %>%
              summarise(n_interviews = n()) %>% ungroup())

check_ttest <- timediff %>%
  filter(interviewer_id %in% time_ttest_analysis$interviewer_id[time_ttest_analysis$signif == 1])
 
# also calculate average interview length for each interviewer
# then calculate the number of standard deviations away they are from the average, average interview time
time_mean_analysis <- survey_interviewer_clean %>%
  group_by(interviewer_id) %>%
  summarise("interviewer_mean_inttime" = mean(interview_length, na.rm = TRUE)) %>% ungroup() %>%
  mutate("mean_mean_inttime" = mean(interviewer_mean_inttime, na.rm = TRUE),
         "sd_of_means" = sd(interviewer_mean_inttime, na.rm = TRUE),
         "sd_from_mean" = (interviewer_mean_inttime - mean_mean_inttime) / sd_of_means) %>%
  left_join(survey_interviewer_clean %>%
              group_by(interviewer_id) %>%
              summarise(n = n()))



check_time <- c(220017942, 221015265, 220011492, 220012787, 220014362)


# -------------< 6. Bivariate Analysis >-------------

# recode covariates of interest that are expected to have a relationship
covariates <- survey_interviewer_clean %>%
  mutate(barrow_approve = dplyr::case_when(Q24 == "Strongly approve" | Q24 == "1" ~ 4,
                                           Q24 == "Somewhat approve" | Q24 == "2" ~ 3,
                                           Q24 == "Somewhat disapprove" | Q24 == "3" ~ 2,
                                           Q24 == "Strongly disapprove" | Q24 == "4" ~ 1,
                                           TRUE ~ as.numeric(NA)),
         barrow_vote = dplyr::case_when(grepl("Barrow", Q82) | Q82 == "1" ~ 1,
                                        TRUE ~ 0),
         barrow_direction_agree = dplyr::case_when(Q43_1 == "FIRST statement MUCH" | Q43_1 == "1" ~ 4,
                                                   Q43_1 == "FIRST statement SOMEWHAT" | Q43_1 == "2" ~ 3,
                                                   Q43_1 == "SECOND statement SOMEWHAT" | Q43_1 == "3" ~ 2,
                                                   Q43_1 == "SECOND statement MUCH" | Q43_1 == "4" ~ 1,
                                                   TRUE ~ as.numeric(NA)),
         education = dplyr::case_when(Q5 == "Master or higher" | Q5 == "9" ~ 8,
                                      Q5 == "Bachelor" | Q5 == "8" ~ 7,
                                      Q5 == "Diploma" | Q5 == "7" ~ 6,
                                      Q5 == "Vocational" | Q5 == "6" ~ 5,
                                      Q5 == "Upper secondary" | Q5 == "5" ~ 4,
                                      Q5 == "Lower secondary" | Q5 == "4" ~ 3,
                                      Q5 == "Primary" | Q5 == "3" ~ 2,
                                      Q5 == "Early childhood" | Q5 == "2" ~ 1,
                                      Q5 == "None" | Q5 == "1" ~ 0),
         employed = dplyr::case_when(Q78 == "Full-time employed" | Q78 == "1" ~ 1,
                                     Q78 == "Part-time employed" | Q78 == "2" ~ 1,
                                     TRUE ~ 0),
         unemployed = dplyr::case_when(Q78 == "Unemployed and looking for work" | Q78 == "3" ~ 1,
                                       TRUE ~ 0),
         retired = dplyr::case_when(Q78 == "Retired" | Q78 == "6" ~ 1,
                                    TRUE ~ 0),
         student = dplyr::case_when(Q78 == "Student" | Q78 == "5" ~ 1,
                                    TRUE ~ 0),
         age = as.numeric(age),
         interviewer_id = as.factor(interviewer_id))


# remove bad IDs
covariates <- covariates %>%
  filter(!(interviewer_id %in% bad_ids))


# specify the columns we want to grab, based off whether we use a logit or linear regression model
logit_cor <- c("interviewer_id", "diff_cor", "diff_nulldeviance")
R2_cor <- c("interviewer_id", "diff_cor", "diff_R2")

# logit model: vote for barrow ~ approve of barrow
vote_approve <- compareRemoveMetrics(covariates, col_y = "barrow_vote", col_x = "barrow_approve", return = c("logit", "cor")) %>% select(logit_cor)
# logit model: vote for barrow ~ continue in direction barrow is taking country
vote_direction <- compareRemoveMetrics(covariates, col_y = "barrow_vote", col_x = "barrow_direction_agree", return = c("logit", "cor")) %>% select(logit_cor)
# linear regression model: approve of barrow ~ continue in direction barrow is taking country
approve_direction <- compareRemoveMetrics(covariates, col_y = "barrow_approve", col_x = "barrow_direction_agree", return = c("R2", "cor")) %>% select(R2_cor)

# logit model: student? ~ age
student_age <- compareRemoveMetrics(covariates, col_y = "student", col_x = "age", return = c("logit", "cor")) %>% select(logit_cor)
# logit model: retired? ~ age
retired_age <- compareRemoveMetrics(covariates, col_y = "retired", col_x = "age", return = c("logit", "cor")) %>% select(logit_cor)


## ***NOTE: these are sensitive to N sizes***
# check for interviewer effects -- vote for barrow ~ approve of barrow
vote_approve1 <- summary(gam(barrow_vote ~ barrow_approve +s(interviewer_id, bs = "re"), data = covariates))
vote_approve2 <- summary(gam(barrow_vote ~ barrow_approve +s(interviewer_id, by = barrow_approve, bs = "re"), data = covariates))
check_vote_approve <- summary(glm(covariates$barrow_vote ~ covariates$barrow_approve:covariates$interviewer_id, family = binomial(link = "logit")))$coefficients %>%
  data.frame(.) %>%
  mutate(var = row.names(.),
         signif = ifelse(Pr...z.. < 0.01, 1, 0)) %>%
  filter(var != "(Intercept)") %>%
  mutate(interviewer_id = str_extract(var, "[0-9]+"),
         mean_estimate = mean(Estimate),
         sd_estimate = sd(Estimate),
         sd_from_mean = abs((Estimate - mean_estimate) / sd_estimate),
         weird = ifelse(sd_from_mean >= 2, 1, 0)) %>%
  left_join(covariates %>%
              group_by(interviewer_id) %>%
              summarise(n = n()) %>% ungroup())

# check for interviewer effects -- vote for barrow ~ continue in direction barrow is taking country
vote_direction1 <- summary(gam(barrow_vote ~ barrow_direction_agree +s(interviewer_id, bs = "re"), data = covariates))
vote_direction2 <- summary(gam(barrow_vote ~ barrow_direction_agree +s(interviewer_id, by = barrow_direction_agree, bs = "re"), data = covariates))
check_vote_direction <- summary(glm(covariates$barrow_vote ~ covariates$barrow_direction_agree:covariates$interviewer_id, family = binomial(link = "logit")))$coefficients %>%
  data.frame(.) %>%
  mutate(var = row.names(.),
         signif = ifelse(Pr...z.. < 0.01, 1, 0)) %>%
  filter(var != "(Intercept)") %>%
  mutate(interviewer_id = str_extract(var, "[0-9]+"),
         mean_estimate = mean(Estimate),
         sd_estimate = sd(Estimate),
         sd_from_mean = abs((Estimate - mean_estimate) / sd_estimate),
         weird = ifelse(sd_from_mean >= 2, 1, 0)) %>%
  left_join(covariates %>%
              group_by(interviewer_id) %>%
              summarise(n = n()) %>% ungroup())
  
# check for interviewer effects -- approve of barrow ~ continue in direction barrow is taking country
approve_direction1 <- summary(gam(barrow_approve ~ barrow_direction_agree +s(interviewer_id, bs = "re"), data = covariates))
approve_direction2 <- summary(gam(barrow_approve ~ barrow_direction_agree +s(interviewer_id, by = barrow_direction_agree, bs = "re"), data = covariates))
check_approve_direction <- summary(lm(covariates$barrow_approve ~ covariates$barrow_direction_agree:covariates$interviewer_id))$coefficients %>%
  data.frame(.) %>%
  mutate(var = row.names(.),
         signif = ifelse(Pr...t.. < 0.01, 1, 0)) %>%
  filter(var != "(Intercept)") %>%
  mutate(interviewer_id = str_extract(var, "[0-9]+"),
         mean_estimate = mean(Estimate),
         sd_estimate = sd(Estimate),
         sd_from_mean = abs((Estimate - mean_estimate) / sd_estimate),
         weird = ifelse(sd_from_mean >= 2, 1, 0)) %>%
  left_join(covariates %>%
              group_by(interviewer_id) %>%
              summarise(n = n()) %>% ungroup())



# join together the results of each bivariate analysis
bivariate_analysis <- select(vote_approve, "interviewer_id", "diff_nulldeviance_voteapprove" = diff_nulldeviance) %>%
  left_join(select(vote_direction, "interviewer_id", "diff_nulldeviance_votedirection" = diff_nulldeviance), by = "interviewer_id") %>%
  left_join(select(approve_direction, "interviewer_id", "diff_R2_approvedirection" = diff_R2), by = "interviewer_id") %>%
  left_join(select(student_age, "interviewer_id", "diff_nulldeviance_studentage" = diff_nulldeviance), by = "interviewer_id") %>%
  left_join(select(retired_age, "interviewer_id", "diff_nulldeviance_retiredage" = diff_nulldeviance), by = "interviewer_id")
bivariate_analysis <- bivariate_analysis %>%
  mutate_at(.vars = grep("diff", names(.), value = TRUE), .funs = abs) %>%  # change all the differences to absolute values
  left_join(covariates %>%
              group_by(interviewer_id) %>%
              summarise(n = n(),
                        n_urban = sum(ifelse(Q86 == "2", 1, 0)),
                        n_rural = sum(ifelse(Q86 == "1", 1, 0)))) %>%
  left_join(check_vote_approve %>% select("interviewer_id", "sd_from_mean_vote_approve" = sd_from_mean)) %>%
  left_join(check_vote_direction %>% select("interviewer_id", "sd_from_mean_vote_direction" = sd_from_mean)) %>%
  left_join(check_approve_direction %>% select("interviewer_id", "sd_from_mean_approve_direction" = sd_from_mean))
### DIDN'T ADD GAM MODELS FOR AGE/STUDENT/RETIRED BECAUSE IT'S TOO SPARSE ACROSS INTERVIEWERS (e.g. some interview mainly older people who are not students)

x <- cor(bivariate_analysis %>% select(diff_nulldeviance_voteapprove, diff_nulldeviance_retiredage, diff_nulldeviance_studentage,
                                       diff_nulldeviance_votedirection, diff_R2_approvedirection, sd_from_mean_vote_approve,
                                       sd_from_mean_vote_direction, sd_from_mean_approve_direction, n))
y <- prcomp(x)$rotation %>% data.frame() %>% mutate(var = row.names(.))

y2 <- prcomp(bivariate_analysis %>% select(n, sd_from_mean_approve_direction, sd_from_mean_vote_approve, sd_from_mean_vote_direction))$rotation %>% data.frame() %>% mutate(var = row.names(.))
ggplot(y2) + geom_point(aes(x = var, y = PC1, color = var), size = 4)

diff_responses <- bivariate_analysis %>%
  filter(sd_from_mean_approve_direction >= 2 | sd_from_mean_vote_direction >= 2 | sd_from_mean_vote_approve >= 2)


## --------------< 7. Distance Analysis >-----------------
# Q86 -- rural/urban interview location

# calculate distance between consecutive interviews for each interviewer per day
distance_df <- data.frame("dist_from_prev_interview" = NA, "id" = NA, "lat" = NA, "lon" = NA, "superid" = NA)  # initialize empty df
# loop through each possible day, calculate distance between each interview for each interviewer in that day
for(date in unique(all_geospatial_clean_nodupes$interview_day)){
  subset_date <- all_geospatial_clean_nodupes[all_geospatial_clean_nodupes$interview_day == date,]

  temp <- map_df(.x = unique(subset_date$interviewer_id)[1:length(unique(subset_date$interviewer_id))],
                 .f = calcGeoDistOneGroupConsecutive,
                 groupcol = "interviewer_id",
                 df = subset_date)
  
  distance_df <- bind_rows(distance_df, temp)
}


# clean up the df to include more information about interview start time, end time, and length of interview
distance_analysis <- distance_df %>%
  full_join(all_geospatial_clean_nodupes) %>%
  select(superid, interviewer_id, interview_day, lat, lon, dist_from_prev_interview, interview_length, interview_start, interview_end, Q86) %>%
  filter(!is.na(superid)) %>%
  left_join(timediff, by = c("interviewer_id", "interview_start", "interview_end", "interview_length")) %>%
  arrange(interviewer_id, interview_start) 

# ggplot(distance_analysis) +
#   geom_density(aes(x = dist_from_prev_interview), alpha = 0.5) +
#   facet_wrap(~interviewer_id, scales = "free")

## interviewers with no change in distance
nochange <- distance_analysis %>%
  group_by(interviewer_id) %>%
  filter(ifelse(min(dist_from_prev_interview, na.rm = TRUE) == 0, TRUE, FALSE))


## calculate average distance between interviewers
distance_difference_analysis <- distance_analysis %>%
  #filter(!(interviewer_id %in% unique(nochange$interviewer_id))) %>%  # remove suspicious interviewers with no change in distance
  group_by(interviewer_id) %>%
  mutate("mean_interview_dist" = mean(dist_from_prev_interview, na.rm = TRUE)) %>% ungroup() %>%
  mutate("sd_mean_interview_dist" = sd(mean_interview_dist, na.rm = TRUE),
         "mean_mean_interview_dist" = mean(mean_interview_dist, na.rm = TRUE),
         "sd_from_mean_interview_dist" = (mean_interview_dist - mean_mean_interview_dist) / sd_mean_interview_dist) %>%
  select(interviewer_id, sd_from_mean_interview_dist) %>% distinct()
  
  
## calculate sd of distance to a centroid for each interviewer 
distance_centroid_df <- geospatial_clean %>%
  group_by(interviewer_id, interview_day) %>%
  mutate(centroid_lat = mean(lat, na.rm = TRUE),
         centroid_lon = mean(lon, na.rm = TRUE))


distance_centroid <- data.frame("dist_from_centroid" = NA, "id" = NA)
for(date in unique(distance_centroid_df$interview_day)){
  for(id in unique(distance_centroid_df$interviewer_id)){
    subset <- distance_centroid_df %>%
      filter(interview_day == date & interviewer_id == id)
    
    if(nrow(subset) <= 1){
      print("Cannot calculate distance from centroid because there's one or fewer observations.")
      next
    }
    
    temp <- calcGeoDistCentroid(subset, 
                                centroid_lat = max(subset$centroid_lat), 
                                centroid_lon = max(subset$centroid_lon))
    
    out <- data.frame("dist_from_centroid" = unlist(temp), "id" = id) %>%
      group_by(id) %>%
      mutate(sd_dist_centroid = dist_from_centroid / sd(dist_from_centroid, na.rm = T)) %>% ungroup()
    
    distance_centroid <- bind_rows(distance_centroid, out)
  }
}

distance_centroid_analysis <- distance_centroid %>%
  filter(!is.na(id)) %>%
  group_by(id) %>%
  summarise(mean_sd_dist_centroid = mean(sd_dist_centroid)) %>% ungroup() %>%
  left_join(geospatial_clean %>%
              group_by(interviewer_id) %>%
              summarise(n = n()),
            by = c("id" = "interviewer_id"))
  


## --------------< 8. TEST Analysis -- Model to ID fakes >-----------------

analysis_dfs <- grep("analysis", ls(), value = TRUE)

length_of_interview_analysis
nochange

model_df <- survey_interviewer_clean %>%
  select(interviewer_id, superid)

for(df in analysis_dfs){
  temp <- get(df)
  model_df <- model_df %>%
    left_join(temp)
}

model_df <- model_df %>%
  group_by(interviewer_id) %>%
  mutate(n = n()) %>% ungroup() %>%
  left_join(nochange) %>%
  mutate('likely_false' = ifelse(dist_from_prev_interview == 0 | less_onethird_median_inttime == 1, 1, 0))
  



#### TABLE FOR DELIVERABLE ####
n_interviews_surveyresponses <- survey_interviewer_clean %>%
  group_by(interviewer_id) %>%
  summarise(n_interviews = n()) %>% ungroup()

n_interviews_geospatial <- geospatial_clean %>%
  group_by(interviewer_id) %>%
  summarise(n_interviews = n()) %>% ungroup()

n_interviews <- n_interviews_surveyresponses %>%
  rename("# Interviews (survey response data)" = n_interviews) %>%
  left_join(n_interviews_geospatial) %>%
  mutate(n_interviews = ifelse(is.na(n_interviews), 0, n_interviews)) %>%
  rename("# Interviews (geospatial data)" = n_interviews) %>%
  mutate(#"Interviewer IDs Not Present in Interviewer Table" = ifelse(interviewer_id %in% bad_ids, "X", "--"), 
         "No Geospatial Coordinate Change" = ifelse(interviewer_id %in% nochange$interviewer_id, "X", "--"),
         "Time Between Interviews Suspicious" = ifelse(interviewer_id %in% timediff_suspicious, "X", "--"),
         "Speedy Interviews" = ifelse(interviewer_id %in% interviewers_speedy$interviewer_id, "X", "--"),
         "Different Responses" = ifelse(interviewer_id %in% diff_responses$interviewer_id, "X", "--"),
         "Reasonable Likelihood of Survey Falsification" = case_when(interviewer_id %in% nochange$interviewer_id ~ "X",
                                                               interviewer_id %in% time_mean_analysis$interviewer_id[time_mean_analysis$sd_from_mean <= -1.25] ~ "X",
                                                               interviewer_id %in% interviewers_speedy$interviewer_id & interviewer_id %in% diff_responses$interviewer_id ~ "X",
                                                               TRUE ~ as.character("--"))) %>%
  mutate("Reasonable Likelihood of Survey Falsification" = ifelse((`No Geospatial Coordinate Change` == "X") + (`Time Between Interviews Suspicious` == "X") +
           (`Speedy Interviews` == "X") + (`Different Responses` == "X") > 1, "X", `Reasonable Likelihood of Survey Falsification`))


## --------------< Deliverable Code >-----------------
finaltable <- n_interviews %>%
  civis_table()

d <- deliverable("The Gambia Survey Analysis") %u%
  unit(title = "Interviewer Metrics",
       object1 = finaltable)

to_word(d, "gambia_700")

  
