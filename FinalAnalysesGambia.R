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
survey <- read_xlsx("gambia_batch1.xlsx", col_types = "text")  # first 237 responses

## geospatial data
geospatial <- read_xlsx("GPSbatch1.xlsx")  # first batch


## --------------< Munge and Join Data >-----------------

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
 

geospatial_clean <- geospatial %>%
  transmute(interviewer_id = as.character(`INTERVIEWER ID`),
            interview_day = dmy(`DATE OF INTERVIEW`),
            interview_day_string = as.character(interview_day),
            r_gender = GENDER,
            r_yearborn = `YEAR OF BIRTH`,
            lat = LAT,
            lon = LON,
            r_education = EDUCATION,
            checkdupes = seq(1:nrow(.)))

all_clean <- survey_interviewer_clean %>%
  left_join(geospatial_clean, by = c("interviewer_id", "interview_day", "r_gender", "r_education", "r_yearborn")) %>%
  group_by(checkdupes) %>%
  mutate(geodupe = ifelse(n() > 1, 1, 0)) %>% ungroup()  # duplicates from joining on geospatial data




#############  ASSESS INTERVIEW QUALITY  ####################

## -------------< 1. Calculate Percent Matches >----------------
# ***according to Kuriakose and Robbins, don't want to exceed 85% matches***

# grab the columns that we want to calculate the perent match for
question_cols <- c(grep("^Q[0-9]", names(survey_interviewer_clean), value = TRUE))  # assume all columns that are Q[digit] are questions that should be answered
question_cols <- question_cols[question_cols != "Q92"]  # remove this because doesn't need to match (interviewer ID)
question_cols <- question_cols[question_cols != "Q93"]  # remove this because doesn't need to match (interviewer ID)

# calculate the percent of questions that match across respondents, grouped by interviewer
grouped_pctmatch <- getPercentMatches(df = survey_interviewer_clean, cols = question_cols, group_col = "interviewer_id", id_col = "superid")
# aclculate the percent of questions that match across all respondents
all_pctmatch <- getPercentMatches(df = survey_interviewer_clean, cols = question_cols, id_col = "superid")

# for interviewers with just one survey, can't get grouped_pctmatch value because no other survey response to compare to, so result is NA
# to remove NAs, coalesce the all_pctmatch value with the grouped_pctmatch value
pctmatch_analysis <- grouped_pctmatch %>%
  left_join(select(all_pctmatch, "allpctmatches" = pct_matches, "ID"), by = c("ID" = "ID")) %>%
  group_by(Group) %>%
  mutate("avg_pctmatch" = mean(pct_matches)) %>% ungroup() %>%
  mutate("avg_pctmatch_removeNAs" = coalesce(avg_pctmatch, allpctmatches)) %>%
  select("interviewer_id" = Group, "avg_pctmatch", "avg_pctmatch_removeNAs") %>% distinct()



## -------------< 2. Compare Average Response for Each Question, by Interviewer >----------------
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
  summarise("mean_sd_from_mean" = mean(sd_from_mean, na.rm = TRUE))  # calculate the average standard deviation from the mean across all questions for each interviewer


# -------------< 3. Time Analysis >--------------
# grabbing demos columns that seem to be the demographic info for survey respondents?
name_cols <- grep("NAME_", names(survey_interviewer_clean), value = TRUE)
age_cols <- grep("AGE_", names(survey_interviewer_clean), value = TRUE)
gender_cols <- grep("GENDER_", names(survey_interviewer_clean), value = TRUE)

want_cols <- c(name_cols, age_cols, gender_cols)

# calculate the time between interviews per interviewer per day
timediff <- survey_interviewer_clean %>%
  arrange(sort(clean$interview_start)) %>% 
  group_by(interviewer_id, interview_day) %>%
  mutate("previous_interview_end" = lag(interview_end, 1)) %>% ungroup() %>%
  mutate("secs_since_prev_interview" = difftime(interview_start, previous_interview_end, units = "secs")) %>%
  select("interviewer_id", "previous_interview_end", "interview_start", "interview_end", "secs_since_prev_interview", "interview_length", "Q89", want_cols) %>%
  arrange(interviewer_id, interview_start)

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
  select("interviewer_id", "previous_interview_end", "interview_start", "interview_end", "secs_since_prev_interview", "interview_length", "Q89", want_cols)


# check difference in distribution of interview times across interviewers
time_check <- compareDistribs(survey_interviewer_clean, "INTTIME")

# turn output into data frame with column indicating whether t-test is statistically significant or not
temp <- unlist(time_check)
temp <- temp[grepl("p_val", names(temp))]
temp <- data.frame("p_val" = unlist(temp))
temp$interviewer_id <- row.names(temp)
time_analysis_ttest <- temp %>% 
  separate(interviewer_id, c("interviewer_id", "remove"), "[.]") %>%
  select(interviewer_id, p_val) %>%
  mutate("signif" = ifelse(p_val <= 0.05, 1, 0))

# also calculate average interview length for each interviewer
# then calculate the number of standard deviations away they are from the average, average interview time
time_analysis_mean <- survey_interviewer_clean %>%
  group_by(interviewer_id) %>%
  summarise("interviewer_mean_inttime" = mean(interview_length, na.rm = TRUE)) %>% ungroup() %>%
  mutate("mean_mean_inttime" = mean(interviewer_mean_inttime, na.rm = TRUE),
         "sd_of_means" = sd(interviewer_mean_inttime, na.rm = TRUE),
         "sd_from_mean" = (interviewer_mean_inttime - mean_mean_inttime) / sd_of_means)


# -------------< 4. Bivariate Analysis >-------------

# recode covariates of interest that are expected to have a relationship
covariates <- survey_interviewer_clean %>%
  mutate(barrow_approve = dplyr::case_when(Q24 == "Strongly approve" ~ 4,
                                           Q24 == "Somewhat approve" ~ 3,
                                           Q24 == "Somewhat disapprove" ~ 2,
                                           Q24 == "Strongly disapprove" ~ 1,
                                           TRUE ~ as.numeric(NA)),
         barrow_vote = dplyr::case_when(grepl("Barrow", Q82) ~ 1,
                                        TRUE ~ 0),
         barrow_direction_agree = dplyr::case_when(Q43_1 == "FIRST statement MUCH" ~ 4,
                                                   Q43_1 == "FIRST statement SOMEWHAT" ~ 3,
                                                   Q43_1 == "SECOND statement SOMEWHAT" ~ 2,
                                                   Q43_1 == "SECOND statement MUCH" ~ 1,
                                                   TRUE ~ as.numeric(NA)),
         education = dplyr::case_when(Q5 == "Master or higher" ~ 8,
                                      Q5 == "Bachelor" ~ 7,
                                      Q5 == "Diploma" ~ 6,
                                      Q5 == "Vocational" ~ 5,
                                      Q5 == "Upper secondary" ~ 4,
                                      Q5 == "Lower secondary" ~ 3,
                                      Q5 == "Primary" ~ 2,
                                      Q5 == "Early childhood" ~ 1,
                                      Q5 == "None" ~ 0),
         employed = dplyr::case_when(Q78 == "Full-time employed" ~ 1,
                                     Q78 == "Part-time employed" ~ 1,
                                     TRUE ~ 0),
         unemployed = dplyr::case_when(Q78 == "Unemployed and looking for work" ~ 1,
                                       TRUE ~ 0),
         retired = dplyr::case_when(Q78 == "Retired" ~ 1,
                                    TRUE ~ 0),
         student = dplyr::case_when(Q78 == "Student" ~ 1,
                                    TRUE ~ 0),
         age = as.numeric(age),
         interviewer_id = as.factor(interviewer_id))


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


# join together the results of each bivariate analysis
bivariate_analysis <- select(vote_approve, "interviewer_id", "diff_nulldeviance_voteapprove" = diff_nulldeviance) %>%
  left_join(select(vote_direction, "interviewer_id", "diff_nulldeviance_votedirection" = diff_nulldeviance), by = "interviewer_id") %>%
  left_join(select(approve_direction, "interviewer_id", "diff_R2_approvedirection" = diff_R2), by = "interviewer_id") %>%
  left_join(select(student_age, "interviewer_id", "diff_nulldeviance_studentage" = diff_nulldeviance), by = "interviewer_id") %>%
  left_join(select(retired_age, "interviewer_id", "diff_nulldeviance_retiredage" = diff_nulldeviance), by = "interviewer_id")
bivariate_analysis <- bivariate_analysis %>%
  mutate_at(.vars = grep("diff", names(.), value = TRUE), .funs = abs)  # change all the differences to absolute values


## ***NOTE: these are sensitive to N sizes***
# check for interviewer effects -- vote for barrow ~ approve of barrow
vote_approve1 <- summary(gam(barrow_vote ~ barrow_approve +s(interviewer_id, bs = "re"), data = covariates))
vote_approve2 <- summary(gam(barrow_vote ~ barrow_approve +s(interviewer_id, by = barrow_approve, bs = "re"), data = covariates))

# check for interviewer effects -- vote for barrow ~ continue in direction barrow is taking country
vote_direction1 <- summary(gam(barrow_vote ~ barrow_direction_agree +s(interviewer_id, bs = "re"), data = covariates))
vote_direction2 <- summary(gam(barrow_vote ~ barrow_direction_agree +s(interviewer_id, by = barrow_direction_agree, bs = "re"), data = covariates))

# check for interviewer effects -- approve of barrow ~ continue in direction barrow is taking country
approve_direction1 <- summary(gam(barrow_approve ~ barrow_direction_agree +s(interviewer_id, bs = "re"), data = covariates))
approve_direction2 <- summary(gam(barrow_approve ~ barrow_direction_agree +s(interviewer_id, by = barrow_direction_agree, bs = "re"), data = covariates))



### DIDN'T ADD GAM MODELS FOR AGE/STUDENT/RETIRED BECAUSE IT'S TOO SPARSE ACROSS INTERVIEWERS (e.g. some interview mainly older people who are not students)