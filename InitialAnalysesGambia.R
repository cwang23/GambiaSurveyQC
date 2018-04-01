#############  GAMBIA ANALYSIS  ####################
## Script to assess Gambia survey responses
## Civis Analytics
## author: Clara Wang
## Spring 2018


## --------------< Set-up Workspace >-----------------
rm(list=ls())

library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
library(purrr)
library(reshape2)
library(lubridate)


setwd("~/Desktop/Gambia")

## load helper functions
source("helpers_Gambia.R")


## --------------< Read in Data >-----------------

interview <- read_xlsx("interview_supervisors.xlsx", skip = 1, col_types = "text")
interview[nrow(interview), "ID"] <- "220012450_2"  # change duplicate ID to have _2 appended on
pilot <- read_xlsx("pilotdata.xlsx", col_types = "text")
batchone <- read_xlsx("gambia_batch1.xlsx", col_types = "text")


## --------------< Munge Data >-----------------

question_cols <- c(grep("^Q[0-9]", names(batchone), value = TRUE))  # assume all columns that are Q[digit] are questions that should be answered
question_cols <- question_cols[question_cols != "Q92"]  # remove this because doesn't need to match
question_cols <- question_cols[question_cols != "Q93"]  # remove this because doesn't need to match


clean <- batchone %>%
  mutate(num_answered = rowSums(!is.na(batchone[,question_cols])),
         proport_answered = num_answered / length(question_cols),
         interview_start = lubridate::ymd_hm(STIME),
         interview_end = interview_start + seconds(INTTIME),
         start_year = substr(STIME, 1, 4),
         start_month = substr(STIME, 5, 6),
         start_day = substr(STIME, 7, 8),
         start_hr = substr(STIME, 9, 10),
         start_min = substr(STIME, 11, 12),
         INTTIME = as.numeric(INTTIME)) %>%
  left_join(interview, by = c("CODE_Enq_CONF" = "ID"))


#############  ASSESS INTERVIEW QUALITY  ####################

## -------------< Calculate Percent Matches >----------------

grouped_pctmatch <- getPercentMatches(df = clean, cols = question_cols, group_col = "CODE_Enq_CONF", id_col = "INTNR")
all_pctmatch <- getPercentMatches(df = clean, cols = question_cols, id_col = "INTNR")


## -------------< Compare Means and Variance for Different Answers by Interviewer >----------------

clean_mean <- clean %>%
  group_by(CODE_Enq_CONF) %>%
  mutate_at(.funs = funs(recode = recodeToNumeric), .vars = c(question_cols, "INTTIME")) %>%
  summarise_at(.funs = funs(mean = mean), .vars = paste0(c(question_cols, "INTTIME"), "_recode"), na.rm = TRUE)

clean_mean_all <- clean %>%
  mutate_at(.funs = funs(recode = recodeToNumeric), .vars = c(question_cols, "INTTIME")) %>%
  summarise_at(.funs = funs(mean = mean), .vars = paste0(c(question_cols, "INTTIME"), "_recode"), na.rm = TRUE)
  

clean_var <- clean %>%
  group_by(CODE_Enq_CONF) %>%
  mutate_at(.funs = funs(recode = recodeToNumeric), .vars = c(question_cols, "INTTIME")) %>%
  summarise_at(.funs = funs(var = var), .vars = paste0(c(question_cols, "INTTIME"), "_recode"), na.rm = TRUE)

clean_var_all <- clean %>%
  mutate_at(.funs = funs(recode = recodeToNumeric), .vars = c(question_cols, "INTTIME")) %>%
  summarise_at(.funs = funs(var = var), .vars = paste0(c(question_cols, "INTTIME"), "_recode"), na.rm = TRUE)
  

# -------------< Question Response Patterns >-------------

# create column with pattern of question responses for each row
col_responses <- map(.x = 1:nrow(clean), .f = rowListEmpty, df = clean, columns = question_cols)


# -------------< Time Analysis >--------------

timediff <- clean %>%
  arrange(sort(clean$interview_start)) %>% 
  group_by(CODE_Enq_CONF) %>%
  mutate("previous_interview_end" = lag(interview_end, 1)) %>% ungroup() %>%
  mutate("secs_since_prev_interview" = difftime(interview_start, previous_interview_end, units = "secs")) %>%
  select("CODE_Enq_CONF", "previous_interview_end", "interview_start", "interview_end", "secs_since_prev_interview", "INTTIME") %>%
  arrange(CODE_Enq_CONF, interview_start)

# grab interviewer IDs for interviewers who had next interview start in within a minute of previous interview
check <- timediff %>%
  filter(secs_since_prev_interview < 0) %>%
  select(CODE_Enq_CONF) %>%
  distinct()
  
# grab all the interviews conducted by interviewers under suspicion (grabbed in "check")
# eyeball interview times and demographic column info about the respondents to check for weirdness
age_cols <- grep("AGE_", names(clean), value = TRUE)
name_cols <- grep("NAME_", names(clean), value = TRUE)
gender_cols <- grep("GENDER_", names(clean), value = TRUE)

want_cols <- c(name_cols, age_cols, gender_cols)

check2 <- timediff %>%
  filter(CODE_Enq_CONF %in% check[[1]]) %>%
  arrange(CODE_Enq_CONF, interview_start) %>%
  select("CODE_Enq_CONF", "previous_interview_end", "interview_start", "interview_end", "secs_since_prev_interview", "INTTIME", want_cols)

time_check <- compareDistribs(clean, "INTTIME")


# -------------< bivariate breakouts between columns; income and education should be pretty stable >-------------

covariates <- clean %>%
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
         age = as.numeric(age))




vote_approve <- compareRemoveMetrics(covariates, col_y = "barrow_vote", col_x = "barrow_approve", return = c("logit", "cor"))
vote_direction <- compareRemoveMetrics(covariates, col_y = "barrow_vote", col_x = "barrow_direction_agree", return = c("logit", "cor"))
approve_direction <- compareRemoveMetrics(covariates, col_y = "barrow_approve", col_x = "barrow_direction_agree", return = c("R2", "cor"))

student_age <- compareRemoveMetrics(covariates, col_y = "student", col_x = "age", return = c("logit", "cor"))
retired_age <- compareRemoveMetrics(covariates, col_y = "retired", col_x = "age", return = c("logit", "cor"))





## Take two covariates that are likely to be related or draw randomly; get R2
## take out data for one interviewer 


# create quality metrics; show across variables, how much does this person differ from the average
# have different measures of respondent quality; take averages and then rank interviewers


# frequencies of age and gender
# sometimes look at covariance by interviewer; correlation between education and income should be fairly stable
# interview 10 people, then repeat it a bunch of times
# look for identical responses and near identical responses


# also should look at time difference between interviews
