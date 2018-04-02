#############  GAMBIA ANALYSIS  ####################
## Script to assess Gambia survey responses
## Civis Analytics
## author: Clara Wang
## Spring 2018


## --------------< Set-up Workspace >-----------------
rm(list=ls())

dyn.load(paste0(system2('/usr/libexec/java_home', stdout = TRUE), '/jre/lib/server/libjvm.dylib'))
library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
library(purrr)
library(reshape2)
library(lubridate)
library(civis)
library(civis.deckR)


setwd("~/Desktop/Gambia/GambiaSurveyQC")

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

pctmatch_analysis <- grouped_pctmatch %>%
  left_join(select(all_pctmatch, "allpctmatches" = pct_matches, "ID"), by = c("ID" = "ID")) %>%
  group_by(Group) %>%
  mutate("avg_pctmatch" = mean(pct_matches)) %>% ungroup() %>%
  mutate("avg_pctmatch_removeNAs" = coalesce(avg_pctmatch, allpctmatches)) %>%
  select("interviewer_id" = Group, "avg_pctmatch", "avg_pctmatch_removeNAs") %>% distinct()

# according to Kuriakose and Robbins, don't want to exceed 85% matches


## -------------< Compare Means and Variance for Different Answers by Interviewer >----------------

question_cols_subset <- question_cols[!(question_cols %in% c("Q95", "Q91", "Q90", "Q89", "Q88", "Q87", "Q86", "Q85", "Q1", "Q1_1", "Q4"))]

mean_analysis <- clean %>%
  mutate_at(.funs = funs("recode" = recodeToNumeric), .vars = c(question_cols_subset)) %>% ungroup() %>%
  select(CODE_Enq_CONF, paste0(question_cols_subset, "_recode")) %>%
  gather(question, value, -CODE_Enq_CONF) %>%
  group_by(question, CODE_Enq_CONF) %>%
  summarise("interviewer_mean" = mean(value, na.rm = TRUE)) %>% ungroup() %>%
  group_by(question) %>%
  mutate("mean_interviewer_mean" = mean(interviewer_mean, na.rm = TRUE),
         "sd_of_means" = sd(interviewer_mean, na.rm = TRUE)) %>% ungroup() %>%
  mutate("sd_from_mean" = (interviewer_mean - mean_interviewer_mean) / sd_of_means) %>%
  group_by(CODE_Enq_CONF) %>%
  summarise("mean_sd_from_mean" = mean(sd_from_mean, na.rm = TRUE))


## maybe should not include ##
var_analysis <- clean %>%
  mutate_at(.funs = funs(recode = recodeToNumeric), .vars = c(question_cols_subset)) %>%
  select(CODE_Enq_CONF, paste0(question_cols_subset, "_recode")) %>%
  gather(question, value, -CODE_Enq_CONF) %>%
  group_by(CODE_Enq_CONF, question) %>%
  summarise("var" = var(value, na.rm = TRUE)) %>% ungroup() %>%
  group_by(question) %>%
  mutate("sd_var" = sd(var, na.rm = TRUE),
         "mean_var" = mean(var, na.rm = TRUE)) %>% ungroup() %>%
  mutate("sd_from_meanvar" = var - mean_var / sd_var) %>%
  group_by(CODE_Enq_CONF) %>%
  summarise("mean_sd_from_meanvar" = mean(sd_from_meanvar, na.rm = TRUE)) %>% ungroup()

# clean_var_all <- clean %>%
#   mutate_at(.funs = funs(recode = recodeToNumeric), .vars = c(question_cols_subset)) %>%
#   summarise_at(.funs = funs(var = var), .vars = paste0(c(question_cols_subset), "_recode"), na.rm = TRUE)
#   
# 
# for(q in question_cols_subset){
#   colname <- paste0(q, "_recode_mean")
#   clean_mean[[q]] <- clean_mean[[colname]] - clean_mean_all[[colname]]
# }
# mean_analysis <- clean_mean %>%
#   select(CODE_Enq_CONF, question_cols_subset) %>% 
#   gather(question, value, -CODE_Enq_CONF) %>%
#   group_by(CODE_Enq_CONF) %>%
#   summarise(mean_mean_diff = mean(value, na.rm = TRUE))
# 
# 
# for(q in question_cols_subset){
#   colname <- paste0(q, "_recode_var")
#   clean_var[[q]] <- clean_var[[colname]] - clean_var_all[[colname]]
# }
# var_analysis <- clean_var %>%
#   select(CODE_Enq_CONF, question_cols_subset) %>% 
#   gather(question, value, -CODE_Enq_CONF) %>%
#   group_by(CODE_Enq_CONF) %>%
#   summarise(mean_var_diff = mean(value, na.rm = TRUE))


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
name_cols <- grep("NAME_", names(clean), value = TRUE)
age_cols <- grep("AGE_", names(clean), value = TRUE)
gender_cols <- grep("GENDER_", names(clean), value = TRUE)

want_cols <- c(name_cols, age_cols, gender_cols)

check2 <- timediff %>%
  filter(CODE_Enq_CONF %in% check[[1]]) %>%
  arrange(CODE_Enq_CONF, interview_start) %>%
  select("CODE_Enq_CONF", "previous_interview_end", "interview_start", "interview_end", "secs_since_prev_interview", "INTTIME", want_cols)

time_check <- compareDistribs(clean, "INTTIME")

temp <- unlist(time_check)
temp <- temp[grepl("p_val", names(temp))]
temp <- data.frame("p_val" = unlist(temp))
temp$interviewer_id <- row.names(temp)
time_analysis_ttest <- temp %>% 
  separate(interviewer_id, c("interviewer_id", "remove"), "[.]") %>%
  select(interviewer_id, p_val) %>%
  mutate("signif" = ifelse(p_val <= 0.05, 1, 0))


time_analysis_mean <- clean %>%
  group_by(CODE_Enq_CONF) %>%
  summarise("interviewer_mean_inttime" = mean(INTTIME, na.rm = TRUE)) %>% ungroup() %>%
  mutate("mean_mean_inttime" = mean(interviewer_mean_inttime, na.rm = TRUE),
         "sd_of_means" = sd(interviewer_mean_inttime, na.rm = TRUE),
         "sd_from_mean" = (interviewer_mean_inttime - mean_mean_inttime) / sd_of_means)


# -------------< bivariate analysis >-------------

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


logit_cor <- c("interviewer_id", "diff_cor", "diff_nulldeviance")
R2_cor <- c("interviewer_id", "diff_cor", "diff_R2")

vote_approve <- compareRemoveMetrics(covariates, col_y = "barrow_vote", col_x = "barrow_approve", return = c("logit", "cor")) %>% select(logit_cor)
vote_direction <- compareRemoveMetrics(covariates, col_y = "barrow_vote", col_x = "barrow_direction_agree", return = c("logit", "cor")) %>% select(logit_cor)
approve_direction <- compareRemoveMetrics(covariates, col_y = "barrow_approve", col_x = "barrow_direction_agree", return = c("R2", "cor")) %>% select(R2_cor)

student_age <- compareRemoveMetrics(covariates, col_y = "student", col_x = "age", return = c("logit", "cor")) %>% select(logit_cor)
retired_age <- compareRemoveMetrics(covariates, col_y = "retired", col_x = "age", return = c("logit", "cor")) %>% select(logit_cor)

bivariate_analysis <- select(vote_approve, "interviewer_id", "diff_nulldeviance_voteapprove" = diff_nulldeviance) %>%
  left_join(select(vote_direction, "interviewer_id", "diff_nulldeviance_votedirection" = diff_nulldeviance), by = "interviewer_id") %>%
  left_join(select(approve_direction, "interviewer_id", "diff_R2_approvedirection" = diff_R2), by = "interviewer_id") %>%
  left_join(select(student_age, "interviewer_id", "diff_nulldeviance_studentage" = diff_nulldeviance), by = "interviewer_id") %>%
  left_join(select(retired_age, "interviewer_id", "diff_nulldeviance_retiredage" = diff_nulldeviance), by = "interviewer_id")
bivariate_analysis <- bivariate_analysis %>%
  mutate_at(.vars = grep("diff", names(.), value = TRUE), .funs = abs)


tab <- bivariate_analysis %>% 
  left_join(select(pctmatch_analysis, "interviewer_id", "avg_pctmatch_removeNAs"), by = "interviewer_id") %>%
  left_join(select(mean_analysis, "interviewer_id" = CODE_Enq_CONF, "questionsmean_sd_from_mean" = mean_sd_from_mean), by = "interviewer_id") %>%
  left_join(select(time_analysis_mean, "interviewer_id" = CODE_Enq_CONF, "inttime_sd_from_mean" = sd_from_mean), by = "interviewer_id") 
tab <- tab %>%
  mutate_at(.vars = names(.)[!grepl("_id", names(.))], .funs = abs) %>%
  mutate_at(.vars = names(.)[!grepl("_id", names(.))], .funs = round, 2) %>%
  civis_table() %>% 
  color_table_palette(2) %>%
  color_table_palette(3) %>%
  color_table_palette(4) %>%
  color_table_palette(5) %>%
  color_table_palette(6) %>%
  color_table_palette(7) %>%
  color_table_palette(8) %>%
  color_table_palette(9)
  


#### DELIVERABLE ####
d <- deliverable("The Gambia Survey Analysis (first 200)") %u%
  unit(title = "Interviewer Metrics",
       object1 = tab)

to_word(d, "gambia_200")





# don't want percent match to be above a certain amount 
# time analysis
# bivariate analysis -- 












## Take two covariates that are likely to be related or draw randomly; get R2
## take out data for one interviewer 


# create quality metrics; show across variables, how much does this person differ from the average
# have different measures of respondent quality; take averages and then rank interviewers


# frequencies of age and gender
# sometimes look at covariance by interviewer; correlation between education and income should be fairly stable
# interview 10 people, then repeat it a bunch of times
# look for identical responses and near identical responses


# also should look at time difference between interviews
