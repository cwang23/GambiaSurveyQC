#############  GAMBIA ANALYSIS  ####################
## Script to assess Gambia survey geospatial data
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


## --------------< Read In Data >-----------------
geospatial <- read_xlsx("GPSbatch1.xlsx")
batchone <- read_xlsx("gambia_batch1.xlsx", col_types = "text")



## --------------< Munge Data >-----------------
batchone_clean <- batchone %>%
  mutate(interviewer_id = CODE_Enq_CONF,
         interview_day = ymd(substr(STIME, start = 1, stop = 8)),
         gender = Q3,
         education = Q5,
         yearborn = as.integer(Q4))


geo_clean <- geospatial %>%
  transmute(interviewer_id = as.character(`INTERVIEWER ID`),
            interview_day = dmy(`DATE OF INTERVIEW`),
            interview_day_string = as.character(interview_day),
            gender = GENDER,
            yearborn = `YEAR OF BIRTH`,
            lat = LAT,
            lon = LON,
            education = EDUCATION,
            check_dupes = seq(1:nrow(.))) %>%
  left_join(batchone_clean, by = c("interviewer_id", "interview_day", "gender", "education", "yearborn")) %>%
  group_by(check_dupes) %>%
  mutate(dupe = ifelse(n() > 1, 1, 0)) %>% ungroup() %>%
  filter(dupe != 1) %>% # remove duplicates
  mutate(interview_start = lubridate::ymd_hm(STIME),
         interview_end = interview_start + seconds(INTTIME),
         start_year = substr(STIME, 1, 4),
         start_month = substr(STIME, 5, 6),
         start_day = substr(STIME, 7, 8),
         start_hr = substr(STIME, 9, 10),
         start_min = substr(STIME, 11, 12),
         INTTIME = as.numeric(INTTIME),
         superid = 1:nrow(.))
  


## calculate distance between consecutive interviews for each interviewer per day
distance_df <- data.frame("dist_from_prev_interview" = NA, "id" = NA, "lat" = NA, "lon" = NA, "superid" = NA)
for(date in unique(geo_clean$interview_day)){
  subset_date <- geo_clean[geo_clean$interview_day == date,]
  #print(subset_date)
  
  temp <- map_df(.x = unique(subset_date$interviewer_id)[1:length(unique(subset_date$interviewer_id))],
                 .f = calcGeoDistOneGroupConsecutive,
                 groupcol = "interviewer_id",
                 df = subset_date)
  
  distance_df <- bind_rows(distance_df, temp)
}


## --------------< Distance Analysis >-----------------
distance_df <- distance_df %>%
  full_join(geo_clean) %>%
  select(superid, interviewer_id, interview_day, lat, lon, dist_from_prev_interview, INTTIME, interview_start, interview_end) %>%
  filter(!is.na(superid)) %>%
  left_join(timediff, by = c("interviewer_id" = "CODE_Enq_CONF", "interview_start", "interview_end", "INTTIME")) %>%
  arrange(interviewer_id, interview_start) 


## --------------< Make Map >-----------------
# getting the map
background <- get_map(location = c(lon = mean(geo_clean$lon) + 0.5, lat = mean(geo_clean$lat)), 
                      zoom = 8, maptype = "hybrid", scale = 2, color = "bw")

# create map for each interviewer ID
maps <- map(.x = unique(geo_clean$interviewer_id)[1:length(unique(geo_clean$interviewer_id))],
            .f = makeMap,
            df = geo_clean, 
            mapbackground = background)

