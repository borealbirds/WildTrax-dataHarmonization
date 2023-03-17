# ---
# title: "NL_BMS_2021 Script"
# author: "Maggie MacPherson"
# date created: "July 11, 2022"
# ---
rm(list=ls()) #clear the Global Environment

update.packages()
library(tidyverse)
library(dplyr)
library(plyr) #rbind.fill
library(reshape2) # melt
library(readxl)

## Initialize variables
wd <- "E:\GitHub\WT-Integration"
#do the backslashes work? Melina's template had forward slashes

setwd(wd)

lu <- "./lookupTables"
WT_spTbl <- "./lookupTables/species_codes.csv"

dataset_code <- "NL_BMS_2021" #replaced "xxxxxxxx" from template script with "NL_BMS_2021"
project <- file.path("./project", dataset_code)
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

WT_spTbl <- "./lookupTables/species_codes.csv"


s_location <-read_excel(file.path(project, "xxxxxxxxx.xlsx"))

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
s_location <-read_excel(file.path(project, "xxxxxxxxx.xlsx"))

s_location$site <- 
s_location$station <- 
s_location$location <- paste(dataset_code,s_location$site, s_location$station, sep=":")
s_location$latitude <- 
s_location$longitude <- 
s_location$elevationMeters <- NA #derived from location
s_location$bufferRadiusMeters <- NA
s_location$isHidden <- NA
s_location$trueCoordinates <- NA
s_location$comments <- 
s_location$internal_wildtrax_id <- NA  #created during the upload
s_location$internal_update_ts <- NA #created during the upload

# If exists in source data
s_location$utmZone	<- NA
s_location$easting	<- NA
s_location$northing	<- NA
s_location$missinginlocations <- NA

#---LOCATION
WTlocation <- c("location", "latitude", "longitude", "bufferRadiusMeters", "elevationMeters", "isHidden", "trueCoordinates", "comments")
location_tbl <- s_location[!duplicated(s_location[,WTlocation]), WTlocation] # 

############################
#### VISIT TABLE ####
############################
s_data <- read_excel(file.path(project, "zzzzzzzz.xlsx"))

#Right join (keep only point location that has actual detection)
data_flat <- merge(s_data, s_location, by = "JOIN_FIELD", all.x = TRUE)

# Translate df with species and abundance only. 
data_flat <-data_flat[!is.na(data_flat$species),]
data_flat <-data_flat[!(data_flat$abundance==0),]

## visitDate
data_flat$visitDate <- format(data_flat$Date, format = "%Y-%m-%d")
## snowDepthMeters, waterDepthMeters, landFeatures, crew, bait, accessMethod, comments_visit,wildtrax_internal_update_ts, wildtrax_internal_lv_id
data_flat$snowDepthMeters <- NA  #derived from location at upload
data_flat$waterDepthMeters <- NA  #derived from location at upload
data_flat$landFeatures <- NA  #ARUs attributes
data_flat$crew <- NA   #ARUs attributes
data_flat$bait <- "None"
data_flat$accessMethod <- NA  #ARUs attributes
data_flat$comments <- NA
data_flat$wildtrax_internal_update_ts <- NA  #created during the upload
data_flat$wildtrax_internal_lv_id <- NA #created during the upload
# surveyDateTime, survey_time
data_flat$time_zone <- NA
data_flat$data_origin <- NA
data_flat$missinginvisit <- NA
data_flat$survey_time <- format(data_flat$Start_Time, format = "%H:%M:%S")
data_flat$survey_year <- sub("\\-.*", "", data_flat$visitDate)
## observer, observer_raw
data_flat$observer <- data_flat$local_ObsID
data_flat$rawObserver <- data_flat$Researcher

## pkey_dt -- Concatenate, separated by colons: [location]:[visitDate]_[survey_time]:[localobservercode]; can also use raw observer or observer ID if needed
data_flat$pkey_dt<- paste(data_flat$location, paste0(gsub("-", "", as.character(data_flat$visitDate)),"_", gsub(":", "", data_flat$survey_time)), data_flat$observer, sep=":")

WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- data_flat[!duplicated(data_flat[,WTvisit]), WTvisit] 

############################
#### SURVEY TABLE ####
############################
# Extract observer
data_flat$surveyDateTime <- paste(data_flat$visitDate, data_flat$survey_time)
## Distance and duration Methods
data_flat$distanceMethod <- "0m-50m-INF"
data_flat$durationMethod <- "0-5-10min"
## Species, species_old, comments, scientificname
data_flat <- merge(data_flat, lu_species[,c(1:2)], by.x ="Common_Name", by.y = "species_common_name" , all.x = TRUE)
data_flat$species <- data_flat$species_code
data_flat$comments <- 
data_flat$original_species <- data_flat$Spp
data_flat$scientificname <- data_flat$Scientific_Name

## isHeard isSeen
data_flat$isHeard <- data_flat$heard
data_flat$isSeen <- data_flat$seen

## abundance
data_flat$abundance <- data_flat$Count

# distance and duration interval
data_flat$distanceband <-  
data_flat$durationinterval <- 
data_flat$raw_distance_code <- 
data_flat$raw_duration_code <- 
data_flat$missingindetections <- NA
# Behaviour
data_flat$originalBehaviourData <- 
data_flat$pc_vt <- 
data_flat$pc_vt_detail <- 
data_flat$age <- 
data_flat$fm <- 
data_flat$group <- 
data_flat$flyover <- 
data_flat$displaytype <- 
data_flat$nestevidence <- 
data_flat$behaviourother <- 


#--------------------------------------------------------------
#
#       EXPORT
#
#--------------------------------------------------------------
write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), row.names = FALSE)

WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen", "comments")
survey_tbl <- data_flat[!duplicated(data_flat[,WTsurvey]), WTsurvey] 
write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), row.names = FALSE)


#Only select location with observation
location_tbl <- subset(location_tbl,location_tbl$location %in% survey_tbl$location)
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), row.names = FALSE)

#---EXTENDED
Extended <- c("location", "surveyDateTime", "species", "utmZone", "easting", "northing", "missinginlocations", "time_zone", 
              "data_origin", "missinginvisit", "pkey_dt", "survey_time", "survey_year", "rawObserver", "original_species", 
              "scientificname", "raw_distance_code", "raw_duration_code", "originalBehaviourData", "missingindetections", 
              "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", "displaytype", "nestevidence", "behaviourother")
extended_tbl <- data_flat[!duplicated(data_flat[,Extended]), Extended] 
write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_extended.csv")), row.names = FALSE)




