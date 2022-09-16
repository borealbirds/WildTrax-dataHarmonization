# ---
# PCODE: * all Alaska PC from BAM v6
# Title: "Translate Alaska data loaded in BAM v6"
# Source dataset is extracted from BAM v6
# Author: "Melina Houle"
# Date: "July 15, 2022"
# Note on translation:
# --- Data were pre-processed by Hedwig Lankau. Script do not use source data. 
# --- BAM v6 has duplicate entries. The output of that script do not match tables from BAM v6 because the script control for duplicates

library(dplyr) # mutate, %>%
library(utils) #read.csv
library(RODBC) #odbcConnect, sqlFetch
library(stringr) #str_replace_all
library(sf) #st_crs, st_as_sf, st_transform, st_drop_geometry
library(purrr) #map
library(tidyr) #separate
library("googledrive")

## Initialize variables
## Conversion do not use source data. It rather use an unfinished version of BAM V4 db
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

organization_code = "BAM"
dataset_code = "BAM-AK"
source_data <- "BAM-V6.5-USE-20211212.accdb"
lu <- "./lookupTables"
WT_spTbl <- "./lookupTables/species_codes.csv"
project_dir <- file.path(wd, "project", dataset_code)
if (!dir.exists(project_dir)) {
  dir.create(project_dir)
}

data_db <- file.path(project_dir, source_data)
if (!file.exists(data_db)) {
  #Download from GoogleDrive
  drive_download("BAM-V6-Use4Jul2022/BAM-V6.5-USE-20211212.accdb", path = data_db)
}
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

#######################################################
##                    Connect
#######################################################
project <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",data_db))
#Connecte and load tables
    #queries<- sqlQuery(project, "BAM-V6")  ##Look up any queries
    #tbls <- sqlTables(project) ##Look up tables
    #tbls$TABLE_NAME
# Check dataset code for ALASKA
s_dataset <- sqlFetch(project, "dataset") # 5-6-7-8-9-10
s_location <- sqlFetch(project, "location")
pc_visit <- sqlFetch(project, "pc_visit")
pc_survey <- sqlFetch(project, "pc_detection")
#--------------------------------------------------------------
#
#       TRANSLATE
# 
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
s_location <- s_location[s_location$dataset_fk <11 & s_location$dataset_fk >4,]
s_location <- s_location[!is.na(s_location$dataset_fk),]
s_location$location <- s_location$location_name_V6
s_location <- s_location %>%
  separate(location_name_V6, c("project", "site", "station"), ":")

s_location$organization <- organization_code 
s_location$project <- s_location$project
s_location <- s_location[!is.na(s_location$latitude),]
s_location$latitude <- s_location$latitude
s_location$longitude <- s_location$longitude
s_location$elevationMeters <- NA
s_location$bufferRadiusMeters <- NA
s_location$isHidden <- NA
s_location$trueCoordinates <- NA
s_location$comments <- NA
s_location$internal_wildtrax_id <- NA
s_location$internal_update_ts <- NA

# If exists in source data
s_location$utmZone	<- NA
s_location$easting	<- NA
s_location$northing	<- NA
s_location$missinginlocations <- NA


############################
#### VISIT TABLE ####
############################
#Fix column names that have space
names(pc_visit) <-str_replace_all(names(pc_visit), c(" " = "_"))

# Merge location tbl to visit to recover location name
s_visit <- merge(pc_visit, s_location, by.x = "location_fk", by.y = "location_autoid") 
## visitDate
s_visit$visitDate <- as.character(s_visit$survey_date)
## snowDepthMeters, waterDepthMeters, landFeatures, crew, bait, accessMethod, comments_visit,wildtrax_internal_update_ts, wildtrax_internal_lv_id
s_visit$snowDepthMeters <- NA  #derived from location at upload
s_visit$waterDepthMeters <- NA  #derived from location at upload
s_visit$landFeatures <- NA  #ARUs attributes
s_visit$crew <- NA   #ARUs attributes
s_visit$bait <- "None"
s_visit$accessMethod <- NA  #ARUs attributes
s_visit$comments <- s_visit$visit_comments
s_visit$wildtrax_internal_update_ts <- NA  #created during the upload
s_visit$wildtrax_internal_lv_id <- NA #created during the upload
# surveyDateTime, survey_time
s_visit$time_zone <- s_visit$time_zone
s_visit$data_origin <- s_visit$Version_V4
s_visit$missinginvisit <- NA
s_visit$StartTime_V4 <- as.POSIXct(s_visit$StartTime_V4, format = "%Y-%m-%d %H:%M:%S")
s_visit$survey_time <- as.character(format(s_visit$StartTime_V4, format = "%H:%M:%S"))
s_visit$survey_year <- s_visit$survey_year
## observer, observer_raw
s_visit$observer <- s_visit$observer_id
s_visit$rawObserver <- s_visit$obs_V4

## pkey_dt -- Concatenate, separated by colons: [location]:[visitDate]_[survey_time]:[localobservercode]; can also use raw observer or observer ID if needed
s_visit$pkey_dt<- paste(s_visit$location, paste0(gsub("-", "", as.character(s_visit$visitDate)),"_", gsub(":", "", s_visit$survey_time)), s_visit$observer, sep=":")

WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- s_visit[!duplicated(s_visit[,WTvisit]), WTvisit] 


############################
#### SURVEY TABLE ####
############################
names(pc_survey)<-str_replace_all(names(pc_survey), c(" " = "_"))

data_flat <- merge(pc_survey, s_visit, by.x = "pv_visit_fk", by.y = "visit_id" )

# Translate df with species only. In this case, species and abundance always present
data_flat <-data_flat[!is.na(data_flat$species_code),]
data_flat <-data_flat[!is.na(data_flat$abundance),]

data_flat$surveyDateTime <- paste(as.character(data_flat$visitDate), data_flat$survey_time)

## Distance and duration Methods
data_flat$distanceMethod <- data_flat$method_distance
data_flat$durationMethod <- data_flat$method_duration
## Species, species_old, comments, scientificname
data_flat$species <- data_flat$species_code
data_flat$comments <- NA
data_flat$original_species <- NA
data_flat$scientificname <- NA

## isHeard isSeen
data_flat$isHeard <- data_flat$heard
data_flat$isSeen <- data_flat$seen

## abundance
data_flat$abundance <- data_flat$abundance

# distance and duration interval
data_flat$distanceband <- data_flat$distance_band
data_flat$durationinterval <- data_flat$duration_interval
data_flat$raw_distance_code <- NA
data_flat$raw_duration_code <- NA
data_flat$missingindetections <- NA

# Behaviour
data_flat$originalBehaviourData <- data_flat$BehCodeBAMV4
data_flat$pc_vt <- data_flat$pc_vt
data_flat$pc_vt_detail <- data_flat$pc_vt_detail
data_flat$age <- data_flat$age
data_flat$fm <- data_flat$fm
data_flat$group <- data_flat$group
data_flat$flyover <- data_flat$flyover
data_flat$displaytype <- data_flat$displaytype
data_flat$nestevidence <- data_flat$nestevidence
data_flat$behaviourother <- data_flat$behaviourother

WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen", "comments")
survey_tbl <- data_flat[!duplicated(data_flat[,WTsurvey]), WTsurvey] 

#--------------------------------------------------------------
#
#       EXPORT
#
#--------------------------------------------------------------
#Set GoogleDrive id
dr<- drive_get(paste0("toUpload/",organization))
if (nrow(drive_ls(as_id(dr), pattern = dataset_code)) == 0){
  dr_dataset_code <-drive_mkdir(dataset_code, path = as_id(dr), overwrite = NA)
} else {
  dr_dataset_code <- drive_ls(as_id(dr), pattern = dataset_code)
}dr_ls <- drive_ls(as_id(dr), pattern = dataset_code)


#---SURVEY
write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), quote = FALSE, row.names = FALSE)
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv")) 

#---VISIT (only where observation exists)
uniqueVisit <- unique(paste0(data_flat$location, " ", data_flat$survey_date))
visit_tbl <- subset(visit_tbl, paste0(visit_tbl$location, " ", visit_tbl$visitDate) %in% uniqueVisit)
write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), quote = FALSE, row.names = FALSE)
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv")) 

#---LOCATION (only select location with observation)
#---LOCATION
WTlocation <- c("location", "latitude", "longitude", "bufferRadiusMeters", "elevationMeters", "isHidden", "trueCoordinates", "comments")
location_tbl <- s_location[!duplicated(s_location[,WTlocation]), WTlocation] # 

location_tbl <- subset(location_tbl,location_tbl$location %in% survey_tbl$location)
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), quote = FALSE, row.names = FALSE)
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv")) 

#---EXTENDED
Extended <- c("organization", "project","location", "surveyDateTime", "species", "distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code", 
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
              "displaytype", "nestevidence", "behaviourother")
extended_tbl <- data_flat[!duplicated(data_flat[,Extended]), Extended] 
write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_extended.csv")), quote = FALSE, row.names = FALSE)
extended_out <- file.path(out_dir, paste0(dataset_code,"_extended.csv"))
drive_upload(media = extended_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_extended.csv")) 



#### TEST
s_location <- sqlFetch(project, "location")
pc_visit <- sqlFetch(project, "pc_visit")
pc_survey <- sqlFetch(project, "pc_detection")

s_location <- s_location[s_location$dataset_fk <11 & s_location$dataset_fk >4,]
visit <- merge(s_location, pc_visit, by.x = "location_autoid", by.y = "location_fk")
survey <- merge(pc_survey, s_visit, by.x = "pv_visit_fk", by.y = "visit_id" )

survey[survey$location =="AKALMS_DIST:110:11",] 
