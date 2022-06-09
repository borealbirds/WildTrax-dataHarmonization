# ---

# Title: "Translate Northwest Territories bird Monitoring Survey"
# Source dataset is an Excel spreadsheet
# Author: "Melina Houle"
# Date: "March 11, 2022"
#
# **** NOTE: there is not lookuptable for species. Code were extracted from source data. It was not possible to know if code fit 
#            WildTrax species code. 
# ---

update.packages()
library(dplyr) # mutate, %>%
library(utils) #read.csv
library(readxl) #read_excel
library(stringr) #str_replace_all
library(sf) #st_crs, st_as_sf, st_transform, st_drop_geometry
library(purrr) #map
library(plyr) #rbind.fill

## Initialize variables
wd <- "E:/MelinaStuff/BAM/dataImport"
setwd(wd)

dataset_code = "NTBMS"
lu <- "./lookupTables"
WT_spTbl <- "./WT_PointCount_Codes/species_codes.csv"
project <- file.path("./project", dataset_code)
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}
#--------------------------------------------------------------
#       LOAD
#--------------------------------------------------------------
s_data <- file.path(project, "NTBMS_HumanPC_Data_working.csv")
lu_path <- file.path(project, "NTBMS-LV-LookUpTables-HB.xlsx")
data_flat <-read.csv(s_data)
lu_observer <- read_excel(lu_path, sheet = "ListObserver")
lu_behaviour <- read_excel(lu_path, sheet = "ListBehaviour")

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
# Translate df with species and abundance only. 
data_flat <-data_flat[!is.na(data_flat$Species),]
data_flat <-data_flat[!(data_flat$Abundance==0),]

## location [dataset_code:site:survey location]
data_flat$site <- data_flat$Site
data_flat$station <- data_flat$Station
data_flat$location <- paste(dataset_code,data_flat$site,data_flat$station, sep=":")
## latitude, longitude
data_flat$latitude <- data_flat$Latitude
data_flat$longitude <- data_flat$Longitude
## bufferRadiusMeters, elevationMeters, isHidden, trueCoordinates, comments, internal_wildtrax_id, internal_update_ts
data_flat$bufferRadiusMeters <- NA
data_flat$elevationMeters <- NA
data_flat$isHidden <- NA
data_flat$trueCoordinates <- NA
data_flat$comments <- NA
data_flat$internal_wildtrax_id <- NA
data_flat$internal_update_ts <- NA

# If exists in source data
data_flat$utmZone	<- NA
data_flat$easting	<- NA
data_flat$northing	<- NA
data_flat$missinginlocations <- NA

#---LOCATION
WTlocation <- c("location", "latitude", "longitude", "bufferRadiusMeters", "elevationMeters", "isHidden", "trueCoordinates", "comments")
location_tbl <- data_flat[!duplicated(data_flat[,WTlocation]), WTlocation] # 


############################
#### VISIT TABLE ####
############################
data_flat$visitDate <- as.character(format(data_flat$Date, format = "%Y-%m-%d"))
## snowDepthMeters, waterDepthMeters, landFeatures, crew, bait, accessMethod, comments_visit,wildtrax_internal_update_ts, wildtrax_internal_lv_id
data_flat$snowDepthMeters <- NA
data_flat$waterDepthMeters <- NA
data_flat$landFeatures <- NA
data_flat$crew <- NA
data_flat$bait <- "None"
data_flat$accessMethod <- NA
data_flat$comments <- NA
data_flat$wildtrax_internal_update_ts <- NA
data_flat$wildtrax_internal_lv_id <- NA

# surveyDateTime, survey_time
data_flat$time_zone <- NA
data_flat$data_origin <- NA
data_flat$missinginvisit <- NA
data_flat$survey_time <- as.character(data_flat$PC_Time)
data_flat$survey_year <- data_flat$PC_Year
## observer, observer_raw
data_flat <- merge(data_flat, lu_observer[,c(2,4)], by.x = "PC_Observer", by.y = "OBSERVER", all.x = TRUE)
data_flat$observer <- data_flat$localobservercode
data_flat$rawObserver <- data_flat$PC_Observer
## pkey_dt -- Concatenate, separated by colons: [location]:[visitDate]_[survey_time]:[localobservercode]; can also use raw observer or observer ID if needed
data_flat$pkey_dt<- paste(data_flat$location, paste0(gsub("-", "", data_flat$visitDate),"_", gsub(":", "", data_flat$survey_time)), data_flat$observer, sep=":")
## Distance and duration Methods

WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- data_flat[!duplicated(data_flat[,WTvisit]), WTvisit] 

############################
#### SURVEY TABLE ####
############################
data_flat$surveyDateTime <- paste(data_flat$visitDate, data_flat$survey_time)
data_flat$distanceMethod <- "0m-50m-100m-INF"
data_flat$durationMethod <- "0-1-2-3-4-5-6-7-8-9-10min"
## Species, species_old, comments, scientificname
data_flat$species <- data_flat$Species
data_flat$comments <- data_flat$Comments
data_flat$original_species <- data_flat$PCSpeciesID
data_flat$scientificname <- NA

## isHeard isSeen
data_flat <- merge(data_flat, lu_behaviour, by.x = "Behaviour", by.y ="BEHAVIOUR" , all.x = TRUE) 
data_flat$isHeard <- data_flat$heard
data_flat$isSeen <- data_flat$seen

## abundance
data_flat$abundance <- data_flat$Abundance

# distance and duration interval
data_flat$distanceband <-  ifelse(data_flat$Distance == "< 50m", "0m-50m",
                                        ifelse(data_flat$Distance == "50m-100m", "50m-100m",
                                        ifelse(data_flat$Distance == "> 100m", "100m-INF",
                                  "UNKNOWN")))

data_flat$durationinterval <- sapply(data_flat$Time, switch, 
                                     '1 min' = "0-1min", 
                                     '2 min' = "1-2min",
                                     '3 min' = "2-3min",
                                     '4 min' = "3-4min",
                                     '5 min' = "4-5min",
                                     '6 min' = "5-6min",
                                     '7 min' = "6-7min",
                                     '8 min' = "7-8min",
                                     '9 min' = "8-9min",
                                     '10 min' = "9-10min")

data_flat$raw_distance_code <- data_flat$Distance
data_flat$raw_duration_code <- data_flat$Time

# Behaviour
data_flat$originalBehaviourData <- data_flat$Behaviour
data_flat$pc_vt <- data_flat$pc_vt
data_flat$pc_vt_detail <- data_flat$pc_vt_detail
data_flat$age <- data_flat$age
data_flat$fm <- data_flat$fm
data_flat$group <- data_flat$group
data_flat$flyover <- data_flat$flyover
data_flat$displaytype <- data_flat$displaytype
data_flat$nestevidence <- data_flat$nestevidence
data_flat$behaviourother <- data_flat$DESCRIPTION
  
WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
                "durationinterval", "abundance", "isHeard", "isSeen", "comments")
survey_tbl <- data_flat[!duplicated(data_flat[,WTsurvey]), WTsurvey] 

#--------------------------------------------------------------
#
#       EXPORT
#
#--------------------------------------------------------------

#subset columns 
outputName  <- c("organization", "dataset_code", "site", "station", "location", "utm_zone", "easting", "northing", "latitude", "longitude",
                 "bufferRadiusMeters", "elevationMeters", "isHidden", "trueCoordinates", "comments_location", "internal_wildtrax_id", "internal_update_ts",
                 "visitDate", "snowDepthMeters", "waterDepthMeters", "landFeatures", "crew", "bait", "accessMethod", "comments_visit", "wildtrax_internal_update_ts",
                 "wildtrax_internal_lv_id", "pkey_dt", "surveyDateTime", "survey_time", "observer", "observer_raw", "distanceMethod", "durationMethod", 
                 "species", "Species_Old", "scientificname", "isHeard", "isSeen", "abundance", "distanceband", "raw_distance_code", "durationinterval",
                 "raw_duration_code", "comments", "behaviour_raw", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", "displaytype", "nestevidence", "otherbehaviour") 


out <- match(outputName, names(data_flat))
out_translated <- data_flat[,out]

##Export local copy
write.csv(out_translated, file.path(wd,"out", paste0(dataset_code, ".csv")))
write.csv(lu_observer_updated, file.path(wd,"out", "lu_observer_updated.csv"))




