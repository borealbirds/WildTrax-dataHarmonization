# ---
# PCODE: ECCC-ON-Between-Rivers
# Title: "Between Rivers Landbird Sampling in the Hudson Plains"
# Source dataset for this script was provided by Hedwig Lankau and uses accdb format
# Author: "Ana Raymundo, Melina Houle"
# Date: "July 20th, 2022"
# ---

update.packages()
library(dplyr) # mutate, %>%
library(utils) #read.csv
library(RODBC) #odbcConnect, sqlFetch
library(stringr) #str_replace_all
library(sf) #st_crs, st_as_sf, st_transform, st_drop_geometry
library(purrr) #map
library(googledrive) #drive_upload

## Initialize variables
#wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
wd <- getwd()
setwd(wd)

organization = "CWS-ONT"
dataset_code = "ON-Between-Rivers"
source_data <- 'BetweenRivers.accdb'
lu <- "./lookupTables"
WT_spTbl <- "./lookupTables/species_codes.csv"

#set working folder
project_dir <- file.path(wd, "project", dataset_code)
if (!dir.exists(project_dir)) {
  dir.create(project_dir)
}
data_db <- file.path(project_dir, source_data)
if (!file.exists(data_db)) {
  #Download from GoogleDrive
  drive_download(paste0("sourceData/",source_data), path = data_db)
}
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

# Data CRS: NAD83 UTM15N,EPSG: 26915
crs_NAD83 <- st_crs(8232)
# WildTrax CRS (EPSG: 4386)
crs_WT <- st_crs(4386)

#######################################################
##                    Connect
#######################################################
#Connect and load tables
con <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", data_db))
#--------------------------------------------------------------
#queries<- sqlQuery(project, "BAM-V6")  ##Look up any queries
tbls <- sqlTables(con) ##Look up tables
tbls$TABLE_NAME
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
pc_location <- sqlFetch(con, "BR_location")

# Transform
pc_location <- st_as_sf(pc_location, coords = c("Longitude", "Latitude"), remove = FALSE)
st_crs(pc_location) <- crs_NAD83
pc_location_DD <- st_transform(pc_location, crs_WT)

s_location <- pc_location_DD %>%
  mutate(lat = unlist(map(pc_location_DD$geometry,2)),
         long = unlist(map(pc_location_DD$geometry,1)))

st_drop_geometry(s_location)
s_location$geometry <- NULL

s_location$site <- s_location$Site
s_location$station <- NA
s_location$location <- s_location$location_name
s_location$latitude <- s_location$lat
s_location$longitude <- s_location$long
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

#---LOCATION
WTlocation <- c("location", "latitude", "longitude", "bufferRadiusMeters", "elevationMeters", "isHidden", "trueCoordinates", "comments")
location_tbl <- s_location[!duplicated(s_location[,WTlocation]), WTlocation] # 

############################
#### VISIT TABLE ####
############################
pc_visit <- sqlFetch(con, "BR_visit")#Fix column names that have space

## visitDate
pc_visit$location <- pc_visit$location_name 
pc_visit$visitDate <- as.character(format(pc_visit$survey_date, format = "%Y-%m-%d"))
## snowDepthMeters, waterDepthMeters, landFeatures, crew, bait, accessMethod, comments_visit,wildtrax_internal_update_ts, wildtrax_internal_lv_id
pc_visit$snowDepthMeters <- NA  #derived from location at upload
pc_visit$waterDepthMeters <- NA  #derived from location at upload
pc_visit$landFeatures <- NA  #ARUs attributes
pc_visit$crew <- NA   #ARUs attributes
pc_visit$bait <- "None"
pc_visit$accessMethod <- NA  #ARUs attributes
pc_visit$comments <- NA
pc_visit$wildtrax_internal_update_ts <- NA  #created during the upload
pc_visit$wildtrax_internal_lv_id <- NA #created during the upload
# surveyDateTime, survey_time
pc_visit$time_zone <- NA
pc_visit$data_origin <- NA
pc_visit$missinginvisit <- NA
pc_visit$survey_time <- as.character(format(pc_visit$survey_time, format = "%H:%M:%S"))
pc_visit$survey_year <- pc_visit$survey_year
## observer, observer_raw
pc_visit$observer <- pc_visit$ObserverID
pc_visit$rawObserver <- pc_visit$ObserverID

## pkey_dt -- Concatenate, separated by colons: [location]:[visitDate]_[survey_time]:[localobservercode]; can also use raw observer or observer ID if needed
pc_visit$pkey_dt<- paste(pc_visit$location, paste0(gsub("-", "", as.character(pc_visit$visitDate)),"_", gsub(":", "", pc_visit$survey_time)), pc_visit$observer, sep=":")

WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- pc_visit[!duplicated(pc_visit[,WTvisit]), WTvisit] 

############################
#### SURVEY TABLE ####
############################
pc_detection <- sqlFetch(con, "BR_detection_grouped")

s_data <-merge(pc_detection, s_location[,c("location_name","Site","Latitude", "Longitude", "lat", "long")], by ="location_name", all.x = TRUE)

data_flat <- merge(s_data, pc_visit, by = c('PKEY'))

# Translate df with species only. In this case, species and abundance always present
data_flat <-data_flat[!is.na(data_flat$SpeciesID),]
data_flat <-data_flat[!is.na(data_flat$Count),]

data_flat$surveyDateTime <- paste(as.character(data_flat$survey_date), data_flat$survey_time)

## Distance and duration Methods
data_flat$distanceMethod <- NA
data_flat$durationMethod <-NA
## Species, species_old, comments, scientificname
data_flat$species <- data_flat$BAM_species ##TODO: ASK HEDWING
data_flat$comments <- NA
data_flat$original_species <- data_flat$SpeciesID
data_flat$scientificname <- NA #TODO: IF THE SCIENTIFIC NAMES IS NOT IN THE RAW DATA DO WE FILL THIS FIELD WITH THE WILDTRAX LOOKUP TABLE

## isHeard isSeen
data_flat$isHeard <- data_flat$heard
data_flat$isSeen <- data_flat$seen

## abundance
data_flat$abundance <- data_flat$Count

# distance and duration interval  
#TODO: PENDING 
# data_flat$distanceband <- data_flat$distance_band
# data_flat$durationinterval <- data_flat$duration_interval
# data_flat$raw_distance_code <- data_flat$distance_raw
# data_flat$raw_duration_code <- data_flat$time_interval
# data_flat$missingindetections <- NA
# Just for testing purposes, waiting for answer from Hedwing
data_flat$distanceband <- NA
data_flat$durationinterval <- NA
data_flat$raw_distance_code <- NA
data_flat$raw_duration_code <- NA
data_flat$missingindetections <- NA

# Behaviour
data_flat$originalBehaviourData <- data_flat$Behaviour
data_flat$pc_vt <- data_flat$howheard
data_flat$pc_vt_detail <- NA
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
#Extract GoogleDrive id to store output
dr<- drive_get(paste0("toUpload/",organization))

if (nrow(drive_ls(as_id(dr), pattern = dataset_code)) == 0){
  dr_dataset_code <-drive_mkdir(dataset_code, path = as_id(dr), overwrite = NA)
} else {
  dr_dataset_code <- drive_ls(as_id(dr), pattern = dataset_code)
}

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
location_tbl <- subset(location_tbl,location_tbl$location %in% survey_tbl$location)
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), quote = FALSE, row.names = FALSE)
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"))

#---EXTENDED
Extended <- c("location", "surveyDateTime", "species", "distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code", 
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
              "displaytype", "nestevidence", "behaviourother")
extended_tbl <- data_flat[!duplicated(data_flat[,Extended]), Extended] 
write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_extended.csv")), quote = FALSE, row.names = FALSE)
extended_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = extended_tbl, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"))
