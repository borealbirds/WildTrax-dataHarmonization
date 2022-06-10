# ---
# PCODE: MN-BBATLAS
# Title: "Translate Minnesota Breeding Bird Atlas"
# Source dataset for this script was provided by Hedwig Lankau and uses accdb format
# Author: "Melina Houle"
# Date: "March 8, 2022"
# Note on translation:
# --- Data were pre-porcessed by Hedwig Lankau. Script do not run using source data. 
# ---

update.packages()
library(dplyr) # mutate, %>%
library(utils) #read.csv
library(RODBC) #odbcConnect, sqlFetch
library(stringr) #str_replace_all
library(sf) #st_crs, st_as_sf, st_transform, st_drop_geometry
library(purrr) #map

## Initialize variables
## Conversion do not use source data. It rather use an unfinished version of BAM V4 db
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

organization_code = "NRRI"
dataset_code = "MN-BBATLAS"
lu <- "./lookupTables"
WT_spTbl <- "./lookupTables/species_codes.csv"
project <- file.path(wd, "project", dataset_code, "NRRI-MNATLAS-20220303.accdb")
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

# Data CRS: NAD83 UTM15N,EPSG: 26915
crs_utm15N <- st_crs(26915)
# WildTrax CRS (EPSG: 4386)
crs_WT <- st_crs(4386)

#######################################################
##                    Connect
#######################################################
#Connecte and load tables
con <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", project))

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
pc_location <- sqlFetch(con, "location")

# Transform
pc_location <- st_as_sf(pc_location, coords = c("x", "y"), remove = FALSE)
st_crs(pc_location) <- crs_utm15N
pc_location_DD <- st_transform(pc_location, crs_WT)

s_location <- pc_location_DD %>%
  mutate(lat = unlist(map(pc_location_DD$geometry,2)),
         long = unlist(map(pc_location_DD$geometry,1)))

st_drop_geometry(s_location)
s_location$geometry <- NULL

names(s_location)<-str_replace_all(names(s_location), c(" " = "_"))
s_location$site <- s_location$site_name 
s_location$station <- s_location$station_name
s_location$location <- s_location$Location_Name
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
s_location$utmZone	<- s_location$zone
s_location$easting	<- s_location$y
s_location$northing	<- s_location$x
s_location$missinginlocations <- NA

#---LOCATION
WTlocation <- c("location", "latitude", "longitude", "bufferRadiusMeters", "elevationMeters", "isHidden", "trueCoordinates", "comments")
location_tbl <- s_location[!duplicated(s_location[,WTlocation]), WTlocation] # 

############################
#### VISIT TABLE ####
############################
pc_visit <- sqlFetch(con, "pc_visit")#Fix column names that have space
names(pc_visit)<-str_replace_all(names(pc_visit), c(" " = "_"))

## visitDate
pc_visit$location <- pc_visit$Location_Name 
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
pc_visit$observer <- pc_visit$localobservercode
pc_visit$rawObserver <- pc_visit$observer_raw

## pkey_dt -- Concatenate, separated by colons: [location]:[visitDate]_[survey_time]:[localobservercode]; can also use raw observer or observer ID if needed
pc_visit$pkey_dt<- paste(pc_visit$location, paste0(gsub("-", "", as.character(pc_visit$visitDate)),"_", gsub(":", "", pc_visit$survey_time)), pc_visit$observer, sep=":")

WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- pc_visit[!duplicated(pc_visit[,WTvisit]), WTvisit] 


############################
#### SURVEY TABLE ####
############################
pc_detection <- sqlFetch(con, "pc_detection")
names(pc_detection)<-str_replace_all(names(pc_detection), c(" " = "_"))

s_data <- merge(pc_detection, pc_visit, by.x = c("Location_Name", as.character("survey_date")), by.y = c("Location_Name", "visitDate"))
data_flat <- merge(s_data, s_location, by = "Location_Name")

# Translate df with species only. In this case, species and abundance always present
data_flat <-data_flat[!is.na(data_flat$species),]
data_flat <-data_flat[!is.na(data_flat$Abundance),]

data_flat$location <- data_flat$Location_Name
data_flat$surveyDateTime <- paste(as.character(data_flat$visitDate), data_flat$survey_time)

## Distance and duration Methods
data_flat$distanceMethod <- data_flat$distance_protocol
data_flat$durationMethod <- data_flat$duration_protocol
## Species, species_old, comments, scientificname
data_flat$species <- data_flat$species
data_flat$comments <- NA
data_flat$original_species <- data_flat$species_old
data_flat$scientificname <- NA

## isHeard isSeen
data_flat$isHeard <- data_flat$Heard
data_flat$isSeen <- data_flat$Seen

## abundance
data_flat$abundance <- data_flat$Abundance

# distance and duration interval
data_flat$distanceband <- data_flat$distance_band
data_flat$durationinterval <- data_flat$duration_interval
data_flat$raw_distance_code <- data_flat$distance_raw
data_flat$raw_duration_code <- data_flat$time_interval
data_flat$missingindetections <- NA

# Behaviour
data_flat$originalBehaviourData <- data_flat$behaviour_raw
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
#---SURVEY
write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), quote = FALSE, row.names = FALSE)

#---VISIT (only where observation exists)
uniqueVisit <- unique(paste0(data_flat$location, " ", data_flat$survey_date))
visit_tbl <- subset(visit_tbl, paste0(visit_tbl$location, " ", visit_tbl$visitDate) %in% uniqueVisit)
write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), quote = FALSE, row.names = FALSE)

#---LOCATION (only select location with observation)
location_tbl <- subset(location_tbl,location_tbl$location %in% survey_tbl$location)
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), quote = FALSE, row.names = FALSE)

#---EXTENDED
Extended <- c("location", "surveyDateTime", "species", "distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code", 
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
              "displaytype", "nestevidence", "behaviourother")
extended_tbl <- data_flat[!duplicated(data_flat[,Extended]), Extended] 
write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_extended.csv")), quote = FALSE, row.names = FALSE)
