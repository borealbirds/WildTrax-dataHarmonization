# ---
# title: "Translate Minnesota Atlas data to make them ready for WT upload"
# Source dataset for this script was provided by Hedwig Lankau and uses accdb format
# author: "Melina Houle"
# date: "March 8, 2022"
# ---

update.packages()
library(dplyr) # mutate, %>%
library(utils) #read.csv
library(RODBC) #odbcConnect, sqlFetch
library(stringr) #str_replace_all
library(sf) #st_crs, st_as_sf, st_transform, st_drop_geometry
library(purrr) #map
library(googledrive)

## Initialize variables
## Conversion do not use source data. It rather use an unfinished version of BAM V4 db
wd <- "E:/MelinaStuff/BAM/dataImport"
setwd(wd)
organization_code = "NRRI"
dataset_code = "NRRI_MNATLAS"
# Data CRS: NAD83 UTM15N,EPSG: 26915
crs_utm15N <- st_crs(26915)
# WildTrax CRS (EPSG: 4386)
crs_WT <- st_crs(4386)
#-----------

#Download from GoogleDrive
out_path <- file.path(wd, "project", organization_code, paste0(dataset_code,".accdb"))
drive_download("NRRI-MNATLAS.accdb", path = out_path, overwrite = TRUE)

#Connecte and load tables
con <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", out_path))
pc_visit <- sqlFetch(con, "pc_visit")
pc_location <- sqlFetch(con, "location")
pc_detection <- sqlFetch(con, "pc_detection")

# Transform
pc_location <- st_as_sf(pc_location, coords = c("x", "y"), remove = FALSE)
st_crs(pc_location) <- crs_utm15N
pc_location_DD <- st_transform(pc_location, crs_WT)

pc_location_flat <- pc_location_DD %>%
  mutate(lat = unlist(map(pc_location_DD$geometry,2)),
         long = unlist(map(pc_location_DD$geometry,1)))

st_drop_geometry(pc_location_flat)
pc_location_flat$geometry <- NULL

s_data <- merge(pc_location_flat, pc_visit[,3:14], by = "Location Name", all = TRUE)
data_flat <- merge(s_data, pc_detection[,c(4,6:26)], by = "Visit-PKEY", all = TRUE)

# Fix column name using space in df
names(data_flat)<-str_replace_all(names(data_flat), c(" " = "_"))

# Translate df with species only. In this case, species and abundance always present
data_flat <-data_flat[!is.na(data_flat$species),]
data_flat <-data_flat[!is.na(data_flat$Abundance),]

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
#### SET  COLUMN ####
## organization
data_flat$organization <- organization_code
## dataset code
data_flat$dataset_code <-dataset_code
## location
data_flat$site <- data_flat$site_name
data_flat$station <- data_flat$station_name
data_flat$location <- data_flat$Location_Name
## UTM zone, Easting, Northing
data_flat$utm_zone <- data_flat$zone 
data_flat$easting <- data_flat$x
data_flat$northing <- data_flat$y 
## latitude, longitude
data_flat$latitude <- data_flat$lat
data_flat$longitude <- data_flat$long
## bufferRadiusMeters, elevationMeters, isHidden, trueCoordinates, comments, internal_wildtrax_id, internal_update_ts
data_flat$bufferRadiusMeters <- NA
data_flat$elevationMeters <- NA
data_flat$isHidden <- NA
data_flat$trueCoordinates <- NA
data_flat$comments_location <- NA
data_flat$internal_wildtrax_id <- NA
data_flat$internal_update_ts <- NA
## visitDate
data_flat$visitDate <- format(as.Date(data_flat$survey_date) ,format = "%Y-%m-%d")
## snowDepthMeters, waterDepthMeters, landFeatures, crew, bait, accessMethod, comments_visit,wildtrax_internal_update_ts, wildtrax_internal_lv_id
data_flat$snowDepthMeters <- NA
data_flat$waterDepthMeters <- NA
data_flat$landFeatures <- NA
data_flat$crew <- NA
data_flat$bait <- NA
data_flat$accessMethod <- NA
data_flat$comments_visit <- NA
data_flat$wildtrax_internal_update_ts <- NA
data_flat$wildtrax_internal_lv_id <- NA
# surveyDateTime, survey_time
data_flat$survey_time <- format(data_flat$survey_time, format = "%H:%M:%S")
data_flat$surveyDateTime <- paste(data_flat$visitDate, data_flat$survey_time)
## pkey_dt -- Concatenate, separated by colons: [location]:[visitDate]_[survey_time]:[localobservercode]; can also use raw observer or observer ID if needed
data_flat$pkey_dt<- paste(data_flat$location, paste0(gsub("-", "", as.character(data_flat$visitDate)),"_", gsub(":", "", data_flat$survey_time)), data_flat$localobservercode, sep=":")
## observer, observer_raw
data_flat$observer <- data_flat$localobservercode
data_flat$observer_raw <- data_flat$observer_raw
## Distance and duration Methods
data_flat$distanceMethod <- data_flat$distance_protocol
data_flat$durationMethod <- data_flat$duration_protocol
## Species, species_old, comments, scientificname
data_flat$species <- data_flat$species
data_flat$comments <- NA
data_flat$Species_Old <- data_flat$species_old
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
  
# Behaviour
data_flat$behaviour_raw <- data_flat$behaviour_raw
data_flat$pc_vt <- data_flat$pc_vt
data_flat$pc_vt_detail <- data_flat$pc_vt_detail
data_flat$age <- data_flat$age
data_flat$fm <- data_flat$fm
data_flat$group <- data_flat$group
data_flat$flyover <- data_flat$flyover
data_flat$displaytype <- data_flat$displaytype
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
                 "raw_duration_code", "comments", "behaviour_raw", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", "displaytype") 


out <- match(outputName, names(data_flat))
out_translated <- data_flat[,out]

##Export local copy
write.csv(out_translated, file.path(wd,"out", paste0(dataset_code, ".csv")))

## Export flat file to GoogleDrive
out_location <- drive_get("NRRI Grinde/FLATtranslated")
drive_upload(file.path(wd,"out", paste0(dataset_code, ".csv")),path=as_id(out_location$id),type = 'spreadsheet', name = paste0(dataset_code, ".csv"), overwrite = TRUE)



