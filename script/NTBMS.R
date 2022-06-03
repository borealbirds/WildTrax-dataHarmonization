# ---
# title: "Translate New England data to make them ready for WT upload"
# Source dataset is an Excel spreadsheet
# author: "Melina Houle"
# date: "March 11, 2022"
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
library(googledrive)
library(plyr) #rbind.fill

## Initialize variables
wd <- "E:/MelinaStuff/BAM/dataImport"
lu <- "E:/MelinaStuff/git/BAM/BAMTools/data_translation/lookupTables"
WT_spTbl <- "./WT_PointCount_Codes/species_codes.csv"

setwd(wd)
organization_code = "CWS-North"
dataset_code = "NTBMS"
# WildTrax CRS (EPSG: 4386)
crs_WT <- st_crs(4386)
#-----------

#Set working folder. Create if doesn't exist. 
project_dir <- file.path(wd, "project", organization_code)

if (!file.exists(project_dir)) {
  dir.create(project_dir, showWarnings = FALSE)
}

#Load lookup table
#lu_detection <- read.csv(file.path(lu,detection_raw))
#lu_observer <- read_excel(file.path(wd,"lookupTables", observer_raw))
#lu_species <- read.csv(WT_spTbl)

#Download from GoogleDrive
out_path <- file.path(project_dir, paste0(dataset_code,".csv"))
drive_download("NTBMS_HumanPC_Data_working.csv", path = out_path, overwrite = TRUE)
lu_out_path <- file.path(project_dir, paste0(dataset_code,"_lookup.xlsx"))
drive_download("NTBMS-LV-LookUpTables-HB.xlsx", path = lu_out_path, overwrite = TRUE)

#Load dfs  and lu
data_flat <-read.csv(out_path)
lu_observer <- read_excel(lu_out_path, sheet = "ListObserver")
lu_behaviour <- read_excel(lu_out_path, sheet = "ListBehaviour")

# Translate df with species and abundance only. 
data_flat <-data_flat[!is.na(data_flat$Species),]
data_flat <-data_flat[!(data_flat$Abundance==0),]

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
## location [dataset_code:site:survey location]
data_flat$site <- data_flat$Site
data_flat$station <- data_flat$Station
data_flat$location <- paste(dataset_code,data_flat$site,data_flat$station, sep=":")
## UTM zone, Easting, Northing
data_flat$utm_zone <- NA
data_flat$easting <- NA
data_flat$northing <- NA 
## latitude, longitude
data_flat$latitude <- data_flat$Latitude
data_flat$longitude <- data_flat$Longitude
## bufferRadiusMeters, elevationMeters, isHidden, trueCoordinates, comments, internal_wildtrax_id, internal_update_ts
data_flat$bufferRadiusMeters <- NA
data_flat$elevationMeters <- NA
data_flat$isHidden <- NA
data_flat$trueCoordinates <- NA
data_flat$comments_location <- data_flat$Location
data_flat$internal_wildtrax_id <- NA
data_flat$internal_update_ts <- NA
## visitDate
data_flat$visitDate <- format(data_flat$Date, format = "%Y-%m-%d")
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
data_flat$survey_time <- data_flat$PC_Time
data_flat$surveyDateTime <- paste(data_flat$PC_Date, data_flat$survey_time)
## observer, observer_raw
data_flat <- merge(data_flat, lu_observer[,c(2,4)], by.x = "PC_Observer", by.y = "OBSERVER", all.x = TRUE)
data_flat$observer <- data_flat$localobservercode
data_flat$observer_raw <- data_flat$PC_Observer
## pkey_dt -- Concatenate, separated by colons: [location]:[visitDate]_[survey_time]:[localobservercode]; can also use raw observer or observer ID if needed
data_flat$pkey_dt<- paste(data_flat$location, paste0(gsub("-", "", as.character(data_flat$PC_Date)),"_", gsub(":", "", data_flat$survey_time)), data_flat$observer, sep=":")
## Distance and duration Methods
data_flat$distanceMethod <- "0m-50m-100m-INF"
data_flat$durationMethod <- "0-1-2-3-4-5-6-7-8-9-10min"
## Species, species_old, comments, scientificname
data_flat$species <- data_flat$Species
data_flat$comments <- NA
data_flat$Species_Old <- data_flat$PCSpeciesID
data_flat$scientificname <- NA

## isHeard isSeen
data_flat <- merge(data_flat, lu_behaviour, by.x = "Behaviour", by.y ="BEHAVIOUR" , all.x = TRUE) 
data_flat$isHeard <- data_flat$heard
data_flat$isSeen <- data_flat$seen

## abundance
data_flat$abundance <- data_flat$Abundance

# distance and duration interval
data_flat$distanceband <-  sapply(data_flat$Distance_Bin, switch, 
                                  '< 50' = "0m-50m", 
                                  '50m-100m' = "50m-100m",
                                  '> 100m' = "100m-INF",
                                  '' = "UNKNOWN")

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
data_flat$behaviour_raw <- data_flat$Behaviour
data_flat$pc_vt <- data_flat$pc_vt
data_flat$pc_vt_detail <- data_flat$pc_vt_detail
data_flat$age <- data_flat$age
data_flat$fm <- data_flat$fm
data_flat$group <- data_flat$group
data_flat$flyover <- data_flat$flyover
data_flat$displaytype <- data_flat$displaytype
data_flat$nestevidence <- data_flat$nestevidence
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

## Export flat file to GoogleDrive
out_location <- drive_get("NewEngland2012-2019/FLATtranslated")
drive_upload(file.path(wd,"out", paste0(dataset_code, ".csv")),path=as_id(out_location$id),type = 'spreadsheet', name = paste0(dataset_code, ".csv"), overwrite = TRUE)
drive_upload(file.path(wd,"out", "lu_NE-FBMP_observer_updated.csv"),path=as_id(out_location$id),type = 'spreadsheet', name = "lu_NE-FBMP_observer_updated.csv", overwrite = TRUE)



