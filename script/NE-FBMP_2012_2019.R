# ---
# title: "Translate New England data to make them ready for WT upload"
# Source dataset is an Excel spreadsheet
# author: "Melina Houle"
# date: "March 11, 2022"
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
detection_raw <- "lu_NE-FBMP_Detection.csv"
observer_raw <- "lu_NE-FBMP_observer.xlsx"
WT_spTbl <- "./WT_PointCount_Codes/species_codes.csv"

setwd(wd)
organization_code = "NE-FBMP"
dataset_code = "NE-FBMP_2012_2019"
# WildTrax CRS (EPSG: 4386)
crs_WT <- st_crs(4386)
#-----------

#Set working folder. Create if doesn't exist. 
project_dir <- file.path(wd, "project", organization_code)

if (!file.exists(project_dir)) {
  dir.create(project_dir, showWarnings = FALSE)
}

#Load lookup table
lu_detection <- read.csv(file.path(lu,detection_raw))
lu_observer <- read_excel(file.path(wd,"lookupTables", observer_raw))
lu_species <- read.csv(WT_spTbl)

#Download from GoogleDrive
out_path <- file.path(project_dir, paste0(dataset_code,".xlsx"))
drive_download("SourceData/VTFBMP_VCE_2012-2019.xlsx", path = out_path, overwrite = TRUE)

#Load dfs and fix column names that have space
detection_df <-read_excel(out_path, sheet = "Bird Data")
names(detection_df)<-str_replace_all(names(detection_df), c(" " = "_"))

location_df <-read_excel(out_path, sheet = "Site Data")
names(location_df)<-str_replace_all(names(location_df), c(" " = "_"))

#Right join (keep only point location that has actual detection)
data_flat <- merge(location_df, detection_df, by = "Point_Number", all.y = TRUE)

# Translate df with species and abundance only. 
data_flat <-data_flat[!is.na(data_flat$Spp),]
data_flat <-data_flat[!(data_flat$Count==0),]

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
data_flat$site <- data_flat$Transect 
data_flat$station <- NA
data_flat$location <- paste(dataset_code,data_flat$site,data_flat$Point_Number, sep=":")
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
data_flat$comments_location <- data_flat$Short_Name
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
data_flat$survey_time <- format(data_flat$Start_Time, format = "%H:%M:%S")
data_flat$surveyDateTime <- paste(data_flat$visitDate, data_flat$survey_time)
## pkey_dt -- Concatenate, separated by colons: [location]:[visitDate]_[survey_time]:[localobservercode]; can also use raw observer or observer ID if needed
data_flat$pkey_dt<- paste(data_flat$location, paste0(gsub("-", "", as.character(data_flat$visitDate)),"_", gsub(":", "", data_flat$survey_time)), data_flat$localobservercode, sep=":")
## observer, observer_raw
data_flat$LName <- sub("\\,.*", "", data_flat$Researcher)
data_flat$FName <- sub(".*,", "", data_flat$Researcher)
data_flat$FName <- gsub(" ", "", data_flat$FName, fixed = TRUE)

# Determine the ones not in lookuptable and add them
new_observer <-unique(subset(data_flat$Researcher, !(data_flat$LName %in% lu_observer$LName) & !(data_flat$FName %in% lu_observer$FName)))
new_obs <- data.frame(local_ObsID=max(lu_observer$local_ObsID) + seq.int(length(new_observer)),
                      LName=sub("\\,.*", "", new_observer),
                      FName=sub(".*,", "", new_observer))
new_obs$FName <- gsub(" ", "", new_obs$FName, fixed = TRUE)
lu_observer_updated <-rbind.fill(lu_observer, new_obs)
# Extract observer
data_flat <- merge(data_flat, lu_observer_updated[,c(1,3:4)], by = c("FName","LName"), all.x = TRUE)
data_flat$observer <- data_flat$local_ObsID
data_flat$observer_raw <- data_flat$Researcher
## Distance and duration Methods
data_flat$distanceMethod <- "0m-50m-INF"
data_flat$durationMethod <- "0-5-10min"
## Species, species_old, comments, scientificname
data_flat <- merge(data_flat, lu_species[,c(1:2)], by.x ="Common_Name", by.y = "species_english_name" , all.x = TRUE)
data_flat$species <- data_flat$species_code
#Hard coded fix code not found in Species Table
data_flat[data_flat$Common_Name=="Chipmunk", "species_code"] <- "UNMA"
data_flat[data_flat$Common_Name=="Unknown sp.", "species_code"] <- "UNBI"
data_flat[data_flat$Common_Name=="Unid. Woodpecker", "species_code"] <- "UNWO"
data_flat[data_flat$Common_Name=="Slate-colored Junco", "species_code"] <- "DEJU"
data_flat[data_flat$Common_Name=="Ground Squirrels", "species_code"] <- "UNMA"
data_flat[data_flat$Common_Name=="Solitary Vireo", "species_code"] <- "SOVI"


data_flat$comments <- data_flat$Point_Note
data_flat$Species_Old <- data_flat$Spp
data_flat$scientificname <- data_flat$Scientific_Name

## isHeard isSeen
data_flat <- merge(data_flat, lu_detection, by = "Detection_Cue", all.x = TRUE) 
data_flat$isHeard <- data_flat$heard
data_flat$isSeen <- data_flat$seen

## abundance
data_flat$abundance <- data_flat$Count

# distance and duration interval
data_flat$distanceband <-  sapply(data_flat$Distance_Bin, switch, 
                                  '<50' = "0m-50m", 
                                  '>50' = "50m-INF",
                                  '<1000' = "UNKNOWN")

data_flat$durationinterval <- sapply(data_flat$Time_Bin, switch, 
                                     '0_3min' = "0-3min", 
                                     '3_5min' = "3-5min",
                                     '5_10min' = "5-10min")

data_flat$raw_distance_code <- data_flat$Distance_Bin
data_flat$raw_duration_code <- data_flat$Time_Bin

# Behaviour
data_flat$behaviour_raw <- data_flat$Detection_Cue
data_flat$pc_vt <- data_flat$vt
data_flat$pc_vt_detail <- data_flat$pc_vt_detail
data_flat$fm <- data_flat$fm
data_flat$group <- data_flat$group
data_flat$flyover <- data_flat$flyover
data_flat$displaytype <- data_flat$displaytype
data_flat$nestevidence <- data_flat$nestevidence
data_flat$otherbehaviour <- data_flat$otherbehaviour
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
write.csv(lu_observer_updated, file.path(wd,"out", "lu_NE-FBMP_observer_updated.csv"))

## Export flat file to GoogleDrive
out_location <- drive_get("NewEngland2012-2019/FLATtranslated")
drive_upload(file.path(wd,"out", paste0(dataset_code, ".csv")),path=as_id(out_location$id),type = 'spreadsheet', name = paste0(dataset_code, ".csv"), overwrite = TRUE)
drive_upload(file.path(wd,"out", "lu_NE-FBMP_observer_updated.csv"),path=as_id(out_location$id), name = "lu_NE-FBMP_observer_updated.csv", overwrite = TRUE)



