# ---
# title: "Translate New England data to make them ready for WT upload"
# Source dataset is an Excel spreadsheet
# author: "Melina Houle"
# date: "March 11, 2022"
# Note on translation:
# -- Download manually source data locally prior to process
# -- Species code used are not equivalent to WildTrax species_code. Species codes need to be derived by using species common name
# -- 3 stations use concatenation of transect name + number instead of a numeric Point_Count (ADAMSCAMP, BAKERBUSH, CRAFTBURYOU). Fix is hardcoded. 
# -- 
# -- 
# ---

library(dplyr) # mutate, %>%
library(utils) #read.csv
library(readxl) #read_excel
library(stringr) #str_replace_all
library(sf) #st_crs, st_as_sf, st_transform, st_drop_geometry
library(purrr) #map
library(plyr) #rbind.fill
library(data.table)

## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

lu <- "./lookupTables"
WT_spTbl <- "./WT_PointCount_Codes/species_codes.csv"
detection_raw <- "lu_NEFBMP_detection.csv"
observer_raw <- "lu_NEFBMP_observer.xlsx"

organization_code = "BAM"
dataset_code = "NEFBMP2012-19"
project <- file.path("./project", dataset_code)
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}


#Load lookup table and source data
lu_detection <- fread(file.path(lu,detection_raw))
lu_observer <- read_excel(file.path(wd,"lookupTables", observer_raw))
lu_species <- fread(WT_spTbl)

# WildTrax CRS (EPSG: 4386)
crs_WT <- st_crs(4386)

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
s_location <-read_excel(file.path(project, "NEFBMP2012-19.xlsx"), sheet = "Site Data", col_types = c("text") )
names(s_location)<-str_replace_all(names(s_location), c(" " = "_"))

s_location$site <- s_location$Short_Name 
s_location$station <- s_location$Point_Number
s_location$location <- paste(dataset_code,s_location$site, s_location$station, sep=":")
s_location$latitude <- s_location$Latitude
s_location$longitude <- s_location$Longitude
s_location$elevationMeters <- NA
s_location$bufferRadiusMeters <- NA
s_location$isHidden <- NA
s_location$trueCoordinates <- NA
s_location$comments <- s_location$Name
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
s_data <- read_excel(file.path(project, "NEFBMP2012-19.xlsx"), sheet = "Bird Data",  col_types = c("text", "text", "text", "text", "text", "date", "guess", "guess", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text"))
#Fix column names that have space
names(s_data)<-str_replace_all(names(s_data), c(" " = "_"))

# Hardcoded fix on Point_Count 
s_data[s_data$Point_Number=="BAKERBUSHPT1", "Point_Number"] <- "11801"
s_data[s_data$Point_Number=="BAKERBUSHPT2", "Point_Number"] <- "11802"
s_data[s_data$Point_Number=="BAKERBUSHPT3", "Point_Number"] <- "11803"
s_data[s_data$Point_Number=="BAKERBUSHPT4", "Point_Number"] <- "11804"
s_data[s_data$Point_Number=="BAKERBUSHPT5", "Point_Number"] <- "11805"
s_data[s_data$Point_Number=="ADAMSCAMPPT1", "Point_Number"] <- "11901"
s_data[s_data$Point_Number=="ADAMSCAMPPT2", "Point_Number"] <- "11902"
s_data[s_data$Point_Number=="ADAMSCAMPPT3", "Point_Number"] <- "11903"
s_data[s_data$Point_Number=="ADAMSCAMPPT4", "Point_Number"] <- "11904"
s_data[s_data$Point_Number=="ADAMSCAMPPT5", "Point_Number"] <- "11905"
s_data[s_data$Point_Number=="CRAFTSBUYPT1", "Point_Number"] <- "12001"
s_data[s_data$Point_Number=="CRAFTSBUYPT2", "Point_Number"] <- "12002"
s_data[s_data$Point_Number=="CRAFTSBUYPT3", "Point_Number"] <- "12003"
s_data[s_data$Point_Number=="CRAFTSBUYPT4", "Point_Number"] <- "12004"
s_data[s_data$Point_Number=="CRAFTSBUYPT5", "Point_Number"] <- "12005"

#Join (keep only point location that has actual detection)
data_flat <- merge(s_data, s_location, by = "Point_Number")

# Translate df with species and abundance only. 
data_flat <-data_flat[!is.na(data_flat$Spp),]
data_flat <-data_flat[!(data_flat$Count==0),]

## visitDate
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
data_flat$survey_time <- format(data_flat$Start_Time, format = "%H:%M:%S")
data_flat$survey_year <- sub("\\-.*", "", data_flat$visitDate)
## observer, observer_raw
data_flat$LName <- sub("\\,.*", "", data_flat$Researcher)
data_flat$FName <- sub(".*,", "", data_flat$Researcher)
data_flat$FName <- gsub(" ", "", data_flat$FName, fixed = TRUE)

# Determine the ones not in lookuptable and add them
new_observer <-unique(subset(data_flat$Researcher, !(data_flat$LName %in% lu_observer$LName & data_flat$FName %in% lu_observer$FName)))
new_obs <- data.frame(local_ObsID=max(lu_observer$local_ObsID) + seq.int(length(new_observer)),
                      LName=sub("\\,.*", "", new_observer),
                      FName=sub(".*,", "", new_observer))
new_obs$FName <- gsub(" ", "", new_obs$FName, fixed = TRUE)
lu_observer_updated <-rbind.fill(lu_observer, new_obs)
data_flat <- merge(data_flat, lu_observer_updated[,c(1,3:4)], by = c("FName","LName"), all.x = TRUE)
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
#Hard coded fix code not found in Species Table
data_flat[data_flat$Common_Name=="Chipmunk", "species"] <- "UNMA"
data_flat[data_flat$Common_Name=="Unknown sp.", "species"] <- "UNBI"
data_flat[data_flat$Common_Name=="Unid. Woodpecker", "species"] <- "UNWO"
data_flat[data_flat$Common_Name=="Slate-colored Junco", "species"] <- "DEJU"
data_flat[data_flat$Common_Name=="Ground Squirrels", "species"] <- "UNMA"
data_flat[data_flat$Common_Name=="Solitary Vireo", "species"] <- "SOVI"


data_flat$comments <- data_flat$Point_Note
data_flat$original_species <- data_flat$Spp
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
data_flat$missingindetections <- NA
# Behaviour
data_flat$originalBehaviourData <- data_flat$Detection_Cue
data_flat$pc_vt <- data_flat$vt
data_flat$pc_vt_detail <- data_flat$pc_vt_detail
data_flat$age <- data_flat$age
data_flat$fm <- data_flat$fm
data_flat$group <- data_flat$group
data_flat$flyover <- data_flat$flyover
data_flat$displaytype <- data_flat$displaytype
data_flat$nestevidence <- data_flat$nestevidence
data_flat$behaviourother <- data_flat$otherbehaviour


#--------------------------------------------------------------
#
#       EXPORT
#
#--------------------------------------------------------------
#---VISIT
write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), quote = FALSE, row.names = FALSE)

WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen", "comments")
survey_tbl <- data_flat[!duplicated(data_flat[,WTsurvey]), WTsurvey] 
#---SURVEY
write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), quote = FALSE, row.names = FALSE)


#Only select location with observation
location_tbl <- subset(location_tbl,location_tbl$location %in% survey_tbl$location)
#---LOCATION
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), quote = FALSE, row.names = FALSE)

#---EXTENDED
Extended <- c("location", "surveyDateTime", "species", "utmZone", "easting", "northing", "missinginlocations", "time_zone", 
              "data_origin", "missinginvisit", "pkey_dt", "survey_time", "survey_year", "rawObserver", "original_species", 
              "scientificname", "raw_distance_code", "raw_duration_code", "originalBehaviourData", "missingindetections", 
              "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", "displaytype", "nestevidence", "behaviourother")
extended_tbl <- data_flat[!duplicated(data_flat[,Extended]), Extended] 
write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_extended.csv")), quote = FALSE, row.names = FALSE)

#---lu_oberver
write.csv(lu_observer_updated, file.path(lu, paste0("lu_NEFBMP_observer_updated.csv")), quote = FALSE, row.names = FALSE)

