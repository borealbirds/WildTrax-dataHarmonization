# --- PENDING: Duplicates are found in source data: Bird Data. We delete it for now. Waiting to hear back from our partner (issue #1)
# ---
# ---
# Title: "Translate New England Forest Bird Monitoring Program 2012-2019"
# Source dataset is an Excel spreadsheet using 2 sheet: Site Data, Bird Data
# Author: "Melina Houle"
# Date: "March 11, 2022"
# Note on translation:
# -- Download manually source data locally prior to process
# -- Species code used are not equivalent to WildTrax species_code. Species codes need to be derived by using species common name
# -- 3 stations use concatenation of transect name + number instead of a numeric Point_Count (ADAMSCAMP, BAKERBUSH, CRAFTBURYOU). Fix is hardcoded. 
# -- Duplicates survey are found in the data. Data collaborator was contacted. They confirmed the data shared do not include any duplicate
# -- Columns allowing the distinction of observation are missing. Observation need to be sum
# -- 
# --- FIX DETAILS
# --- POINT NUMBER in sheet "Bird Data" need to be manually fix to retrieve coordinates from sheet "Site Data". Was replaced by 00:00:01
# ------- 15 Point numbers to fix
# --- Survey without observation were in fact aborted due to rain
# ------- 2 obs to delete
# --- Survey with species recorded but abundance =0  are assume to be a typo. 
# --- To avoid loosing the obs, fix it with abundance =1 
# ------- 1 obs: change abundance = 0 to 1 when species is present. 
# --- Fix species (some species don't join using common name)
# ------- "Chipmunk" translated as  "UNMA"                 
# ------- "Ground Squirrels" translated as "UNMA"
# ------- "Slate-colored Junco" translated as "DEJU"
# ------- "Solitary Vireo" translated as "SOVI"
# ------- "Unid. Woodpecker" translated as  "UNWO"
# ------- "Unknown sp." translated as "UNBI"
# --- Fix abundance - sum duplicate observation
# ------- 


library(utils) #read.csv
library(readxl) #read_excel
library(stringr) #str_replace_all
library(sf) #st_crs, st_as_sf, st_transform, st_drop_geometry
library(purrr) #map
library(plyr) #rbind.fill
library(data.table)
library(googledrive) #drive_get, drive_mkdir, drive_ls, drive_upload
library(dplyr) # mutate, %>%
library(readr) # write_lines

## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

WT_spTbl <- read.csv(file.path("./lookupTables/species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path("./lookupTables/duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path("./lookupTables/distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path("./lookupTables/duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path("./lookupTables/distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

organization = "BAM"
dataset_code = "NEFBMP2012-19"
detection_raw <- "lu_NEFBMP_detection.csv"
observer_raw <- "lu_NEFBMP_observer.xlsx"

project_dir <- file.path(wd, "project", dataset_code)
if (!dir.exists(project_dir)) {
  stop(message(paste0(dataset_code , " lookuptables needs to be added in the project directory")))
}
lu <- file.path(project_dir, "lookupTable")

out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

#Load lookup table and source data
lu_detection <- fread(file.path(lu,detection_raw))
lu_observer <- read_excel(file.path(lu, observer_raw))

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
s_location <-read_excel(file.path(project_dir, "NEFBMP2012-19.xlsx"), sheet = "Site Data", col_types = c("text") )
names(s_location)<-str_replace_all(names(s_location), c(" " = "_"))

s_location$organization <- organization
s_location$project <- dataset_code
s_location$site <- s_location$Short_Name 
s_location$station <- s_location$Point_Number
s_location$location <- paste(dataset_code,s_location$site, s_location$station, sep=":")
s_location[is.na(s_location$Latitude),]
s_location$latitude <- s_location$Latitude
s_location[is.na(s_location$Longitude),]
s_location$longitude <- s_location$Longitude
#s_location$elevationMeters <- NA
#s_location$bufferRadiusMeters <- NA
#s_location$isHidden <- NA
#s_location$trueCoordinates <- NA
s_location$comments <- s_location$Name
#s_location$internal_wildtrax_id <- NA
#s_location$internal_update_ts <- NA

# If exists in source data
s_location$utmZone	<- NA
s_location$easting	<- NA
s_location$northing	<- NA
s_location$missinginlocations <- NA

############################
#### VISIT TABLE ####
############################
s_data <- read_excel(file.path(project_dir, "NEFBMP2012-19.xlsx"), sheet = "Bird Data",  col_types = c("text", "text", "text", "text", "text", "date", "guess", "guess", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text"))
#Fix column names that have space
names(s_data)<-str_replace_all(names(s_data), c(" " = "_"))

# Hardcoded fix on Point_Count prior to join
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

#Join 
data_flat <- merge(s_data, s_location, by = "Point_Number")


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
data_flat$rawObserver <- gsub(", ", " ", data_flat$Researcher, fixed = TRUE)

## pkey_dt -- Concatenate, separated by colons: [location]:[visitDate]_[survey_time]:[localobservercode]; can also use raw observer or observer ID if needed
data_flat$pkey_dt<- paste(data_flat$location, paste0(gsub("-", "", as.character(data_flat$visitDate)),"_", gsub(":", "", data_flat$survey_time)), data_flat$observer, sep=":")

############################
#### SURVEY TABLE ####
############################
# Extract observer
data_flat$surveyDateTime <- paste(data_flat$visitDate, data_flat$survey_time)
## Distance and duration Methods
data_flat$distanceMethod <- "0m-50m-INF"
print(unique(data_flat$distanceMethod[!(data_flat$distanceMethod %in% WT_distMethTbl$distance_method_type)]))

data_flat$durationMethod <- "0-5-10min"
print(unique(data_flat$durationMethod[!(data_flat$durationMethod %in% WT_durMethTbl$duration_method_type)]))

## Species, species_old, comments, scientificname
# Check note when original species is absent 
data_flat[is.na(data_flat$Spp), "Point_Note"] #"Rain! Survey aborted"  "Rain!  Survey aborted"
# Delete incomplete survey
data_flat <- subset(data_flat, !(is.na(Spp)))
# Recover species_code
data_flat <- merge(data_flat, WT_spTbl[,c(1:2)], by.x ="Common_Name", by.y = "species_common_name" , all.x = TRUE)
# Check rows where the join didn't work
unique(data_flat[is.na(data_flat$species), "Common_Name"])

#Hard coded fix code not found in Species Table
data_flat[data_flat$Common_Name=="Chipmunk", "species"] <- "UNMA"
data_flat[data_flat$Common_Name=="Unknown sp.", "species"] <- "UNBI"
data_flat[data_flat$Common_Name=="Unid. Woodpecker", "species"] <- "UNWO"
data_flat[data_flat$Common_Name=="Slate-colored Junco", "species"] <- "DEJU"
data_flat[data_flat$Common_Name=="Ground Squirrels", "species"] <- "UNMA"
data_flat[data_flat$Common_Name=="Solitary Vireo", "species"] <- "SOVI"
# Cross-check species code
print(unique(data_flat$species[!(data_flat$species %in% WT_spTbl$species_code)]))

data_flat$comments <- data_flat$Point_Note
data_flat$original_species <- data_flat$Spp
data_flat$scientificname <- WT_spTbl$scientific_name[match(data_flat$species, WT_spTbl$species_code)]

## isHeard isSeen
data_flat <- merge(data_flat, lu_detection, by = "Detection_Cue", all.x = TRUE) 
data_flat$isHeard <- data_flat$heard
data_flat$isSeen <- data_flat$seen

## abundance
data_flat$abundance <- as.numeric(data_flat$Count)
# Check note when abundance =0 
data_flat[as.numeric(data_flat$Count)<1,] # There is 1. Replace 0 to 1 to avoid losing the observation
data_flat$abundance[as.numeric(data_flat$Count==0)] <- 1# 

# distance and duration interval
data_flat$distanceband <-  sapply(data_flat$Distance_Bin, switch, 
                                  '<50' = "0m-50m", 
                                  '>50' = "50m-INF",
                                  '<1000' = "UNKNOWN")

data_flat$durationinterval <- sapply(data_flat$Time_Bin, switch, 
                                     '0_3min' = "0-3min", 
                                     '3_5min' = "3-5min",
                                     '5_10min' = "5-10min")
# Cross-check distance and duration interval
print(unique(data_flat$distanceband[!(data_flat$distanceband %in% WT_distBandTbl$distance_band_type)]))
print(unique(data_flat$durationinterval[!(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))

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
dr<- drive_get(paste0("toUpload/",organization))

if (nrow(drive_ls(as_id(dr), pattern = dataset_code)) == 0){
  dr_dataset_code <-drive_mkdir(dataset_code, path = as_id(dr), overwrite = NA)
} else {
  dr_dataset_code <- drive_ls(as_id(dr), pattern = dataset_code)
}

#---LOCATION
#Only select location with observation
location_tbl <- subset(s_location,s_location$location %in% data_flat$location)
WTlocation <- c("location", "latitude", "longitude", "comments")
location_tbl <- s_location[!duplicated(s_location[,WTlocation]), WTlocation]  

write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), quote = FALSE, row.names = FALSE, na = "")
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 

#---VISIT
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- data_flat[!duplicated(data_flat[,WTvisit]), WTvisit] 


write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), quote = FALSE, row.names = FALSE, na = "")
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 

#---SURVEY
survey_tbl <- data_flat %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband,
           durationinterval, isHeard, isSeen, comments) %>% 
  dplyr::summarise(abundance = sum(abundance), .groups= "keep")

WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen", "comments")
survey_tbl <- survey_tbl[!duplicated(survey_tbl[,WTsurvey]), WTsurvey] 
write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), quote = FALSE, row.names = FALSE, na = "")
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv"), overwrite = TRUE) 



#---EXTENDED
extended_tbl <- data_flat %>% 
  group_by(organization, project, location, surveyDateTime, species, distanceband, durationinterval, site, station, utmZone, easting, 
           northing, missinginlocations, time_zone, data_origin, missinginvisit, pkey_dt, survey_time,
           survey_year, rawObserver, original_species, scientificname, raw_distance_code, raw_duration_code, 
           originalBehaviourData, missingindetections, pc_vt, pc_vt_detail, age, fm, group, flyover, 
           displaytype, nestevidence, behaviourother) %>% 
  dplyr::summarise(abundance = sum(abundance), .groups= "keep")

Extended <- c("organization", "project", "location", "surveyDateTime", "species", "abundance", "distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code", 
               "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
               "displaytype", "nestevidence", "behaviourother")
extended_tbl <- extended_tbl[!duplicated(extended_tbl[,Extended]), Extended] 
write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_extended.csv")), quote = FALSE, row.names = FALSE)
extended_out <- file.path(out_dir, paste0(dataset_code,"_extended.csv"))
drive_upload(media = extended_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_extended.csv"), overwrite = TRUE) 

#---lu_observer
write.csv(lu_observer_updated, file.path(lu, "lu_NEFBMP_observer_updated.csv"), quote = FALSE, row.names = FALSE)
observer_out <- file.path(out_dir, paste0(dataset_code,"_extended.csv"))
drive_upload(media = observer_out, path = as_id(dr_dataset_code), name = "lu_NEFBMP_observer_updated.csv", overwrite = TRUE) 

#---PROCESSING STATS
write_lines(paste0("Organization: ", organization), file.path(out_dir, paste0(dataset_code, "_stats.csv")))
write_lines(paste0("Project: ", dataset_code), file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
nrow_location <- paste0("Number of locations: ", nrow(location_tbl))
write_lines(nrow_location, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
nrow_visit <- paste0("Number of visit: ", nrow(visit_tbl))
write_lines(nrow_visit, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
nrow_survey <- paste0("Number of survey: ", nrow(survey_tbl))
write_lines(nrow_survey, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
