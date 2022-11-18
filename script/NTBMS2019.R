# --- PENDING: Duplicates are found in source data: Bird Data. We delete it for now. Waiting to hear back from our partner (issue #5)
# ---

# Title: "Translate Northwest Territories Bird Monitoring Survey"
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
library(googledrive) #drive_get, drive_mkdir, drive_ls, drive_upload

## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

WT_spTbl <- read.csv(file.path("./lookupTables/species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path("./lookupTables/duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path("./lookupTables/distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path("./lookupTables/duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path("./lookupTables/distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

organization <- "CWS-NOR"
dataset_code = "NTBMS2019"
lu <- "./lookupTables"
project <- file.path("./project", dataset_code)
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}
project_dir <- file.path(wd, "project", dataset_code)
if (!dir.exists(project_dir)) {
  dir.create(project_dir)
}
data_db <- file.path(project_dir, dataset_code)
if (!file.exists(data_db)) {
  #Download from GoogleDrive
  drive_download("SourceData/NTBMS_HumanPC_Data_working.csv", path = data_db)
  drive_download("SourceData/NTBMS-LV-LookUpTables-HB.xlsx", path = data_db)
}
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}


#--------------------------------------------------------------
#       LOAD
#--------------------------------------------------------------
data_flat <- read.csv(file.path(project, "NTBMS_HumanPC_Data_working.csv"))
lu_observer <- read_excel(file.path(project, "NTBMS-LV-LookUpTables-HB.xlsx"), sheet = "ListObserver")
lu_behaviour <-read_excel(file.path(project, "NTBMS-LV-LookUpTables-HB.xlsx"), sheet = "ListBehaviour")

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
## location [dataset_code:site:survey location]
data_flat$organization <- organization
data_flat$project <- dataset_code
data_flat$site <- data_flat$Site
data_flat$station <- data_flat$Station
data_flat$location <- paste(dataset_code,data_flat$site,data_flat$station, sep=":")
## latitude, longitude
data_flat[is.na(data_flat$Latitude),]
data_flat$latitude <- data_flat$Latitude
data_flat[is.na(data_flat$Longitude),]
data_flat$longitude <- data_flat$Longitude
#data_flat$bufferRadiusMeters <- NA
#data_flat$elevationMeters <- NA
#data_flat$isHidden <- NA
#data_flat$trueCoordinates <- NA
data_flat$comments <- NA
#data_flat$internal_wildtrax_id <- NA
#data_flat$internal_update_ts <- NA

# If exists in source data
data_flat$utmZone	<- NA
data_flat$easting	<- NA
data_flat$northing	<- NA
data_flat$missinginlocations <- NA

############################
#### VISIT TABLE ####
############################
data_flat$visitDate <- as.character(format(data_flat$PC_Date, format = "%Y-%m-%d"))
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


############################
#### SURVEY TABLE ####
############################
data_flat$surveyDateTime <- paste(data_flat$visitDate, data_flat$survey_time)
data_flat$distanceMethod <- "0m-50m-100m-INF"
data_flat$durationMethod <- "0-1-2-3-4-5-6-7-8-9-10min"
# Cross-check distance and duration method
print(unique(data_flat$distanceMethod[!(data_flat$distanceMethod %in% WT_distMethTbl$distance_method_type)]))
print(unique(data_flat$durationMethod[!(data_flat$durationMethod %in% WT_durMethTbl$duration_method_type)]))

## check if species is missing
data_flat[is.na(data_flat$Species),]
data_flat$species <- data_flat$Species
# Cross-check species code
print(unique(data_flat$species[!(data_flat$species %in% WT_spTbl$species_code)]))

data_flat$comments <- gsub(",",";", data_flat$Comments)
data_flat$original_species <- data_flat$PCSpeciesID
data_flat$scientificname <- WT_spTbl$scientific_name[match(data_flat$species, WT_spTbl$species_code)]

## isHeard isSeen
data_flat <- merge(data_flat, lu_behaviour, by.x = "Behaviour", by.y ="BEHAVIOUR" , all.x = TRUE) 
data_flat$isHeard <- data_flat$heard
data_flat$isSeen <- data_flat$seen

## abundance
unique(data_flat$Abundance)
data_flat$abundance <- data_flat$Abundance

# distance and duration interval
data_flat$distanceband <-  ifelse(data_flat$Distance == "< 50m", "0m-50m",
                                        ifelse(data_flat$Distance == "50m-100m", "50m-100m",
                                        ifelse(data_flat$Distance == "> 100m", "100m-INF",
                                  "UNKNOWN")))
print(unique(data_flat$distanceband[!(data_flat$distanceband %in% WT_distBandTbl$distance_band_type)]))

data_flat$durationinterval <- ifelse(data_flat$Time == "1 min", "0-1min",
                                     ifelse(data_flat$Time == "2 min", "1-2min",
                                     ifelse(data_flat$Time == "3 min", "2-3min",
                                     ifelse(data_flat$Time == "4 min", "3-4min",
                                     ifelse(data_flat$Time == "5 min", "4-5min",
                                      ifelse(data_flat$Time == "6 min", "5-6min",
                                      ifelse(data_flat$Time == "7 min", "6-7min",
                                      ifelse(data_flat$Time == "8 min", "7-8min",
                                      ifelse(data_flat$Time == "9 min", "8-9min",
                                      ifelse(data_flat$Time == "10 min", "9-10min",
                                  "UNKNOWN"))))))))))

print(unique(data_flat$durationinterval[!(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))
# Fix NA
data_flat[is.na(data_flat$Time), "durationinterval"] <- "UNKNOWN"
print(unique(data_flat$durationinterval[!(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))

data_flat$raw_distance_code <- data_flat$Distance
data_flat$raw_duration_code <- data_flat$Time
data_flat$missingindetections <- NA

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
WTlocation <- c("location", "latitude", "longitude", "comments")
location_tbl <- data_flat[!duplicated(data_flat[,WTlocation]), WTlocation] # 
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), quote = FALSE, row.names = FALSE)
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
WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen", "comments")
survey_tbl <- data_flat[!duplicated(data_flat[,WTsurvey]), WTsurvey] 


write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), quote = FALSE, row.names = FALSE)

#---
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

Extended <- c("organization", "project", "location", "surveyDateTime", "species", "abundance","distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code", 
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
              "displaytype", "nestevidence", "behaviourother")
extended_tbl <- extended_tbl[!duplicated(extended_tbl[,Extended]), Extended] 
write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_extended.csv")), quote = FALSE, row.names = FALSE)
extended_out <- file.path(out_dir, paste0(dataset_code,"_extended.csv"))
drive_upload(media = extended_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_extended.csv"), overwrite = TRUE) 

#---PROCESSING STATS
write_lines(paste0("Organization: ", organization), file.path(out_dir, paste0(dataset_code, "_stats.csv")))
write_lines(paste0("Project: ", dataset_code), file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
nrow_location <- paste0("Number of locations: ", nrow(location_tbl))
write_lines(nrow_location, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
nrow_visit <- paste0("Number of visit: ", nrow(visit_tbl))
write_lines(nrow_visit, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
nrow_survey <- paste0("Number of survey: ", nrow(survey_tbl))
write_lines(nrow_survey, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)

