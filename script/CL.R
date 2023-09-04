# ---
# PCODE: * all CL PC from BAM v6
# Title: "Translate old Calling Lake data loaded in BAM v6"
# Source dataset is extracted from BAM v6. Dataset is 20
# Author: "Melina Houle"
# Date: "November 28, 2022"
# Note on translation:
# --- Data were pre-processed by Hedwig Lankau. Script do not use source data. 
# --- BAM v6 has duplicate entries. The output of that script do not match tables from BAM v6 because the script control for duplicates
# ------- 

# --- FIX DETAILS
# --- Species Oven and oven changed for OVEN
# ------- 2 obs are Oven
# ------- 1 obs is oven
# --- Duration and distance interval don't fit methodology. 
# ------FIX distanceMethod to 0-50m-100m-INF to follow distanceband
# ------- 15627 obs :distanceband=="100-INF" 
# ------DELETE (don't respect duration protocol)
# ------- 19 obs :durationinterval = "before or after/incidental"

library(utils) #read.csv
library(RODBC) #odbcConnect, sqlFetch
library(stringr) #str_replace_all
library(tidyr) #separate
library(googledrive)
library(dplyr) # mutate, %>%
library(readr) # write_lines

## Initialize variables
## Conversion do not use source data. It rather use an unfinished version of BAM V4 db
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

organization = "BAM"
dataset_code <- "CL"
project_name <- "Calling_Lake_1993_2013"
source_data <- "BAM-V6-USE.accdb"
WT_spTbl <- read.csv(file.path("./lookupTables/species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path("./lookupTables/duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path("./lookupTables/distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path("./lookupTables/duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path("./lookupTables/distance_band_codes.csv"), fileEncoding="UTF-8-BOM")
project_dir <- file.path(wd, "project", dataset_code)
if (!dir.exists(project_dir)) {
  dir.create(project_dir)
}

data_db <- file.path(project_dir, source_data)
if (!file.exists(data_db)) {
  #Download from GoogleDrive
  drive_download(paste0("BAM-V6/", source_data), path = data_db)
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
s_dataset <- sqlFetch(project, "dataset") # 
s_location <- sqlFetch(project, "location")
pc_visit <- sqlFetch(project, "pc_visit")
pc_survey <- sqlFetch(project, "pc_detection")
lu_duration_interval <- sqlFetch(project, "lu_pc_duration_interval")
lu_duration_method <- sqlFetch(project, "lu_pc_protocol_duration")
lu_distance_method <- sqlFetch(project, "lu_pc_protocol_distance")
lu_distance_interval <- sqlFetch(project, "lu_pc_distance_band")
lu_species <- sqlFetch(project, "WT_Species_codes")
#--------------------------------------------------------------
#
#       TRANSLATE
# 
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
s_location <- subset(s_location, s_location$dataset_fk ==20) # 469 locations
s_location$location <- s_location$location_name_V6
s_location <- s_location %>%
  separate(location_name_V6, c("project", "site", "station"), ":")

s_location$organization <- organization 
s_location$project <- project_name
#Check if lat/long are missing. Should be empty
subset(s_location,is.na(s_location$latitude))
s_location[is.na(s_location$longitude),]
s_location$latitude <- s_location$latitude
s_location$longitude <- s_location$longitude
#s_location$elevationMeters <- NA
#s_location$bufferRadiusMeters <- NA
#s_location$isHidden <- NA
#s_location$trueCoordinates <- NA
#s_location$comments <- NA
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
#Fix column names that have space
names(pc_visit) <-str_replace_all(names(pc_visit), c(" " = "_"))

# Merge location tbl to visit to recover location name
s_visit <- merge(pc_visit, s_location, by.x = "location_fk", by.y = "location_autoid") 
## visitDate
s_visit$visitDate <- as.character(s_visit$survey_date)
# snowDepthMeters, waterDepthMeters, landFeatures, crew, bait, accessMethod, comments_visit,wildtrax_internal_update_ts, wildtrax_internal_lv_id
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
# Fix time. When time is missing, fill it with 00:00:01
s_visit$StartTime_V4 <- as.POSIXct(s_visit$StartTime_V4, format = "%Y-%m-%d %H:%M:%S")
s_visit$survey_time <- as.character(format(s_visit$StartTime_V4, format = "%H:%M:%S"))
s_visit$survey_year <- s_visit$survey_year
## observer, observer_raw
s_visit$observer <- "NA"
s_visit$rawObserver <- s_visit$obs_V4
## pkey_dt -- Concatenate, separated by colons: [location]:[visitDate]_[survey_time]:[localobservercode]; can also use raw observer or observer ID if needed
s_visit$pkey_dt<- paste(s_visit$location, paste0(gsub("-", "", as.character(s_visit$visitDate)),"_", gsub(":", "", s_visit$survey_time)), s_visit$observer, sep=":")


############################
#### SURVEY TABLE ####
############################
names(pc_survey)<-str_replace_all(names(pc_survey), c(" " = "_"))

data_flat <- merge(pc_survey, s_visit, by.x = "pv_visit_fk", by.y = "visit_id" )

# Translate df with species only. In this case, species and abundance always present
#data_flat <-data_flat[!is.na(data_flat$species_code),]
#data_flat <-data_flat[!is.na(data_flat$abundance),]

data_flat$surveyDateTime <- paste(as.character(data_flat$visitDate), data_flat$survey_time)

## Distance and duration Methods
data_flat$distanceMethod <- lu_distance_method$protocol_distance_range[match(data_flat$method_distance, lu_distance_method$protocol_distance_numid)]
print(unique(data_flat$distanceMethod[!(data_flat$distanceMethod %in% WT_distMethTbl$distance_method_type)]))

data_flat$durationMethod <- data_flat$method_duration
data_flat$durationMethod <- lu_duration_method$protocol_duration_range[match(data_flat$method_duration, lu_duration_method$protocol_duration_id)]
print(unique(data_flat$durationMethod[!(data_flat$durationMethod %in% WT_durMethTbl$duration_method_type)]))

## Species, species_old, comments, scientificname
  #print(unique(data_flat$species_code[!(data_flat$species_code %in% WT_spTbl$species_code)]))
  #lu_species[lu_species$species_code =="UNHU",] # Contact Alex to add UNHU as unidentified hummingbird
data_flat$species <- WT_spTbl$species_code[match(data_flat$species_code, WT_spTbl$species_code)]
print(unique(data_flat$species_code[!(data_flat$species_code %in% WT_spTbl$species_code)]))
# FIX BAM species code Oven and oven to OVEN
data_flat$species_code[data_flat$species_code == "Oven"] <- "OVEN"
data_flat$species_code[data_flat$species_code == "oven"] <- "OVEN"
print(unique(data_flat$species_code[!(data_flat$species_code %in% WT_spTbl$species_code)]))

data_flat$species_name <- WT_spTbl$species_common_name[match(data_flat$species, WT_spTbl$species_code)]
data_flat$comments <- NA
data_flat$original_species <- data_flat$species_code
data_flat$scientificname <- WT_spTbl$scientific_name[match(data_flat$original_species, WT_spTbl$species_code)]
## isHeard isSeen
unique(data_flat$heard) #"Yes" "No"  NA    "DNC" "N/A" "NR" 
data_flat$isHeard[data_flat$heard == "Yes"] <- "yes"
data_flat$isHeard[data_flat$heard =="No"] <-"no"
data_flat$isHeard[data_flat$heard =="N/A"] <- NA
data_flat$isHeard[data_flat$heard =="NR"] <-NA
data_flat$isHeard[data_flat$heard =="DNC"] <-NA
data_flat$isHeard[is.na(data_flat$heard)] <-NA

unique(data_flat$seen) #"Yes" "No"  "DNC" "N/A" 
data_flat$isSeen[data_flat$seen == "Yes"] <- "yes"
data_flat$isSeen[data_flat$seen =="No"] <-"no"
data_flat$isSeen[data_flat$seen =="N/A"] <-NA
data_flat$isSeen[data_flat$seen =="DNC"] <-NA

## abundance
data_flat$abundance <- data_flat$abundance

# Distance band do not match distance methodology
# Assign and fix distance band
data_flat$distanceband <- lu_distance_interval$distance_band_description[match(data_flat$distance_band, lu_distance_interval$distance_band_numid)]
unique(data_flat$distanceMethod) # "0m-50m-100m-INF"
unique(data_flat$distanceband[data_flat$distanceMethod=="0m-50m-100m-INF"]) # MATCH

# check if it follow WT distance code. Should be empty
print(unique(data_flat$distanceMethod[!(data_flat$distanceMethod %in% WT_distMethTbl$distance_method_type)]))
print(unique(data_flat$distanceband[!(data_flat$distanceband %in% WT_distBandTbl$distance_band_type)]))

# CHECK duration
data_flat$durationinterval <- lu_duration_interval$duration_description[match(data_flat$duration_interval, lu_duration_interval$duration_interval)]
unique(data_flat$durationMethod) # "0-5min" 
unique(data_flat$durationinterval)
unique(data_flat$durationinterval[data_flat$durationMethod=="0-5min"]) # "0-5min"  "UNKNOWN"

# check if it follow WT duration code. Should only have "before or after/incidental". We will deal with them later
print(unique(data_flat$durationinterval[!(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))

data_flat$raw_distance_code <- data_flat$distance_band
data_flat$raw_duration_code <- data_flat$duration_interval
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
}
dr_ls <- drive_ls(as_id(dr), pattern = dataset_code)

#---LOCATION
WTlocation <- c("location", "latitude", "longitude")
  
#WTlocation <- c("location", "latitude", "longitude", "bufferRadiusMeters", "elevationMeters", "isHidden", "trueCoordinates", "comments")
location_tbl <- s_location[!duplicated(s_location[,WTlocation]), WTlocation] # 
location_tbl <- subset(location_tbl,location_tbl$location %in% data_flat$location)
  
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), row.names = FALSE, na = "")
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_ls), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 
  
#---VISIT
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double visit on the same site, same day n = 17). 
visit_tbl <- s_visit[!duplicated(s_visit[,WTvisit]), WTvisit] # 
visit_tbl <- subset(visit_tbl,visit_tbl$location %in% data_flat$location)
write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), row.names = FALSE, na = "")
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_ls), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 
  
#---SURVEY
# Sum obs that are the same based on WildTrax field n = 46 (behavior differ)
# Delete "before or after/incidental". Don't fit protocol n =19
data <- subset(data_flat, data_flat$durationinterval %in% c("UNKNOWN", "0-5min"))

survey_tbl <- data %>% 
            group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband, durationinterval, isHeard, isSeen, comments) %>%
dplyr::summarise(abundance = sum(abundance), .groups= "keep")
  
WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
                "durationinterval", "abundance", "isHeard", "isSeen", "comments")
survey_tbl <- survey_tbl[!duplicated(survey_tbl[,WTsurvey]), WTsurvey]
write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), row.names = FALSE, na = "")
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_ls), name = paste0(dataset_code,"_survey.csv"), overwrite = TRUE) 
  
#---EXTENDED
extended_tbl <- data_flat %>% 
  group_by(organization, project,location, surveyDateTime, species, abundance,distanceband, durationinterval, site, station, utmZone, easting, 
           northing, missinginlocations, time_zone, data_origin, missinginvisit, pkey_dt, survey_time,
           survey_year, rawObserver, original_species, scientificname, raw_distance_code, raw_duration_code, 
           originalBehaviourData, missingindetections, pc_vt, pc_vt_detail, age, fm, group, flyover, 
           displaytype, nestevidence, behaviourother) %>%
  dplyr::summarise(ind_count = sum(abundance), .groups= "keep")

Extended <- c("organization", "project","location", "surveyDateTime", "species", "abundance", "ind_count", "distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
            "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
            "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code", 
            "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
            "displaytype", "nestevidence", "behaviourother")
extended_tbl <- extended_tbl[!duplicated(extended_tbl[,Extended]), Extended] 
write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_extended.csv")), quote = FALSE, row.names = FALSE, na = "")
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

  
