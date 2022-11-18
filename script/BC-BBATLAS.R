# ---
# PCODE: BC-BBATLAS
# Title: "Translate BC Breeding Bird Atlas"
# Source dataset is extracted from BAM v6
# Author: "Melina Houle"
# Date: "June 29, 2022"
# Note on translation:
# --- Data were pre-porcessed by Hedwig Lankau. Script do not run using source data. 
# --- delete 56 observations. Didn't have lat/long coordinates
# --- delete 1 observations. Didn't have survey date (only year)

library(dplyr) # mutate, %>%
library(utils) #read.csv
library(RODBC) #odbcConnect, sqlFetch
library(stringr) #str_replace_all
library(tidyr) #separate
library(googledrive) #drive_get, drive_mkdir, drive_ls, drive_upload
library(readr) #write_lines

## Initialize variables
## Conversion do not use source data. It rather use an unfinished version of BAM V4 db
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

organization = "BAM"
dataset_code = "BC-BBATLAS"
source_data <- "BAM-V6-USE.accdb"
lu <- "./lookupTables"
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
# Check dataset code for BC-ATLAS
s_dataset <- sqlFetch(project, "dataset") # 12
s_location <- sqlFetch(project, "location")
pc_visit <- sqlFetch(project, "pc_visit")
pc_survey <- sqlFetch(project, "pc_detection")
lu_duration_interval <- sqlFetch(project, "lu_pc_duration_interval")
lu_duration_method <- sqlFetch(project, "lu_pc_protocol_duration")
lu_distance_method <- sqlFetch(project, "lu_pc_protocol_distance")
lu_distance_interval <- sqlFetch(project, "lu_pc_distance_band")
lu_species <- sqlFetch(project, "WT_Species_codes")
#
#--------------------------------------------------------------
#
#       TRANSLATE
# pc_survey_1 <- pc_survey[pc_survey$pv_visit_fk %in% s_visit$visit_id]
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
s_location <- s_location[s_location$dataset_fk ==12,]
s_location$location <- s_location$location_name_V6
s_location <- s_location %>%
  separate(location_name_V6, c("project", "site", "station"), ":")

s_location$organization <- organization 
s_location$project <- s_location$project
# Delete PC without lat/long
s_location <- s_location[!is.na(s_location$latitude),] #delete 56 entries
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
s_visit <- s_visit[!is.na(s_visit$survey_date),] #delete 1 entries

s_visit$visitDate <- as.character(s_visit$survey_date)
## snowDepthMeters, waterDepthMeters, landFeatures, crew, bait, accessMethod, comments_visit,wildtrax_internal_update_ts, wildtrax_internal_lv_id
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
s_visit$StartTime_V4[is.na(s_visit$StartTime_V4)] <- "1899-12-30 00:00:01 EST"
s_visit$StartTime_V4 <- as.POSIXct(s_visit$StartTime_V4, format = "%Y-%m-%d %H:%M:%S")
s_visit$survey_time <- as.character(format(s_visit$StartTime_V4, format = "%H:%M:%S"))
s_visit$survey_year <- s_visit$survey_year
## observer, observer_raw
s_visit$observer <- "NA"
s_visit$rawObserver <- s_visit$obs_V4

## pkey_dt -- Concatenate, separated by colons: [location]:[visitDate]_[survey_time]:[localobservercode]; can also use raw observer or observer ID if needed
s_visit$pkey_dt<- paste(s_visit$location, paste0(gsub("-", "", as.character(s_visit$visitDate)),"_", gsub(":", "", s_visit$survey_time)), s_visit$observer, sep=":")

WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- s_visit[!duplicated(s_visit[,WTvisit]), WTvisit] 


############################
#### SURVEY TABLE ####
############################
names(pc_survey)<-str_replace_all(names(pc_survey), c(" " = "_"))

data_flat <- merge(pc_survey, s_visit, by.x = "pv_visit_fk", by.y = "visit_id" )

# Check if species is missing. 
  #data_flat[is.na(data_flat$species_code),]

data_flat$surveyDateTime <- paste(as.character(data_flat$visitDate), data_flat$survey_time)

## Distance and duration Methods
data_flat$distanceMethod <- lu_distance_method$protocol_distance_range[match(data_flat$method_distance, lu_distance_method$protocol_distance_numid)]
print(unique(data_flat$distanceMethod[!(data_flat$distanceMethod %in% WT_distMethTbl$distance_method_type)]))

data_flat$durationMethod <- data_flat$method_duration
data_flat$durationMethod <- lu_duration_method$protocol_duration_range[match(data_flat$method_duration, lu_duration_method$protocol_duration_id)]
print(unique(data_flat$durationMethod[!(data_flat$durationMethod %in% WT_durMethTbl$duration_method_type)]))

## Species, species_old, comments, scientificname
data_flat$species <- WT_spTbl$species_code[match(data_flat$species_code, WT_spTbl$species_code)]
print(unique(data_flat$species_code[!(data_flat$species_code %in% WT_spTbl$species_code)]))

data_flat$species_name <- WT_spTbl$species_common_name[match(data_flat$species, WT_spTbl$species_code)]
data_flat$comments <- NA
data_flat$original_species <- data_flat$species_code
data_flat$scientificname <- WT_spTbl$scientific_name[match(data_flat$species_code, WT_spTbl$species_code)]

## isHeard isSeen
data_flat$isHeard <- data_flat$heard
data_flat$isSeen <- data_flat$seen

## abundance
data_flat$abundance <- data_flat$abundance

# distance and duration interval
data_flat$distanceband <- lu_distance_interval$distance_band_description[match(data_flat$distance_band, lu_distance_interval$distance_band_numid)]
unique(data_flat$distanceMethod) # "0m-INF"  
unique(data_flat$distanceband) # MATCH
# check
print(unique(data_flat$distanceband[!(data_flat$distanceband %in% WT_distBandTbl$distance_band_type)]))

# Duration
data_flat$durationinterval <- lu_duration_interval$duration_description[match(data_flat$duration_interval, lu_duration_interval$duration_interval)]
unique(data_flat$durationMethod) #  "0-5min" 
unique(data_flat$durationinterval) # MATCH
# check if it follow WT duration code. Should be empty
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
dr<- drive_get(paste0("toUpload/",organization))

if (nrow(drive_ls(as_id(dr), pattern = dataset_code)) == 0){
  dr_dataset_code <-drive_mkdir(dataset_code, path = as_id(dr), overwrite = NA)
} else {
  dr_dataset_code <- drive_ls(as_id(dr), pattern = dataset_code)
}
#---SURVEY
# Sum abundance in the same observation that were split because of the behaviour code
survey_tbl <- data_flat %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband,
           durationinterval, isHeard, isSeen, comments) %>% 
  dplyr::summarise(abundance = sum(abundance), .groups= "keep")
WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen", "comments")
survey_tbl <- survey_tbl[!duplicated(survey_tbl[,WTsurvey]), WTsurvey]
write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), row.names = FALSE, na = "")
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv"), overwrite = TRUE) 

species <-sort(unique(data_flat$species_name))
write.csv(species, file= file.path(out_dir, paste0(dataset_code,"_speciesList.csv")), row.names = FALSE)

#---VISIT (only where observation exists)
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")
visit_tbl <- visit_tbl[!duplicated(visit_tbl[,WTvisit]), WTvisit] # 
visit_tbl <- subset(visit_tbl,visit_tbl$location %in% data_flat$location)
write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), quote = FALSE, row.names = FALSE, na = "")
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 

#---LOCATION
WTlocation <- c("location", "latitude", "longitude")
#WTlocation <- c("location", "latitude", "longitude", "bufferRadiusMeters", "elevationMeters", "isHidden", "trueCoordinates", "comments")
location_tbl <- s_location[!duplicated(s_location[,WTlocation]), WTlocation] # 

location_tbl <- subset(location_tbl,location_tbl$location %in% survey_tbl$location)
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), quote = FALSE, row.names = FALSE, na = "")
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 

#---EXTENDED
Extended <- c("organization", "project","location", "surveyDateTime", "species", "distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code", 
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
              "displaytype", "nestevidence", "behaviourother")
extended_tbl <- data_flat[!duplicated(data_flat[,Extended]), Extended] 
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
