# ---
# PCODE: MN-BBATLAS
# Title: "Translate Minnesota Breeding Bird Atlas"
# Source dataset for this script was provided by Hedwig Lankau and uses accdb format
# Author: "Melina Houle"
# Date: "March 8, 2022"
# Note on translation:
# --- Data were pre-porcessed by Hedwig Lankau. Script do not run using source data. 
# --- All PC have lat, long coordinates
# --- All obs have date and time

library(googledrive)
library(dplyr) # mutate, %>%
library(utils) #read.csv
library(RODBC) #odbcConnect, sqlFetch
library(stringr) #str_replace_all
library(sf) #st_crs, st_as_sf, st_transform, st_drop_geometry
library(purrr) #map
library(readr)

## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

organization = "NRRI"
dataset_code = "MN-BBATLAS"
BAM_ludb <- "BAM-V6-USE.accdb"
source_data <- "NRRI-MNATLAS.accdb"
lu <- "./lookupTables"
WT_spTbl <- read.csv(file.path("./lookupTables/species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path("./lookupTables/duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path("./lookupTables/distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path("./lookupTables/duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path("./lookupTables/distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

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
bam_db <- file.path(project_dir, BAM_ludb)
if (!file.exists(bam_db)) {
  #Download from GoogleDrive
  drive_download(paste0("BAM-V6/", BAM_ludb), path = bam_db)
}
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
con <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",data_db))
BAMdb <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",bam_db))
#Connecte and load tables
#queries<- sqlQuery(con, "BAM-V6")  ##Look up any queries
#tbls <- sqlTables(con) ##Look up tables
#tbls$TABLE_NAME
# Load lookup table
lu_duration_interval <- sqlFetch(BAMdb, "lu_pc_duration_interval")
lu_duration_method <- sqlFetch(BAMdb, "lu_pc_protocol_duration")
lu_distance_method <- sqlFetch(BAMdb, "lu_pc_protocol_distance")
lu_distance_interval <- sqlFetch(BAMdb, "lu_pc_distance_band")
#
#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
pc_location <- sqlFetch(con, "location")
# Check if location have all x, y coordinates, Should be empty
pc_location[is.na(pc_location$x),]
pc_location[is.na(pc_location$y),]

# Transform
pc_location <- st_as_sf(pc_location, coords = c("x", "y"), remove = FALSE)
st_crs(pc_location) <- crs_utm15N
pc_location_DD <- st_transform(pc_location, crs_WT)

s_location <- pc_location_DD %>%
  mutate(latitude = unlist(map(pc_location_DD$geometry,2)),
         longitude = unlist(map(pc_location_DD$geometry,1)))

st_drop_geometry(s_location)
s_location$geometry <- NULL

# Fill column name using space by underscore
names(s_location)<-str_replace_all(names(s_location), c(" " = "_"))
s_location$organization <- organization
s_location$project <- dataset_code
s_location$site <- s_location$site_name 
s_location$station <- s_location$station_name
s_location$location <- s_location$Location_Name
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
s_location$utmZone	<- s_location$zone
s_location$easting	<- s_location$y
s_location$northing	<- s_location$x
s_location$missinginlocations <- NA

#---LOCATION
#WTlocation <- c("location", "latitude", "longitude", "bufferRadiusMeters", "elevationMeters", "isHidden", "trueCoordinates", "comments")
WTlocation <- c("location", "latitude", "longitude")
location_tbl <- s_location[!duplicated(s_location[,WTlocation]), WTlocation] # 

############################
#### VISIT TABLE ####
############################
pc_visit <- sqlFetch(con, "pc_visit")
#Fix column names that have space
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


############################
#### SURVEY TABLE ####
############################
pc_detection <- sqlFetch(con, "pc_detection")
names(pc_detection)<-str_replace_all(names(pc_detection), c(" " = "_"))

s_data <- merge(pc_detection, pc_visit,by.x = c("Location_Name", as.character("survey_date")), by.y = c("Location_Name", "visitDate"))

data_flat <- merge(s_data, s_location, by.x = "Location_Name", by.y = "location")

data_flat$location <- data_flat$Location_Name 
data_flat$surveyDateTime <- paste(as.character(data_flat$visitDate), data_flat$survey_time)

## Distance and duration Methods
data_flat$distanceMethod <- lu_distance_method$protocol_distance_range[match(data_flat$distance_protocol, lu_distance_method$protocol_distance_numid)]
print(unique(data_flat$distanceMethod[!(data_flat$distanceMethod %in% WT_distMethTbl$distance_method_type)]))

data_flat$durationMethod <- lu_duration_method$protocol_duration_range[match(data_flat$duration_protocol, lu_duration_method$protocol_duration_id)]
print(unique(data_flat$durationMethod[!(data_flat$durationMethod %in% WT_durMethTbl$duration_method_type)]))

## Species, species_old, comments, scientificname
data_flat$original_species <- data_flat$species
data_flat$species <- WT_spTbl$species_code[match(data_flat$original_species, WT_spTbl$species_code)]
print(unique(data_flat$original_species[!(data_flat$original_species %in% WT_spTbl$species_code)]))

data_flat$species_name <- WT_spTbl$species_common_name[match(data_flat$original_species, WT_spTbl$species_code)]
data_flat$comments <- NA
data_flat$scientificname <- WT_spTbl$scientific_name[match(data_flat$original_species, WT_spTbl$species_code)]

## isHeard isSeen
data_flat$isHeard <- data_flat$Heard
data_flat$isSeen <- data_flat$Seen

## abundance
data_flat$abundance <- data_flat$Abundance

# distance and duration interval
data_flat$distanceband <- lu_distance_interval$distance_band_description[match(data_flat$distance_band, lu_distance_interval$distance_band_numid)]
unique(data_flat$distanceMethod) # " "0m-50m-100m-INF" 
unique(data_flat$distanceband) # MATCH "50m-100m" "100m-INF" "0m-50m" 
# check
print(unique(data_flat$distanceband[!(data_flat$distanceband %in% WT_distBandTbl$distance_band_type)]))

# Duration
data_flat$durationinterval <- lu_duration_interval$duration_description[match(data_flat$duration_interval, lu_duration_interval$duration_interval)]
unique(data_flat$durationMethod) #  "0-2-3-4-5-6-7-8-9-10min" 
sort(unique(data_flat$durationinterval)) # MATCH
# check if it follow WT duration code. Should be empty
print(unique(data_flat$durationinterval[!(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))

data_flat$raw_distance_code <- data_flat$distance_band
data_flat$raw_duration_code <- data_flat$duration_interval
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
survey_tbl <- data_flat %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband,
           durationinterval, isHeard, isSeen, comments) %>% 
  dplyr::summarise(abundance = sum(abundance), .groups= "keep")
WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen")
survey_tbl <- survey_tbl[!duplicated(survey_tbl[,WTsurvey]), WTsurvey] 


write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), quote = FALSE, row.names = FALSE, na = "")
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv")) 

#---VISIT (only where observation exists)
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- pc_visit[!duplicated(pc_visit[,WTvisit]), WTvisit] 
uniqueVisit <- unique(paste0(data_flat$location, " ", data_flat$survey_date))
visit_tbl <- subset(visit_tbl, paste0(visit_tbl$location, " ", visit_tbl$visitDate) %in% uniqueVisit)
write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), quote = FALSE, row.names = FALSE, na = "")
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 

#---LOCATION
location_tbl <- subset(location_tbl,location_tbl$location %in% survey_tbl$location)
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), quote = FALSE, row.names = FALSE)
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 

#---EXTENDED
Extended <- c("organization", "project", "location", "surveyDateTime", "species", "distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
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

