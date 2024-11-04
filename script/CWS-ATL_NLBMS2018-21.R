# ---
# title: "NL_BMS_2021 Script"
# author: "Maggie MacPherson"
# date created: "July 11, 2022"
# Note on translation:
# --- Source data are stored in a .accdb format
# --- Tbl DATA_STATIONS stores info on location
# --- Tbl DATA_SURVEY INFO stores info on visit
# --- Tbl DATA_SPECIES COUNTS FROM ARUs for survey
# --- FIX DETAILS
# --- 6 locations didn't have XY coordinates. Delete
# --- 
# --- 

library(tidyverse)
library(dplyr)
library(plyr) #rbind.fill
library(reshape2) # melt
library(readxl)
library(googledrive)
library(RODBC)
library(googlesheets4)

## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

organization <- "CWS-ATL"
dataset_code <- "NLBMS2018-21"
pcode <- "NLBMS2018-21"
source_data <- "NL_BMS_2021.accdb"
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
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

#--------------------------------------------------------------
#       LOAD SOURCE FILE 
#--------------------------------------------------------------
WTpj_Tbl <- read_sheet("https://docs.google.com/spreadsheets/d/1fqifS_E5O_IpW1B-UG_xthr9hzY6FIek-nFjCrt1G0w", sheet = "project")
if (length(list.files(project_dir)) ==0) {
  pid <- WTpj_Tbl %>%
    filter(dataset_code ==pcode) %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  data_id <- gd.list %>%
    filter(name ==source_data) %>%
    select("id")
  drive_download(as_id(as.character(data_id)), path = file.path(project_dir, source_data))
}
#######################################################
##                    Connect
#######################################################
#Connecte and load tables
con <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",data_db))
pc_location <- sqlFetch(con, "DATA_STATIONS")
pc_visit <- sqlFetch(con, "DATA_SURVEY INFO")
pc_survey <- sqlFetch(con, "DATA_SPECIES COUNTS FROM ARUs")
lu_observer <- sqlFetch(con, "Lookup_Observer")
lu_species <- sqlFetch(con, "Lookup_Species")
#Connecte and load tables
#queries<- sqlQuery(con, "BAM-V6")  ##Look up any queries
#tbls <- sqlTables(con) ##Look up tables
#tbls$TABLE_NAME

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
# Check if location have all x, y coordinates, Should be empty
pc_location[is.na(pc_location$Longitude),]
pc_location[is.na(pc_location$Latitude),]
# delete location that have no lat/long coordinates
pc_location <- subset(pc_location, !is.na(pc_location$Latitude))

# Fill column name using space by underscore
names(pc_location)<-str_replace_all(names(pc_location), c(" " = "_"))

pc_location <- pc_location %>% 
  mutate(organization = organization,
         project = dataset_code, 
         site = Plot,
         station = StationID,
         location = paste(str_replace_all(Strata, c(" " = "_")), Plot, StationID, sep = ":"),
         latitude = Latitude ,
         longitude = Longitude,
         # If exists in source data
         utmZone = NA,
         easting = NA, 
         northing = NA,
         missinginlocations = NA)

#---LOCATION
WTlocation <- c("location", "latitude", "longitude")
location_tbl <- pc_location[!duplicated(pc_location[,WTlocation]), WTlocation] # 

############################
#### VISIT TABLE ####
############################
#Fix column names that have space
names(pc_visit)<-str_replace_all(names(pc_visit), c(" " = "_"))

#Recover location name
visit_flat <- merge(pc_visit, pc_location, by ="StationID")
visit_flat <- visit_flat %>% 
  mutate(visitDate = as.character(format(SurveyDate, format = "%Y-%m-%d")),
         snowDepthMeters = NA,  #derived from location at upload
         waterDepthMeters = NA,  #derived from location at upload
         landFeatures = NA,  #ARUs attributes
         crew = NA,   #ARUs attributes
         bait = "None",
         accessMethod = NA,  #ARUs attributes
         comments = NA,
         wildtrax_internal_update_ts = NA,  #created during the upload
         wildtrax_internal_lv_id = NA, #created during the upload
         # surveyDateTime, survey_time
         time_zone = NA,
         data_origin = NA,
         missinginvisit = NA,
         survey_year = substr(visitDate, 1, 4),
         ## observer, observer_raw
         observer = case_when(is.na(Observer) ~ "NA",
                              !is.na(Observer) ~ Observer),
         rawObserver = case_when(is.na(Observer) ~ "NA",
                                 !is.na(Observer) ~ lu_observer$Name[match(visit_flat$Observer, lu_observer$Observer)]),
         time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%S"),
         survey_time = case_when(is.na(time) ~ "00:00:01",
                                 !is.na(time)  ~ as.character(format(time, format = "%H:%M:%S"))),
         ## pkey_dt -- Concatenate, separated by colons: [location]:[visitDate]_[survey_time]:[localobservercode]; can also use raw observer or observer ID if needed
         pkey_dt = paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), observer, sep=":"))

############################
#### SURVEY TABLE ####
############################
names(pc_survey)<-str_replace_all(names(pc_survey), c(" " = "_"))

s_data <- merge(pc_survey, visit_flat, by.x ="CountNumber", by.y = "SurveyNumber")

data_flat <- s_data %>% 
  mutate(surveyDateTime = paste(as.character(visitDate), survey_time),
         ## Distance and duration Methods
         distanceMethod = "0m-25m-50m-100m-INF",
         durationMethod = "0-1-2-3-4-5-6-7-8-9-10min",
         ## Species, species_old, comments, scientificname
         original_species = SPECIES,
         species = case_when(SPECIES =="UNKN" ~ "UNBI",
                             !SPECIES =="UNKN" ~ SPECIES,
                             is.na(SPECIES) ~ "NONE"),
         scientificname = WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)],
         isHeard = ifelse(DISTANCE == "ARU" | DETECTION_TYPE %in% c("Call", "Song"), "Yes", "No"),
         isSeen = ifelse(DETECTION_TYPE == "Visual","Yes", "No"),
         ind_count = ifelse(is.na(SPECIES), 0, Count),
         distanceband = case_when(DISTANCE == "0-25 m" ~ "0m-25m",
                                  DISTANCE == "26-50 m" ~ "25m-50m",
                                  DISTANCE == "50-100 m" ~ "50m-100m",
                                  DISTANCE == "51-100 m" ~ "50m-100m",
                                  DISTANCE == ">100 m" ~ "100m-INF",
                                  DISTANCE == "ARU" ~ "UNKNOWN",
                                  is.na(SPECIES) ~ "UNKNOWN" ),
         durationinterval = ifelse(is.na(Time_interval), "UNKNOWN", gsub(" ", "", Time_interval, fixed = TRUE)),
         raw_distance_code = DISTANCE,
         raw_duration_code = Time_interval,
         missingindetections = NA,
         originalBehaviourData = BREED_EVID,
         pc_vt = ifelse(DETECTION_TYPE %in% c("Call", "Song"), DETECTION_TYPE, "DNC"),
         pc_vt_detail = "DNC",
         age = "DNC",
         fm =  "DNC",
         group =  "DNC",
         flyover = FLYOVER,
         displaytype = case_when(BREED_EVID== "S" ~ "Vocal",
                                 BREED_EVID== "D" ~ "General"),
         nestevidence = "DNC",
         behaviourother = BREED_EVID)

# CHECK
print(unique(data_flat$distanceMethod[!(data_flat$distanceMethod %in% WT_distMethTbl$distance_method_type)]))
print(unique(data_flat$durationMethod[!(data_flat$durationMethod %in% WT_durMethTbl$duration_method_type)]))
print(unique(data_flat$species[!(data_flat$species %in% WT_spTbl$species_code)]))
print(unique(data_flat$distanceband[!(data_flat$distanceband %in% WT_distBandTbl$distance_band_type)]))
print(unique(data_flat$durationinterval[!(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))

#--------------------------------------------------------------
#
#       EXPORT
#
#--------------------------------------------------------------
#Extract GoogleDrive id to store output
dr<- drive_get(paste0("toUpload/",organization), shared_drive = "BAM_Core")

if (nrow(drive_ls(as_id(dr), pattern = dataset_code)) == 0){
  dr_dataset_code <-drive_mkdir(dataset_code, path = as_id(dr), overwrite = NA)
} else {
  dr_dataset_code <- drive_ls(as_id(dr), pattern = dataset_code)
}

#---SURVEY
survey_tbl <- data_flat %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband,
           durationinterval, isHeard, isSeen, comments) %>% 
  dplyr::summarise(abundance = sum(ind_count), .groups= "keep")
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
visit_tbl <- visit_flat[!duplicated(visit_flat[,WTvisit]), WTvisit] 
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
Extended <- c("organization", "project", "location", "surveyDateTime", "species", "ind_count","distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
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


