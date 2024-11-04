# ---
# title: "NLBMS2011-16 Script"
# author: "Melina Houle"
# date created: "July 11, 2022"
# Note on translation:
# --- Source data are stored in a .mdb format
# --- Tbl tblStations stores info on location
# --- Tbl tblSurveyConditions INFO stores info on visit
#
# --- FIX DETAILS
# --- 8 locations didn't have XY coordinates. Delete
# --- Fix BirdNameID 86, 2071
# ----- 86 Bald Eagle for Bald eagle
# ----- 2071 None for NONE
# ----- 11 occurence of NA. Delete. Plot was not done. 
# --- Only keep location that have visit/survey. 
# --- Fix location name
# ----- Two surveys were done at the same place, 1 hour apart, use the same coordinates but have different station name.
# ----- Duplicates in coordinates prevent to upload the location. Need to fix location name but assigning the same location name to the second survey. 
# ----- NLBMS2011-16:05155:6 will be changed for NLBMS2011-16:05155:4
# --- Comments column uses newlines and double quote which prevent to upload. 
# ----- Remove newline (\r\n), "", ' in comments. 


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

organization = "CWS-ATL"
dataset_code = "NLBMS2011-16"
pcode = "NLBMS2011-16"

source_data <- "Landbird_Surveys_2016.mdb"
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
pc_location <- sqlFetch(con, "tblStations")
pc_visit <- sqlFetch(con, "tblSurveyConditions")
pc_survey <- sqlFetch(con, "tblData_TimeDetection")
pc_observer <- sqlFetch(con, "tblObservers")
lu_species <- sqlFetch(con, "tblBird_Names")
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
  ## Check if location have all x, y coordinates, Should be empty
  #pc_location[is.na(pc_location$Longitude),]
  #pc_location[is.na(pc_location$Latitude),]
# delete location that have no lat/long coordinates
pc_location <- subset(pc_location, !is.na(pc_location$Latitude))

pc_location <- pc_location %>% 
  select(SiteID, Station, Latitude, Longitude)  %>% 
  mutate(StationID = Station) %>% 
  dplyr::rename(latitude = Latitude,
                longitude = Longitude)
  
pc_location <- pc_location  %>%
  separate(StationID, into= c("site", "station"))%>%
  mutate(organization = organization,
         project = dataset_code, 
         #location = paste(dataset_code, str_replace_all(SiteID, c("site-" = "")), station, sep = ":"),
         location = paste(dataset_code, site, station, sep = ":"),
         # If exists in source data
         utmZone = NA,
         easting = NA, 
         northing = NA,
         missinginlocations = NA,
         )  
# Fix location names
pc_location <- pc_location  %>%
  mutate(location = ifelse(location == "NLBMS2011-16:5155:6", "NLBMS2011-16:5155:4", location))
############################
#### VISIT TABLE ####
############################
#Recover location name
pc_visit <- pc_visit %>% 
  select(Station, SurveyID, Observer, Date, StartTime, Comments)
  
# create Observer unique ID
observerID <- pc_observer %>%
  dplyr::mutate(observerID = 1:n())

visit_obs <- merge(pc_visit, observerID[,c("Name","observerID")], by.x ="Observer", by.y = "Name", all.x = TRUE)
visit_flat <- merge(visit_obs, pc_location, by ="Station", all.x = TRUE)

visit_flat <- visit_flat %>% 
  mutate(visitDate = as.character(format(Date, format = "%Y-%m-%d")),
         snowDepthMeters = NA,  #derived from location at upload
         waterDepthMeters = NA,  #derived from location at upload
         landFeatures = NA,  #ARUs attributes
         crew = NA,   #ARUs attributes
         bait = "None",
         accessMethod = NA,  #ARUs attributes
         comments = NA,
         wildtrax_internal_update_ts = NA,  #created during the upload
         wildtrax_internal_lv_id = NA, #created during the upload
         time_zone = NA,
         data_origin = NA,
         missinginvisit = NA,
         survey_year = substr(visitDate, 1, 4),
         ## observer, observer_raw
         observer = case_when(is.na(Observer) ~ "NA",
                              !is.na(Observer) ~ paste0("NLBMS", str_pad(observerID, 3, pad = "0"))),
         rawObserver = case_when(is.na(Observer) ~ "NA",
                                 !is.na(Observer) ~ Observer),
         # survey_time, surveyDateTime
         survey_time = ifelse(is.na(StartTime), "00:00:01", as.character(StartTime, format = "%H:%M:%S")),
         surveyDateTime = paste(as.character(visitDate), survey_time),
         ## pkey_dt -- Concatenate, separated by colons: [location]:[visitDate]_[survey_time]:[localobservercode]; can also use raw observer or observer ID if needed
         pkey_dt = paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), observer, sep=":"),
         comments <- Comments
         )
  

############################
#### SURVEY TABLE ####
############################
s_data <- merge(pc_survey, visit_flat, by ="SurveyID", all= TRUE)
s_data <- subset(s_data, !is.na(s_data$BirdNameID))
data_flat <- merge(s_data, lu_species[,c("Common_Name", "AOU_acronym", "ID")], by.x ="BirdNameID", by.y = "ID", all.x= TRUE)

survey <- data_flat %>% 
  mutate(distanceMethod = "0m-100m-400m",
        durationMethod = "0-3-6-9-12min",
        ## Species, species_old, comments, scientificname
        original_species = BirdNameID,
        species = ifelse(BirdNameID == 86, "BAEA",
                         ifelse(BirdNameID == 2071, "NONE", 
                                       WT_spTbl$species_code[match(Common_Name, WT_spTbl$species_common_name)])),
        scientificname = WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)],
        isHeard = "DNC",
        isSeen = "DNC",
        count = ifelse(TimePeriod1 == 0 & TimePeriod2 == 0 & TimePeriod3 == 0 & TimePeriod4 == 0, 0, 1),
        distanceband = case_when(DistanceCategory == "0 - 50 m" ~ "0m-100m",
                                 DistanceCategory == "50 - 100 m" ~ "0m-100m",
                                 DistanceCategory == "100 - 200 m" ~ "100m-400m",
                                 DistanceCategory == "0 - 100 m" ~ "0m-100m",
                                 DistanceCategory == "100 - 400 m" ~ "100m-400m",
                                 is.na(DistanceCategory) ~ "UNKNOWN" ),
        durationinterval = case_when(TimePeriod1 == 1 ~ "0-3min",
                                    TimePeriod2 == 1 ~ "3-6min",
                                    TimePeriod3 == 1 ~ "6-9min",
                                    TimePeriod4 == 1 ~ "9-12min",
                                    TimePeriod1 == 0 & TimePeriod2 == 0 & TimePeriod3 == 0 & TimePeriod4 == 0 ~ "before/after"),
        comments = Comments,
        raw_distance_code = DistanceCategory,
        raw_duration_code = case_when(TimePeriod1 == 1 & TimePeriod2 == 0 & TimePeriod3 == 0 & TimePeriod4 == 0 ~ "TimePeriod1",
                                      TimePeriod1 == 1 & TimePeriod2 == 1 & TimePeriod3 == 0 & TimePeriod4 == 0 ~ "TimePeriod1-2",
                                      TimePeriod1 == 1 & TimePeriod2 == 0 & TimePeriod3 == 1 & TimePeriod4 == 0 ~ "TimePeriod1-3",
                                      TimePeriod1 == 1 & TimePeriod2 == 0 & TimePeriod3 == 0 & TimePeriod4 == 1 ~ "TimePeriod1-4",
                                      TimePeriod1 == 1 & TimePeriod2 == 1 & TimePeriod3 == 1 & TimePeriod4 == 0 ~ "TimePeriod1-2-3",
                                      TimePeriod1 == 1 & TimePeriod2 == 1 & TimePeriod3 == 0 & TimePeriod4 == 1 ~ "TimePeriod1-2-4",
                                      TimePeriod1 == 1 & TimePeriod2 == 0 & TimePeriod3 == 1 & TimePeriod4 == 1 ~ "TimePeriod1-3-4",
                                      TimePeriod1 == 1 & TimePeriod2 == 1 & TimePeriod3 == 1 & TimePeriod4 == 1 ~ "TimePeriod1-2-3-4",
                                      TimePeriod1 == 0 & TimePeriod2 == 1 & TimePeriod3 == 0 & TimePeriod4 == 0 ~ "TimePeriod2",
                                      TimePeriod1 == 0 & TimePeriod2 == 1 & TimePeriod3 == 1 & TimePeriod4 == 0 ~ "TimePeriod2-3",
                                      TimePeriod1 == 0 & TimePeriod2 == 1 & TimePeriod3 == 0 & TimePeriod4 == 1 ~ "TimePeriod2-4",
                                      TimePeriod1 == 0 & TimePeriod2 == 1 & TimePeriod3 == 1 & TimePeriod4 == 1 ~ "TimePeriod2-3-4",
                                      TimePeriod1 == 0 & TimePeriod2 == 0 & TimePeriod3 == 1 & TimePeriod4 == 0 ~ "TimePeriod3",
                                      TimePeriod1 == 0 & TimePeriod2 == 0 & TimePeriod3 == 1 & TimePeriod4 == 1 ~ "TimePeriod3-4",
                                      TimePeriod1 == 0 & TimePeriod2 == 0 & TimePeriod3 == 0 & TimePeriod4 == 1 ~ "TimePeriod4",
                                      TimePeriod1 == 0 & TimePeriod2 == 0 & TimePeriod3 == 0 & TimePeriod4 == 0 ~ "NA"),
        missingindetections = NA,
        originalBehaviourData = NA,
        pc_vt = "DNC",
        pc_vt_detail = "DNC",
        age = "DNC",
        fm =  "DNC",
        group =  "DNC",
        flyover = "DNC",
        displaytype = "DNC",
        nestevidence = "DNC",
        behaviourother = "DNC")

# Fix comments
survey$comments <- gsub("[\r\n]", "", survey$comments)

survey <- survey %>% 
  mutate(comments = gsub('["]', '', comments),
         comments = gsub("'", '', comments))


# CHECK
print(unique(survey$distanceMethod[!(survey$distanceMethod %in% WT_distMethTbl$distance_method_type)]))
print(unique(survey$durationMethod[!(survey$durationMethod %in% WT_durMethTbl$duration_method_type)]))
print(unique(survey$BirdNameID[!(survey$species %in% WT_spTbl$species_code)]))
print(unique(survey$distanceband[!(survey$distanceband %in% WT_distBandTbl$distance_band_type)]))
print(unique(survey$durationinterval[!(survey$durationinterval %in% WT_durBandTbl$duration_interval_type)]))
print(unique(survey$comments))
print(survey[is.na(survey$latitude),])
print(survey[is.na(survey$visitDate),])
print(survey[is.na(survey$survey_time),])
print(survey[is.na(survey$ind_count),])
print(survey[is.na(survey$species),])

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
# subset obs that are not before/after
survey_tbl <- subset(survey, !(survey$durationinterval =="before/after"))
survey_tbl <- survey_tbl %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband,
           durationinterval, isHeard, isSeen, comments) %>% 
  dplyr::summarise(abundance = sum(count), .groups= "keep")
WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen", "comments")
survey_tbl <- survey_tbl[!duplicated(survey_tbl[,WTsurvey]), WTsurvey] 


write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), quote = FALSE, row.names = FALSE, na = "")
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv")) 

#---VISIT (only where observation exists)
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- data_flat[!duplicated(data_flat[,WTvisit]), WTvisit] 
write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), quote = FALSE, row.names = FALSE, na = "")
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 

#---LOCATION
WTlocation <- c("location", "latitude", "longitude")
location_tbl <- data_flat[!duplicated(data_flat[,WTlocation]), WTlocation] # 

write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), quote = FALSE, row.names = FALSE)
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 

#---EXTENDED
pc_extended <- survey %>% 
  group_by(organization, project, location, surveyDateTime, species, count, distanceband, durationinterval, site, station, utmZone, easting, 
           northing, missinginlocations, time_zone, data_origin, missinginvisit, pkey_dt, survey_time,
           survey_year, rawObserver, original_species, scientificname, raw_distance_code, raw_duration_code, 
           originalBehaviourData, missingindetections, pc_vt, pc_vt_detail, age, fm, group, flyover, 
           displaytype, nestevidence, behaviourother, comments) %>% 
  dplyr::summarise(ind_count = sum(count), .groups= "keep")
Extended <- c("organization", "project", "location", "surveyDateTime", "species", "ind_count","distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code", 
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
              "displaytype", "nestevidence", "behaviourother","comments")
extended_tbl <- pc_extended[!duplicated(pc_extended[,Extended]), Extended] 
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


   
