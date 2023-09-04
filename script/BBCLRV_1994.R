# ---
# Title: "Translate Breeding bird communities in the forests of the Liard River Valley, Yukon"
# Source dataset is an Excel spreadsheet
# Author: "Ana Raymundo & Melina Houle"
# Date: "December 31, 2022"
#
# **** NOTE:
# --- check if the transalation for behaviour into isSeen and isHeard is correct, also, age translation 

update.packages()
library(pacman)
p_load(dplyr,hms,googledrive,glue, lubridate, purrr, readxl, sf,tidyr)
## Initialize variables
wd <- getwd()
setwd(wd)

organization = "CWS-NOR"
dataset_code = "BBCLRV_1994"
source_data <- '1-BBCLRV_1994_Liard_4BAM_new.xlsx'
lu <- "./lookupTables"
WT_spTbl <- "./lookupTables/species_codes.csv"

# Set working folder  ---------------------------------------------------
#set working folder
project_dir <- file.path(wd, "project", dataset_code)
if (!dir.exists(project_dir)) {
  dir.create(project_dir)
}
data_db <- file.path(project_dir, source_data)
if (!file.exists(data_db)) {
  #Download from GoogleDrive
  drive_download(glue('sourceData/',{organization},'/',{dataset_code},'/',{source_data}), path = data_db)
}
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

# Data CRS: WGS84
crs_data <- st_crs(4386) ## TODO: CHECk MAP 
# WildTrax CRS (EPSG: 4386)
crs_WT <- st_crs(4386)

############################
## IMPORT ####
############################
source('./R/multipleSheets.R') #this is a function to read all spreadsheets and returns a list of dataframes  
data_all <- multipleSheets(data_db)
names(data_all)

############################
#### LOCATION TABLE ####
############################

#Read in template
location_raw <- data_all [['Point coordinates']]
survey_raw <- data_all[['Bird data']]

#lookup tables 
lu_sp <- data_all [['Species LU']]
lu_sp_WT <- read.csv("lookupTables/species_codes.csv")

#Tidy raw data
location_data <- location_raw %>% select(POINT,DDLONG, DDLAT,Comment)
s_location <- location_data %>%
  tidyr::separate(POINT, c("project", "site", "station"), ":", remove = FALSE) %>% 
  dplyr::rename('location' = 'POINT',
                'latitude'='DDLAT',
                'longitude'= 'DDLONG',
                'comments' = 'Comment') %>% 
  mutate('bufferRadiusMeters' = NA,
         'elevationMeters' = NA, #created from location
         'isHidden' = FALSE,
         'trueCoordinates' = NA,
         'internal_wildtrax_id' = NA,
         'internal_update_ts' = NA,
         'missinginlocations' = case_when(comments == 'missing' ~ 'Yes', TRUE ~ 'No'),
         'utmZone' = NA,
         'easting' = NA,
         'northing' = NA)


#---LOCATION
WTlocation <- c("location", "latitude", "longitude", "bufferRadiusMeters", "elevationMeters", "isHidden", "trueCoordinates", "comments")
location_tbl <- s_location[!duplicated(s_location[,WTlocation]), WTlocation] 

############################
#### VISIT TABLE ####
############################
pc_raw <- survey_raw %>% select_all(tolower) # change to lower case all column names 
pc_raw$visitDate <- format(pc_raw$date, format = "%Y-%m-%d") 
pc_raw<- tidyr::separate(pc_raw, visitDate, c("year", "month", "day"), "-", remove = FALSE)


pc_visit <- pc_raw %>%  dplyr::rename('location' = 'point',
                                      'observer' = 'obs-code') %>% 
  mutate(snowDepthMeters = NA, #derived from location at upload
         waterDepthMeters = NA,  #derived from location at upload
         landFeatures = NA,  #ARUs attributes
         crew = NA,   #ARUs attributes
         bait = "None", # Wildtrax need to have 'None' instead of NA to avoid problems
         accessMethod = NA, #ARUs attributes
         comments = NA,
         wildtrax_internal_update_ts = NA,  #created during the upload
         wildtrax_internal_lv_id = NA, #created during the upload
         # surveyDateTime, survey_time
         time_zone = NA,
         data_origin = NA,
         missinginvisit = NA,
         survey_time = as.character(format(pc_raw$time, format = "%H:%M:%S")),
         survey_year = year,
         ## observer, observer_raw
         rawObserver = case_when(observer == 'CDE' ~ 'Cameron Eckert',
                                 observer == 'PHS' ~ 'Pamela Sinclair'),
         pkey_dt = glue('{location}:{visitDate}:{survey_time}'))

##EXPORT
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew",
             "bait", "accessMethod", "landFeatures", "comments", "wildtrax_internal_update_ts",
             "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- pc_visit[!duplicated(pc_visit[,WTvisit]),] 

############################
#### SURVEY TABLE ####
############################
pc_survey <- pc_visit %>% dplyr::rename('species' = `species-code`) 
pc_survey <- merge(pc_survey, lu_sp_WT, by.x= 'species', by.y ='species_code', all.x = TRUE)  ## add scientific name and common name in WT format  

pc_survey <- pc_survey %>% mutate(behavior_WT = case_when(behavior == 'C' ~ 'Call',
                                                         behavior == 'S' ~ 'Song',
                                                         behavior == 'D' ~ 'Drumming',
                                                         behavior == 'V' ~ 'Visual',
                                                         TRUE ~ as.character (NA)))

pc_survey <- pc_survey %>% mutate(age_WT = case_when(age == 'AD' ~ 'Adult',
                                                     age == 'JUV' ~ 'Juvenile',
                                                     age %in% c('IMM', 'SUBADULT') ~ 'Immature',
                                                     age == 'U' ~ 'Unknown',
                                                     age == 'NA' ~ 'NA',
                                                     TRUE ~ as.character(age)))
pc_survey<- pc_survey %>% mutate(sex_WT = case_when(sex == 'F' ~ 'Female',
                                                    sex == 'M' ~ 'Male',
                                                    sex == 'U' ~ 'Unknown'))


pc_survey <- pc_survey %>% rename(scientificname = 'scientific_name') %>% 
  mutate(surveyDateTime = glue('{visitDate}:{survey_time}'),
         ## Distance and duration Methods
         distanceMethod = NA, #'0m-75m', ## point counts with 75 m radius
         durationMethod = NA, #"0-10min"  #10 minutes point counts
         comments = NA,
         original_species = species,
         ## isHeard isSeen
         isHeard = ifelse(behavior_WT %in% c('Song', 'Call', 'Drumming'), "Yes", "No"),
         isSeen = ifelse(behavior_WT %in% c('V', 'FE', 'NE', 'FL,C'), "Yes", "No"),
         abundance = abundance,
         distanceband = '0-75m',
         raw_distance_code = NA,
         durationinterval = NA,
         raw_duration_code = NA,
         missingindetections = NA,
         # Behaviour
         originalBehaviourData = behavior,
         pc_vt = ifelse(behavior_WT %in% c('Call', 'Song'), pc_survey$behavior, NA),
         pc_vt_detail = ifelse(behavior_WT == 'Drumming', pc_survey$behavior, NA),
         age = age_WT,
         fm = sex_WT,
         group = NA,
         flyover = NA,
         displaytype = NA,
         nestevidence = ifelse(behavior == 'NE', "Yes", "No"),
         behaviourother = NA)


                                 
survey_tbl <- pc_survey %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband,
           durationinterval, isHeard, isSeen, comments) %>% 
  dplyr::summarise(abundance = sum(abundance), .groups= "keep")



WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen", "comments")
survey_tbl <- survey_tbl[!duplicated(survey_tbl[,WTsurvey]), WTsurvey] 
write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), row.names = FALSE)


############################
#### EXTENDED TABLE ####
############################

#Read in template
extended.temp <- read_xlsx("template/Template-WT-4Extended.xlsx", sheet=1)
extended <- left_join(s_location, pc_survey)

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
write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), quote = FALSE, row.names = FALSE)
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv"))

#---VISIT (only where observation exists)
uniqueVisit <- unique(paste0(pc_survey$location, " ", pc_survey$visitDate))
visit_tbl <- subset(visit_tbl, paste0(visit_tbl$location, " ", visit_tbl$visitDate) %in% uniqueVisit)
write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), quote = FALSE, row.names = FALSE)
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"))

#---LOCATION (only select location with observation)
location_tbl <- subset(location_tbl,location_tbl$location %in% survey_tbl$location)
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), quote = FALSE, row.names = FALSE)
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"))

#---EXTENDED

#Extend
extended.wt <- extended %>% 
  dplyr::select(colnames(extended.temp))

Extended <- c("location", "surveyDateTime", "species", "distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code", 
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
              "displaytype", "nestevidence", "behaviourother")
extended_tbl <- extended[!duplicated(extended[,Extended]), Extended] 
write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_extended.csv")), quote = FALSE, row.names = FALSE)
extended_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = extended_tbl, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"))

