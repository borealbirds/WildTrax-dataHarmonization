## ---
# PCODE: PCHE-EHJV
# Title: "PCHE-EHJV SCF-CWS Legacy Sites"
# Source dataset were pre-processed by Bruno Drolet from ECCC. They were provided in a .csv format
# Author: "Ana Raymundo, "
# Date: "October 4th, 2022"
# Note on translation: 
#1. Bruno Drolet fixed the old file and created three new files with the right project assignation. 
#2. We only fixed some issues with dates and separated the tables into 8 tables that correspond to each of the projects.  
# ---
# --- FIX DETAILS
# --- 



# Load libraries ----------------------------------------------------------
update.packages()
library(pacman)
library(tidyr) #separate
p_load(dplyr,hms, googledrive,glue, lubridate, purrr, readxl, sf)

# Initialize variables ----------------------------------------------------
# working directory 
wd <- getwd()
setwd(wd)

organization <- "CWS-QUE"
dataset_code <- "PCHE-EHJV"
source_data <- c('PCHE_EHJV_location_BD.csv','PCHE_EHJV_survey_BD.csv','PCHE_EHJV_visit_BD.csv')

lu <- "./lookupTables"
WT_spTbl <- read.csv(file.path("./lookupTables/species_codes.csv"))
WT_durMethTbl <- read.csv(file.path("./lookupTables/duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path("./lookupTables/distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path("./lookupTables/duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path("./lookupTables/distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

# Set working folder  ---------------------------------------------------
project_dir <- file.path(wd, "project", dataset_code)
if (!dir.exists(project_dir)) {
  dir.create(project_dir)
}
data_db <- file.path(project_dir, source_data)
if (!file.exists(data_db)) {
  #Download from GoogleDrive
  drive_download(paste0("sourceData/", source_data), path = data_db)
}
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

############################
#### LOCATION TABLE ####
############################
#Import raw data 
location_raw <- read.csv(glue('{project_dir}/PCHE-EHJV_location_BD.csv'))

#Format
location_all <- location_raw %>% 
  dplyr::rename(organization = Wildtrax_Organization,
                project = Widtrax_Project,
                location = Location_New,
                latitude = Latitude,
                longitude = Longitude) %>%
  mutate(site = NA,
         station = NA,
         easting = NA,
         northing = NA,
         missinginlocations = NA)

############################
#### VISIT TABLE ####
############################
#Import raw data
visit_raw <- read.csv(glue('{project_dir}/PCHE-EHJV_visit_BD.csv'))

#Format
pc_visit <- visit_raw %>% 
  dplyr::rename(location = location_New) %>% 
  mutate(missingvisit = NA,
         rawObserver = NA, 
         pkey_dt = paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), "NA", sep=":"),
         utmZone = NA,
         time_zone = NA,       
         data_origin = NA,
         missinginvisit = NA,
         survey_year = substr(visitDate, 1, 4))

pc_visit <- merge(pc_visit, location_all, by = 'location')

############################
#### SURVEY TABLE ####
############################
#Import raw data
survey_raw <- read.csv(glue('{project_dir}/PCHE-EHJV_survey_BD.csv'))
pc_survey <- survey_raw %>% 
              dplyr::rename(location = Location_New,
                            visitDate = surveyDateTime,
                            survey_time = surveyTime,
                            original_species = Species)
pc_detection <- merge(pc_survey, pc_visit, by = c('location', 'visitDate', 'survey_time'))
                     
pc_detection <- pc_detection %>%
  mutate(surveyDateTime = paste(visitDate, survey_time),
         #species = Species,
         #species = case_when(Species == "UDEJ" ~ "DEJU",
         #                    Species == "CAGO"  ~ "CANG",
         #                    Species == "BDOW" ~ "BADO",
         #                    Species == "UDAN"~ "UDAB",
         #                    Scientific.Name == "Colaptes auratus" ~ "NOFL",
         #                    Scientific.Name == "Lagopus lagopus" ~ "WIPT",
         #                    Scientific.Name == "Bonasa umbellus" ~ "RUGR",
         #                    Scientific.Name == "Empidonax sp." ~ "UEFL",
         #                    Scientific.Name == "Vireo sp." ~ "UNVI",
         #                    Scientific.Name == "Meleagris gallopavo" ~ "WITU",
         #                    Scientific.Name == "Tympanuchus phasianellus" ~ "STGR",
          #                   .default = Species),
         species = original_species,
         scientificname = WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)],
         distanceMethod = "0m-INF",
         distanceband = "0m-INF",
         durationinterval = ifelse(durationMethod == "0-10min", "0-10min",
                             ifelse(durationMethod == "0-3min", "0-3min",
                               ifelse(durationMethod == "0-15min", "0-15min",
                                  ifelse(durationMethod == "0-20min", "0-20min",
                                    ifelse(durationMethod == "0-5min", "0-5min", "UNKNOWN"))))),
         abundance = ifelse(abundance == "TMTT",  as.integer(999), as.integer(abundance)),
         observer = "NA",
         missingindetections = NA,
         raw_distance_code = "NA",
         raw_duration_code = "NA",
         #Behaviour
         originalBehaviourData = NA,
         pc_vt = NA,
         pc_vt_detail = NA,
         age = NA,
         fm = NA,
         group = NA,
         flyover = NA,
         displaytype = NA,
         nestevidence = NA,
         behaviourother = NA,
         comments = NA)


## CHECK
print(unique(pc_detection$distanceMethod[!(pc_detection$distanceMethod %in% WT_distMethTbl$distance_method_type)]))
print(unique(pc_detection$durationMethod[!(pc_detection$durationMethod %in% WT_durMethTbl$duration_method_type)]))
print(unique(pc_detection$species[!(pc_detection$species %in% WT_spTbl$species_code)]))
# Fix species 
pc_detection <- pc_detection  %>%
  mutate(species = ifelse(original_species == "UDEJ", "DEJU",
                    ifelse(original_species == "CAGO", "CANG",
                      ifelse(original_species == "BDOW", "BADO",
                        ifelse(original_species == "UDAN", "UDAB",
                            ifelse(Scientific.Name == "Colaptes auratus", "NOFL",
                              ifelse(Scientific.Name == "Lagopus lagopus", "WIPT",
                                ifelse(Scientific.Name == "Bonasa umbellus", "RUGR",
                                  ifelse(Scientific.Name == "Empidonax sp.", "UEFL",
                                    ifelse(Scientific.Name == "Vireo sp.", "UNVI",
                                      ifelse(Scientific.Name == "Meleagris gallopavo", "WITU",
                                        ifelse(Scientific.Name == "Tympanuchus phasianellus", "STGR", species))))))))))),
         scientificname = WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)],
 )
print(unique(pc_detection$species[!(pc_detection$species %in% WT_spTbl$species_code)]))

print(unique(pc_detection$durationinterval[!(pc_detection$durationinterval %in% WT_durBandTbl$duration_interval_type)]))
print(unique(pc_detection$distanceband[!(pc_detection$distanceband %in% WT_distBandTbl$distance_band_type)]))
print(unique(pc_detection$durationinterval[!(pc_detection$durationinterval %in% WT_durBandTbl$duration_interval_type)]))


#--------------------------------------------------------------
#
#       EXPORT
#
#--------------------------------------------------------------
location_pc <- subset(location_all,location_all$location %in% pc_detection$location)
visit_pc <- subset(pc_visit,pc_visit$location %in% pc_detection$location)

dataset_code <- unique(pc_detection$project)
dr<- drive_get(paste0("toUpload/",organization))
for (x in dataset_code) {
  #Set GoogleDrive id
  #if (nrow(drive_ls(as_id(dr), pattern = x)) == 0){
  #  dr_dataset_code <-drive_mkdir(x, path = as_id(dr), overwrite = NA)
  #} else {
  #  dr_dataset_code <- drive_ls(as_id(dr), pattern = x)
  #}
  #dr_ls <- drive_ls(as_id(dr), pattern = x)
  
  #---LOCATION
  location <- location_pc[location_pc$project==x,]
  WTlocation <- c("location", "latitude", "longitude")
  
  # Remove duplicated location
  location_tbl <- location[!duplicated(location[,WTlocation]), WTlocation] # 
  
  
  write.csv(location_tbl, file= file.path(out_dir, paste0(x,"_location.csv")), row.names = FALSE, na = "")
  #location_out <- file.path(out_dir, paste0(x,"_location.csv"))
  #drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(x,"_location.csv"), overwrite = TRUE) 
  
  #---VISIT
  visit <- visit_pc[visit_pc$project==x,]
  WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
               "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")
  
  #Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
  visit_tbl <- visit[!duplicated(visit[,WTvisit]), WTvisit] # 
  
  write.csv(visit_tbl, file= file.path(out_dir, paste0(x,"_visit.csv")), row.names = FALSE, na = "")
  #visit_out <- file.path(out_dir, paste0(x,"_visit.csv"))
  #drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(x,"_visit.csv"), overwrite = TRUE) 
  
  #---SURVEY
  survey_tbl <- pc_detection[pc_detection$project==x,]

  WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
                "durationinterval", "abundance", "isHeard", "isSeen", "comments")
  write.csv(survey_tbl, file= file.path(out_dir, paste0(x,"_survey.csv")), row.names = FALSE, na = "")
  #survey_out <- file.path(out_dir, paste0(x,"_survey.csv"))
  #drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(x,"_survey.csv"), overwrite = TRUE) 
  
  #---EXTENDED
  Extended <- c("organization", "project","location", "surveyDateTime", "species", "abundance", "distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
                "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
                "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code", 
                "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
                "displaytype", "nestevidence", "behaviourother")
  extended_tbl <- survey_tbl[!duplicated(survey_tbl[,Extended]), Extended] 
  write.csv(extended_tbl, file.path(out_dir, paste0(x, "_extended.csv")), quote = FALSE, row.names = FALSE, na = "")
  #extended_out <- file.path(out_dir, paste0(x,"_extended.csv"))
  #drive_upload(media = extended_out, path = as_id(dr_dataset_code), name = paste0(x,"_extended.csv"), overwrite = TRUE) 
  
  #---PROCESSING STATS
  write_lines(paste0("Organization: ", organization), file.path(out_dir, paste0(x, "_stats.csv")))
  write_lines(paste0("Project: ", x), file.path(out_dir, paste0(x, "_stats.csv")), append= TRUE)
  nrow_location <- paste0("Number of locations: ", nrow(location_tbl))
  write_lines(nrow_location, file.path(out_dir, paste0(x, "_stats.csv")), append= TRUE)
  nrow_visit <- paste0("Number of visit: ", nrow(visit_tbl))
  write_lines(nrow_visit, file.path(out_dir, paste0(x, "_stats.csv")), append= TRUE)
  nrow_survey <- paste0("Number of survey: ", nrow(survey_tbl))
  write_lines(nrow_survey, file.path(out_dir, paste0(x, "_stats.csv")), append= TRUE)
  nrow_extended <- paste0("Number of extended: ", nrow(extended_tbl))
  write_lines(nrow_extended, file.path(out_dir, paste0(x, "_stats.csv")), append= TRUE)
}


