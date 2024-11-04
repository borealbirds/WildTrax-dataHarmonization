# Title: "Translate Alaska Landbird Monitoring Survey 2002-2022"
# Source dataset is an excel spreadsheet
# Author: "Melina Houle"
# Date: "January 9, 2024"
# Note on translation:
# --- Data were pre-processed by Hedwig Lankau and were stored following the BAM v6 standard. 
# --- Drop before/after detection.
# --- 3 detections have empty cell as abundance. Filles it with 1. 

#update.packages()
library(googlesheets4)
library(dplyr) # mutate, %>%
library(terra)
library(googledrive) #drive_get, drive_mkdir, drive_ls, drive_upload
library(stringr)
library(readr)
library(RODBC) #odbcConnect, sqlFetch
## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

organization <- "INTERFOR"
dataset_code <- "INTERFOR2017"

project_dir <- file.path("./project", dataset_code)
if (!dir.exists(project_dir)) {
  dir.create(project_dir)
}
out_dir <- file.path("./out", dataset_code)   
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

# Lookup Table
WTpj_Tbl <- read_sheet("https://docs.google.com/spreadsheets/d/1fqifS_E5O_IpW1B-UG_xthr9hzY6FIek-nFjCrt1G0w", sheet = "project")
lu <- "./lookupTables"
WT_spTbl <- read.csv(file.path(lu, "species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path(lu, "duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path(lu, "distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path(lu, "duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path(lu, "distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

#--------------------------------------------------------------
#       LOAD
#--------------------------------------------------------------
if (length(list.files(project_dir)) ==0) {
  pid <- WTpj_Tbl %>%
    filter(dataset_code =="INTERFOR2017") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  data_id <- gd.list %>%
    filter(name =="Interfor-Proofed-V6-08Feb2021.accdb") %>%
    select("id")
  drive_download(as_id(as.character(data_id)), path = file.path(project_dir, "Interfor-Proofed-V6-08Feb2021.accdb"))
}

data_db <- file.path(project_dir, "Interfor-Proofed-V6-08Feb2021.accdb")
con <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",data_db))

detection <- sqlFetch(con, "INT_detection")
names(detection)<-str_replace_all(names(detection), c(" " = "_"))
visit <- sqlFetch(con, "INT_visit")
names(visit)<-str_replace_all(names(visit), c(" " = "_"))
location <- sqlFetch(con, "INT_location")
names(location)<-str_replace_all(names(location), c(" " = "_"))

#--------------------------------------------------------------
#
#       TRANSLATE
data_flat <- location %>%
  inner_join(visit, by= "location_name") %>%
  select(-PKEY2, -ROUND,-OBSRVR, -observer_id,-COMMENTS, -location_name) %>%
  inner_join(detection, by="PKEY")

############################
#### VISIT TABLE ####
############################
s_data <- data_flat %>% 
  rename(location = location_name,
         rawObserver = OBSRVR,
         comments = COMMENTS,
         utmZone = UTM_Zone,
         latitude = Latitude, 
         longitude = Longitude,
         visitDate = survey_date,
         site = Site,
         station = Station,
         easting = Easting, 
         northing = Northing)  %>%
  mutate(observer = paste0("obs",observer_id),
         snowDepthMeters= NA,
         waterDepthMeters = NA,
         crew = NA,
         bait = "NONE",
         accessMethod = NA,
         landFeatures = NA,
         wildtrax_internal_update_ts = NA,
         wildtrax_internal_lv_id = NA,
         comments = NA,
         time_zone = NA,       
         data_origin = NA,
         missinginlocations = NA,
         missinginvisit = NA)

############################
#### SURVEY TABLE ####
############################
pc_survey <- s_data %>% 
  mutate(organization = organization,
         project = dataset_code,
         survey_time = format(data_flat$survey_time, format = "%H:%M:%S"),
         surveyDateTime = paste(visitDate, survey_time),
         pkey_dt = paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), observer, sep=":"),
         raw_distance_code = distance_band,
         raw_duration_code = duration_interval,
         durationMethod = "0-3-5-10min",
         durationinterval = case_when(duration_interval == "2" ~ "5-10min",
                                      duration_interval == "3" ~ "before/after",
                                      duration_interval == "5" ~ "0-3min",
                                      duration_interval == "7" ~ "3-5min",
                                      TRUE ~ "UNKNOWN"),
        # distanceMethod = ifelse(Distance == ">150", "0m-10m-20m-30m-40m-50m-60m-70m-80m-90m-100m-125m-150m-INF", "0m-10m-20m-30m-40m-50m-60m-70m-80m-90m-100m-125m-150m-200m-250m-300m-350m-400m-INF"),
         distanceMethod = "0m-50m-100m-INF", 
         distanceband = case_when(distance_band == "1" ~ "0m-50m",
                                  distance_band == "2" ~ "50m-100m",
                                  distance_band == "3" ~ "100m-INF",
                                  TRUE ~ "UNKNOWN"),
         species= case_when(BAM_Species == "WOOD_UNI" ~ "UNWO",
                            BAM_Species == "VIRE_UNI" ~ "UNVI",
                            BAM_Species == "HUMM_UNI" ~ "UNHU",
                            TRUE ~ BAM_Species),
         scientificname = WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)],
         missingindetections = NA,
         isHeard = heard,
         isSeen = seen,
        abundance = ifelse(is.na(abundance), 1, abundance)) %>%
  rename(ind_count = abundance)


############################
####    BEHAVIOR        ####
############################
#age : Adult, Juvenile, Fledgling, Unknown,  DNC,  Null, Immature
#fm: Female, Male, Unknown,  DNC, Null
#group: Flock, Family, Pair, DNC, Null
#flyover: Yes, No, DNC, NULL
#nestevidence: Yes, No, DNC, Null
#displaytype: General, Aerial, Distraction, Auditory, DNC, Null, Agitated
#pc_vt: Song, Call, Non-Vocal, S-C, NV-C, DNC, Null
#pc_vt_detail: Drumming, Whinnowing, Wing boom, Null, DNC

#### 
pc_behavior <- pc_survey %>% 
  dplyr::mutate(originalBehaviourData = BAM_Behaviour,
                pc_vt = nvtype,
                pc_vt_detail= "DNC")

## CHECK
print(unique(pc_survey$distanceMethod[!(pc_survey$distanceMethod %in% WT_distMethTbl$distance_method_type)]))
print(unique(pc_survey$durationMethod[!(pc_survey$durationMethod %in% WT_durMethTbl$duration_method_type)]))
print(unique(pc_survey$species[!(pc_survey$species %in% WT_spTbl$species_code)]))
print(unique(pc_survey$original_species[!(pc_survey$species %in% WT_spTbl$species_code)]))

print(unique(pc_survey$durationinterval[!(pc_survey$durationinterval %in% WT_durBandTbl$duration_interval_type)]))
print(unique(pc_survey$distanceband[!(pc_survey$distanceband %in% WT_distBandTbl$distance_band_type)]))


#--------------------------------------------------------------
#
#       EXPORT
#
#--------------------------------------------------------------
dr<- drive_get(paste0("DataTransfered/",organization), shared_drive= "BAM_Core")

#Set GoogleDrive id
if (nrow(drive_ls(as_id(dr), pattern = dataset_code)) == 0){
  dr_dataset_code <-drive_mkdir(dataset_code, path = as_id(dr), overwrite = NA)
} else {
  dr_dataset_code <- drive_ls(as_id(dr), pattern = dataset_code)
}
dr_ls <- drive_ls(as_id(dr), pattern = dataset_code)

#---LOCATION
no_before_after <- pc_behavior %>%
  filter(!durationinterval == "before/after")

WTlocation <- c("location", "latitude", "longitude")

# Remove duplicated location
location_tbl <- no_before_after[!duplicated(no_before_after[,WTlocation]), WTlocation] 
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), row.names = FALSE, na = "")
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 

#---VISIT
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- no_before_after[!duplicated(no_before_after[,WTvisit]), WTvisit] # 

write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), row.names = FALSE, na = "")
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 

#---SURVEY
survey_tbl <- no_before_after %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband, durationinterval, isHeard, isSeen, comments) %>%
  dplyr::summarise(abundance = sum(ind_count), .groups= "keep")

write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), row.names = FALSE, na = "")
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv"), overwrite = TRUE) 

#---EXTENDED
extended_tbl <- pc_behavior %>% 
  group_by(organization, project,location, surveyDateTime, species, ind_count, distanceband, durationinterval, site, station, utmZone, easting, 
           northing, missinginlocations, time_zone, data_origin, missinginvisit, pkey_dt, survey_time,
           survey_year, rawObserver, original_species, scientificname, raw_distance_code, raw_duration_code, 
           originalBehaviourData, missingindetections, pc_vt, pc_vt_detail,age, fm, group, flyover, 
           displaytype, nestevidence, behaviourother) %>%
  dplyr::summarise(abundance = sum(ind_count), .groups= "keep")

write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_behavior.csv")), quote = FALSE, row.names = FALSE, na = "")
extended_out <- file.path(out_dir, paste0(dataset_code,"_behavior.csv"))
drive_upload(media = extended_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_behavior.csv"), overwrite = TRUE) 

#---PROCESSING STATS
write_lines(paste0("Organization: ", organization), file.path(out_dir, paste0(dataset_code, "_stats.csv")))
write_lines(paste0("Project: ", dataset_code), file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
nrow_location <- paste0("Number of locations: ", nrow(location_tbl))
write_lines(nrow_location, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
nrow_visit <- paste0("Number of visit: ", nrow(visit_tbl))
write_lines(nrow_visit, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
nrow_survey <- paste0("Number of survey: ", nrow(survey_tbl))
write_lines(nrow_survey, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
nrow_extended <- paste0("Number of extended: ", nrow(extended_tbl))
write_lines(nrow_extended, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)


