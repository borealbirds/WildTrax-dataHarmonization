# Title: "Translate Alaska Landbird Monitoring Survey 2002-2022"
# Source dataset is an excel spreadsheet
# Author: "Melina Houle"
# Date: "January 9, 2024"
#update.packages()
library(googlesheets4)
library(dplyr) # mutate, %>%
library(terra)
library(googledrive) #drive_get, drive_mkdir, drive_ls, drive_upload
library(stringr)
library(readr)

## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

organization <- "USGS-ALASKA"
dataset_code <- "ALMS2002-22"

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
    filter(dataset_code =="ALMS2002-22") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  detection_id <- gd.list %>%
    filter(name =="AlaskaLandbirdMonitoringSurvey_bird_detections.csv") %>%
    select("id")
  drive_download(as_id(as.character(detection_id)), path = file.path(project_dir, "AlaskaLandbirdMonitoringSurvey_bird_detections.csv"))
  location_id <- gd.list %>%
    filter(name =="AlaskaLandbirdMonitoringSurvey_location_info.csv") %>%
    select("id")
  drive_download(as_id(as.character(location_id)), path = file.path(project_dir, "AlaskaLandbirdMonitoringSurvey_location_info.csv"))
}

detection <- read.csv(file.path(project_dir, "AlaskaLandbirdMonitoringSurvey_bird_detections.csv"))
location <- read.csv(file.path(project_dir, "AlaskaLandbirdMonitoringSurvey_location_info.csv"))

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
#Format
location <- location %>% 
  select(BlockPt, BlockNo, Point, Lat_NAD83,	Long_NAD83)  %>%
  dplyr::rename(northing = Lat_NAD83,
                easting = Long_NAD83,
                site = BlockNo, 
                station = Point) %>%
  mutate(location = paste(dataset_code, BlockPt, sep= ":"),
         missinginlocations = NA)

# REPROJECT
spLocation <- vect(location, geom=c("easting", "northing"), crs="epsg:4269")
spLocation_pj <- as.data.frame(project(spLocation,"EPSG:4326"),  geom = "XY") # WILDTRAX

s_location <- location %>%
  left_join(spLocation_pj[,c("BlockPt", "x", "y")], by = "BlockPt") %>%
  dplyr::rename(longitude = x,
                latitude = y) %>%
 select(location, longitude, latitude, northing, easting, missinginlocations,  site, station)

############################
#### VISIT TABLE ####
############################
s_visit <- detection %>% 
  select(BlockPt, Year, Date, Hr, Min, CountDur, SpeciesCode, Period, TotalBirds, Distance, Behavior, ObsNo) %>% 
  mutate(location = paste(dataset_code, BlockPt, sep= ":"),
         visitDate = as.character(Date),
         rawObserver = as.character(ObsNo),
         observer = paste0("obs",ObsNo),
         survey_time = as.character(sprintf("%s:%s:00", detection$Hr, str_pad(detection$Min,2,"left","0"))),
         snowDepthMeters= NA,
         waterDepthMeters = NA,
         crew = NA,
         bait = "NONE",
         accessMethod = NA,
         landFeatures = NA,
         wildtrax_internal_update_ts = NA,
         wildtrax_internal_lv_id = NA,
         comments = NA,
         utmZone = NA,
         time_zone = NA,       
         data_origin = NA,
         missinginvisit = NA,
         survey_year = Year) %>%
  left_join(s_location, by = "location")

############################
#### SURVEY TABLE ####
############################
# create a list of closed habitat based on distnce >150m
closed_location <- unique(subset(s_visit$location, s_visit$Distance == ">150"))

pc_survey <- s_visit %>% 
  mutate(organization = organization,
         project = dataset_code,
         original_species = SpeciesCode,
         surveyDateTime = paste(visitDate, survey_time),
         pkey_dt = paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), observer, sep=":"),
         raw_distance_code = Distance,
         raw_duration_code = Period,
         durationMethod = "0-3-5-8-10min",
         durationinterval = case_when(Period == "3" ~ "0-3min",
                                      Period == "5" ~ "3-5min",
                                      Period == "8" ~ "5-8min",
                                      Period == "10" ~ "8-10min",
                                      TRUE ~ "UNKNOWN"),
        # distanceMethod = ifelse(Distance == ">150", "0m-10m-20m-30m-40m-50m-60m-70m-80m-90m-100m-125m-150m-INF", "0m-10m-20m-30m-40m-50m-60m-70m-80m-90m-100m-125m-150m-200m-250m-300m-350m-400m-INF"),
         distanceMethod = "0m-10m-20m-30m-40m-50m-60m-70m-80m-90m-100m-125m-150m-INF", 
         distanceband = case_when(Distance == "10" ~ "0m-10m",
                                  Distance == "20" ~ "10m-20m",
                                  Distance == "30" ~ "20m-30m",
                                  Distance == "40" ~ "30m-40m",
                                  Distance == "50" ~ "40m-50m",
                                  Distance == "60" ~ "50m-60m",
                                  Distance == "70" ~ "60m-70m",
                                  Distance == "80" ~ "70m-80m",
                                  Distance == "90" ~ "80m-90m",
                                  Distance == "100" ~ "90m-100m",
                                  Distance == "125" ~ "100m-125m",
                                  Distance == "150" ~ "125m-150m",
                                  Distance == "200" ~ "150m-INF",
                                  Distance == "250" ~ "150m-INF",
                                  Distance == "300" ~ "150m-INF",
                                  Distance == "350" ~ "150m-INF",
                                  Distance == "400" ~ "150m-INF",
                                  Distance == ">150" ~ "150m-INF",
                                  Distance == ">400" ~ "150m-INF",
                                  TRUE ~ "UNKNOWN"),
         species = case_when(SpeciesCode == "WEFL" ~ "PSFL",
                             SpeciesCode == "CCGO" ~ "CACG",
                             SpeciesCode == "UNCH" ~ "UPCH",
                             SpeciesCode == "UNCO" ~ "UNCV",
                             SpeciesCode == "UNCR" ~ "UCRS",
                             SpeciesCode == "UNGD" ~ "UGOL",
                             SpeciesCode == "UNSA" ~ "UPEE",
                             SpeciesCode == "UNSL" ~ "UNSW",
                             SpeciesCode == "UNSO" ~ "USCT",
                             SpeciesCode == "UNSW" ~ "USWN",
                             SpeciesCode == "UNFR" ~ "UNFN",
                             TRUE ~ SpeciesCode), 
         scientificname = WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)],
         missingindetections = NA,
         isHeard = ifelse(Behavior %in% c("B","C","CD","CS","CV","D","DC",
                                          "DV","FC", "FOC", "FOS", "JC", "MC", 
                                          "MFC", "PB", "PC", "PD", "PFOC", "PJ","PS",
                                          "S","SC","SM","SV","VC"), "Yes", "No"),
         isSeen = ifelse(Behavior %in% c("B","C","CD","CS","D","DC","JC", "N", 
                                          "P", "PB", "PC", "PD", "PJ", "PS",
                                          "S","SC"), "No", "Yes")
         ) %>%
  rename(ind_count = TotalBirds)


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
  dplyr::mutate(
         originalBehaviourData = Behavior,
         age = case_when(Behavior %in% c("F","FN","M", "MC","MF","PMF","SM")~ "Adult",
                         Behavior %in% c("J","PJ")~ "Juvenile", 
                         TRUE ~ "DNC"),
         fm = case_when(Behavior %in% c("F","FC", "FN") ~ "Female",
                        Behavior %in% c("M","MC") ~ "Male", 
                        Behavior =="N" ~ "Null", 
                        Behavior %in% c("CV","DV","FOV","J","JC","PV","SV","V","VC") ~ "Unknown", 
                        TRUE ~ "DNC"),
         group = case_when(Behavior %in% c("MF","MFC", "MFN", "PMF") ~ "Pair",
                           TRUE ~ "Null"),
         flyover = ifelse(Behavior %in% c("FO","FOC","FOS","FOV","PFOC"), "Yes", "No"),
         nestevidence = ifelse(Behavior %in% c("N","FN", "MFN"), "Yes", "No"),
         displaytype = case_when(Behavior %in% c("CD","CV","D","DC","DV","PB","PD") ~ "General",
                                  Behavior %in% c("FD","PFD") ~ "Aerial",
                                  TRUE ~ "Null"),
         pc_vt = case_when(Behavior %in% c("FOS","PS", "S","SC","SM","SV") ~ "Song",
                           Behavior %in% c("B","D","DC","DV","PB","PD") ~ "Non-Vocal",
                           Behavior %in% c("P","PFD","PJ","PMF") ~ "DNC",
                           Behavior %in% c("C","CD","CS","CV","FC","FOC","JC","MC",
                                           "MFC","PC","PFOC","VC") ~ "Call",
                           TRUE ~ "Null"),
         pc_vt_detail = case_when(Behavior %in% c("D","DC","DV","PD") ~ "Drumming", 
                                  Behavior %in% c("B","PB") ~ "Wing bloom",
                                  TRUE ~ "Null"),
         behaviourother = NA,
         comments = NA)

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
no_flyover <- pc_behavior %>%
  filter(!flyover == "Yes")

WTlocation <- c("location", "latitude", "longitude")

# Remove duplicated location
location_tbl <- no_flyover[!duplicated(no_flyover[,WTlocation]), WTlocation] 
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), row.names = FALSE, na = "")
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 

#---VISIT
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- no_flyover[!duplicated(no_flyover[,WTvisit]), WTvisit] # 

write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), row.names = FALSE, na = "")
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 

#---SURVEY
survey_tbl <- no_flyover %>% 
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
extended_out <- file.path(out_dir, paste0(dataset_code,"_extended.csv"))
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


