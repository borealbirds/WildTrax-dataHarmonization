# Title: "Ontario Frontenac Birds Studies point  count survey 2014-2023"
# Source dataset is a xlsx file
# Author: "Melina Houle"
# Date: "March 20, 2024"
#  Notes on translation:
#   -- 6 rows don't have XY coordinates. They were removed. 
#   -- point_id =893 has an error typo for easting 189958. Should be 389958.
#   -- Duplicates location names for different XY. Contacted Birds Canada on 20-03-2024.
#   -- Decision is to use the mean coordinates when locations are sample over different years. 
library(dplyr) # mutate, %>%
library(readxl) #read_excel
library(sf) #st_crs, st_as_sf, st_transform, st_drop_geometry
library(purrr) #map
library(googledrive) #drive_get, drive_mkdir, drive_ls, drive_upload
library(googlesheets4)
library(data.table) #pivot.longer
library(readr) #write_lines

## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

organization <- "Birds Canada"
dataset_code <- "ONFFIBA2014-23"

# Lookup Table
WTpj_Tbl <- read_sheet("https://docs.google.com/spreadsheets/d/1fqifS_E5O_IpW1B-UG_xthr9hzY6FIek-nFjCrt1G0w", sheet = "project")
lu <- "./lookupTables"
WT_spTbl <- read.csv(file.path(lu, "species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path(lu, "duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path(lu, "distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path(lu, "duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path(lu, "distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}
project_dir <- file.path(wd, "project", dataset_code)
if (!dir.exists(project_dir)) {
  dir.create(project_dir)
}

#--------------------------------------------------------------
#       LOAD
#--------------------------------------------------------------
if (length(list.files(project_dir)) ==0) {
  pid <- WTpj_Tbl %>%
    filter(dataset_code == "ONFFIBA2014-23") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  location_id <- gd.list %>%
    filter(name =="Birds Canada Frontenac Data 2015-2023.xlsx") %>%
    select("id")
  drive_download(as_id(as.character(location_id)), path = file.path(project_dir, "Birds Canada Frontenac Data 2015-2023.xlsx"))
}

detection <- read_excel(file.path(project_dir, "Birds Canada Frontenac Data 2015-2023.xlsx"))

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
# Transform
# Subset only the ones with XY coordinates
detection <- detection[!is.na(detection$easting),] # delete 6 rows

location.distinct <- detection %>%
  mutate(location = paste(dataset_code, site_id, point_id, sep= ":"),
         easting = case_when(point_id == 893 ~ 389958,
                             TRUE ~ easting)) %>%
  select(location, easting, northing) %>%
  distinct() 

loc.easting <- location.distinct %>%
  group_by(location) %>%
  dplyr::summarise(mean_easting = mean(easting))
loc.northing <- location.distinct %>%
  group_by(location) %>%
  dplyr::summarise(mean_northing = mean(northing))
 
location.fix <- loc.easting %>%
  left_join(loc.northing)

crs_utm18N <- st_crs(26918) # Data CRS: NAD83 UTM18N
crs_WT <- st_crs(4326)# WildTrax CRS 

detection_18N <- st_as_sf(location.fix, coords = c("mean_easting", "mean_northing"), remove = FALSE)
st_crs(detection_18N) <- crs_utm18N
detection_DD <- st_transform(detection_18N, crs_WT)

s_location <- detection_DD %>%
  mutate(latitude = unlist(map(detection_DD$geometry,2)),
         longitude = unlist(map(detection_DD$geometry,1)),
         missinginlocations = NA) %>%
  select(-mean_easting, -mean_northing)

st_drop_geometry(s_location)
s_location$geometry <- NULL

############################
#### VISIT TABLE ####
############################
pc_visit <- detection %>% 
  mutate(location = paste(dataset_code, site_id, point_id, sep= ":"),
         month2 = sprintf("%02d", as.numeric(month)),
         day2 = sprintf("%02d", as.numeric(day)),
         visitDate = paste(as.character(year), as.character(month2), as.character(day2), sep="-"),
         missingvisit = NA,
         rawObserver = observer,
         observer = paste(dataset_code, observer, sep= "_"),
         time = as.character(format(as.POSIXct(start_time, format='%H%M'), format = "%H:%M:%S")),
         snowDepthMeters= NA,
         waterDepthMeters = NA,
         crew = NA,
         bait = "NONE",
         accessMethod = NA,
         landFeatures = NA,
         wildtrax_internal_update_ts = NA,
         wildtrax_internal_lv_id = NA,
         utmZone = zone,
         time_zone = NA,       
         data_origin = NA,
         missinginvisit = NA,
         survey_year = as.character(year))%>%
  left_join(s_location)

############################
#### SURVEY TABLE ####
############################
#Check species codes again
survey.sp <- dplyr::filter(pc_visit, !species %in% WT_spTbl$species_code)
table(survey.sp$species)
#"SCJU" "WVI"

pc_survey <- pc_visit %>% 
  mutate(organization = organization,
         project = dataset_code,
         original_species = species,
         survey_time = ifelse(is.na(time), "00:00:01", time),
         pkey_dt = paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), rawObserver, sep=":"),
         surveyDateTime = paste(visitDate, survey_time),
         species = case_when(species =="WVI" ~ "WAVI",
                             species =="SCJU" ~ "DEJU",
                             TRUE ~ species),
         scientificname = WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)],
         distanceMethod = "0m-50m-100m-INF",
         durationMethod = "0-3-5-10min",
         durationinterval = "UNKNOWN")

survey.sp <- dplyr::filter(pc_survey, !species %in% WT_spTbl$species_code)
table(survey.sp$species)

# Pivot to extract distance band
pc_survey_dist <- pc_survey %>%
  pivot_longer(cols = c("less_than_50","50_to_100","more_than_100","flyovers"), 
               names_to = c("Variable")) %>% 
  mutate(ind_count = as.numeric(value),
         distanceband = case_when(Variable == "less_than_50" & ind_count >= 1 ~ "0m-50m",
                                  Variable == "50_to_100" & ind_count >= 1 ~ "50m-100m",
                                  Variable == "more_than_100" & ind_count >= 1 ~ "100m-INF",
                                  Variable == "flyovers" & ind_count >= 1 ~ "UNKNOWN"), 
         raw_distance_code = Variable,
         raw_duration_code = "UNKNOWN",
         isHeard = "DNC",
         isSeen = "DNC",
         site = site_id,
         station = point_id,
         missingindetections = NA,
         originalBehaviourData = NA,
         pc_vt = NA,
         pc_vt_detail = NA,
         age = NA,
         fm = NA,
         group = NA,
         flyover = ifelse(Variable =="flyovers" & value>= 1, "Yes", "No"),
         displaytype = NA,
         nestevidence = NA,
         behaviourother = NA) %>% 
  filter(ind_count >0)  

## CHECK
print(unique(pc_survey_dist$distanceMethod[!(pc_survey_dist$distanceMethod %in% WT_distMethTbl$distance_method_type)]))
print(unique(pc_survey_dist$durationMethod[!(pc_survey_dist$durationMethod %in% WT_durMethTbl$duration_method_type)]))
print(unique(pc_survey_dist$species[!(pc_survey_dist$species %in% WT_spTbl$species_code)]))
print(unique(pc_survey_dist$durationinterval[!(pc_survey_dist$durationinterval %in% WT_durBandTbl$duration_interval_type)]))
print(unique(pc_survey_dist$distanceband[!(pc_survey_dist$distanceband %in% WT_distBandTbl$distance_band_type)]))

#--------------------------------------------------------------
#
#       EXPORT
#
#--------------------------------------------------------------
dr<- drive_get(paste0("DataTransfered/",organization), shared_drive = "BAM_Core")
#Set GoogleDrive id
if (nrow(drive_ls(as_id(dr), pattern = dataset_code)) == 0){
  dr_dataset_code <-drive_mkdir(dataset_code, path = as_id(dr), overwrite = NA)
} else {
  dr_dataset_code <- drive_ls(as_id(dr), pattern = dataset_code)
}
dr_ls <- drive_ls(as_id(dr), pattern = dataset_code)
  
#---LOCATION
WTlocation <- c("location", "latitude", "longitude")
  
# Remove duplicated location
location_tbl <- pc_survey_dist[!duplicated(pc_survey_dist[,WTlocation]), WTlocation] 
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), row.names = FALSE, na = "")
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 
  
#---VISIT
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
               "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")
  
#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- pc_survey[!duplicated(pc_survey[,WTvisit]), WTvisit] # 
  
write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), row.names = FALSE, na = "")
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 
  
#---SURVEY
survey_tbl <- pc_survey_dist %>% 
  filter(flyover == "No") %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband, durationinterval, isHeard, isSeen, comments) %>%
  dplyr::summarise(abundance = sum(ind_count), .groups= "keep")

WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
                "durationinterval", "abundance", "isHeard", "isSeen", "comments")
survey_tbl <- survey_tbl[!duplicated(survey_tbl[,WTsurvey]), WTsurvey] # 

write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), row.names = FALSE, na = "")
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv"), overwrite = TRUE) 
  
#---EXTENDED
Extended <- c("organization", "project","location", "surveyDateTime", "species", "ind_count", "distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code", 
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
              "displaytype", "nestevidence", "behaviourother", "comments")
extended_tbl <- pc_survey_dist[!duplicated(pc_survey_dist[,Extended]), Extended] 
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



