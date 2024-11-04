# Title: "Translate Ontario Forest Birds At Risk program 2019-2022  (southwestern Ontario)"
# Source dataset is an excel spreadsheet
# Author: "Melina Houle"
# Date: "March 18, 2024"
# Note on translation:
# -- Waiting to hear from BirdsCanada. No XY coordinates with the data.
# -- 

#udate.packages()
library(googlesheets4)
library(dplyr) # mutate, %>%
library(terra)
library(googledrive) #drive_get, drive_mkdir, drive_ls, drive_upload
library(stringr)
library(readr)
library(readxl)

## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

organization <- "Birds Canada"
dataset_code <- "ONFBAR2019-22"

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
    filter(dataset_code =="ONFBAR2019-22") %>% 
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  detection <- gd.list %>%
    filter(name =="all_FBAR_PC_data_(2019-present).xlsx") %>%
    select("id")
  drive_download(as_id(as.character(detection)), path = file.path(project_dir, "all_FBAR_PC_data_(2019-present).xlsx"))
}

detection <- read_excel(file.path(project_dir, "all_FBAR_PC_data_(2019-present).xlsx"))
names(detection)<-str_replace_all(names(detection), c(" " = "_"))

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION           ####
############################
#Format
  detection <- detection %>%
    select(Site_ID, Point_ID, Year, Month, Day, Date, Start_Time, Bird_Species, 
           Dist_50, Dist_50-100, Dist_100, Flyover, Total, Detection_Type) %>%
     drop_na(Latitude, Longitude)

# REPROJECT
spLocation <- vect(detection, geom=c("Longitude", "Latitude"), crs="epsg:4269")
spLocation_pj <- as.data.frame(project(spLocation,"EPSG:4326"),  geom = "XY") # WILDTRAX

pc_location <- spLocation_pj %>%
  dplyr::rename(longitude = x,
                latitude = y) %>%
  mutate(location = paste0(dataset_code, ":", SiteID, StationID, "_", index_vis ))

############################
#### VISIT TABLE ####
############################
pc_visit <- pc_location %>% 
  select(location, longitude, latitude, StationID, SiteID, UtmEasting, UtmNorthing, UtmZone, Date, Time, VolunteerID, SpeciesEnglishName, TotalCount) %>% 
  mutate(visitDate = format(as.Date(Date, format = "%m/%d/%Y"), "%Y-%m-%d"),
         rawObserver = as.character(VolunteerID),
         observer = paste0("obs",as.character(VolunteerID)),
         time2 = as.character(as.POSIXct(Time, format = "%H:%M")),
         survey_time = format(as.POSIXct(time2), format = "%H:%M:%S"),
         snowDepthMeters= NA,
         waterDepthMeters = NA,
         crew = NA,
         bait = "NONE",
         accessMethod = NA,
         landFeatures = NA,
         wildtrax_internal_update_ts = NA,
         wildtrax_internal_lv_id = NA,
         comments = NA,
         utmZone = UtmZone,
         time_zone = NA,       
         data_origin = NA,
         missinginvisit = NA,
         survey_year = sub("\\-.*", "", visitDate))

############################
#### SURVEY TABLE ####
############################
# Species
unique(subset(detection$Bird_Species, !(detection$Bird_Species %in% WT_spTbl$species_code))) 
# "WOOD SP"        "GULL SP"        "WPWA"           "WARBLER SP"     "CUCKOO SP"     
# "SPARROW SP"     "WOODPECKER SP"  "BLUJ"           "UKSP"           "woodpecker sp" 
# "Woodpecker sp." "Blackbird sp."  "BWWA/GWWA"      "RBWL"  


survey <- pc_visit %>% 
  mutate(organization = organization,
         project = dataset_code,
         original_species = SpeciesEnglishName,
         surveyDateTime = paste(visitDate, survey_time),
         pkey_dt = paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), observer, sep=":"),
         raw_distance_code = NA,
         raw_duration_code = NA,
         durationMethod = "0-5-10min",
         distanceMethod = "0m-100m-INF",
         distanceband = "UNKNOWN", 
         durationinterval = "UNKNOWN",
         abundance = TotalCount, 
         species = WT_spTbl$species_code[match(SpeciesEnglishName, WT_spTbl$species_common_name)], 
         scientificname = WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)],
         missingindetections = NA,
         isHeard = NA,
         isSeen = NA)

#-- Extended 
pc_survey <- survey %>%
  mutate(originalBehaviourData = NA,
         age = NA,
         fm = NA,
         group = NA,
         flyover = NA,
         nestevidence = NA,
         displaytype = NA,
         pc_vt = NA,
         pc_vt_detail = NA,
         behaviourother = NA,
         comments = NA,
         site = SiteID,
         station = StationID,
         easting= UtmEasting ,
         northing = UtmNorthing,
         original_species= NA,
         raw_distance_code = NA,
         raw_duration_code=NA, 
         originalBehaviourData= NA)

## CHECK
print(unique(pc_survey$distanceMethod[!(pc_survey$distanceMethod %in% WT_distMethTbl$distance_method_type)]))
print(unique(pc_survey$durationMethod[!(pc_survey$durationMethod %in% WT_durMethTbl$duration_method_type)]))
print(unique(pc_survey$species[!(pc_survey$species %in% WT_spTbl$species_code)]))

print(unique(pc_survey$durationinterval[!(pc_survey$durationinterval %in% WT_durBandTbl$duration_interval_type)]))
print(unique(pc_survey$distanceband[!(pc_survey$distanceband %in% WT_distBandTbl$distance_band_type)]))

#--------------------------------------------------------------
#
#       EXPORT
#
#--------------------------------------------------------------
dr<- drive_get(paste0("toUpload/",organization), shared_drive= "BAM_Core")

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
location_tbl <- pc_survey[!duplicated(pc_survey[,WTlocation]), WTlocation] 
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
WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "abundance","distanceband", "durationinterval", "isHeard", "isSeen")
survey_tbl <- pc_survey[!duplicated(pc_survey[,WTsurvey]), WTsurvey]

write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), row.names = FALSE, na = "")
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv"), overwrite = TRUE) 

#---EXTENDED
extended_tbl <- pc_survey %>% 
  group_by(organization, project,location, surveyDateTime, species, distanceband, durationinterval, site, station, utmZone, easting, 
           northing, time_zone, data_origin, missinginvisit, pkey_dt, survey_time,
           survey_year, rawObserver, original_species, scientificname, raw_distance_code, raw_duration_code, 
           originalBehaviourData, missingindetections, pc_vt, pc_vt_detail,age, fm, group, flyover, 
           displaytype, nestevidence, behaviourother) %>%
  dplyr::summarise(abundance = sum(abundance), .groups= "keep")

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
nrow_extended <- paste0("Number of extended: ", nrow(extended_tbl))
write_lines(nrow_extended, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)




