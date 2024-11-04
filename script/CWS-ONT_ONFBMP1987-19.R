# Title: "Translate Ontario Forest Bird Monitoring Program 1987-2017"
# Source dataset is an excel spreadsheet
# Author: "Melina Houle"
# Date: "February 20, 2024"
# Note on translation:
# -- SpeciesID ==ZERO: No species observe. Delete 4 obs.
# -- Some species aren't define in the species list provided by the FBMP, but are described in other documentation and they fit the WildTrax definition. 
# -- Count is sometimes underestimate. When possible, use Protocol columns to extract count. 
# -- 73 obs have species with Count of 0. Those were deleted as we couldn't get info from the data owner. 

#update.packages()
library(googlesheets4)
library(dplyr) # mutate, %>%
library(terra)
library(googledrive) #drive_get, drive_mkdir, drive_ls, drive_upload
library(stringr)
library(readr)
library(tidyr)

## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

organization <- "CWS-ONT"
dataset_code <- "ONFBMP1987-19"

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
    filter(dataset_code =="ONFBMP1987-19") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  detection_8717 <- gd.list %>%
    filter(name =="m2.alljoined.csv") %>%
    select("id")
  drive_download(as_id(as.character(detection_8717)), path = file.path(project_dir, "m2.alljoined.csv"))
  detection_2018 <- gd.list %>%
    filter(name =="m2_2018.alljoined.csv") %>%
    select("id")
  drive_download(as_id(as.character(detection_2018)), path = file.path(project_dir, "m2_2018.alljoined.csv"))
  detection_2019 <- gd.list %>%
    filter(name =="m2_2019.alljoined.csv") %>%
    select("id")
  drive_download(as_id(as.character(detection_2019)), path = file.path(project_dir, "m2_2019.alljoined.csv"))
  speciesList <- gd.list %>%
    filter(name =="FBMP_species_list.csv") %>%
    select("id")
  drive_download(as_id(as.character(speciesList)), path = file.path(project_dir, "FBMP_species_list.csv"))
}

detection_87 <- read.csv(file.path(project_dir, "m2.alljoined.csv"), fileEncoding="UTF-8-BOM")
detection_18 <- read.csv(file.path(project_dir, "m2_2018.alljoined.csv"), fileEncoding="UTF-8-BOM")
detection_19 <- read.csv(file.path(project_dir, "m2_2019.alljoined.csv"), fileEncoding="UTF-8-BOM")
#Append
detection <- rbind.fill(detection_87, detection_18, detection_19)

# load species list
speciesList <- read.csv(file.path(project_dir, "FBMP_species_list.csv"), fileEncoding="UTF-8-BOM")

#--------------------------------------------------------------
#
#       Fix species list
#
#--------------------------------------------------------------
#
species_check <- merge(speciesList, WT_spTbl, by.x ="SpeciesID", by.y ="species_code", all.x = TRUE)
species_diff <- species_check %>%
  filter(English_Name != species_common_name | is.na(species_common_name)) %>%
  select(SpeciesID, English_Name, species_common_name)
print(species_diff)
#GNBH GTBH RPHE  SCJU  STSP  YSFL ZERO       

# Fix detection prior to strat processing
detection <- detection %>%
  mutate(SpeciesID = case_when(SpeciesID == "GNBH" ~ "GRHE",
                               SpeciesID == "GTBH" ~ "GBHE",
                               SpeciesID == "RPHE" ~ "RNEP",
                               SpeciesID == "SCJU" ~ "DEJU",
                               SpeciesID == "STSP" ~ "NESP",
                               SpeciesID == "YSFL" ~ "NOFL",
                               TRUE ~ SpeciesID),)

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION           ####
############################
#Format
detection <- detection[!is.na(detection$Latitude),]
detection <- detection[!is.na(detection$Longitude),]
detection <- detection[detection$SpeciesID != "ZERO",]

# REPROJECT
spLocation <- vect(detection, geom=c("Longitude", "Latitude"), crs="epsg:4269")
spLocation_pj <- as.data.frame(project(spLocation,"EPSG:4326"),  geom = "XY") # WILDTRAX

pc_location <- spLocation_pj %>%
  dplyr::rename(longitude = x,
                latitude = y) %>%
  mutate(location = paste0(dataset_code, ":", SiteID, StationID))

############################
#### VISIT TABLE ####
############################
pc_visit <- pc_location %>% 
  select(location, longitude, latitude, StationID, SiteID, Easting, Northing, UTM_Zone, Date, Time, VolunteerID, SpeciesID, Count, First5_In, First5_Out, Second5_In, Second5_Out, Year) %>% 
  separate(Time, c("hour", "min", "sec"), sep = ":") %>%
  mutate(visitDate = as.character(Date),
         rawObserver = as.character(VolunteerID),
         observer = paste0("obs",VolunteerID),
         hour2 = sprintf("%02d", as.numeric(hour)),
         min2 = sprintf("%02d", as.numeric(min)),
         sec2 = ifelse(is.na(sec), "00", sprintf("%02d", as.numeric(sec))),
         survey_time = paste(as.character(hour2), as.character(min2), as.character(sec2), sep=":"),
         snowDepthMeters= NA,
         waterDepthMeters = NA,
         crew = NA,
         bait = "NONE",
         accessMethod = NA,
         landFeatures = NA,
         wildtrax_internal_update_ts = NA,
         wildtrax_internal_lv_id = NA,
         comments = NA,
         utmZone = UTM_Zone,
         time_zone = NA,       
         data_origin = NA,
         missinginvisit = NA,
         survey_year = sub("\\-.*", "", visitDate))

############################
#### SURVEY TABLE ####
############################
survey <- pc_visit %>% 
  mutate(organization = organization,
         project = dataset_code,
         original_species = SpeciesID,
         surveyDateTime = paste(visitDate, survey_time),
         pkey_dt = paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), observer, sep=":"),
         raw_distance_code = NA,
         raw_duration_code = NA,
         durationMethod = "0-5-10min",
         distanceMethod = "0m-100m-INF", 
         species = SpeciesID, 
         scientificname = WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)],
         missingindetections = NA,
         isHeard = NA,
         isSeen = NA)

## Split df according to protocol. 
#no info on protocol
pc_survey_noinfo <- survey %>%
  filter(First5_In ==0 & First5_Out ==0 & Second5_In ==0 & Second5_Out ==0)

pc_survey_noinfo <- pc_survey_noinfo %>% 
  mutate(durationinterval = "UNKNOWN",
         distanceband = "UNKNOWN",
         ind_count = Count,
         raw_distance_code = "UNKNOWN",
         raw_duration_code = "UNKNOWN"
  ) %>%
  filter(ind_count >0)

# with protocol
pc_survey_winfo <- survey %>%
  filter(First5_In !=0 | First5_Out !=0 | Second5_In !=0 | Second5_Out !=0) %>%
  pivot_longer(cols = c("First5_In","First5_Out","Second5_In","Second5_Out"), 
             names_to = c("Variable")) %>% 
  mutate(ind_count = as.numeric(value),
         distanceband = case_when(Variable == "First5_In" & ind_count >= 1 ~ "0m-100m",
                                      Variable == "First5_Out" & ind_count >= 1 ~ "100m-INF",
                                      Variable == "Second5_In" & ind_count >= 1 ~ "0m-100m",
                                      Variable == "Second5_Out" & ind_count >= 1 ~ "100m-INF"), 
         durationinterval = case_when(Variable == "First5_In" & ind_count >= 1 ~ "0-5min",
                                      Variable == "First5_Out" & ind_count >= 1 ~ "0-5min",
                                      Variable == "Second5_In" & ind_count >= 1 ~ "5-10min",
                                      Variable == "Second5_Out" & ind_count >= 1 ~ "5-10min"),
         raw_distance_code = Variable,
         raw_duration_code = Variable) %>% 
  filter(ind_count >0)

data_bind <- rbind.fill(pc_survey_noinfo, pc_survey_winfo)

#-- Extended 
pc_survey <- data_bind %>%
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
         easting= Easting ,
         northing = Northing,
         original_species= NA,
         originalBehaviourData= NA)

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
#WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband", "durationinterval", "isHeard", "isSeen", "comments")
survey_tbl <- pc_survey %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband, durationinterval, isHeard, isSeen, comments) %>%
  dplyr::summarise(abundance = sum(ind_count), .groups= "keep")

write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), row.names = FALSE, na = "")
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv"), overwrite = TRUE) 

#---EXTENDED
extended_tbl <- pc_survey %>% 
  group_by(organization, project,location, surveyDateTime, species, ind_count, distanceband, durationinterval, site, station, utmZone, easting, 
           northing, time_zone, data_origin, missinginvisit, pkey_dt, survey_time,
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

