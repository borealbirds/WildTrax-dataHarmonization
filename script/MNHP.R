# Title: "Translate Michigan Northern Hardwood Project"
# Source dataset are two .rds files
# Author: "Melina Houle"
# Date: "February 21, 2023"
#   DROP 264 location, no visit, no survey associated
#   DROP 12 visit: survey and visit don't match. 

#update.packages()
library(dplyr) # mutate, %>%
#library(utils) #read.csv
#library(readxl) #read_excel
#library(stringr) #str_replace_all
#library(sf) #st_crs, st_as_sf, st_transform, st_drop_geometry
#library(purrr) #map
#library(plyr) #rbind.fill
library(googledrive) #drive_get, drive_mkdir, drive_ls, drive_upload

## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

WT_spTbl <- read.csv(file.path("./lookupTables/species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path("./lookupTables/duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path("./lookupTables/distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path("./lookupTables/duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path("./lookupTables/distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

organization <- "MSU"
dataset <- "Michigan Northern Hardwood Project"
dataset_code <- "MNHP"
lu <- "./lookupTables"
project <- file.path("./project", dataset_code)
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}
project_dir <- file.path(wd, "project", dataset_code)
if (!dir.exists(project_dir)) {
  dir.create(project_dir)
}
data_db <- file.path(project_dir, "all_sites_xy.rds")
if (!file.exists(data_db)) {
  #Download from GoogleDrive
  drive_download("sourceData/all_sites_xy.rds", path = file.path(project_dir, "all_sites_xy.rds"))
  drive_download("sourceData/survey_data.rds", path = file.path(project_dir, "survey_data.rds"))
  drive_download("sourceData/spp_codes.rds", path = file.path(project_dir, "spp_codes.rds"))
  drive_download("sourceData/detection_data.rds", path = file.path(project_dir, "detection_data.rds"))
  
}
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

#--------------------------------------------------------------
#       LOAD
#--------------------------------------------------------------
raw_location <- readRDS(file.path(project_dir, "all_sites_xy.rds"))
raw_survey <- readRDS(file.path(project_dir, "detection_data.rds"))
lu_observer <- readRDS(file.path(project_dir, "survey_data.rds"))
lu_species <- readRDS(file.path(project_dir, "spp_codes.rds"))


#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
#Format
pc_location <- raw_location %>% 
  select(Site, X_coord, Y_coord)  %>%
  dplyr::rename(latitude = Y_coord,
                longitude = X_coord) %>%
  mutate(location = paste(dataset_code, Site, sep= "_"),
         station = Site,
         easting = NA,
         northing = NA,
         missinginlocations = NA)

############################
#### VISIT TABLE ####
############################
# According to comments, date is wrong in one visit.  Fix
lu_observer$Date[lu_observer$Date ==20020720 & lu_observer$Site ==4083] <- 20020719

pc_visit <- lu_observer %>% 
  select(Site, Date, Surveyor, Time, Comments) %>% 
  mutate(surveyDate = as.Date(as.character(Date), format = "%Y%m%d"),
         missingvisit = NA,
         rawObserver = toupper(Surveyor),
         observer = case_when(rawObserver == "ED"  ~ "MNHP_obs01",
                              rawObserver == "NICK"  ~ "MNHP_obs02",
                              rawObserver == "AL"  ~ "MNHP_obs03",
                              rawObserver == "MISSING"  ~ "NA",
                              rawObserver == "ANDREW"  ~ "MNHP_obs04",
                              rawObserver == "MIKE"  ~ "MNHP_obs05",
                              rawObserver == "EMM"  ~ "MNHP_obs06",
                              rawObserver == "CHANTALE" ~ "MNHP_obs07",
                              rawObserver == "MELISSA" ~ "MNHP_obs08"),
         time = as.character(format(as.POSIXct(sprintf("%04.0f", Time), format='%H%M'), format = "%H:%M:%S")),
         snowDepthMeters= NA,
         waterDepthMeters = NA,
         crew = NA,
         bait = "NONE",
         accessMethod = NA,
         landFeatures = NA,
         wildtrax_internal_update_ts = NA,
         wildtrax_internal_lv_id = NA,
         utmZone = NA,
         time_zone = NA,       
         data_origin = NA,
         missinginvisit = NA,
         survey_year = substr(Date, 1, 4),
         comments = Comments)

############################
#### SURVEY TABLE ####
############################
s_data <- merge(raw_survey, pc_location, by = c("Site")) 
data_flat <- merge(s_data, pc_visit, by = c("Site", "Date"), all = TRUE)

# Delete all rows where visit don't match survey
data_flat <- subset(data_flat, !(is.na(longitude)))

pc_survey <- data_flat %>% 
  rename(site = Site) %>%
  mutate(organization = organization,
         project = dataset,
         original_species = Speccode,
         visitDate = ifelse(is.na(surveyDate), "1900-01-01", as.character(surveyDate)),
         survey_time = ifelse(is.na(time), "00:00:01", time),
         pkey_dt = paste(location, paste0(gsub("-", "", as.character(Date)),"_", gsub(":", "", survey_time)), observer, sep=":"),
         surveyDateTime = paste(visitDate, survey_time),
         species = ifelse(Speccode %in% c("SPEC1", "SPEC2", "SPEC3"), "UNBI",
                     WT_spTbl$species_code[match(Speccode, WT_spTbl$species_code)]),
         scientificname = WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)],
#         scientificname = ifelse(Speccode %in% c("SPEC1", "SPEC2", "SPEC3"), NA,
#                          WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)]),
         distanceMethod = "0m-30m-50m-100m-200m",
         distanceband = case_when(Distance == "30" ~ "0m-30m",
                                  Distance == "50" ~ "30m-50m",
                                  Distance == "100" ~ "50m-100m",
                                  Distance == "200" ~ "100m-200m",
                                  is.na(Number) ~ "UNKNOWN"),
         durationMethod = "0-3-5-10min",
         durationinterval = case_when(Minutes == "3" ~ "0-3min",
                                      Minutes == "5" ~ "3-5min",
                                      Minutes == "10" ~ "5-10min",
                                      is.na(Number) ~ "UNKNOWN"),
         ind_count = ifelse(is.na(Number), 0, Number),
         isHeard = ifelse(Detection == "A", "Yes", "No"),
         isSeen = ifelse(Detection %in% c("Flush", "Fly", "V", "fly"), "Yes", "No"),
         missingindetections = NA,
         raw_distance_code = Distance,
         raw_duration_code = Minutes,
         #Behaviour
         originalBehaviourData = Detection,
         pc_vt = NA,
         pc_vt_detail = NA,
         age = NA,
         fm = NA,
         group = NA,
         flyover = ifelse(Detection %in% c("Fly", "fly"), "Yes", "No"),
         displaytype = NA,
         nestevidence = NA,
         behaviourother = NA,
         comments = NA)

## CHECK
print(unique(pc_survey$distanceMethod[!(pc_survey$distanceMethod %in% WT_distMethTbl$distance_method_type)]))
print(unique(pc_survey$durationMethod[!(pc_survey$durationMethod %in% WT_durMethTbl$duration_method_type)]))
print(unique(pc_survey$species[!(pc_survey$species %in% WT_spTbl$species_code)]))
print(unique(pc_survey$Speccode[!(pc_survey$species %in% WT_spTbl$species_code)]))
#"SASP"  "NOOR"  "NOWT"  "BLWA"  "TRSW"  "CAGO"  "SCJU"  "WHIP"  "BAOW"  "CAGO " "REVI "
# Fix or fill missing species
pc_survey <- pc_survey  %>%
  mutate(species = case_when(Speccode == "SASP" ~ "SAVS",
                             Speccode == "NOOR" ~ "BAOR",
                             Speccode == "NOWT" ~ "NOWA",
                             Speccode == "BLWA" ~ "BLBW",
                             Speccode == "TRSW" ~ "TRES",
                             Speccode == "CAGO" ~ "CANG",
                             Speccode == "SCJU" ~ "DEJU",
                             Speccode == "WHIP" ~ "EWPW",
                             Speccode == "BAOW" ~ "BADO",
                             Speccode == "CAGO " ~ "CANG",
                             Speccode == "REVI " ~ "REVI",
                             Speccode == "GRJA" ~ "GRAJ",
                             Speccode == "MADU" ~ "MALL",
                             is.na(Speccode) ~ "NONE", 
                             TRUE ~ species),                                      
         scientificname = WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)],
  )
print(unique(pc_survey$species[!(pc_survey$species %in% WT_spTbl$species_code)]))
print(unique(pc_survey$durationinterval[!(pc_survey$durationinterval %in% WT_durBandTbl$duration_interval_type)]))
print(unique(pc_survey$distanceband[!(pc_survey$distanceband %in% WT_distBandTbl$distance_band_type)]))
print(unique(pc_survey$durationinterval[!(pc_survey$durationinterval %in% WT_durBandTbl$duration_interval_type)]))


#--------------------------------------------------------------
#
#       EXPORT
#
#--------------------------------------------------------------
dr<- drive_get(paste0("toUpload/",organization))
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
survey_tbl <- pc_survey %>% 
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
              "displaytype", "nestevidence", "behaviourother")
extended_tbl <- pc_survey[!duplicated(pc_survey[,Extended]), Extended] 
write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_extended.csv")), quote = FALSE, row.names = FALSE, na = "")
extended_out <- file.path(out_dir, paste0(dataset_code,"_extended.csv"))
drive_upload(media = extended_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_extended.csv"), overwrite = TRUE) 
  
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



