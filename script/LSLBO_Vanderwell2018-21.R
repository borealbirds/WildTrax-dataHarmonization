# ---
# title: "Translate LSLBO data"
# author: Siu Chung WU, Diego
# date: "December 13, 2024"
# Note on translation:
# --metadata: https://docs.google.com/document/d/1pJQ1qsW_31QMRTCek-6yE72VcKU8dja_/edit
# --Only Site number, No Station number -> set to NA
# --88 obs recorded outside time: Delete.
# --4 species code are different from WildTrax: CAGO, SCJU, WPWA, HOWR. Fix 
# few rows record 'displaytype' in the column 'Status' with 'D' or 'DD', 'displaytype' of those row become 'Yes'
# 'flyover' is not separated from 'observed' in original record

library(googledrive)
library(googlesheets4)
library(tidyr)
library(readr)
library(chron)
library(readxl)
library(plyr) 
library(dplyr)
library(sf)
library(lubridate)
library(stringr)
library(reshape2) # melt
source("./config.R")


## Initialize variables
organization = "LSLBO"
dataset_code = "Vanderwell2018-21"
setwd(file.path(wd))
data_db <- "AllBirdCounts.csv"

WTpj_Tbl <- read_sheet("https://docs.google.com/spreadsheets/d/1fqifS_E5O_IpW1B-UG_xthr9hzY6FIek-nFjCrt1G0w", sheet = "project")
observer_Tbl <-  read_sheet("https://docs.google.com/spreadsheets/d/1yp0tjhQC7EJ_WqgP_vUNPfvgwwBhMDeSex4OghoIS3s", sheet = "master_observer")

lu <- "./lookupTables"
WT_spTbl <- read.csv(file.path(lu, "species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path(lu, "duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path(lu, "distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path(lu, "duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path(lu, "distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

## Path
dataDir <- file.path("./project", dataset_code)
if (!dir.exists(dataDir)) {
  dir.create(dataDir, recursive = TRUE)
}

out_dir <- file.path("./out", dataset_code)    # where output data frame will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE) 
}

#--------------------------------------------------------------
#
#       DOWNLOAD FILE FROM DRIVE
#
#--------------------------------------------------------------
if (length(list.files(dataDir)) ==0) {
  pid <- WTpj_Tbl %>%
    filter(dataset_code =="Vanderwell2018-21") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  detection_id <- gd.list %>%
    filter(name =="AllBirdCounts.csv") %>%
    select("id")
  drive_download(as_id(as.character(detection_id)), path = file.path(dataDir, data_db))
  site_id <- gd.list %>%
    filter(name =="AllSiteDescription.csv") %>%
    select("id")
  drive_download(as_id(as.character(site_id)), path = file.path(dataDir, "AllSiteDescription.csv"))
}

BirdCounts_ori <- read.csv(file.path(dataDir, data_db))
SiteDescription_ori <- read.csv(file.path(dataDir, "AllSiteDescription.csv"))


#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
data_flat <- merge(BirdCounts_ori, SiteDescription_ori, 
                   by = c('Site', 'X_dd', 'Y_dd', 'Age', 'Type', 'Min_Edge_m', 'Date', 'Crew', 'Comments'), 
                   all.x = TRUE)

# Find rows in observation_ori that were not joined with data_flat 
# unmatched_rows <- anti_join(data_flat, SiteDescription_ori, by = "Site")
# print(unmatched_rows) 

data_flat <- data_flat %>%
  mutate(organization = organization,
         project = dataset_code,
         station = NA,
         location = paste(dataset_code, Site, sep=":"),
         longitude = X_dd,
         latitude = Y_dd) %>%
  rename(site = Site,
         comments = Comments)
     

# Not exist in source data will be set NA
data_flat <- data_flat %>%
  mutate(elevationMeters = NA,
         bufferRadiusMeters = NA,
         isHidden = NA,
         trueCoordinates = NA,
         internal_wildtrax_id = NA,
         internal_update_ts = NA,
         utmZone	= NA,
         easting	= NA,
         northing	= NA,
         missinginlocations = NA)

# options(digits = 7)
# View(location_tbl)

############################
#### VISIT TABLE ####
############################
# all rows contain 'Time'
# na_rows <- data_flat[is.na(data_flat$Start.Time), ]
# print(na_rows)

data_flat <- data_flat %>%
  mutate(visitDate = format(as.Date(Date, format = "%d-%b-%y"), "%Y-%m-%d"),
         snowDepthMeters = NA,
         waterDepthMeters = NA,
         crew = NA,
         bait = "None",
         accessMethod = NA,
         landFeatures = Site.Description,
         wildtrax_internal_update_ts = NA,
         wildtrax_internal_lv_id = NA,
         time_zone = NA,
         data_origin = dataset_code,
         missinginvisit = NA,
         survey_year = sub("\\-.*", "", visitDate),
         survey_time = sub(".*\\s", "", Start.Time),
         surveyDateTime = paste(visitDate, survey_time),
         observer = case_when(Crew == "NDK/RNP"  ~ "obs01",
                              Crew == "RNP/MMM" ~ "obs02",
                              Crew == "RGK/NDK" ~ "obs03",
                              Crew == "NDK/RGK"  ~ "obs04",
                              Crew == "RNP/NDK" ~ "obs05",
                              Crew == "BPR/SLS" ~ "obs06",
                              Crew == "SLS/BPR"  ~ "obs07",
                              Crew == "RNP/CLC" ~ "obs08",
                              Crew == "SLS/CLC" ~ "obs09",
                              Crew == "BPR/RNP" ~ "obs10",
                              Crew == "RNP/BPR" ~ "obs11",
                              Crew == "RNP/GBH"  ~ "obs12",
                              Crew == "BPR/CLC" ~ "obs13",
                              Crew == "SLS/RNP" ~ "obs14",
                              Crew == "RNP/SLS" ~ "obs15",
                              is.na(Crew)   ~ "NA"),
         pkey_dt= paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), observer, sep=":")
  ) 

################################
#### Update master_observer ####
################################
unique_observers <- data_flat %>%
  select(Crew, observer) %>% 
  distinct() %>%
  filter(!is.na(Crew)) %>% # Exclude rows where Observer is NA
  mutate(
    observer_name = Crew,
    observer_id = observer
  )

# Create the append_obs data frame
append_obs <- unique_observers %>%
  select(observer_id, observer_name) %>%
  mutate(
    organization = "LSLBO",
    project = dataset_code
  ) %>%
  select(organization, project, observer_id, observer_name)

# Identify rows in append_obs that are not in observer_Tbl
new_rows <- anti_join(append_obs, observer_Tbl, 
                      by = c("organization", "project", "observer_id", "observer_name"))

# Combine new rows with the existing observer_Tbl
if(nrow(new_rows)>1){
  updated_observer_Tbl <- bind_rows(observer_Tbl, new_rows)
  write_csv(updated_observer_Tbl, file.path(wd, "master_observer.csv"), append = FALSE)
  dr<- drive_get("WildTrax-dataHarmonization", shared_drive = "BAM_Core")
  observer_Tbl <- file.path(wd, "master_observer.csv")
  drive_upload(media = observer_Tbl, path = as_id(dr), name = "master_observer", type = "spreadsheet", overwrite = TRUE)
}

############################
#### SURVEY TABLE ####
############################
#pivot table to split protocol band
data_expanded <- melt(data_flat, measure.vars = c("Within.50","X51.100","Over.100","Outside.Time"), value.name = "ind_count")
data_expanded$ind_count <- as.numeric(data_expanded$ind_count)

data_expanded <- data_expanded %>%
  filter(ind_count>0 & variable != "Outside.Time") %>% #delete false 0 created by melt and obs recorded outside of survey
  mutate(distanceMethod = "0m-50m-100m-INF",
         distanceband = case_when(variable == "Within.50" ~ "0m-50m",
                                  variable == "X51.100" ~ "50m-100m", 
                                  variable =="Over.100" ~ "100m-INF"),
         durationMethod = "0-5min",
         durationinterval = "0-5min",
         species = case_when(Species.Code =="CAGO" ~ "CANG",
                             Species.Code =="SCJU" ~ "JUNHYE",
                             Species.Code =="WPWA" ~ "PAWA",
                             Species.Code =="HOWR" ~ "NHWR",
                             TRUE ~ Species.Code),
         isHeard = case_when(Status == "S" ~ "Yes",
                             Status == "C" ~ "Yes",
                             Status == "O" ~ "No",
                             Status == "D" ~ "No",
                             Status == "N" ~ "No",
                             Status == "DD" ~ "No",
                             Status == "F" ~ "No",
                             NA ~ "DNC"),
         isSeen = case_when(Status == "S" ~ "No",
                            Status == "C" ~ "No",
                            Status == "O" ~ "Yes",
                            Status == "D" ~ "Yes",
                            Status == "N" ~ "Yes",
                            Status == "DD" ~ "Yes",
                            Status == "F" ~ "Yes",
                            NA ~ "DNC")
         )

# # create 'isDuplicate' column for signaling the duplicated column , *For information only
# WTspecies <- c("surveyDateTime", "location", "species", "distanceband", "durationinterval")
# duplicates <- duplicated(data_flat[WTspecies]) | duplicated(data_flat[WTspecies], fromLast = TRUE)
# data_flat$isDuplicate <- duplicates
# print(data_flat[data_flat$isDuplicate == TRUE, c("surveyDateTime", "location", "distanceband", "durationinterval", "species", "observer", "ind_count", "isDuplicate")])
# print(nrow(data_flat[data_flat$isDuplicate == TRUE, ])) # 1532 duplicated record

# Validations: species code, duration band, distance band, all in specification
print(unique(data_expanded$species[!(data_expanded$species %in% WT_spTbl$species_code)]))
print(unique(data_expanded$durationinterval[!(data_expanded$durationinterval %in% WT_durBandTbl$duration_interval_type)]))
print(unique(data_expanded$distanceband[!(data_expanded$distanceband %in% WT_distBandTbl$distance_band_type)]))
print(unique(data_expanded$durationinterval[!(data_expanded$durationinterval %in% WT_durBandTbl$duration_interval_type)]))


############################
#### BEHAVIOR TABLE ####
############################
data_expanded <- data_expanded %>%
  mutate(rawObserver  = Crew,
         original_species = Species.Code,
         scientificname = WT_spTbl$scientific_name[match(data_expanded$species, WT_spTbl$species_code)],
         raw_distance_code = variable,
         raw_duration_code = NA,
         originalBehaviourData = Status,
         missingindetections = "DNC",
         pc_vt = case_when(
           Status == "S" ~ "Song",
           Status == "C" ~ "Call",
           Status == "O" ~ "None-vocal",
           Status == "D" ~ "None-vocal",
           Status == "N" ~ "None-vocal",
           Status == "NA" ~ "None-vocal",
           Status == "NB" ~ "None-vocal",
           Status == "DD" ~ "None-vocal",
           Status == "F" ~ "None-vocal",
           TRUE ~ "DNC"), 
         pc_vt_detail = "DNC",
         age = case_when(
           grepl("FEMALE", comments, ignore.case = TRUE) ~ "Adult",
           grepl("MALE", comments, ignore.case = TRUE) ~ "Adult",
           grepl("YOUNG", comments, ignore.case = TRUE) ~ "Juvenile",
           grepl("FLEDGLINGS", comments, ignore.case = TRUE) ~ "Juvenile",
           TRUE ~ "DNC" # Default value if none of the above conditions are met
         ),
         fm = case_when(comments == "FEMALE CARRYING FOOD" ~ "Female",
                        comments == "MALE AND FEMALE" ~ "Male and female",
                        comments == "CARRYING FOOD; MALE" ~ "Male",
                        comments == "FEMALE" ~ "Female",
                        TRUE ~ "DNC"
         ),
         group = case_when(
           grepl("FLOCK", comments, ignore.case = TRUE) ~ "Flock", 
           comments == "MALE AND FEMALE" ~ "Pair",
           TRUE ~ "DNC"),
         flyover = "DNC",
         displaytype = case_when(Status == "D" ~ "Display",
                                 Status == "DD" ~ "Display",
                                 TRUE ~ "No"),
         nestevidence = case_when(Status == "NB" ~ "Yes",
                                  Status == "NA" ~ "Yes",
                                  Status == "F" ~ "Yes",
                                  Status == "N" ~ "Yes",
                                  TRUE ~ "No"),
         behaviourother = "DNC",
         atlas_breeding_code = "DNC"
  )

############################
##EXPORT
############################
# Create sub folder in 'toUpload' with the organization name
beh_dr<- drive_get("behavior/", shared_drive = "BAM_Core")
dr<- drive_get("toUpload/", shared_drive = "BAM_Core")

to_upload_contents <- drive_ls(as_id(dr)) # print(to_upload_contents)
gg_folder <- to_upload_contents[to_upload_contents$name == organization, ]
if (nrow(gg_folder) == 0) {
  gg_folder <- drive_mkdir(organization, path = as_id(dr))
}

# Create sub folder in 'toUpload/organisation' with the dataset name
dr<- drive_get(paste0("toUpload/",organization), shared_drive = "BAM_Core")
folder_list <- drive_ls(as_id(dr), pattern = dataset_code) # print(folder_list)

if (nrow(folder_list[folder_list$name == dataset_code, ]) == 0){
  dr_dataset_code <-drive_mkdir(dataset_code, path = as_id(dr), overwrite = NA)
  print(paste("Folder", dataset_code, "created successfully."))
} else {
  dr_dataset_code <- folder_list[folder_list$name == dataset_code, ]
} # print(drive_ls(as_id(dr)))


#---LOCATION
# Remove duplicated location
WTlocation <- c("location", "longitude", "latitude")
location_tbl <- data_expanded[!duplicated(data_expanded[,WTlocation]), WTlocation]
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), row.names = FALSE, na = "")
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 


#---VISIT
# Delete duplicated based on WildtTrax attributes (double observer on the same site, same day).
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")
visit_tbl <- data_expanded[!duplicated(data_expanded[,WTvisit]), WTvisit] # 
write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), row.names = FALSE, na = "")
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 


#---SURVEY
survey_tbl <- data_expanded %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband, durationinterval, isHeard, isSeen, comments) %>%
  dplyr::summarise(abundance = sum(ind_count), .groups= "keep")

WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen", "comments")
survey_tbl <- survey_tbl[!duplicated(survey_tbl[,WTsurvey]), WTsurvey] # 
write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), row.names = FALSE, na = "")
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv"), overwrite = TRUE) 


#---EXTENDED
# "atlas_breeding_code" still kept
# the operation of grouping "ind_count" into "abundance" is not done
Extended <- c("organization", "project", "location", "surveyDateTime", "species", "ind_count", "distanceband", "durationinterval", "site", "station", "utmZone", "easting",
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code",
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover",
              "displaytype", "nestevidence", "behaviourother", "atlas_breeding_code")

extended_tbl <- data_expanded[!duplicated(data_expanded[,Extended]), Extended]
write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_behavior.csv")), na = "", row.names = FALSE)
extended_out <- file.path(out_dir, paste0(dataset_code,"_behavior.csv"))
drive_upload(media = extended_out, path = as_id(beh_dr), name = paste0(dataset_code,"_behavior.csv"), overwrite = TRUE)




# ---PROCESSING STATS, no. of locations, visits, and surveys
file_name <- file.path(out_dir, paste0(dataset_code, "_stats.csv"))
con <- file(file_name, open = "a")
writeLines(paste0("Organization: ", organization), con)
writeLines(paste0("Project: ", dataset_code), con)
nrow_location <- paste0("Number of locations: ", nrow(location_tbl))
writeLines(nrow_location, con)
nrow_visit <- paste0("Number of visit: ", nrow(visit_tbl))
writeLines(nrow_visit, con)
nrow_survey <- paste0("Number of survey: ", nrow(survey_tbl))
writeLines(nrow_survey, con)
close(con)
