# ---
# PCODE: * all Alaska PC from BAM v6
# Title: "Translate Alaska data loaded in BAM v6"
# Source dataset is extracted from BAM v6
# Author: "Melina Houle"
# Date: "September 20, 2022"
# Note on translation:
# --- Data were pre-processed by Hedwig Lankau. Script do not use source data. 
# --- BAM v6 has duplicate entries. The output of that script do not match tables from BAM v6 because the script control for duplicates
# ------- locations: 665 duplicates

# --- FIX DETAILS
# --- Time is missing. Was replaced by 00:00:01
# ------- 950 visit
# --- Species ACGO changed for CACG
# ------- 76 obs
# --- Duration and distance interval don't fit methodology. 
# ------SET TO UNKNOWN
# ------- 202 obs :distanceMethod ="0m-10m-20m-30m-40m-50m-60m-70m-80m-90m-100m-125m-150m-INF" with band "50m-INF","0m-50m","50m-100m","100m-150m"
# ------- 138 obs :durationMethod ="0-3-5-8-10min" with durationinterval = "0-5min"
# ------FIX durationinterval to 0-5min
# ------- 248 obs :durationMethod=="0-5-8min" with durationinterval %in% c("3-5min", "0-3min")
# ------FIX durationinterval to 0-10min
# ------- 4710 obs: durationMethod=="0-10min" with durationinterval ="0-3min"
# ------DELETE (don't respect duration protocol)
# ------- 68458 obs :durationinterval = "before or after/incidental"
# ------- 2771 obs :durationinterval = "15-20min"
# ------- 24 obs :durationMethod=="0-5-8min" with durationinterval = "8-10min"

library(utils) #read.csv
library(RODBC) #odbcConnect, sqlFetch
library(stringr) #str_replace_all
library(tidyr) #separate
library(googledrive)
library(dplyr) # mutate, %>%
library(readr) # write_lines
library(googlesheets4)


## Initialize variables
## Conversion do not use source data. It rather use an unfinished version of BAM V4 db
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

organization_name = "USGS-ALASKA"
dataset_folder <- "BAM-AK"

WTpj_Tbl <- read_sheet("https://docs.google.com/spreadsheets/d/1fqifS_E5O_IpW1B-UG_xthr9hzY6FIek-nFjCrt1G0w", sheet = "project")
lu <- "./lookupTables"
WT_spTbl <- read.csv(file.path(lu, "species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path(lu, "duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path(lu, "distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path(lu, "duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path(lu, "distance_band_codes.csv"), fileEncoding="UTF-8-BOM")


project_dir <- file.path(wd, "project", dataset_folder)
if (!dir.exists(project_dir)) {
  dir.create(project_dir)
}

out_dir <- file.path("./out", dataset_folder)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

#######################################################
##                    Connect
#######################################################
if (length(list.files(project_dir)) ==0) {
  pid <- WTpj_Tbl %>%
    filter(dataset_code =="BAM-AK-V6") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(unique(pid)))
  detection_id <- gd.list %>%
    filter(name =="BAM-V6-USE.accdb") %>%
    select("id")
  drive_download(as_id(as.character(detection_id)), path = file.path(project_dir, "BAM-V6-USE.accdb"))
}

project <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",file.path(project_dir, "BAM-V6-USE.accdb")))
#Connecte and load tables
    #queries<- sqlQuery(project, "BAM-V6")  ##Look up any queries
    #tbls <- sqlTables(project) ##Look up tables
    #tbls$TABLE_NAME
# Check dataset code for ALASKA
s_dataset <- sqlFetch(project, "dataset") # 5-6-7-8-9-10
s_location <- sqlFetch(project, "location")
pc_visit <- sqlFetch(project, "pc_visit")
names(pc_visit) <-str_replace_all(names(pc_visit), c(" " = "_")) #Fix column names that have space
pc_survey <- sqlFetch(project, "pc_detection")
names(pc_survey)<-str_replace_all(names(pc_survey), c(" " = "_"))

lu_duration_interval <- sqlFetch(project, "lu_pc_duration_interval")
lu_duration_method <- sqlFetch(project, "lu_pc_protocol_duration")
lu_distance_method <- sqlFetch(project, "lu_pc_protocol_distance")
lu_distance_interval <- sqlFetch(project, "lu_pc_distance_band")
lu_species <- sqlFetch(project, "WT_Species_codes")

#--------------------------------------------------------------
#
#       TRANSLATE
# 
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
s_location <- s_location %>%
  filter(dataset_fk <11 & dataset_fk >4) %>%
  mutate(organization = organization_name,
         location = location_name_V6,
         elevationMeters = NA,
         bufferRadiusMeters = NA,
         isHidden = NA,
         trueCoordinates = NA,
         comments = NA,
         internal_wildtrax_id = NA,
         internal_update_ts = NA,
         utmZone	= NA,
         easting	= NA,
         northing	= NA,
         missinginlocations = NA) %>%
  separate(location_name_V6, c("project", "site", "station"), ":")

############################
#### VISIT TABLE ####
############################
# Merge location tbl to visit to recover location name
s_visit <- merge(pc_visit, s_location, by.x = "location_fk", by.y = "location_autoid") 
## visitDate
s_visit <- s_visit %>%
  mutate(visitDate = as.character(s_visit$survey_date),
         snowDepthMeters = NA,  #derived from location at upload
         waterDepthMeters = NA,  #derived from location at upload
         landFeatures = NA,  #ARUs attributes
         crew = NA,   #ARUs attributes
         bait = "None",
         accessMethod = NA,  #ARUs attributes
         comments = visit_comments, 
         wildtrax_internal_update_ts = NA,  #created during the upload 
         wildtrax_internal_lv_id = NA, #created during the upload 
         data_origin = Version_V4,
         missinginvisit = NA,
         time1= case_when(is.na(StartTime_V4) ~ "1899-12-30 00:00:01",
                                 TRUE ~ as.character(StartTime_V4)),
         time2 = as.POSIXct(time1, format = "%Y-%m-%d %H:%M:%S"),
         survey_time = as.character(format(time2, format = "%H:%M:%S")),
         observer = "NA",
         rawObserver = obs_V4,
         ## pkey_dt -- Concatenate, separated by colons: [location]:[visitDate]_[survey_time]:[localobservercode]; can also use raw observer or observer ID if needed
         pkey_dt = paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), observer, sep=":")
         )

############################
#### SURVEY TABLE ####
############################
data_flat <- merge(pc_survey, s_visit, by.x = "pv_visit_fk", by.y = "visit_id" )

data_flat <- data_flat %>%
  mutate(surveyDateTime = paste(as.character(visitDate), survey_time),
         durationMethod = lu_duration_method$protocol_duration_range[match(data_flat$method_duration, lu_duration_method$protocol_duration_id)],
         distanceMethod = lu_distance_method$protocol_distance_range[match(data_flat$method_distance, lu_distance_method$protocol_distance_numid)],
         species = case_when(species_code =="ACGO" ~ "CACG", # FIX BAM species code ACGO to CACG
                             TRUE ~ WT_spTbl$species_code[match(data_flat$species_code, WT_spTbl$species_code)]),
         species_name = WT_spTbl$species_common_name[match(data_flat$species, WT_spTbl$species_code)],
         comments = NA,
         original_species = data_flat$species_code,
         scientificname = WT_spTbl$scientific_name[match(data_flat$original_species, WT_spTbl$species_code)],
         isHeard = case_when(heard =="N/A" ~ "DNC",
                              TRUE ~ heard),
         isSeen = case_when(seen =="N/A" ~ "DNC",
                              TRUE ~ seen),
         distanceband = lu_distance_interval$distance_band_description[match(data_flat$distance_band, lu_distance_interval$distance_band_numid)]
         ) 
         
print(unique(data_flat$distanceMethod[!(data_flat$distanceMethod %in% WT_distMethTbl$distance_method_type)]))
print(unique(data_flat$durationMethod[!(data_flat$durationMethod %in% WT_durMethTbl$duration_method_type)]))
print(unique(data_flat$species[!(data_flat$species %in% WT_spTbl$species_code)]))

#####################################################
##### FIX distance band and duration interval to fit protocol
#####################################################
#FIX distanceband with UNKNOWN when band doesn't fit protocol
unique(data_flat$distanceMethod) # "0m-10m-20m-30m-40m-50m-60m-70m-80m-90m-100m-125m-150m-INF" "0m-50m-INF"  
unique(data_flat$distanceband[data_flat$distanceMethod=="0m-50m-INF"]) # MATCH
unique(data_flat$distanceband[data_flat$distanceMethod=="0m-10m-20m-30m-40m-50m-60m-70m-80m-90m-100m-125m-150m-INF"]) # WRONG:"50m-INF"   "0m-50m"    "50m-100m"  "100m-150m"
data_flat$distanceband[data_flat$distanceMethod=="0m-10m-20m-30m-40m-50m-60m-70m-80m-90m-100m-125m-150m-INF" & data_flat$distanceband %in% c("50m-INF","0m-50m","50m-100m","100m-150m")] <- "UNKNOWN"
# check if it follow WT distance code. Should be empty
print(unique(data_flat$distanceband[!(data_flat$distanceband %in% WT_distBandTbl$distance_band_type)]))

# CHECK duration
data_flat$durationinterval <- lu_duration_interval$duration_description[match(data_flat$duration_interval, lu_duration_interval$duration_interval)]
unique(data_flat$durationMethod) # "0-3-5-8-10min" "0-5min"        "0-5-8min"      "0-10min"       "0-3-5min"
unique(data_flat$durationinterval)
# Delete "before or after/incidental" "15-20min" . Don't fit protocol
data_flat <- data_flat[!(data_flat$durationinterval %in% c("before or after/incidental", "15-20min")),]


unique(data_flat$durationinterval[data_flat$durationMethod=="0-3-5-8-10min"]) # DON'T MATCH "0-5min" 
#FIX durationinterval with UNKNOWN when band doesn't fit protocol
data_flat$durationinterval[data_flat$durationMethod=="0-3-5-8-10min" & data_flat$durationinterval %in% c("0-5min")] <- "UNKNOWN"
unique(data_flat$durationinterval[data_flat$durationMethod=="0-5min"]) # MATCH
unique(data_flat$durationinterval[data_flat$durationMethod=="0-5-8min"]) # DON'T MATCH "3-5min" "0-3min"  "8-10min"
#FIX "3-5min" "0-3min" and delete "8-10min" that is outside the protocol
data_flat$durationinterval[data_flat$durationMethod=="0-5-8min" & data_flat$durationinterval %in% c("3-5min", "0-3min")] <- "0-5min"
data_flat <- data_flat[!(data_flat$durationMethod=="0-5-8min" & data_flat$durationinterval %in% c("8-10min")),] 

unique(data_flat$durationinterval[data_flat$durationMethod=="0-10min"]) # DON'T MATCH "0-3min"      
# FIX
data_flat$durationinterval[data_flat$durationMethod=="0-10min" & data_flat$durationinterval %in% c("0-3min")] <- "0-10min"

unique(data_flat$durationinterval[data_flat$durationMethod=="0-3-5min"]) # MATCH

# check if it follow WT duration code. Should be empty
print(unique(data_flat$durationinterval[!(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))


# Extended
data_flat <- data_flat %>%
  mutate(originalBehaviourData = BehCodeBAMV4,
         raw_distance_code = distance_band,
         raw_duration_code = duration_interval,
         missingindetections = NA)
  

#--------------------------------------------------------------
#
#       EXPORT
#
#--------------------------------------------------------------
#Set GoogleDrive id
dr <- drive_get(paste0("toUpload/",organization_name), shared_drive= "BAM_Core")
dataset_code <- unique(data_flat$project)

for (x in dataset_code) {
  if (nrow(drive_ls(as_id(dr), pattern = paste0(x,"s"))) == 0){
    dr_dataset_code <-drive_mkdir(x, path = as_id(dr), overwrite = NA)
  } else {
    dr_dataset_code <- drive_ls(as_id(dr), pattern = paste0(x,"s"))
  }
  dr_ls <- drive_ls(as_id(dr), pattern = paste0(x,"s"))

  #---LOCATION
  location <- s_location[s_location$project==x,]
  WTlocation <- c("location", "latitude", "longitude")
  
  #WTlocation <- c("location", "latitude", "longitude", "bufferRadiusMeters", "elevationMeters", "isHidden", "trueCoordinates", "comments")
  location_tbl <- location[!duplicated(location[,WTlocation]), WTlocation] # 
  location_tbl <- subset(location_tbl,location_tbl$location %in% data_flat$location)
  
  write.csv(location_tbl, file= file.path(out_dir, paste0(x,"_location.csv")), row.names = FALSE, na = "")
  location_out <- file.path(out_dir, paste0(x,"_location.csv"))
  drive_upload(media = location_out, path = as_id(dr_dataset_code$id), name = paste0(x,"_location.csv"), overwrite = TRUE) 
  
  #---VISIT
  visit_tbl <- s_visit[s_visit$project==x,]
  WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
               "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

  #Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
  visit_tbl <- visit_tbl[!duplicated(visit_tbl[,WTvisit]), WTvisit] # 
  visit_tbl <- subset(visit_tbl,visit_tbl$location %in% data_flat$location)
  write.csv(visit_tbl, file= file.path(out_dir, paste0(x,"_visit.csv")), row.names = FALSE, na = "")
  visit_out <- file.path(out_dir, paste0(x,"_visit.csv"))
  drive_upload(media = visit_out, path = as_id(dr_dataset_code$id), name = paste0(x,"_visit.csv"), overwrite = TRUE) 
  
  #---SURVEY
  survey_flat <- data_flat[data_flat$project==x,]
  survey_flat$project <- x
  # Sum obs that are the same based on WildTrax field
  survey_tbl <- survey_flat %>% 
            group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband, durationinterval, isHeard, isSeen, comments) %>%
    dplyr::summarise(abundance = sum(abundance), .groups= "keep")
  
  WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
                "durationinterval", "abundance", "isHeard", "isSeen", "comments")
  survey_tbl <- survey_tbl[!duplicated(survey_tbl[,WTsurvey]), WTsurvey]
  write.csv(survey_tbl, file= file.path(out_dir, paste0(x,"_survey.csv")), row.names = FALSE, na = "")
  survey_out <- file.path(out_dir, paste0(x,"_survey.csv"))
  drive_upload(media = survey_out, path = as_id(dr_dataset_code$id), name = paste0(x,"_survey.csv"), overwrite = TRUE) 
  
  species <-sort(unique(survey_flat$species_name))
  write.csv(species, file= file.path(out_dir, paste0(x,"_speciesList.csv")), row.names = FALSE)
  
  #---EXTENDED
  Extended <- c("organization", "project","location", "surveyDateTime", "species", "distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code", 
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
              "displaytype", "nestevidence", "behaviourother")
  extended_tbl <- survey_flat[!duplicated(survey_flat[,Extended]), Extended] 
  write.csv(extended_tbl, file.path(out_dir, paste0(x, "_extended.csv")), quote = FALSE, row.names = FALSE)
  extended_out <- file.path(out_dir, paste0(x,"_extended.csv"))
  drive_upload(media = extended_out, path = as_id(dr_dataset_code$id), name = paste0(x,"_extended.csv"), overwrite = TRUE) 
  
  #---PROCESSING STATS
  write_lines(paste0("Organization: ", organization_name), file.path(out_dir, paste0(x, "_stats.csv")))
  write_lines(paste0("Project: ", x), file.path(out_dir, paste0(x, "_stats.csv")), append= TRUE)
  nrow_location <- paste0("Number of locations: ", nrow(location_tbl))
  write_lines(nrow_location, file.path(out_dir, paste0(x, "_stats.csv")), append= TRUE)
  nrow_visit <- paste0("Number of visit: ", nrow(visit_tbl))
  write_lines(nrow_visit, file.path(out_dir, paste0(x, "_stats.csv")), append= TRUE)
  nrow_survey <- paste0("Number of survey: ", nrow(survey_tbl))
  write_lines(nrow_survey, file.path(out_dir, paste0(x, "_stats.csv")), append= TRUE)
}
  
