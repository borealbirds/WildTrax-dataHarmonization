# TRANSLATE BAM v6 BEHAVIOR
#
# The script aim to batch export behaviour data left behind in the translation process of BAM v4 and V6 to WildTrax. Data coming from the db
# were translated in Microdoft Access and were individually uploaded in WildTrax by Hedwig Lankau. 
# The behavior were retrieved 5 years later using a R script. Inconsistencies between output may occurred. 
# Decision made during the transition were not documentated. 

# A lookup table was built in order to retrieved the project_id using the name used in BAM db v6. 

library(utils) #read.csv
library(RODBC) #odbcConnect, sqlFetch
library(stringr) #str_replace_all
library(tidyr) #separate
library(googledrive)
library(dplyr) # mutate, %>%
library(readr) # write_csv

## Initialize variables
## Conversion do not use source data. It rather use an unfinished version of BAM V4 db
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

dataset_code <- "BAM-V6-USE_beh"
source_data <- "BAM-V6-USE.accdb"
project_lu <- read.csv(file.path("./lookupTables/BAM-V6-USE_beh_lookup.csv"))
WT_spTbl <- read.csv(file.path("./lookupTables/species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path("./lookupTables/duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path("./lookupTables/distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path("./lookupTables/duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path("./lookupTables/distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

project_dir <- file.path(wd, "project", dataset_code)
if (!dir.exists(project_dir)) {
  dir.create(project_dir)
}

data_db <- file.path(project_dir, source_data)
if (!file.exists(data_db)) {
  file_to_download <- drive_get(
    path = paste0("BAM-V6/", source_data),
    shared_drive = "BAM_Core"
  )
  #Download from GoogleDrive
  drive_download(file_to_download, path = data_db)
}
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

#######################################################
##                    Connect
#######################################################
project <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",data_db))
#Connecte and load tables
#queries<- sqlQuery(project, "BAM-V6")  ##Look up any queries
#tbls <- sqlTables(project) ##Look up tables
#tbls$TABLE_NAME
# Check dataset code for ALASKA
s_dataset <- sqlFetch(project, "dataset") # 
s_dataset$WT_project_name <- trimws(s_dataset$WT_project_name)
s_location <- sqlFetch(project, "location")
pc_visit <- sqlFetch(project, "pc_visit")
pc_survey <- sqlFetch(project, "pc_detection")
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
  dplyr::select(-SS_V4, -SS_V4_CRT, -original_name, -location_buffer_radius_m, -location_comments,-SS_SITE2_V4, -dataset_fk_V4, -QAQC_V4, -NRCANLink_V4, -OnRoad_V4, -timezone_V4) %>%
  mutate(location = toupper(s_location$location_name_V6),
         utmZone = NA,
         easting = NA,
         northing = NA,
         missinginlocations = NA
         ) %>%
  separate(location_name_V6, into = c("pcode", "site", "station", "nvisit"), sep = ":", remove = FALSE) %>%
  dplyr::filter(!is.na(longitude))

s_location <- merge(s_location, s_dataset[, c("dataset_autoid", "WT_Organization", "WT_project_name", "dataset_name", "dataset_code")], by.x = "dataset_fk", by.y = "dataset_autoid")

s_location <- s_location %>%
  mutate(pcode = dataset_code,
         WT_project_name = case_when(is.na(WT_project_name) ~ dataset_name,
                            TRUE ~ WT_project_name),
         site = as.character(site),
         station = as.character(station))
############################
#### VISIT TABLE ####
############################
#Fix column names that have space
names(pc_visit) <-str_replace_all(names(pc_visit), c(" " = "_"))
pc_visit <- pc_visit %>%
  dplyr::select(-PKEY_DT, -origin_database, -QC_V4,  -MM_V4, -DD_V4, -HR_V4, -MIN_V4, -'mm/dd/yyy_V4', -survey_time)

# Merge location tbl to visit to recover location name
s_visit <- merge(pc_visit, s_location, by.x = "location_fk", by.y = "location_autoid") 

## visitDate
s_visit <- s_visit %>%
  mutate(organization = project_lu$Organization_WT[match(WT_project_name, project_lu$project_name_db)],
         project = project_lu$project_full_nm_WT[match(WT_project_name, project_lu$project_name_db)],
         location = if_else(project == "Calling Lake Fragmentation Experiment Private", gsub(":", "-", location), location),
         survey_date = as.character(survey_date),
         snowDepthMeters = NA, 
         waterDepthMeters = NA,
         landFeatures = NA,
         crew = NA,
         bait = "None",
         accessMethod = NA,
         comments = visit_comments,
         wildtrax_internal_update_ts = NA,
         wildtrax_internal_lv_id = NA, 
         data_origin = Version_V4,
         missinginvisit = NA,
         StartTime_V4 = as.POSIXct(StartTime_V4, format = "%Y-%m-%d %H:%M:%S"),
         survey_time = ifelse(is.na(StartTime_V4), "00:01:01", as.character(format(StartTime_V4, format = "%H:%M:%S"))),
         observer = obs_V4,
         rawObserver = obs_V4,
         pkey_dt = paste(location, paste0(gsub("-", "", as.character(survey_date)),"_", gsub(":", "", survey_time)), obs_V4, sep=":")
  )

# Fix project name
s_visit <- s_visit %>%
  dplyr::filter(!is.na(project)) %>%
  mutate(project = case_when(WT_project_name == "Teslin Forest Birds Project 2000-2003 YT, Teslin Forest Bird Surveys 2009 to 2010 YT" & survey_year <2006 ~ "Teslin Forest Birds Project 2000-2003 YT",
                                     WT_project_name == "Teslin Forest Birds Project 2000-2003 YT, Teslin Forest Bird Surveys 2009 to 2010 YT" & survey_year >2006 ~ "Teslin Forest Bird Surveys 2009 to 2010 YT",
                                     WT_project_name == "PAN CANADA Christina Lake Bird Survey2001, PAN CANADA Christina Lake Bird Surveys 2003" & survey_year <2002 ~ "PAN CANADA Christina Lake Bird Surveys 2001",
                                     WT_project_name == "PAN CANADA Christina Lake Bird Survey2001, PAN CANADA Christina Lake Bird Surveys 2003" & survey_year >2002 ~ "PAN CANADA Christina Lake Bird Surveys 2003",
                                     TRUE ~ project))

############################
#### SURVEY TABLE ####
############################
names(pc_survey)<-str_replace_all(names(pc_survey), c(" " = "_"))
pc_survey <- pc_survey %>%
  dplyr::select(-PKEY, -PKEY_V6, -duration_3, -distance_3, -'BehCodeBAMV4-old', -QC_V4, -Version_V4, -obs_V4) 

data_flat <- merge(pc_survey, s_visit, by.x = "pv_visit_fk", by.y = "visit_id" )
print(unique(data_flat$species_code[!(data_flat$species_code %in% WT_spTbl$species_code)]))

data_flat <- data_flat %>%
  mutate(project_id = project_lu$WT_project_id[match(WT_project_name, project_lu$project_name_db)],
         surveyDateTime = paste(as.character(survey_date), survey_time),
         distanceMethod = lu_distance_method$protocol_distance_range[match(method_distance, lu_distance_method$protocol_distance_numid)],
         durationMethod = lu_duration_method$protocol_duration_range[match(method_duration, lu_duration_method$protocol_duration_id)],
         species = case_when(species_code == "Oven" ~ "OVEN",
                             species_code == "oven" ~ "OVEN",
                             species_code == "BADO" ~ "BAOW",
                             species_code == "CORE" ~ "REDP",
                             species_code == "HOWR" ~ "NHWR",
                             species_code == "ACGO" ~ "CACG",
                             species_code == "PISi" ~ "PISI",
                             species_code == "Weta" ~ "WETA",
                             species_code == "blpw" ~ "BLPW",
                             species_code == "tres" ~ "TRES",
                             species_code == "wewp" ~ "WEWP",
                             species_code == "HAWO  " ~ "HAWO",
                             species_code == "WTSP " ~ "WTSP",
                             TRUE ~ species_code),
         scientificname = WT_spTbl$scientific_name[match(species_code, WT_spTbl$species_code)],
         comments = NA,
         original_species = species_code,
         isHeard = case_when(heard =="N/A" ~ "DNC",
                             heard =="NR" ~ "DNC",
                             is.na(heard) ~ "DNC",
                             TRUE ~ heard),
         isSeen = case_when(seen =="N/A" ~ "DNC",
                             TRUE ~ seen),
         ind_count = abundance,
         distanceband = lu_distance_interval$distance_band_description[match(data_flat$distance_band, lu_distance_interval$distance_band_numid)],
         distanceband = case_when(distanceband ==">>100m (likely diff habitat)" ~ "100m-INF",
                                  distanceband =="Clearcut Edge" ~ "UNKNOWN",
                                  is.na(distanceband) ~ "UNKNOWN",
                                  TRUE ~ distanceband),
         durationinterval = lu_duration_interval$duration_description[match(data_flat$duration_interval, lu_duration_interval$duration_interval)],
         raw_distance_code = data_flat$distance_band,
         raw_duration_code = data_flat$duration_interval,
         missingindetections = NA)

# check 
print(unique(data_flat$species[!(data_flat$species %in% WT_spTbl$species_code)]))
print(unique(data_flat$distanceMethod[!(data_flat$distanceMethod %in% WT_distMethTbl$distance_method_type)]))
print(unique(data_flat$distanceband[!(data_flat$distanceband %in% WT_distBandTbl$distance_band_type)]))
unique(data_flat$durationMethod) # "0-5min" 
unique(data_flat$durationinterval)
unique(data_flat$durationinterval[data_flat$durationMethod=="0-5min"]) # "0-5min"  "UNKNOWN"
print(unique(data_flat$durationinterval[!(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))


# Behaviour
data_flat <- data_flat %>%
  mutate(originalBehaviourData = BehCodeBAMV4,
         pc_vt = data_flat$pc_vt,
         pc_vt_detail = pc_vt_detail,
         age = age,
         fm = fm,
         group = group,
         flyover = flyover,
         displaytype = displaytype,
         nestevidence = data_flat$nestevidence,
         behaviourother = data_flat$behaviourother)

################################
#### Update master_observer ####
################################
#- observer
gd.list <- drive_ls("https://drive.google.com/drive/u/1/folders/1mQtYwwvFhZUlQCpQUjSFTqUuj6zkH1-6")
observer_TBLid <- gd.list %>%
  filter(name =="master_observer.csv") %>%
  pull("id")
drive_download(file = drive_get(observer_TBLid), path = file.path(wd, "master_observer.csv"), overwrite = TRUE) 
observer_Tbl <-  read.csv(file.path(wd, "master_observer.csv"), fileEncoding="UTF-8-BOM")

unique_observers <- data_flat %>%
  select(organization, project, observer) %>% 
  distinct() %>%
  filter(!is.na(observer)) %>% # Exclude rows where Observer is NA
  mutate(
    observer_name = observer,
    observer_id = observer
  )

# Identify rows in append_obs that are not in observer_Tbl
new_rows <- anti_join(unique_observers, observer_Tbl, 
                      by = c("organization", "project", "observer_id", "observer_name"))

# Combine new rows with the existing observer_Tbl
if(nrow(new_rows)>1){
  updated_observer_Tbl <- bind_rows(observer_Tbl, new_rows)
  write_csv(updated_observer_Tbl, file.path(wd, "master_observer.csv"))
  dr<- drive_get("WildTrax-dataHarmonization", shared_drive = "BAM_Core")
  observer_Tbl <- file.path(wd, "master_observer.csv")
  drive_upload(media = observer_Tbl, path = as_id(dr), name = "master_observer.csv", overwrite = TRUE)
}


#--------------------------------------------------------------
#
#       EXPORT
#
#--------------------------------------------------------------
#--unique_combos <- data_flat %>%
#--       distinct(organization, project)
#--write.csv(unique_combos, file.path(out_dir, paste0(dataset_code, "_lookup.csv")), quote = FALSE, row.names = FALSE, na = "")

## Delete project not on WildTrax and group by
extended_tbl <- data_flat %>% 
  filter(pcode != "FBMP") %>%
  group_by(organization, project, location, latitude, longitude, surveyDateTime, species, abundance,distanceband, durationinterval, pcode, site, station, utmZone, easting, 
           northing, missinginlocations, time_zone, data_origin, missinginvisit, pkey_dt, survey_time,
           survey_year, observer, original_species, scientificname, raw_distance_code, raw_duration_code, 
           originalBehaviourData, missingindetections, pc_vt, pc_vt_detail, age, fm, group, flyover, 
           displaytype, nestevidence, behaviourother, project_id) %>%
  dplyr::summarise(ind_count = sum(abundance), .groups= "keep")

Extended <- c("organization", "project","location", "latitude", "longitude", "surveyDateTime", "species", "abundance", "ind_count", "distanceband", "durationinterval", "pcode", "site", "station", "utmZone", "easting", 
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "observer", "original_species", "scientificname", "raw_distance_code", "raw_duration_code", 
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
              "displaytype", "nestevidence", "behaviourother", "project_id")
extended_tbl <- extended_tbl[!duplicated(extended_tbl[,Extended]), Extended] 


# Save each subset as a CSV
split_data <- extended_tbl %>%
  group_by(project_id) %>%
  group_split()

names(split_data) <- extended_tbl %>%
  group_by(project_id) %>%
  group_keys() %>%
  pull(project_id)

for (project_id in names(split_data)) {
  pcode <- split_data[[project_id]]$pcode[1]
  write.csv(split_data[[project_id]], file = file.path(out_dir, paste0(project_id, "_", pcode, "_behavior.csv")), row.names = FALSE, na = "")
}
