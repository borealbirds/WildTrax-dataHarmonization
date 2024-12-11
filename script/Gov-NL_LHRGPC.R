# ---
# title: "Translate CWS-ATL data"
# author: Siu Chung WU, Diego
# date: "November 8, 2024"
# Note on translation:
### Drop 7 observations. pc_id  don't have any vsit match           
### Drop 5 observations. XY coordinates missing. 
### 10 observations has Time = NA. Assign 09:00:00 as agreed by email.  reference: MealyMt-Proofing-Checklist.xlsx
### Drop 140 obs with 'Quadrant' ="7" - Before/after 10-min period (if not detected on pc)

library(googledrive)
library(googlesheets4)
library(RODBC)
library(tidyr)
library(readr)
library(chron)
library(readxl)
library(plyr) 
library(dplyr)
library(sf)
library(lubridate)
library(stringr)
source("./config.R")


## Initialize variables
organization = "GOV-NL"
dataset_code = "LHRGPC"
setwd(file.path(wd))
data_db <- "bird_pointcounts2008-10-18.mdb"
  
WTpj_Tbl <- read_sheet("https://docs.google.com/spreadsheets/d/1fqifS_E5O_IpW1B-UG_xthr9hzY6FIek-nFjCrt1G0w", sheet = "project")

##lookup tables 
#- observer
gd.list <- drive_ls("https://drive.google.com/drive/u/1/folders/1mQtYwwvFhZUlQCpQUjSFTqUuj6zkH1-6")
observer_TBLid <- gd.list %>%
  filter(name =="master_observer.csv") %>%
  pull("id")
drive_download(file = drive_get(observer_TBLid), path = file.path(wd, "master_observer.csv"), overwrite = TRUE) 
observer_TBL <-  read.csv(file.path(wd, "master_observer.csv"), fileEncoding="UTF-8-BOM")
#- species
lu <- "./lookupTables"
WT_spTbl <- read.csv(file.path(lu, "species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
#- methods
WT_durMethTbl <- read.csv(file.path(lu, "duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path(lu, "distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path(lu, "duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path(lu, "distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

## Path
project <- file.path("./project", dataset_code)
if (!dir.exists(project)) {
  dir.create(project, recursive = TRUE)
}
dataDir <- file.path(wd,"project",dataset_code)   # where files would be downloaded
if (!dir.exists(dataDir)) {
  dir.create(dataDir, recursive = TRUE)
}
out_dir <- file.path(wd, "out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE) 
}

#--------------------------------------------------------------
#
#       LOAD - this part uses modified Excel Sheets
#
#--------------------------------------------------------------
if (length(list.files(project)) ==0) {
  pid <- WTpj_Tbl %>%
    filter(dataset_code =="LHRGPC") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  detection_id <- gd.list %>%
    filter(name =="bird_pointcounts2008-10-18.mdb") %>%
    select("id")
  drive_download(as_id(as.character(detection_id)), path = file.path(project, data_db))
}

#######################################################
##                    Connect
#######################################################
#Connecte and load tables
con <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",file.path(project,data_db)))
#tbls <- sqlTables(con) 
lu_species <- sqlFetch(con, "bird species names")
names(lu_species)<-str_replace_all(names(lu_species), c(" " = "_"))

pc_visit <- sqlFetch(con, "point counts")
pc_location <- sqlFetch(con, "points")
names(pc_location)<-str_replace_all(names(pc_location), c(" " = "_"))
pc_survey <- sqlFetch(con, "sightings")
odbcClose(con)

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
data_flat <- pc_visit %>%
  select(pc_id, Point_id, Date, Visit, Time, Observer) %>%
  left_join(pc_location, by = "Point_id") %>%
  select(pc_id, Point_id, Date, Visit, Time, Observer, Location, Site_Number, Zone, Easting, Northing) %>%
  left_join(pc_survey, by = "pc_id") %>%
  left_join(lu_species, by = "Species_id") %>%
  select(-Latin_Name, -taxon, -Habitat_Guild, -Migratory_Status, -Foraging, -Nest_Type, -Nest_Location) %>%
  filter(!is.na(Easting)) # drop XY NAs

data_flat <- data_flat %>%
  mutate(organization = organization,
         project = dataset_code,
         site = paste0(substr(Location, 3, 3), substr(Site_Number, 1, 2)),
         station = substr(Site_Number, 3, 3),
         location = paste(project, site, station, sep=":"),
         # Not exist in source data will be set NA
         elevationMeters = data_flat$Elevation,
         bufferRadiusMeters = NA,
         isHidden = NA,
         trueCoordinates = NA,
         comments = Comment,
         internal_wildtrax_id = NA,
         internal_update_ts = NA,
         utmZone	= "UTM zone 21N",
         easting = data_flat$Easting,
         northing	= data_flat$Northing,
         missinginlocations = NA)

# # correct projection from EPSG 32621 to EPSG 4269, and save them back to the original dataframe (data_flat)
xy_sf <- st_as_sf(data_flat, coords = c("easting", "northing"))
xy_sf <- st_set_crs(xy_sf, 32621)
xy_sf_4269 <- st_transform(xy_sf, crs = 4269)
xy_4269 <- st_coordinates(xy_sf_4269)
data_flat$longitude <- round(xy_4269[,1], 5)
data_flat$latitude <- round(xy_4269[,2], 5)

############################
#### VISIT TABLE ####
############################
data_flat <- data_flat %>%
  mutate(visitDate = format(as.Date(Date, format = "%Y-%m-%d"), "%Y-%m-%d"),
         snowDepthMeters = NA,
         waterDepthMeters =- NA,
         crew = NA,
         bait = "None",
         accessMethod = NA,
         landFeatures = NA,
         comments = NA,
         wildtrax_internal_update_ts = NA,
         wildtrax_internal_lv_id = NA,
         time_zone = NA,
         data_origin = dataset_code,
         missinginvisit = NA,
         survey_year = sub("\\-.*", "", visitDate),
         survey_time = case_when(is.na(Time) ~ "09:00:00",
                                 !is.na(Time)  ~ as.character(format(Time, format = "%H:%M:%S"))),
         surveyDateTime = paste(visitDate, survey_time),
         observer = case_when(Observer == "DF"  ~ "obs01",
           Observer == "KPL" ~ "obs02",
           Observer == "BMS" ~ "obs03",
           is.na(Observer)   ~ "NA"
         ),
         pkey_dt= paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), observer, sep=":")
  ) 

################################
#### Update master_observer ####
################################
unique_observers <- data_flat %>%
  select(Observer, observer) %>% 
  distinct() %>%
  filter(!is.na(Observer)) %>% # Exclude rows where Observer is NA
  mutate(
    observer_name = Observer,
    observer_id = observer
  )

# Create the append_obs data frame
append_obs <- unique_observers %>%
  select(observer_id, observer_name) %>%
  mutate(
    organization = "Gov-NL",
    project = "Labrador Highlands Research Group Point Counts 2007"
  ) %>%
  select(organization, project, observer_id, observer_name)

# Identify rows in append_obs that are not in observer_Tbl
new_rows <- anti_join(append_obs, observer_Tbl, 
                      by = c("organization", "project", "observer_id", "observer_name"))

# Combine new rows with the existing observer_Tbl
if(nrow(new_rows)>1){
  updated_observer_Tbl <- bind_rows(observer_Tbl, new_rows)
  write_csv(updated_observer_Tbl, file.path(wd, "master_observer.csv"))
  dr<- drive_get("WildTrax-dataHarmonization", shared_drive = "BAM_Core")
  observer_Tbl <- file.path(wd, "master_observer.csv")
  drive_upload(media = observer_Tbl, path = as_id(dr), name = "master_observer.csv", overwrite = TRUE)
}

############################
#### SURVEY TABLE ####
############################
data_flat <- data_flat %>%
  filter(Quadrant != 7 | is.na(Quadrant)) %>% # drop before/after
  mutate(distanceMethod = "0m-100m-INF",
         durationMethod = "0-10min",
         durationinterval = "0-10min",
         distanceband = case_when(Quadrant == 1 ~ "0m-100m",
                                  Quadrant == 2 ~ "0m-100m",
                                  Quadrant == 3 ~ "0m-100m",
                                  Quadrant == 4 ~ "0m-100m",
                                  Quadrant == 5 ~ "100m-INF",
                                  Quadrant == 6 ~ "UNKNOWN", #flyover
                                  Quadrant == 8 ~ "UNKNOWN",
                                  is.na(Quadrant) ~ "UNKNOWN"), 
         species = case_when(Code == "CORE"  ~ "REDP", # for 'Acanthis flammea' as indicated in the science paper 'https://doi.org/10.1139/cjz-2014-0309'
                             Code == "UNID"  ~ "UNBI", # for 'Unidentified Species'
                             Code == "UFIN"  ~ "UNFI", # for 'Unidentified Finch'
                             Code == "DUCK"  ~ "UNDU", # for 'Unidentified Duck'
                             Code == "SASP"  ~ "SAVS", # for 'Savannah Sparrow'
                             Code == "TRSW"  ~ "TRES",  # for 'Tree Swallow'
                             Code == "AMTR"  ~ "ATSP", # for 'American Tree Sparrow'                            
                             Code == "UMER"  ~ "UNME", # for 'Unidentified Merganser'                              
                             Code == "NOSH"  ~ "NSHR", # for 'Northern Shrike'
                             is.na(Code) ~ "NONE",
                             TRUE ~ Code),
         ind_count = case_when(is.na(Code)~ 0, 
                               Comment =="three birds together" ~ 3,
                               Comment == "two birds together" ~ 2,
                               TRUE ~ 1),
         isHeard = case_when(Voc %in% c("S", "D", "C") ~ "Yes",
                             Voc =="Q" ~ "Yes",
                             Comment == "viewed and heard"~ "Yes",
                             TRUE  ~ "DNC"),
         isSeen = case_when(Voc =="Q" ~ "Yes",
                            Comment == "OBSERVED"~ "Yes",
                            Comment == "viewed and heard"~ "Yes",
                            Comment == "visual"~ "Yes",
                            Comment == "observed"~ "Yes",
                            Comment == "obs"~ "Yes",
                            TRUE  ~ "DNC")
         ) 

# CHECK
print(unique(data_flat$distanceMethod[!(data_flat$distanceMethod %in% WT_distMethTbl$distance_method_type)]))
print(unique(data_flat$durationMethod[!(data_flat$durationMethod %in% WT_durMethTbl$duration_method_type)]))
print(unique(data_flat$species[!(data_flat$species %in% WT_spTbl$species_code)]))
print(unique(data_flat$distanceband[!(data_flat$distanceband %in% WT_distBandTbl$distance_band_type)]))
print(unique(data_flat$durationinterval[!(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))

############################
#### BEHAVIOR TABLE ####
############################

#### Using information in 'Comment' field to feed behavior ####
#Step1: Visualize all types of 'Comment' # 140 rows
unique(data_flat$Comment)

# set up keyword list and update:
# 1. abundance: "flock", "several", "individuals", "birds"
keywords <- c("flock", "several", "individuals", "birds", "pair")
print(data_flat %>% filter(str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE))) %>% select (Comment, ind_count)) # 24 rows

# 2. isSeen : "observed", "seen", "viewed", "moving", "obs", "visual", "appear", "identified", "confirmed"
keywords <- c("observed", "seen", "viewed", "obs", "visual", "identified")
print(data_flat %>% filter(str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE))) %>% select (Comment, isSeen)) # 27 rows

data_flat <- data_flat %>%
  mutate(rawObserver = Observer,
         original_species = Code,
         scientificname = WT_spTbl$scientific_name[match(data_flat$species, WT_spTbl$species_code)],
         raw_distance_code = NA,
         raw_duration_code = NA,
         originalBehaviourData = data_flat$Voc,
         missingindetections = "DNC",
         age = case_when(Sex %in% c("M", "F", "P") ~ "Adult",
                          Sex %in% c("N", "Y") ~ "Juvenile",
                          Sex %in% c("U") | is.na(Sex) ~ "DNC",
                          TRUE ~ "DNC"
                          ),
         fm = case_when(Sex %in% c("M") ~ "Male",
                         Sex %in% c("F") ~ "Female",
                         Sex %in% c("P", "N", "Y", "U") | is.na(Sex) ~ "DNC",
                         TRUE ~ "DNC"),
         group =case_when(Comment =="FLOCKS OF 16+ AND 35+" ~ "flock",
                          Comment =="FLOCK OF 15" ~ "flock",
                          TRUE ~ "No"),
         flyover =case_when(Quadrant ==6 ~ "Yes",
                            TRUE ~ "No"),
         displaytype = "DNC",
         nestevidence = "DNC",
         behaviourother = "DNC",
         atlas_breeding_code = "DNC",
         pc_vt = case_when(Voc == "C" ~ "Calls",
                           Voc == "S" ~ "Sing",
                           Voc == "N" ~ "None-vocal",
                           TRUE ~ "DNC"),
         pc_vt_detail = "DNC"  
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


# flyover (flying) data singled out
no_flyover<- data_flat %>%
  filter(!flyover == "Yes")

yes_flyover<- data_flat %>%
  filter(flyover == "Yes")
print(yes_flyover)



#---LOCATION
# Remove duplicated location
WTlocation <- c("location", "longitude", "latitude")
location_tbl <- no_flyover[!duplicated(no_flyover[,WTlocation]), WTlocation]
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), row.names = FALSE, na = "")
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 


#---VISIT
# Delete duplicated based on WildtTrax attributes (double observer on the same site, same day).
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")
visit_tbl <- no_flyover[!duplicated(no_flyover[,WTvisit]), WTvisit] # 
write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), row.names = FALSE, na = "")
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 


#---SURVEY
# Delete duplicated in the eye of the survey table *abundance is used
survey_tbl <- no_flyover %>% 
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

extended_tbl <- data_flat[!duplicated(data_flat[,Extended]), Extended]
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
