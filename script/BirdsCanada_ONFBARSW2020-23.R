# Title: "Ontariuo Forest Birds at Risk (southwest Ontario) 2020-2023"
# Source dataset is an excel spreadsheet
# Author: "Melina Houle"
# Date: "April 8, 2026"
# Note on translation:
# --- 30 rows have empty coordinates. Delete 
# --- In source file, column "Sing/call/visual" has information on how the bird was detected and behavioral details. 
# --- A translation table was built and save with source file to facilitate translation.  
# --- Some sites used means coordinates due to position variation

#update.packages()
library(googlesheets4)
library(dplyr) # mutate, %>%
library(terra)
library(googledrive) #drive_get, drive_mkdir, drive_ls, drive_upload
library(stringr)
library(readr)
library(readxl) #read_excel
library(reshape2) # melt

## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

organization <- "Birds Canada"
dataset_code <- "ONFBARSW2020-23"

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
    filter(dataset_code =="ONFBARSW2020-23") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  data_id <- gd.list %>%
    filter(name =="Birds Canada Southwestern Ontario Data 2020-2023.xlsx") %>%
    select("id")
  drive_download(as_id(as.character(data_id)), path = file.path(project_dir, "Birds Canada Southwestern Ontario Data 2020-2023.xlsx"))
  data_id <- gd.list %>%
    filter(name =="Birds Canada Southwestern Ontario Data 2020-2023_beh_translationrules.xlsx") %>%
    select("id")
  drive_download(as_id(as.character(data_id)), path = file.path(project_dir, "Birds Canada Southwestern Ontario Data 2020-2023_beh_translationrules.xlsx"))
  
}

data_tbl <- read_excel(file.path(project_dir, "Birds Canada Southwestern Ontario Data 2020-2023.xlsx"))
names(data_tbl)<-str_replace_all(names(data_tbl), c(" " = "_"))
behavior <- read_excel(file.path(project_dir, "Birds Canada Southwestern Ontario Data 2020-2023_beh_translationrules.xlsx"))

#--------------------------------------------------------------
#
#       TRANSLATE
############################
#### Location TABLE ####
############################
coords_origin <- data_tbl[,c("easting", "northing")]
  
data_tbl_wgs<- vect(data_tbl, geom = c("easting", "northing"), crs = "EPSG:2958") %>%
  project("EPSG:4326")

coords <- geom(data_tbl_wgs)[, c("x", "y")]
data_tbl<- cbind(as.data.frame(data_tbl_wgs), coords, coords_origin)

s_location <- data_tbl %>%
  dplyr::select(x, y, point_id, site_id) %>%
  mutate(organization = organization,
         site = sub(" .*", "", site_id),,
         location = paste(dataset_code, site, point_id, sep= "_")) %>%
  group_by(location) %>%
  dplyr::summarize(
    latitude = mean(y, na.rm = TRUE),
    longitude = mean(x, na.rm = TRUE),
    location_comments = if_else(
      any(abs(y - mean(y, na.rm = TRUE)) > 1e-6) |
        any(abs(x - mean(x, na.rm = TRUE)) > 1e-6),
      "Mean coordinates used due to variation across years",
      NA_character_
    ),
    .groups = "drop"
  ) %>%
  mutate(buffer_m = 100,
         location_visibility = "Visible",
         true_coordinates = FALSE,
         internal_wildtrax_id = NA)

############################
#### VISIT TABLE ####
############################
s_data <- data_tbl %>% 
  rename(station = point_id)  %>%
  mutate(site = sub(" .*", "", site_id),
         location = paste(dataset_code, site, station, sep= "_"),
         visitDate = paste(year, month, day, sep = "-"),
         rawObserver = sub("/", "_", observer),
         observer = paste0("obs", rawObserver),
         utmZone = zone,
         snowDepthMeters= NA,
         waterDepthMeters = NA,
         crew = NA,
         bait = "NONE",
         accessMethod = NA,
         landFeatures = NA,
         wildtrax_internal_update_ts = NA,
         wildtrax_internal_lv_id = NA,
         time_zone = NA,       
         data_origin = NA,
         survey_year = sub("\\-.*", "", visitDate),
         missinginlocations = NA,
         missinginvisit = NA)

############################
#### SURVEY TABLE ####
############################
pc_survey <- s_data %>% 
  left_join(s_location, by="location")  %>%
  melt(measure.vars = c("less_than_50","50_to_100", "more_than_100", "flyovers" ), value.name = "ind_count") %>%
  filter(!is.na(ind_count)) %>%
  mutate(organization = organization,
         project = dataset_code,
         survey_time = ifelse(is.na(start_time), "00:00:01", as.character(format(start_time, "%H:%M:%S"))),
         surveyDateTime = paste(visitDate, survey_time),
         pkey_dt = paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), observer, sep=":"),
         raw_distance_code = variable,
         raw_duration_code = "0-10min",
         original_species = species,
         durationMethod = "0-10min",
         durationinterval = "0-10min",
         distanceMethod = "0m-50m-100m-INF", 
         distanceband = case_when(variable == "less_than_50" ~ "0m-50m",
                                  variable == "50_to_100" ~ "50m-100m",
                                  variable == "more_than_100" ~ "100m-INF",
                                  variable == "flyovers" ~ "Flyover"),
         species= case_when(species == "BWWA/GWWA" ~ "UNWA",
                            species == "RBWL" ~ "RWBL",
                            species == "HOWR" ~ "NHWR",
                            TRUE ~ species),
         scientificname = WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)],
         isHeard = case_when(detection == "S" ~ "Yes",
                             detection == "C" ~ "Yes",
                             detection == "V"~ "No",
                             detection ==  "DR" ~ "Yes",
                             TRUE ~ "DNC"),
         isSeen = case_when(detection == "S" ~ "No",
                             detection == "C" ~ "No",
                             detection == "V"~ "Yes",
                             detection == "DR" ~ "DNC",
                             TRUE ~ "DNC"),
         missingindetections = NA)


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
  dplyr::mutate(originalBehaviourData = detection,
                age = "DNC",
                fm = "DNC",
                group = "DNC",
                flyover = ifelse(variable=="flyovers", "Yes", "No"),
                nestevidence = "DNC",
                displaytype = "DNC",
                pc_vt = behavior$pc_vt[match(detection, behavior$detection_code)],
                pc_vt_detail = behavior$pc_vt_detail[match(detection, behavior$detection_code)],
                behaviourother = "DNC",
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
dr<- drive_get(paste0("DataTransfered/",organization), shared_drive= "BAM_AvianData")

#Set GoogleDrive id
if (nrow(drive_ls(as_id(dr), pattern = dataset_code)) == 0){
  dr_dataset_code <-drive_mkdir(dataset_code, path = as_id(dr), overwrite = NA)
} else {
  dr_dataset_code <- drive_ls(as_id(dr), pattern = dataset_code)
}
dr_ls <- drive_ls(as_id(dr), pattern = dataset_code)

#---LOCATION
no_flyover<- pc_behavior %>%
  filter(!flyover == "Yes")

WTlocation <- c("organization", "location", "latitude", "longitude", "buffer_m", "location_visibility", "true_coordinates", "location_comments", "internal_wildtrax_id")

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


