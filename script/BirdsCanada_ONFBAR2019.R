# Title: "Translate Alaska Landbird Monitoring Survey 2002-2022"
# Source dataset is an excel spreadsheet
# Author: "Melina Houle"
# Date: "January 9, 2024"
# Note on translation:
# --- 30 rows have empty coordinates. Delete 
# --- 
# ---  

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
dataset_code <- "ONFBARSW2019"

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
    filter(dataset_code =="ONFBARSW2019") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  data_id <- gd.list %>%
    filter(name =="2019_PointCountData.xlsx") %>%
    select("id")
  drive_download(as_id(as.character(data_id)), path = file.path(project_dir, "2019_PointCountData.xlsx"))
  data_id <- gd.list %>%
    filter(name =="2019_PointCountData_beh_translationrules.xlsx") %>%
    select("id")
  drive_download(as_id(as.character(data_id)), path = file.path(project_dir, "2019_PointCountData_beh_translationrules.xlsx"))
}

data_tbl <- read_excel(file.path(project_dir, "2019_PointCountData.xlsx"))
names(data_tbl)<-str_replace_all(names(data_tbl), c(" " = "_"))
behavior <- read_excel(file.path(project_dir, "2019_PointCountData_beh_translationrules.xlsx"))

#--------------------------------------------------------------
#
#       TRANSLATE
############################
#### Location TABLE ####
############################
# Delete PC with empty location
data_tbl <- data_tbl %>%
  filter(!is.na(Easting))
coords_origin <- data_tbl[,c("Easting", "Northing")]
  
data_tbl_wgs<- vect(data_tbl, geom = c("Easting", "Northing"), crs = "EPSG:2958") %>%
  project("EPSG:4326")

coords <- geom(data_tbl_wgs)[, c("x", "y")]
data_tbl<- cbind(as.data.frame(data_tbl_wgs), coords, coords_origin)

s_location <- data_tbl %>%
  rename(latitude = y,
         longitude = x) %>%
  mutate(bufferRadiusMeters = 100,
         isHidden = TRUE,	
         trueCoordinates = FALSE)	

############################
#### VISIT TABLE ####
############################
s_data <- s_location %>% 
  rename(rawObserver = Observer,
         comments = Comments,
         visitDate = Date,
         site = Site_ID,
         station = Point_ID,
         easting = Easting, 
         northing = Northing)  %>%
  mutate(location = paste(dataset_code, site, station, sep= "_"),
         observer = paste0("obs", rawObserver),
         utmZone = Zone,
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
  melt(measure.vars = c("Dist_50","Dist_50-100", "Dist_100", "Flyover" ), value.name = "ind_count") %>%
  filter(!is.na(ind_count)) %>%
  rename(detection_cd = 'Sing/call/visual') %>%
  mutate(organization = organization,
         project = dataset_code,
         survey_time = format(Start_Time, format = "%H:%M:%S"),
         surveyDateTime = paste(visitDate, survey_time),
         pkey_dt = paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), observer, sep=":"),
         raw_distance_code = variable,
         raw_duration_code = "0-10min",
         durationMethod = "0-10min",
         durationinterval = "0-10min",
        # distanceMethod = ifelse(Distance == ">150", "0m-10m-20m-30m-40m-50m-60m-70m-80m-90m-100m-125m-150m-INF", "0m-10m-20m-30m-40m-50m-60m-70m-80m-90m-100m-125m-150m-200m-250m-300m-350m-400m-INF"),
         distanceMethod = "0m-50m-100m-INF", 
         distanceband = case_when(variable == "Dist_50" ~ "0m-50m",
                                  variable == "Dist_50-100" ~ "50m-100m",
                                  variable == "Dist_100" ~ "100m-INF",
                                  variable == "Flyover" ~ "Flyover"),
         species= case_when(Bird_Species == "WOODPECKER SP" ~ "UNWO",
                            Bird_Species == "WOOD SP" ~ "UNWO",
                            Bird_Species == "GULL SP" ~ "UNGU",
                            Bird_Species == "WPWA" ~ "PAWA",
                            Bird_Species == "CUCKOO SP" ~ "UNCU",
                            Bird_Species == "SPARROW SP" ~ "UNSP",
                            TRUE ~ Bird_Species),
         scientificname = WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)],
         isHeard = behavior$heard[match(detection_cd, behavior$detection_code)],
         isSeen = behavior$seen[match(detection_cd, behavior$detection_code)],
         original_species = Bird_Species,
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
  dplyr::mutate(originalBehaviourData = detection_cd,
                age = behavior$age[match(detection_cd, behavior$detection_code)],
                fm = behavior$fm[match(detection_cd, behavior$detection_code)],
                group = behavior$group[match(detection_cd, behavior$detection_code)],
                flyover = ifelse(variable=="Flyover", "Yes", "No"),
                nestevidence = behavior$nestevidence[match(detection_cd, behavior$detection_code)],
                displaytype = behavior$displaytype[match(detection_cd, behavior$detection_code)],
                pc_vt = behavior$pc_vt[match(detection_cd, behavior$detection_code)],
                pc_vt_detail = behavior$pc_vt_detail[match(detection_cd, behavior$detection_code)],
                behaviourother = behavior$heard[match(detection_cd, behavior$detection_code)],
                comments = NA,
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
no_flyover<- pc_behavior %>%
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


