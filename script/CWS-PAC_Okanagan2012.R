# Title: "Okanagan2012"
# Source dataset is an excel spreadsheet
# Author: "Melina Houle"
# Date: "January 07, 2026"
# Note: source table visit has duplicated because of second observer. 
#      - Delete all points where Distance_Band == Outside
#      - Delete all points where Species == NONE
#      - Delete all points where Minute_Detected is Before | After | Before/after | 11 | ?

#----------------------------------------------
#update.packages()
library(dplyr) # mutate, %>%
library(readxl) #read_excel, read_xls
library(stringr) #str_replace_all
library(googledrive) #drive_get, drive_mkdir, drive_ls, drive_upload
library(googlesheets4)
library(terra)

source("./config.R")

## Initialize variables (wd is define in config.R)
setwd(file.path(wd))

drive_auth()
#project_integration
WTpj_Tbl <- read_sheet("https://docs.google.com/spreadsheets/d/1fqifS_E5O_IpW1B-UG_xthr9hzY6FIek-nFjCrt1G0w", sheet = "project")
#Observer
obs_url <- "https://docs.google.com/spreadsheets/d/1gsm4LSwU31vJQIh5Ahpy70dhYvPr9ftHqaW1gw75IeU"
observer_Tbl <-  read_sheet(obs_url, sheet = "master_observer.csv")
#species
WT_spTbl <- read.csv(file.path("./lookupTables/species_codes.csv")) %>%
  dplyr::filter(!species_code %in% c("CORBRA", "PICHUD", "GRAJ", "PSFL"))

WT_durMethTbl <- read.csv(file.path("./lookupTables/duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path("./lookupTables/distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path("./lookupTables/duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path("./lookupTables/distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

organization <- "CWS-PAC"
dataset_code <- "Okanagan2012"
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

#--------------------------------------------------------------
#
#       DOWNLOAD FILE FROM DRIVE 
#
#--------------------------------------------------------------
if (length(list.files(project)) ==0) {
  pid <- WTpj_Tbl %>%
    filter(dataset_code =="Okanagan2012") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  data1_db <- gd.list %>%
    filter(name =="OKpointcounts2012fixed.xlsx") %>%
    select("id")
  drive_download(as_id(as.character(data1_db)), path = file.path(project_dir, "OKpointcounts2012fixed.xlsx"))
}

data <- file.path(project_dir, "OKpointcounts2012fixed.xlsx")

#--------------------------------------------------------------
#       LOAD
#--------------------------------------------------------------
survey <- read_excel(data, sheet = "Detections") |>
  rename_with(~ str_replace_all(.x, " ", "_")) |>
  dplyr::select(`Unique_Point_Count_Stn_#`, `Point_Count_Visit_#`, PC_Stn_Survey_Date, Minute_Detected, `#_of_Birds_Detected`, 
                AOU_Species_Code, Distance_Band, Detection_Type, `Gender/Sex`, Ageclass, Activity_Code,  SPPNotes) |>
  mutate(PC_Stn_Survey_Date = coalesce(as.Date(PC_Stn_Survey_Date), as.Date("1900-01-01"))) |>
  dplyr::filter(AOU_Species_Code != "NONE" & 
                  Distance_Band  != "Outside" &
                  !(Minute_Detected %in% c("Before", "After", "Before/After", "11", "?", "N/A")))

visit <- read_excel(data, sheet = "Visits") |>
  rename_with(~ str_replace_all(.x, " ", "_")) |>
  dplyr::select(`Unique_Point_Count_Stn_#`, `Point_Count_Visit_#`, `Observer_#`, PC_Stn_Survey_Date, Count_Start_Time, Count_End_Time,          
                PCVisitNotes, Count_Duration)  |>
  mutate(PC_Stn_Survey_Date = coalesce(as.Date(PC_Stn_Survey_Date), as.Date("1900-01-01"))) |>
  dplyr::filter((Count_End_Time - Count_Start_Time) %in% c(5, 10)) %>%
  group_by(
    `Unique_Point_Count_Stn_#`,
    `Point_Count_Visit_#`,
    PC_Stn_Survey_Date,
    Count_Start_Time,
    Count_End_Time
  ) %>%
  summarise(
    across(-`Observer_#`, first),
    PCObserver = paste(unique(`Observer_#`), collapse = ", "),
    .groups = "drop"
  )

location <- read_excel(data, sheet = "Stations") |>
  rename_with(~ str_replace_all(.x, " ", "_")) |>
  dplyr::select(`Unique_Point_Count_Stn_#`, `Point_Count_Stn_#`, UTM__Easting, UTM_Northing)

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
# REPROJECT UTM 11N
spLocation_pj <- vect(location, geom=c("UTM__Easting", "UTM_Northing"), crs="epsg:2955")
spLocation_pj <- as.data.frame(project(spLocation_pj,"EPSG:4326"),  geom = "XY") # WILDTRAX

pc_location <- spLocation_pj %>%
  dplyr::rename(longitude = x,
                latitude = y) %>%
  mutate(organization = "CWS-PAC",
         location = paste0(dataset_code, ":", `Unique_Point_Count_Stn_#`),
         buffer_m = NA,
         location_visibility = "Visible",
         true_coordinates = TRUE,
         location_comments = NA,
         internal_wildtrax_id = NA)

  ####### CHECK MAPPING 
  #library(sf)
  #library(sp)
  #canada <- st_read("E:/MelinaStuff/BAM/GIS_layer/CanadaLAEA.shp")
  #bnd <- st_transform(canada, crs= st_crs(4326)) %>% filter(NAME == "British Columbia / Colombie-Britannique")
  #plot(bnd$geometry)
  #xy <- pc_location[,c("longitude", "latitude")]
  #spdf <- SpatialPointsDataFrame(coords = xy, data = pc_location,
  #                             proj4string = CRS("+proj=longlat +ellps=GRS80 +towgs84=0.0,0.0,0.0,0.0,0.0,0.0,0.0 +no_defs"))
  #pc <- st_as_sf(spdf, coords = c("longitude", "latitude"), crs = UTM)
  #plot(pc$geometry, col = "red", add= TRUE)
  
############################
#### Visit TABLE ####
############################
s_visit <- visit %>%
  left_join(location, by=c("Unique_Point_Count_Stn_#")) %>%
  dplyr::mutate(site = `Unique_Point_Count_Stn_#`,
                location =  paste0(dataset_code, ":", `Unique_Point_Count_Stn_#`),
                visitDate = ifelse(is.na(PC_Stn_Survey_Date), "1900-01-01", as.character(format(PC_Stn_Survey_Date, "%Y-%m-%d"))),
                missingvisit = NA,
                survey_time = ifelse(is.na(Count_Start_Time), "00:00:01", as.character(format(Count_Start_Time, "%H:%M:%S"))),
                snowDepthMeters= NA,
                waterDepthMeters = NA,
                crew = NA,
                bait = "NONE",
                accessMethod = NA,
                landFeatures = NA,
                wildtrax_internal_update_ts = NA,
                wildtrax_internal_lv_id = NA,
                comments = NA,
                utmZone = "11N",
                time_zone = NA,       
                data_origin = NA,
                missinginvisit = NA,
                survey_year = substr(PC_Stn_Survey_Date, 1, 4),
                missinginlocations = NA)  

############################
#### SURVEY TABLE       ####
############################
pc_survey <- survey %>% 
  inner_join(s_visit, by=c("Unique_Point_Count_Stn_#", "Point_Count_Visit_#", "PC_Stn_Survey_Date"), relationship = "many-to-one") %>%
  mutate(organization = organization,
         project = dataset_code,
         site = `Unique_Point_Count_Stn_#`,
         station = NA,
         observer = PCObserver,
         rawObserver = PCObserver,
         pkey_dt = paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), observer, sep=":"),
         original_species = AOU_Species_Code,
         ind_count = `#_of_Birds_Detected`,
         easting = UTM__Easting,
         northing = UTM_Northing,
         distanceMethod = "0m-10m-20m-30m-40m-50m-INF",
         distanceband = case_when(Distance_Band == "1-10" ~ "0m-10m",
                                  Distance_Band == "11-20" ~ "10m-20m",
                                  Distance_Band == "20-30" ~ "20m-30m",
                                  Distance_Band == "21-30" ~ "20m-30m",
                                  Distance_Band == "30-40" ~ "30m-40m",
                                  Distance_Band == "31-40" ~ "30m-40m",
                                  Distance_Band == "31-50" ~ "UNKNOWN",
                                  Distance_Band == "41-50" ~ "40m-50m",
                                  Distance_Band == "41233" ~ "UNKNOWN",
                                  Distance_Band == ">50" ~ "50m-INF",
                                  Distance_Band == "Inside" ~ "UNKNOWN", 
                                  Distance_Band == "inside" ~ "UNKNOWN", 
                                  TRUE ~ "UNKNOWN"),
         durationMethod = "0-1-2-3-4-5-6-7-8-9-10min", 
         durationinterval = case_when(Minute_Detected == "1" ~ "0-1min",
                                      Minute_Detected == "2" ~ "1-2min",
                                      Minute_Detected == "3" ~ "2-3min",
                                      Minute_Detected == "4" ~ "3-4min",
                                      Minute_Detected == "5" ~ "4-5min",
                                      Minute_Detected == "6" ~ "5-6min",
                                      Minute_Detected == "7" ~ "6-7min",
                                      Minute_Detected == "8" ~ "7-8min",
                                      Minute_Detected == "9" ~ "8-9min",
                                      Minute_Detected == "10" ~ "9-10min",
                                      TRUE ~ "UNKNOWN"),,
         isHeard = case_when(Detection_Type == "V/S" ~ "Yes",
                             Detection_Type == "C" ~ "Yes",
                             Detection_Type == "S" ~ "Yes",
                             Detection_Type == "V/C" ~ "Yes",
                             Detection_Type == "V" ~ "No",
                             Detection_Type == "S/C" ~ "Yes",
                             Detection_Type == "D" ~ "Yes",
                             Detection_Type == "V/S/C" ~ "Yes",
                             Detection_Type == "V/D" ~ "Yes"),
         isSeen = case_when(Detection_Type == "V/S" ~ "Yes",
                            Detection_Type == "C" ~ "No",
                            Detection_Type == "S" ~ "No",
                            Detection_Type == "V/C" ~ "Yes",
                            Detection_Type == "V" ~ "Yes",
                            Detection_Type == "S/C" ~ "No",
                            Detection_Type == "D" ~ "No",
                            Detection_Type == "V/S/C" ~ "Yes",
                            Detection_Type == "V/D" ~ "Yes"),
         missingindetections = NA,
         raw_distance_code = Distance_Band,
         raw_duration_code = Minute_Detected,
         #Behaviour
         originalBehaviourData = Activity_Code,
         pc_vt = case_when(Activity_Code == "TF" ~ "Travelling flight",
                           Activity_Code == "CC" ~ "COntact call",
                           Activity_Code == "LI" ~ "Living - unknown",
                           Activity_Code == "TE" ~ "Territorial",
                           Activity_Code == "D" ~ "Drumming",
                           Activity_Code == "FD" ~ "Feeding",
                           Activity_Code == "CH" ~ "Chasing",
                           Activity_Code == "BE" ~ "Bedding/Roosting",
                           Activity_Code == "C" ~ "Call",
                           TRUE ~ "DNC"),
         pc_vt_detail ="DNC",
         age = case_when(Ageclass == "A" ~ "Adult",
                         Ageclass == "UC" ~ "NA",
                         Ageclass == "J" ~ "Juvenile",
                         TRUE ~ "UNKNOWN"),
         fm = case_when(`Gender/Sex` == "M" ~ "Male",
                        `Gender/Sex` == "F" ~ "Female",
                          TRUE ~ "Unknown"),
         group = "DNC",
         flyover = "DNC",
         displaytype = "DNC",
         nestevidence = "DNC",
         behaviourother = "DNC")

# Test species using scientific name
species_tbl <- pc_survey %>%
  select(AOU_Species_Code) %>%
  dplyr::distinct(AOU_Species_Code) %>%
  left_join(WT_spTbl, by=c("AOU_Species_Code"="species_code"))

#list the ones that didn't pass
species_tbl[is.na(species_tbl$scientific_name),]

data_flat <- pc_survey %>% 
  left_join(species_tbl, by="AOU_Species_Code")  %>%
  mutate(species = case_when(AOU_Species_Code == "CEWA" ~ "CEDW",
                             AOU_Species_Code == "WWPE" ~ "WEWP",
                             AOU_Species_Code == "HOWR" ~ "NHWR",
                             AOU_Species_Code == "UNCH" ~ "UNBI",
                             AOU_Species_Code == "YWAR" ~ "YEWA",
                             AOU_Species_Code == "TRSW" ~ "TRES",
                             AOU_Species_Code == "RGPH" ~ "UNBI",
                             AOU_Species_Code == "RODO" ~ "UNBI", 
                             AOU_Species_Code == "MAKE" ~ "UNBI",
                             AOU_Species_Code == "CAGO" ~ "CANG",
                             AOU_Species_Code == "NOSH?" ~ "NSHR", 
                             AOU_Species_Code == "MACW" ~ "MGWA",
                             AOU_Species_Code == "EAST?" ~ "UNBI", 
                             AOU_Species_Code == "UNRA" ~ "UNBI", 
                             AOU_Species_Code == "UNEM" ~ "UEFL", 
                             AOU_Species_Code == "ECDO" ~ "EUCD",    
                             TRUE ~  AOU_Species_Code),
         scientificname = scientific_name,
         surveyDateTime = paste(visitDate, survey_time))

## CHECK
print(unique(data_flat$distanceMethod[!(data_flat$distanceMethod %in% WT_distMethTbl$distance_method_type)]))
print(unique(data_flat$durationMethod[!(data_flat$durationMethod %in% WT_durMethTbl$duration_method_type)]))
print(unique(data_flat$species[!(data_flat$species %in% WT_spTbl$species_code)]))
print(unique(data_flat$durationinterval[!(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))
print(unique(data_flat$distanceband[!(data_flat$distanceband %in% WT_distBandTbl$distance_band_type)]))
print(unique(data_flat$durationinterval[!(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))


#### Update master_observer ####
unique_observers <- data_flat %>%
  select(observer) %>% 
  distinct() %>%
  filter(!is.na(observer)) %>% # Exclude rows where Observer is NA
  mutate(
    observer_name = observer,
    observer_id = observer
  )

# Create the append_obs data frame
append_obs <- unique_observers %>%
  select(observer_id, observer_name) %>%
  mutate(
    organization = "CWS-PAC",
    project = dataset_code
  ) %>%
  select(organization, project, observer_id, observer_name)

# Identify rows in append_obs that are not in observer_Tbl
new_rows <- anti_join(append_obs, observer_Tbl, 
                      by = c("organization", "project", "observer_id", "observer_name"))

# Combine new rows with the existing observer_Tbl
if (nrow(new_rows) > 0) {
  sheet_append(obs_url, new_rows)
}

#--------------------------------------------------------------
#
#       EXPORT
#
#--------------------------------------------------------------

# Create sub folder in 'toUpload' with the organization name
dr<- drive_get("toUpload/", shared_drive = "BAM_AvianData")
to_upload_contents <- drive_ls(as_id(dr)) # print(to_upload_contents)
upload_folder <- to_upload_contents[to_upload_contents$name == organization, ]
if (nrow(upload_folder) == 0) {
  upload_folder <- drive_mkdir(organization, path = as_id(dr))
}

#Set GoogleDrive id
if (nrow(drive_ls(as_id(dr), pattern = dataset_code)) == 0){
  dr_dataset_code <-drive_mkdir(dataset_code, path = as_id(dr), overwrite = NA)
} else {
  dr_dataset_code <- drive_ls(as_id(dr), pattern = dataset_code)
}
dr_ls_a <- drive_ls(as_id(dr), pattern = dataset_code)

#---LOCATION
WTlocation <- c("organization", "location", "latitude", "longitude", "buffer_m", "location_visibility", "true_coordinates", "location_comments", "internal_wildtrax_id")

# Remove duplicated location
location_tbl <- pc_location[!duplicated(pc_location[,WTlocation]), WTlocation] 
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), row.names = FALSE, na = "")
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 

#---VISIT
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

no_flyover <- data_flat %>%
  dplyr::filter(flyover != "Yes")

visit_tbl <- no_flyover[!duplicated(no_flyover[,WTvisit]), WTvisit] # 

write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), row.names = FALSE, na = "")
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 

#---SURVEY
WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen", "comments")

survey_tbl <- no_flyover %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband, durationinterval, isHeard, isSeen, comments) %>%
  dplyr::summarise(abundance = sum(ind_count), .groups= "keep")

write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), row.names = FALSE, na = "")
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv"), overwrite = TRUE) 

#---EXTENDED
Extended <- c("organization", "project","location", "surveyDateTime", "species", "ind_count", "distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code", 
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
              "displaytype", "nestevidence", "behaviourother")

extended_tbl <- data_flat[!duplicated(data_flat[,Extended]), Extended] 
write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_behavior.csv")), quote = FALSE, row.names = FALSE, na = "")
extended_out <- file.path(out_dir, paste0(dataset_code,"_behavior.csv"))
drive_upload(media = extended_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_behavior.csv"), overwrite = TRUE) 


#---PROCESSING STATS
#write_lines(paste0("Organization: ", organization), file.path(out_dir, paste0(dataset_code, "_stats.csv")))
#write_lines(paste0("Project: ", dataset_code), file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
#nrow_location <- paste0("Number of locations: ", nrow(location_tbl))
#write_lines(nrow_location, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
#nrow_visit <- paste0("Number of visit: ", nrow(visit_tbl))
#write_lines(nrow_visit, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
#nrow_survey <- paste0("Number of survey: ", nrow(survey_tbl))
#write_lines(nrow_survey, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
#nrow_extended <- paste0("Number of extended: ", nrow(extended_tbl))
#write_lines(nrow_extended, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)




