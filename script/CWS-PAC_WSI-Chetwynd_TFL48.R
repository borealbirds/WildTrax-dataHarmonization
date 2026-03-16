# Title: "WSI-Chetwynd-TFL48"
# Source dataset is an excel spreadsheet
# Author: "Melina Houle"
# Date: "November 4, 2025"
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
  dplyr::filter(!species_code %in% c("CORBRA", "PICHUD", "GRAJ"))

BC_spTbl <- "https://docs.google.com/spreadsheets/d/107U-tUtD5rRv3AA0akXh8DYNAEisvFYBbdDh5lfU6fg"
bird_Tbl <-  read_sheet(BC_spTbl, sheet = "birdlist")
names(bird_Tbl)<-str_replace_all(names(bird_Tbl), c(" " = "_"))

WT_durMethTbl <- read.csv(file.path("./lookupTables/duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path("./lookupTables/distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path("./lookupTables/duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path("./lookupTables/distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

organization <- "CWS-PAC"
dataset <- "Chetwynd-TFL48"
dataset_code <- "WSI-Chetwynd"
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
    filter(dataset_code =="WSI-Chetwynd") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  data1_db <- gd.list %>%
    filter(name =="Off-road point counts TFL 48.xls") %>%
    select("id")
  drive_download(as_id(as.character(data1_db)), path = file.path(project_dir, "Off-road point counts TFL 48.xls"))
}

data <- file.path(project_dir, "Off-road point counts TFL 48.xls")

#--------------------------------------------------------------
#       LOAD
#--------------------------------------------------------------
survey <- read_xls(data, sheet = "General Survey")
location <- read_xls(data, sheet = "Sample Station Information")
names(survey)<-str_replace_all(names(survey), c(" " = "_"))
names(location)<-str_replace_all(names(location), c(" " = "_"))
location <- location %>%
  mutate(
    Easting_DC = as.double(Easting_DC),
    Northing_DC = as.double(Northing_DC)
  )
#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
# REPROJECT
spLocation_pj <- vect(location, geom=c("Easting_DC", "Northing_DC"), crs="epsg:3157")
spLocation_pj <- as.data.frame(project(spLocation_pj,"EPSG:4326"),  geom = "XY") # WILDTRAX

pc_location <- spLocation_pj %>%
  dplyr::rename(longitude = x,
                latitude = y) %>%
  mutate(organization = "CWS-PAC",
         Study_Area_Name = gsub(" ", "_", Study_Area_Name),
         location = paste0(dataset_code, ":", Study_Area_Name, ":", Sample_Station_Label ),
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
#### Visit/Location TABLE ####
############################
s_visit <- survey %>%
  inner_join(location, by = c("Study_Area_Name","Sample_Station_Label")) %>%
  dplyr::select(Study_Area_Name, Sample_Station_Label , Date, Time, Species, Count, Detect_Type, `Detect_Distance_(m)`, Time_Interval, Easting_DC, Northing_DC)  %>%
  dplyr::mutate(Study_Area_Name = gsub(" ", "_", Study_Area_Name),
                location =  paste0(dataset_code, ":", Study_Area_Name, ":", Sample_Station_Label),
                visitDate = ifelse(is.na(Date), "1900-01-01", as.character(format(Date, "%Y-%m-%d"))),
                missingvisit = NA,
                rawObserver = NA,
                observer = "Kelly Squires",
                survey_time = ifelse(is.na(Time), "00:00:01", as.character(format(Time, "%H:%M:%S"))),
                pkey_dt = paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), observer, sep=":"),
                snowDepthMeters= NA,
                waterDepthMeters = NA,
                crew = NA,
                bait = "NONE",
                accessMethod = NA,
                landFeatures = NA,
                wildtrax_internal_update_ts = NA,
                wildtrax_internal_lv_id = NA,
                comments = NA,
                easting = Easting_DC,
                northing = Northing_DC,
                utmZone = "10N",
                time_zone = NA,       
                data_origin = NA,
                missinginvisit = NA,
                survey_year = substr(Date, 1, 4),
                missinginlocations = NA)




################################
#### Update master_observer ####
################################
unique_observers <- s_visit %>%
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
    organization = "CWS-PACIFIC",
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

############################
#### SURVEY TABLE ####
############################
##Explore what's in note
pc_survey <- s_visit %>% 
  dplyr::rename(site = Study_Area_Name , 
                station = Sample_Station_Label,
                ind_count = Count) %>%
  mutate(organization = organization,
         project = dataset,
         original_species = Species,
         visitDate = ifelse(is.na(Date), "1900-01-01", as.character(Date)),
         distanceMethod = "0m-10m-20m-30m-40m-50m-60m-70m-80m-90m-100m-125m-150m-INF",
         distanceband = case_when(
           `Detect_Distance_(m)` >= 0  & `Detect_Distance_(m)` < 10  ~ "0m-10m",
           `Detect_Distance_(m)` >= 10 & `Detect_Distance_(m)` < 20  ~ "10m-20m",
           `Detect_Distance_(m)` >= 20 & `Detect_Distance_(m)` < 30  ~ "20m-30m",
           `Detect_Distance_(m)` >= 30 & `Detect_Distance_(m)` < 40  ~ "30m-40m",
           `Detect_Distance_(m)` >= 40 & `Detect_Distance_(m)` < 50  ~ "40m-50m",
           `Detect_Distance_(m)` >= 50 & `Detect_Distance_(m)` < 60  ~ "50m-60m",
           `Detect_Distance_(m)` >= 60 & `Detect_Distance_(m)` < 70  ~ "60m-70m",
           `Detect_Distance_(m)` >= 70 & `Detect_Distance_(m)` < 80  ~ "70m-80m",
           `Detect_Distance_(m)` >= 80 & `Detect_Distance_(m)` < 90  ~ "80m-90m",
           `Detect_Distance_(m)` >= 90 & `Detect_Distance_(m)` < 100  ~ "90m-100m",
           `Detect_Distance_(m)` >= 100 & `Detect_Distance_(m)` < 125  ~ "100m-125m",
           `Detect_Distance_(m)` >= 125 & `Detect_Distance_(m)` <= 150 ~ "125m-150m",
           `Detect_Distance_(m)` > 150 ~ "150m-INF",
           TRUE ~ "UNKNOWN"),
         durationMethod = "0-3-5min",
         durationinterval = case_when(Time_Interval == 3  ~ "0-3min",
                                      Time_Interval == 5  ~ "3-5min",
                                      TRUE ~ "UNKNOWN"),
         isHeard = "Yes",
         isSeen = "DNC",
         missingindetections = NA,
         raw_distance_code = `Detect_Distance_(m)`, 
         raw_duration_code = Time_Interval,
         #Behaviour
         originalBehaviourData = Detect_Type,
         pc_vt = Detect_Type,
         pc_vt_detail ="DNC",
         age = "DNC",
         fm = "DNC",
         group = "DNC",
         flyover = "DNC",
         displaytype = "DNC",
         nestevidence = "DNC",
         behaviourother = "DNC") %>% 
  filter(!is.na(original_species))


# Test species using scientific name
species_tbl <- pc_survey %>%
  mutate(Species = if_else(str_starts(Species, "B-"), Species, paste0("B-", Species))) %>%
  select(Species) %>%
  dplyr::distinct(Species) %>%
  left_join(bird_Tbl, by=c("Species"="Species_Code")) %>%
  left_join(WT_spTbl, by=c("Scientific_Name"="scientific_name"))

#list the ones that didn't pass
species_tbl[is.na(species_tbl$species_code),]

data_flat <- pc_survey %>% 
  mutate(Species = if_else(str_starts(Species, "B-"), Species, paste0("B-", Species))) %>%
  left_join(species_tbl, by="Species")  %>%
  mutate(species = if_else(is.na(species_code), "UNBI", species_code),
         scientificname = Scientific_Name,
         surveyDateTime = paste(visitDate, survey_time))

## CHECK
print(unique(data_flat$distanceMethod[!(data_flat$distanceMethod %in% WT_distMethTbl$distance_method_type)]))
print(unique(data_flat$durationMethod[!(data_flat$durationMethod %in% WT_durMethTbl$duration_method_type)]))
print(unique(data_flat$species[!(data_flat$species %in% WT_spTbl$species_code)]))
print(unique(data_flat$durationinterval[!(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))
print(unique(data_flat$distanceband[!(data_flat$distanceband %in% WT_distBandTbl$distance_band_type)]))
print(unique(data_flat$durationinterval[!(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))


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
dr_ls <- drive_ls(as_id(dr), pattern = dataset_code)


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

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- data_flat[!duplicated(data_flat[,WTvisit]), WTvisit] # 

write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), row.names = FALSE, na = "")
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 

#---SURVEY
survey_tbl <- data_flat %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband, durationinterval, isHeard, isSeen, comments) %>%
  dplyr::summarise(abundance = sum(ind_count), .groups= "keep")

WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen", "comments")

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




