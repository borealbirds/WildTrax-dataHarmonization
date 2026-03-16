# Title: "WSI-WossRoadside2003"
# Source dataset is an excel spreadsheet
# Author: "Melina Houle"
# Date: "December 1, 2025"
## Note on translation:
#   
#----------------------------------------------
#update.packages()
library(dplyr) # mutate, %>%
library(readxl) #read_excel, read_xls
library(stringr) #str_replace_all
library(googledrive) #drive_get, drive_mkdir, drive_ls, drive_upload
library(googlesheets4)
library(terra)
library(sf)

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

BC_spTbl <- "https://docs.google.com/spreadsheets/d/107U-tUtD5rRv3AA0akXh8DYNAEisvFYBbdDh5lfU6fg"
bird_Tbl <-  read_sheet(BC_spTbl, sheet = "birdlist")
names(bird_Tbl)<-str_replace_all(names(bird_Tbl), c(" " = "_"))

WT_durMethTbl <- read.csv(file.path("./lookupTables/duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path("./lookupTables/distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path("./lookupTables/duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path("./lookupTables/distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

organization <- "CWS-PAC"
dataset <- "WSI-WossRoadside2003"
dataset_code <- "WSI-WossRoadside2003"
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
    filter(dataset_code =="WSI-WossRoadside2003") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))

  survey2003 <- gd.list %>%
    filter(name =="TFL 37 Canfor Woss Road Transect Songbird Database 2003.xls") %>%
    select("id")
  drive_download(as_id(as.character(survey2003)), path = file.path(project_dir, "TFL 37 Canfor Woss Road Transect Songbird Database 2003.xls"))
  
  location <- gd.list %>%
    filter(name =="TFL 37 Songbird Road Transect UTM Database.xlsx") %>%
    select("id")
  drive_download(as_id(as.character(location)), path = file.path(project_dir, "TFL 37 Songbird Road Transect UTM Database.xlsx"))
}

survey_2003 <- file.path(project_dir, "TFL 37 Canfor Woss Road Transect Songbird Database 2003.xls")
location <- file.path(project_dir, "TFL 37 Songbird Road Transect UTM Database.xlsx")

#--------------------------------------------------------------
#       LOAD
#--------------------------------------------------------------
location <- read_excel(location, sheet = "roadtransect_utm")
names(location)<-str_replace_all(names(location), c(" " = "_"))

location <- location %>%
  mutate(Survey = gsub(" ", "_", TRANSECT), 
          location = paste0(dataset_code, ":", Survey, ":", POINT))

survey2003 <- read_excel(survey_2003, sheet = "Sheet1")
names(survey2003)<-str_replace_all(names(survey2003), c(" " = "_"))
survey2003 <- survey2003 %>%
  select(Survey, Point_Count, Date, Start_Time, Surveyor, Time_Interval, Count, Species, Distance, `V/C/S`, Sex, Age, Flyover, Bird_Comments, Point_Count_Comments) %>%
  mutate(Survey = gsub(" ", "_", Survey),
         Survey = case_when(Survey == "Kaipit" ~ "Kaipit_Road",
                            Survey == "Croman" ~ "Croman_Road",
                            Survey == "Vernon" ~ "Vernon_Road",
                            Survey == "Schoen_Lake_PP" ~ "Schoen_Road",
                            Survey == "Pinder" ~ "Pinder_Road",
                            Survey == "Plateau" ~ "plateau_road",
                            TRUE ~ Survey)
         ) %>%
  dplyr::filter(Survey %in% location$Survey)

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------

# REPROJECT UTM 10N
spLocation_pj <- vect(location, geom=c("EASTINGS", "NORTHINGS"), crs="epsg:3157")
spLocation_pj <- as.data.frame(project(spLocation_pj,"EPSG:4326"),  geom = "XY") # WILDTRAX

pc_location <- spLocation_pj %>%
  dplyr::rename(longitude = x,
                latitude = y) %>%
  mutate(organization = "CWS-PAC",
         location = paste0(dataset_code, ":", Survey, ":", POINT),
         buffer_m = NA,
         location_visibility = "Visible",
         true_coordinates = TRUE,
         internal_wildtrax_id = NA,
         location_comments = NA)


####### CHECK MAPPING 
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
survey <- survey2003 %>%
  mutate(location = paste0(dataset_code, ":", Survey, ":", Point_Count)) %>%
  inner_join(location %>% select(location, EASTINGS, NORTHINGS), by = "location")
    
s_visit <- survey %>%
  dplyr::mutate(visitDate = ifelse(is.na(Date), "1900-01-01", as.character(as.Date(Date, format = "%Y %m %d"))),
                missingvisit = NA,
                rawObserver = NA,
                observer = case_when(Surveyor == "Paul Chytyk" ~ "PC",
                                     Surveyor == "Mike Shepard" ~ "MS",
                                     Surveyor == "Paul Levesque" ~ "PL"),
                survey_time = ifelse(is.na(Start_Time), "00:00:01", as.character(format(Start_Time, "%H:%M:%S"))),
                pkey_dt = paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), observer, sep=":"),
                snowDepthMeters= NA,
                waterDepthMeters = NA,
                crew = NA,
                bait = "NONE",
                accessMethod = NA,
                landFeatures = NA,
                wildtrax_internal_update_ts = NA,
                wildtrax_internal_lv_id = NA,
                comments = Bird_Comments,
                easting = EASTINGS,
                northing = NORTHINGS,
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
  select(Surveyor) %>% 
  distinct() %>%
  filter(!is.na(Surveyor)) %>% # Exclude rows where Observer is NA
  mutate(
    observer_name = c("Paul Chytyk","Mike Shepard", "Paul Levesque"),
    observer_id = c("PC", "MS", "PL")
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

############################
#### SURVEY TABLE ####
############################
##Explore what's in note
pc_survey <- s_visit %>% 
  dplyr::rename(site = Survey , 
                station = Point_Count,
                ind_count = Count) %>%
  mutate(organization = organization,
         project = dataset,
         ind_count = as.numeric(ind_count),
         original_species = Species,
         distanceMethod = "0m-50m-75m-INF",
         distanceband = case_when(Distance == "0-50" ~ "0m-50m",
                                  Distance == "51-75" ~ "50m-75m",
                                  Distance == "75+" ~ "75m-INF",
                                  is.na(Distance)~ "UNKNOWN"),
         durationMethod = "0-3-5min",
         durationinterval = case_when(Time_Interval == "0-3" ~"0-3min", 
                                      Time_Interval == "3-5" ~"3-5min",
                                      is.na(Time_Interval) ~ "UNKNOWN"),
         isHeard = case_when(`V/C/S` %in% c("C", "S") ~ "Yes",
                             `V/C/S`== "V" ~ "No",
                            TRUE ~ "DNC"),
         isSeen = case_when(`V/C/S` == "V" ~ "Yes",
                            `V/C/S` %in% c("C", "S") ~ "No",
                            TRUE ~ "DNC"),
         missingindetections = NA,
         raw_distance_code = Distance,
         raw_duration_code = Time_Interval,
         #Behaviour
         originalBehaviourData = `V/C/S`,
         pc_vt = "DNC",
         pc_vt_detail ="DNC",
         age = "DNC",
         fm = Sex,
         group = "DNC",
         flyover = ifelse(is.na(Flyover), "DNC", Flyover),
         displaytype = "DNC",
         nestevidence = "DNC",
         behaviourother = "DNC") %>% 
  filter(!is.na(Species), !Species %in% c("M-TAHU", "M-URAM", "M-ODHE","no birds"))


# Test species using scientific name
species_tbl <- pc_survey %>%
  select(Species) %>%
  dplyr::distinct(Species) %>%
  left_join(bird_Tbl, by=c("Species"="Species_Code")) %>%
  left_join(WT_spTbl, by=c("Scientific_Name"="scientific_name"))

#list the ones that didn't pass
species_tbl[is.na(species_tbl$Scientific_Name),]

data_flat <- pc_survey %>% 
  left_join(species_tbl, by="Species")  %>%
  mutate(species = case_when(Species  == "B-BLGR" ~ "BLGR",
                             Species  == "B-MGVW" ~ "UNBI",
                             Species  == "B-YWAR" ~ "UNBI",
                             Species  == "B-BTYW" ~ "BTYW",
                             TRUE ~ species_code),
         scientificname = Scientific_Name,
         surveyDateTime = paste(visitDate, survey_time))

## CHECK
print(unique(data_flat$distanceMethod[!(data_flat$distanceMethod %in% WT_distMethTbl$distance_method_type)]))
print(unique(data_flat$durationMethod[!(data_flat$durationMethod %in% WT_durMethTbl$duration_method_type)]))
print(unique(data_flat$species[!(data_flat$species %in% WT_spTbl$species_code)]))
print(unique(data_flat$durationinterval[!(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))
print(unique(data_flat$distanceband[!(data_flat$distanceband %in% WT_distBandTbl$distance_band_type)]))

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

noflyover <- data_flat %>%
  filter(!flyover =="Yes")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- noflyover[!duplicated(noflyover[,WTvisit]), WTvisit] # 

write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), row.names = FALSE, na = "")
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 

#---SURVEY
survey_tbl <- noflyover %>%
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




