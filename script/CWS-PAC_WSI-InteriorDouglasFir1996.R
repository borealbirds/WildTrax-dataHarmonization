# Title: "WSI-InteriorDouglasFir"
# Source dataset is an excel spreadsheet
# Author: "Melina Houle"
# Date: "November 26, 2025"
## Note on translation:
#   - Original data uses XLSX format. Both 1996 and 1997 are avaiable. Data structure isn't the same and 1997 don't have time. 
#   - This script only translate data from 1996.
#   - One obs was made outside of the duration methodology and was deleted  (minute 17)
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
dataset <- "WSI-InteriorDouglasFir1996"
dataset_code <- "WSI-InteriorDouglasFir1996"
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
    filter(dataset_code =="WSI-InteriorDouglasFir1996") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))

  obs_db <- gd.list %>%
    filter(name =="655_WSI_655_DCT_SAMPLESTATIONSANDOBSERVATIONS.XLSX") %>%
    select("id")
  drive_download(as_id(as.character(obs_db)), path = file.path(project_dir, "655_WSI_655_DCT_SAMPLESTATIONSANDOBSERVATIONS.XLSX"))
}

survey_db <- file.path(project_dir, "655_WSI_655_DCT_SAMPLESTATIONSANDOBSERVATIONS.XLSX")

#--------------------------------------------------------------
#       LOAD
#--------------------------------------------------------------
location <- read_excel(survey_db, sheet = "SampleStations")
names(location)<-str_replace_all(names(location), c(" " = "_"))
location <- location %>%
  mutate(Study_Area_Name = gsub(" ", "_", Study_Area_Name), 
          location = paste0(dataset_code, ":", Study_Area_Name, ":", Sample_Station_Label))

survey <- read_excel(survey_db, sheet = "Survey Observations - 1996")
names(survey)<-str_replace_all(names(survey), c(" " = "_"))
survey$Study_Area_Name <- gsub(" ", "_", survey$Study_Area_Name)
#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------

# REPROJECT UTM 10N
spLocation_pj <- vect(location, geom=c("DC_Easting", "DC_Northing"), crs="epsg:3157")
spLocation_pj <- as.data.frame(project(spLocation_pj,"EPSG:4326"),  geom = "XY") # WILDTRAX

pc_location <- spLocation_pj %>%
  dplyr::rename(longitude = x,
                latitude = y) %>%
  mutate(organization = "CWS-PAC",
         location = paste0(dataset_code, ":", Sample_Station_Label),
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
survey <- survey %>%
  filter(Minutes <13) %>%
  mutate(location = paste0(dataset_code, ":", Sample_Station_Label)) %>%
  left_join(location %>% select(location, DC_Easting, DC_Northing), by = "location")
    
s_visit <- survey %>%
  dplyr::mutate(visitDate = ifelse(is.na(Date), "1900-01-01", as.character(as.Date(Date, format = "%B %d/%Y"))),
                missingvisit = NA,
                rawObserver = NA,
                observer = "obsNA",
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
                comments = Comments,
                easting = DC_Easting,
                northing = DC_Northing,
                utmZone = "10N",
                time_zone = NA,       
                data_origin = NA,
                missinginvisit = NA,
                survey_year = substr(Date, 1, 4),
                missinginlocations = NA)

################################
#### Update master_observer ####
################################
unique_observers <- tibble(
  observer_name = "NA",
  observer_id = "obsNA"
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
  dplyr::rename(site = Study_Area_Name , 
                station = Sample_Station_Label,
                ind_count = Count) %>%
  mutate(organization = organization,
         project = dataset,
         original_species = Species,
         distanceMethod = "0m-10m-20m-30m-40m-50m-60m-70m-80m-90m-100m-110m-120m-130m-140m-150m-200m-250m",
         distanceband = case_when(`Detect_Distance_(m)` >0 & `Detect_Distance_(m)` <=10~ "0m-10m",
                                  `Detect_Distance_(m)` >10 & `Detect_Distance_(m)` <=20~ "10m-20m",
                                  `Detect_Distance_(m)` >20 & `Detect_Distance_(m)` <=30~ "20m-30m",
                                  `Detect_Distance_(m)` >30 & `Detect_Distance_(m)` <=40~ "30m-40m",
                                  `Detect_Distance_(m)` >40 & `Detect_Distance_(m)` <=50~ "40m-50m",
                                  `Detect_Distance_(m)` >50 & `Detect_Distance_(m)` <=60~ "50m-60m",
                                  `Detect_Distance_(m)` >60 & `Detect_Distance_(m)` <=70~ "60m-70m",
                                  `Detect_Distance_(m)` >70 & `Detect_Distance_(m)` <=80~ "70m-80m",
                                  `Detect_Distance_(m)` >80 & `Detect_Distance_(m)` <=90~ "80m-90m",
                                  `Detect_Distance_(m)` >90 & `Detect_Distance_(m)` <=100~ "90m-100m",
                                  `Detect_Distance_(m)` >100 & `Detect_Distance_(m)` <=110~ "100m-110m",
                                  `Detect_Distance_(m)` >110 & `Detect_Distance_(m)` <=120~ "110m-120m",
                                  `Detect_Distance_(m)` >120 & `Detect_Distance_(m)` <=130~ "120m-130m",
                                  `Detect_Distance_(m)` >130 & `Detect_Distance_(m)` <=140~ "130m-140m",
                                  `Detect_Distance_(m)` >140 & `Detect_Distance_(m)` <=150~ "140m-150m",
                                  `Detect_Distance_(m)` ==200~ "150m-200m",
                                  `Detect_Distance_(m)` ==250~ "200m-250m",
                                  is.na(`Detect_Distance_(m)`)~ "UNKNOWN"),
         durationMethod = "0-1-2-3-4-5-6-7-8-9-10-11-12min",
         durationinterval = case_when(Minutes == 1 ~"0-1min", 
                                      Minutes == 2 ~"1-2min",
                                      Minutes == 3 ~"2-3min",
                                      Minutes == 4 ~"3-4min",
                                      Minutes == 5 ~"4-5min",
                                      Minutes == 6 ~"5-6min",
                                      Minutes == 7 ~"6-7min",
                                      Minutes == 8 ~"7-8min",
                                      Minutes == 9 ~"8-9min",
                                      Minutes == 10 ~"9-10min",
                                      Minutes == 11 ~"10-11min",
                                      Minutes == 12 ~"11-12min",
                                      is.na(Minutes) ~ "UNKNOWN"),
         isHeard = case_when(Detect_Type %in% c("CA", "SO", "DR") ~ "Yes",
                            Detect_Type == "VI" ~ "No",
                            TRUE ~ "DNC"),
         isSeen = case_when(Detect_Type == "VI" ~ "Yes",
                            Detect_Type %in% c("CA", "SO", "DR") ~ "No",
                            TRUE ~ "DNC"),
         missingindetections = NA,
         raw_distance_code = `Detect_Distance_(m)`,
         raw_duration_code = Minutes,
         #Behaviour
         originalBehaviourData = Detect_Type,
         pc_vt = "DNC",
         pc_vt_detail ="DNC",
         age = "DNC",
         fm = Sex,
         group = "DNC",
         flyover = "DNC",
         displaytype = "DNC",
         nestevidence = "DNC",
         behaviourother = "DNC") %>% 
  filter(!is.na(original_species))


# Test species using scientific name
species_tbl <- pc_survey %>%
  select(Species_old, Species) %>%
  dplyr::distinct(Species_old, Species) %>%
  left_join(WT_spTbl, by=c("Species_old"="species_code"))

#list the ones that didn't pass
species_tbl[is.na(species_tbl$scientific_name),]

data_flat <- pc_survey %>% 
  left_join(species_tbl, by="Species_old")  %>%
  mutate(species = case_when(Species_old  == "UNK" ~ "UNBI",
                             Species_old  == "CAGO" ~ "CANG",
                             Species_old  == "RCKJ" ~ "RCKI",
                             Species_old  == "EUGR" ~ "EVGR",
                             Species_old  == "YWWA" ~ "YRWA",
                             Species_old  == "RNBU" ~ "UNBI",
                             Species_old  == "SONI" ~ "UNBI",
                             Species_old  == "TONA" ~ "UNBI",
                             Species_old  == "YRBL" ~ "UNBI",
                             Species_old  == "RAFL" ~ "UNBI",
                             Species_old  == "CALO" ~ "COLO",
                             Species_old  == "URWA" ~ "UNBI",
                             Species_old  == "OSFI" ~ "OSFL",
                             Species_old  == "HAWA" ~ "UNBI",
                             Species_old  == "WWPE" ~ "WEWP",
                             Species_old  == "RHCO" ~ "UNBI",
                             Species_old  == "WAVE" ~ "UNBI",
                             Species_old  == "RWBB" ~ "RWBL",
                             Species_old  == "MGTWA" ~ "MGWA",
                             Species_old  == "MALLARD" ~ "MALL",
                             Species_old  == "DEFL" ~ "UNBI",
                             Species_old  == "RUMU" ~ "UNBI",
                             Species_old  == "UNK HU" ~ "UNHU",
                             Species_old  == "RCBL" ~ "UNBI",
                             TRUE ~ Species_old),
         scientificname = scientific_name,
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




