# Title: "WSI-NimpkishValleyRoadside"
# Source dataset is an excel spreadsheet
# Author: "Melina Houle"
# Date: "December 1, 2025"
## Note on translation:
#   - The inventory include 3 dataset (1a, 1b and 2). Only 1a and 2 will be translated.  1b uses Spot mapping protocol. 
#   - Detection_Distance_(m) is precise to the 5 m.The report classify Detection_Distance_(m) as in (0-75) and out(75-INF). I decided to classify the distance 0m-25m-50m-75m-INF
#   - Duration in the raw data says start_time + 5 minutes as end_time. This info isn't found in the WSI data. 
#   - Delete 93 PC with no obs
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
dataset <- "WSI-NimpkishValleyRoadside"
dataset_code <- "WSI-NimpkishValleyRoadside"
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
    filter(dataset_code =="WSI-NimpkishValleyRoadside") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))

  survey1a <- gd.list %>%
    filter(name =="wsi_4230_dct1a.xls") %>%
    select("id")
  drive_download(as_id(as.character(survey1a)), path = file.path(project_dir, "wsi_4230_dct1a.xls"))
  
  survey2 <- gd.list %>%
    filter(name =="wsi_4230_dct2.xls") %>%
    select("id")
  drive_download(as_id(as.character(survey2)), path = file.path(project_dir, "wsi_4230_dct2.xls"))
}

survey1a <- file.path(project_dir, "wsi_4230_dct1a.xls")
survey2 <- file.path(project_dir, "wsi_4230_dct2.xls")

#--------------------------------------------------------------
#       LOAD
#--------------------------------------------------------------
location1a <- read_excel(survey1a, sheet = "Block Information")

location1a <- location1a %>%
  filter(!if_all(everything(), is.na)) %>%
  rename_with(~ str_replace_all(., " ", "_")) %>%
  mutate(location = paste0(dataset_code, ":", str_to_upper(Design_Component_Label))) %>%
  filter(!is.na(DC_UTM_Easting))

location2 <- read_excel(survey2, sheet = "Block Information")

location2 <- location2 %>%
  filter(!if_all(everything(), is.na)) %>%
  rename_with(~ str_replace_all(., " ", "_")) %>%
  mutate(location = paste0(dataset_code, ":", str_to_upper(Design_Component_Label))) %>%
  filter(!is.na(DC_UTM_Easting))

location <- rbind(location1a, location2)
location <- location[!duplicated(location[,c("Design_Component_Label", "DC_UTM_Zone", "DC_UTM_Easting", "DC_UTM_Northing", "location")]), c("Design_Component_Label", "DC_UTM_Zone", "DC_UTM_Easting", "DC_UTM_Northing", "location")]

survey_1a <- read_excel(survey1a, sheet = "Location Behaviour and Sign")

survey_2 <- read_excel(survey2, sheet = "Location Behaviour and Sign")

survey <- rbind(survey_1a, survey_2)

survey <- survey %>%
  rename_with(~ str_replace_all(., " ", "_")) %>%
  dplyr::select(Design_Component_Label, Date, Time, Surveyor, Species, Count, Detection_Type, `Detection_Distance_(m)`, Life_Stage, Gender) %>%
  filter(Species != "NULL")

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------

# REPROJECT UTM 9N
spLocation_pj <- vect(location, geom=c("DC_UTM_Easting", "DC_UTM_Northing"), crs="epsg:3156")
spLocation_pj <- as.data.frame(project(spLocation_pj,"EPSG:4326"),  geom = "XY") # WILDTRAX

pc_location <- spLocation_pj %>%
  dplyr::rename(longitude = x,
                latitude = y) %>%
  mutate(organization = "CWS-PAC",
         location = sub(" ", "_", location),
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
  mutate(location = paste0(dataset_code, ":", str_to_upper(Design_Component_Label)),
         location = sub(" ", "_", location)) %>%
  inner_join(location %>% select(location, DC_UTM_Easting, DC_UTM_Northing), by = "location")
    
s_visit <- survey %>%
  dplyr::mutate(visitDate = ifelse(is.na(Date), "1900-01-01", as.character(as.Date(Date, format = "%Y %m %d"))),
                missingvisit = NA,
                rawObserver = Surveyor,
                observer = case_when(Surveyor == "Guillermo Perez"  ~ "GP",
                                     Surveyor == "Suzanne Beauchesne"  ~ "SB",
                                     Surveyor == "Michael G. Shepard"  ~ "MGS",
                                     Surveyor == "Paul Chytyk"  ~ "PC",
                                     Surveyor == "John Cooper"  ~ "JC",
                                     Surveyor == "Paul Chytyk, Chis Chutter"  ~ "PC-CC",
                                     Surveyor == "Chis Chutter, Paul Chytyk"  ~ "PC-CC",
                                     Surveyor == "Chris Chutter"  ~ "CC",
                                     Surveyor == "Chris Chutter, Paul Chytyk" ~ "PC-CC",
                                     Surveyor == "CMC, PC" ~ "PC-CMC",
                                     Surveyor == "PC, CMC" ~ "PC-CMC",
                                     TRUE ~ Surveyor),
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
                easting = DC_UTM_Easting,
                northing = DC_UTM_Northing,
                utmZone = "9N",
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
    observer_name = c("Michael G. Shepard","Paul Chytyk", "Guillermo Perez", "Suzanne Beauchesne", "John Cooper",
                      "Paul Chytyk, Chis Chutter","Chis Chutter, Paul Chytyk", "Chris Chutter", "Chris Chutter, Paul Chytyk",
                      "PC", "MGS", "PGL", "ETM", "CMC, PC", "GP", "SMB", "CMC", "PC, CMC", "JMC"),
    observer_id = c("MGS","PC", "GP", "SMB", "JMC", 
                    "PC-CC", "PC-CC", "CC", "PC-CC",
                    "PC", "MGS", "PGL", "ETM", "CMC-PC", "GP", "SMB", "CMC", "PC-CMC", "JMC")
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
  dplyr::rename(site = Design_Component_Label , 
                ind_count = Count) %>%
  mutate(organization = organization,
         project = dataset,
         station =  NA,
         ind_count = as.numeric(ind_count),
         original_species = Species,
         distanceMethod = "0m-25m-50m-75m-INF",
         distanceband = case_when(`Detection_Distance_(m)` %in% c("2","5","0","10", "20","12", "24","25", "15")~ "0m-25m",
                                  `Detection_Distance_(m)` %in% c("30", "50", "35","45", "40")~ "25m-50m",
                                  `Detection_Distance_(m)`%in% c("55", "60", "70", "65", "75") ~ "50m-75m",
                                  `Detection_Distance_(m)` %in% c("90","80", "100", ">100", "85", "150", "200", "250", "125", "130", "175",  "500", "300", "350", "120", "190", "140", "170", "160", "135", "110", "400", "95", "600", "180",  "450",  "225") ~ "75m-INF",
                                  `Detection_Distance_(m)`%in% c("ZZ", "<50", ">50", "zz") ~ "UNKNOWN",
                                  is.na(`Detection_Distance_(m)`)~ "UNKNOWN"),
         durationMethod = "0-5min",
         durationinterval = "0-5min",
         isHeard = case_when(Detection_Type %in% c("S","C","DR","C/V", "V, S","V/C",
                                                   "C, V","S, V", "V, C","tapping","DRUM","dr",
                                                   "c","S/V","V, C, F","S/C/V","C,V",      
                                                   "P, V", "C - V" , "C,D,V", "TP", "C,M", "S,C", "V,D", "C,S", "C,M,V",
                                                   "CA,D,V", "C,SW,V", "V,D,WN", "D,C", "C,V,D", "V,D,C", "D,C,V", "S,C,D,V", "C,WN",
                                                   "V/S", "S/C", "V/S/C" ) ~ "Yes",
                             Detection_Type %in% c("V") ~ "No",
                             Detection_Type %in% c("BZ","U", "BZ, V", "P, V","V,D", "WN", "H", "see notes", "P,V", "P,D,V", "D,V", "FO", "D", "V/BZ", "BUZZ") ~ "DNC",
                             is.na(Detection_Type)  ~ "DNC",
                            TRUE ~ "DNC"),
         isSeen = case_when(Detection_Type %in% c("S","C","DR","tapping","DRUM","dr", "c", "TP", "C,M", "S,C",  "C,S",  "D,C",  "C,WN", "S/C" ) ~ "No",
                            Detection_Type %in% c("V","BZ, V", "P, V","V,D","P,V", "P,D,V", "D,V","V/BZ", "C/V", "V, S","V/C",
                                                  "C, V","S, V", "V, C", "S/V","V, C, F","S/C/V","C,V", "P, V", "C - V" , "C,D,V", "C,M,V",      
                                                  "CA,D,V", "C,SW,V", "V,D,WN","V,D","C,V,D", "V,D,C", "D,C,V", "S,C,D,V","V/S","V/S/C" ) ~ "Yes",
                            Detection_Type %in% c("BZ","U", "WN", "H", "see notes", "FO", "D", "BUZZ") ~ "DNC",
                            is.na(Detection_Type)  ~ "DNC",
                            TRUE ~ "DNC"),
         missingindetections = NA,
         raw_distance_code = `Detection_Distance_(m)`,
         raw_duration_code = NA,
         #Behaviour
         originalBehaviourData = Detection_Type,
         pc_vt = "DNC",
         pc_vt_detail ="DNC",
         age = "DNC",
         fm = case_when(Gender == "U"~ "DNC",
                        Gender == "M"~ "Male",
                        Gender == "F"~ "Female",
                        Gender == "M,F"~ "Male, Female",
                        is.na(Gender) ~  "DNC"),
         group = "DNC",
         flyover = case_when(Detection_Type == "FO"~ "Yes",
                             TRUE ~ "No" ),
         displaytype = "DNC",
         nestevidence = "DNC",
         behaviourother = "DNC")


# Test species using scientific name
species_tbl <- pc_survey %>%
  select(Species) %>%
  dplyr::distinct(Species) %>%
  left_join(bird_Tbl, by=c("Species"="Species_Code")) %>%
  left_join(WT_spTbl, by=c("Scientific_Name"="scientific_name"))

#list the ones that didn't pass
species_tbl[is.na(species_tbl$species_code),]

data_flat <- pc_survey %>% 
  left_join(species_tbl, by="Species")  %>%
  mutate(species = case_when(Species  == "B-MGVW" ~ "UNBI",
                             Species  == "B-YWAR" ~ "YEWA",
                             Species  == "B-BLGR" ~ "BLGR",
                             Species  == "B-XXSW" ~ "UNSW",
                             Species  == "B-BTYW" ~ "BTYW",
                             Species  == "B-XXWO" ~ "UNWO",
                             Species  == "B-CONH" ~ "UNBI",
                             Species  == "B-XXTH" ~ "UNTH",
                             Species  == "B-WI" ~ "UNBI",
                             Species  == "B-NOCR" ~ "AMCR",
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




