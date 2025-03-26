# Title: "Translate Newfounland Wildsape historical data - Goose Bay Remediation Project"
# Source dataset is an excel spreadsheet
# Author: "Melina Houle"
# Date: "March 24, 2025"
#  BUG: 29 visit don't have survey. We assume the site was visited but no obs. 
#update.packages()
library(dplyr) # mutate, %>%
#library(utils) #read.csv
library(readxl) #read_excel
library(stringr) #str_replace_all
library(sf) #st_crs, st_as_sf, st_transform, st_drop_geometry
library(purrr) #map
library(plyr) #rbind.fill
library(googledrive) #drive_get, drive_mkdir, drive_ls, drive_upload
library(sp)
library(sf)
library(reshape2) # melt
library(readr) #write_lines
library(googlesheets4)

source("./config.R")

## Initialize variables
setwd(file.path(wd))

drive_auth()
WTpj_Tbl <- read_sheet("https://docs.google.com/spreadsheets/d/1fqifS_E5O_IpW1B-UG_xthr9hzY6FIek-nFjCrt1G0w", sheet = "project")

WT_spTbl <- read.csv(file.path("./lookupTables/species_codes.csv"))

WT_durMethTbl <- read.csv(file.path("./lookupTables/duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path("./lookupTables/distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path("./lookupTables/duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path("./lookupTables/distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

organization <- "CWS-ATL"
dataset <- "Goose Bay Remediation Project Environmental Assessment Bird Inventory"
dataset_code <- "WH_P4I1C1"
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
    filter(dataset_code =="WH_P4I1C1") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  data_db <- gd.list %>%
    filter(name =="P4 I1 C1 Goose Bay Remediaiton survey.xlsm") %>%
    select("id")
  drive_download(as_id(as.character(data_db)), path = file.path(project_dir, "P4 I1 C1 Goose Bay Remediaiton survey.xlsm"))
}

data <- file.path(project_dir, "P4 I1 C1 Goose Bay Remediaiton survey.xlsm")
#--------------------------------------------------------------
#       LOAD
#--------------------------------------------------------------
raw_location <- read_xlsx(data, sheet = "Sites", skip=3)
names(raw_location)<-str_replace_all(names(raw_location), c(" " = "_"))

raw_visit <- read_xlsx(data, sheet = "Site Survey General", skip=3)
names(raw_visit)<-str_replace_all(names(raw_visit), c(" " = "_"))

raw_survey <- read_xlsx(data, sheet = "Site Survey Obs", skip=3)
names(raw_survey)<-str_replace_all(names(raw_survey), c(" " = "_"))

lu_species <-  read_xlsx(data, sheet = "SpeciesTable")
names(lu_species)<-str_replace_all(names(lu_species), c(" " = "_"))

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
#Format
pc_location <- raw_location %>% 
  select(Site_ID, Easting, Northing, Zone, Longitude, Latitude)  %>%
  dplyr::rename(northing = Northing,
                easting = Easting,
                site = Site_ID) %>%
  mutate(location = paste(dataset_code, site, sep= ":"),
         missinginlocations = NA)

# correct projection from EPSG:2961 to EPSG 4269, and save them back to the original dataframe (data_flat)
xy_sf <- st_as_sf(pc_location, coords = c("easting", "northing"))
xy_sf <- st_set_crs(xy_sf, 2961)
xy_sf_4269 <- st_transform(xy_sf, crs = 4269)
xy_4269 <- st_coordinates(xy_sf_4269) 
pc_location$longitude <- round(xy_4269[,1], 5)
pc_location$latitude <- round(xy_4269[,2], 5)

    ####### CHECK MAPPING 
    #canada <- st_read("E:/MelinaStuff/BAM/GIS_layer/CanadaLAEA.shp")
    #bnd <- st_transform(canada, crs= st_crs(4386)) %>% filter(NAME == "Newfoundland and Labrador / Terre-Neuve-et-Labrador")
    #plot(bnd$geometry)
    #xy <- pc_location[,c("longitude", "latitude")]
    #spdf <- SpatialPointsDataFrame(coords = xy, data = pc_location,
    #                              proj4string = CRS("+proj=longlat +ellps=GRS80 +towgs84=0.0,0.0,0.0,0.0,0.0,0.0,0.0 +no_defs"))
    #pc <- st_as_sf(spdf, coords = c("longitude", "latitude"), crs = UTM)
    #plot(pc$geometry, col = "red", add= TRUE)

############################
#### VISIT TABLE ####
############################
s_visit <- raw_visit %>%
  select(Site_ID, Station_ID, `Date_________(yyyy-mm-dd)`, Observer)  %>%
  dplyr::rename(site = Site_ID, 
                Date = `Date_________(yyyy-mm-dd)`) %>%
  mutate(location = paste(dataset_code, site, sep= ":"),
         visitDate = ifelse(is.na(Date), "1900-01-01", as.character(Date)),
         missingvisit = NA,
         rawObserver = NA,
         observer = "NA",
         #time = as.character(format(as.POSIXct(sprintf("%04.0f", Time), format='%H%M'), format = "%H:%M:%S")),
         survey_time = "00:00:01",
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
         utmZone = "utm20N",
         time_zone = NA,       
         data_origin = NA,
         missinginvisit = NA,
         survey_year = substr(Date, 1, 4))

############################
#### SURVEY TABLE ####
############################
pc_survey <- raw_survey %>% 
  dplyr::rename(station = Station_ID,
                Date = `Date_________(yyyy-mm-dd)`,
                site = Site_ID,
                ind_count = Count) %>%
  mutate(organization = organization,
         project = dataset,
         original_species = Species,
         visitDate = ifelse(is.na(Date), "1900-01-01", as.character(Date)),
         location = paste(dataset_code, site, sep= ":"),
         surveyDateTime = paste(visitDate, "00:00:01"),
         distanceMethod = "0m-INF",
         distanceband = "0m-INF",
         durationMethod = "0-10min",
         durationinterval = "0-10min",
         isHeard = case_when(Note == "Calling"  ~ "Yes",
                             Note == "Singing" ~ "Yes",
                             Note == "3 Singing, Family Group of 4 Feeding" ~ "Yes",
                             Note == "2 Singing, 1 Calling"~ "Yes",
                             Note == "Agitated"~ "Yes",
                             is.na(Note) ~ "DNC",
                             TRUE ~ "No"),
         isSeen = case_when(Note == "Calling"  ~ "No",
                            Note == "Singing" ~ "No",
                            Note == "2 Singing, 1 Calling" ~ "No",
                            is.na(Note) ~ "DNC",
                            TRUE ~ "Yes"),
         missingindetections = NA,
         raw_distance_code = NA,
         raw_duration_code = NA,
         #Behaviour
         originalBehaviourData = NA,
         pc_vt = case_when(Note == "Calling"  ~ "Call",
                           Note == "Singing" ~ "Sing",
                           Note =="2 Singing, 1 Calling" ~ "Call/Sing",
                           Note =="3 Singing, Family Group of 4 Feeding" ~ "Sing",
                           TRUE ~ NA),
         pc_vt_detail = NA,
         age = NA,
         fm = NA,
         group = case_when(Note == "Family Group Feeding"  ~ "Group",
                           Note == "3 Singing, Family Group of 4 Feeding" ~ "Group",
                           Note == "Pair" ~ "Pair",
                           TRUE ~ NA),
         flyover = ifelse(Note == "Flyover", "Yes", "No"),
         displaytype = NA,
         nestevidence = ifelse(Note == "On Nest", "Yes", "No"),
         behaviourother = case_when(Note == "Feeding"  ~ "Feeding",
                                    Note == "Carrying Food" ~ "Carrying Food",
                                    Note == "Agitated" ~ "Agitated",
                                    TRUE ~ NA)) %>% 
  filter(!is.na(original_species)) 


# Test species using scientific name
#species_compatible <- lu_species %>%
#  filter(TaxonGroup == "Birds",
#         SpeciesID  %in% raw_survey$Species) %>%
#  left_join(WT_spTbl, by=c("Scientific_Name"="scientific_name")) %>%
#  filter(is.na(species_code)) 

# Fix lu_species
lu_species_fix <- lu_species %>%
  filter(TaxonGroup == "Birds",
         SpeciesID  %in% raw_survey$Species,
         SpeciesName != "Doubled-crested Cormorant",
         SpeciesName != "Three-toed Woodpecker") %>%
  mutate(Scientific_Name = case_when(Scientific_Name == "Anas americana" ~ "Mareca americana", 
                                     Scientific_Name == "Dendroica castanea" ~ "Setophaga castanea",
                                     Scientific_Name == "Ceryle alcyon" ~ "Megaceryle alcyon",
                                     Scientific_Name == "Dendroica striata" ~ "Setophaga striata",
                                     Scientific_Name == "Dendroica virens" ~ "Setophaga virens",
                                     Scientific_Name == "Dendroica tigrina" ~ "Setophaga tigrina",
                                     Scientific_Name == "Carduelis flammea" ~ "Acanthis flammea",
                                     Scientific_Name == "Phalacrocorax auritus" ~ "Nannopterum auritus",
                                     Scientific_Name == "Picoides villosus" ~ "Dryobates villosus",
                                     Scientific_Name == "Dendroica magnolia" ~ "Setophaga magnolia",
                                     Scientific_Name == "Vermivora ruficapilla" ~ "Leiothylpis ruficapilla",
                                     Scientific_Name == "Parula americana" ~ "Setophaga americana",
                                     Scientific_Name == "Seiurus noveboracensis" ~ "Parkesia noveboracensis",
                                     Scientific_Name == "Vermivora celata" ~ "Leiothlypis celata",
                                     Scientific_Name == "Dendroica palmarum" ~ "Setophaga palmarum",
                                     Scientific_Name == "Carduelis pinus" ~ "Spinus pinus",
                                     Scientific_Name == "Carpodacus purpureus" ~ "Haemorhous purpureus",
                                     Scientific_Name == "Regulus calendula" ~ "Corthylio calendula",
                                     Scientific_Name == "Actitis macularia" ~ "Actitis macularius",
                                     Scientific_Name == "Falcipennis canadensis" ~ "Canachites canadensis",
                                     Scientific_Name == "Vermivora peregrina" ~ "Leiothlypis peregrina",
                                     Scientific_Name == "Wilsonia pusilla" ~ "Cardellina pusilla",
                                     Scientific_Name == "Dendroica petechia" ~ "Setophaga petechia",
                                     Scientific_Name == "Dendroica coronata coronata" ~ "Setophaga coronata",
                                     TRUE ~ Scientific_Name )) 

# Delete duplicate species in WT_spTbl -- CAJA/GRAJ and CORBRA/AMCR
#WT_spTbl <- WT_spTbl %>%
#  inner_join(lu_species_fix, by=c("scientific_name"="Scientific_Name"))
#dup <- WT_spTbl_fix[duplicated(WT_spTbl_fix[,"scientific_name"]),] 
WT_spTbl <- WT_spTbl %>%
  filter(species_code != "CORBRA",
         species_code != "CAJA")

data_flat <- pc_survey %>% 
  left_join(s_visit, by=c("location", "visitDate")) %>%
  left_join(pc_location, by=c("location")) %>%
  left_join(lu_species_fix, by=c("original_species"="SpeciesID"))  %>%
  left_join(WT_spTbl, by=c("Scientific_Name"="scientific_name")) %>%
  mutate(species = WT_spTbl$species_code[match(Scientific_Name, WT_spTbl$scientific_name)],
         scientificname = Scientific_Name)

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
dr<- drive_get("toUpload/", shared_drive = "BAM_Core")
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
no_flyover<- data_flat %>%
  filter(!flyover == "Yes")

#---LOCATION
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


