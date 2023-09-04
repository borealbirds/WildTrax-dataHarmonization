# Title: "Translate Newfounland Wildsape historical data - IRMR River Valley"
# Source dataset is an excel spreadsheet
# Author: "Melina Houle"
# Date: "February 28, 2023"
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

## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

WT_spTbl <- read.csv(file.path("./lookupTables/species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path("./lookupTables/duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path("./lookupTables/distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path("./lookupTables/duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path("./lookupTables/distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

organization <- "CWS-ATL"
dataset <- "Neal_Simon"
dataset_code <- "WH_P6L1C1"
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
data_db <- file.path(project_dir, "P6 I1 C1 Neal Simon.xls")
if (!file.exists(data_db)) {
  #Download from GoogleDrive
  drive_download("sourceData/P6 I1 C1 Neal Simon.xls", path = file.path(project_dir, "P6 I1 C1 Neal Simon.xls"))
}
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

#--------------------------------------------------------------
#       LOAD
#--------------------------------------------------------------
raw_location <- read_xls(data_db, sheet = "Sites", skip=3)
names(raw_location)<-str_replace_all(names(raw_location), c(" " = "_"))

raw_visit <- read_xls(data_db, sheet = "Site Survey General", skip=3)
names(raw_visit)<-str_replace_all(names(raw_visit), c(" " = "_"))

raw_survey <- read_xls(data_db, sheet = "Site Survey Obs", skip=3)
names(raw_survey)<-str_replace_all(names(raw_survey), c(" " = "_"))

lu_species <-  read_xls(data_db, sheet = "SpeciesTable")
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
  dplyr::rename(longitude = Longitude,
                latitude = Latitude,
                northing = Northing,
                easting = Easting,
                site = Site_ID) %>%
  mutate(location = paste(dataset_code, site, sep= ":"),
         missinginlocations = NA)

    ####### CHECK MAPPING 
    #canada <- st_read("E:/MelinaStuff/BAM/GIS_layer/CanadaLAEA.shp")
    #bnd <- st_transform(canada, crs= st_crs(4386))
    #plot(bnd$geometry)
    #xy <- pc_location[,c("longitude", "latitude")]
    #spdf <- SpatialPointsDataFrame(coords = xy, data = pc_location,
    #                              proj4string = CRS("+proj=longlat +ellps=GRS80 +towgs84=0.0,0.0,0.0,0.0,0.0,0.0,0.0 +no_defs"))
    #pc <- st_as_sf(spdf, coords = c("longitude", "latitude"), crs = UTM)
    #plot(pc$geometry, add= TRUE)

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

#---VISIT
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double point count on the same site, same day). 
visit_tbl <- s_visit[!duplicated(s_visit[,WTvisit]), ] # 

############################
#### SURVEY TABLE ####
############################
# Filter species lookupTable. Delete duplicated rows
lu_species <- lu_species %>%
  filter(TaxonGroup == "Birds",
         !SpeciesName == "Blue-headed Vireo",
         !SpeciesName == "Three-toed Woodpecker")

pc_survey <- raw_survey %>% 
  dplyr::rename(station = Station_ID,
                Date = `Date_________(yyyy-mm-dd)`,
                site = Site_ID,
                ind_count = Count) %>%
  mutate(organization = organization,
         project = dataset,
         original_species = Species ,
         visitDate = ifelse(is.na(Date), "1900-01-01", as.character(Date)),
         location = paste(dataset_code, site, sep= ":"),
         surveyDateTime = paste(visitDate, "00:00:01"),
         distanceMethod = "UNKNOWN",
         distanceband = "UNKNOWN",
         durationMethod = "UNKNOWN",
         durationinterval = "UNKNOWN",
         isHeard = "Yes",
         isSeen = "DNC",
         missingindetections = NA,
         raw_distance_code = NA,
         raw_duration_code = NA,
         #Behaviour
         originalBehaviourData = NA,
         pc_vt = NA,
         pc_vt_detail = NA,
         age = NA,
         fm = NA,
         group = NA,
         flyover = NA,
         displaytype = NA,
         nestevidence = NA,
         behaviourother = NA)

data_flat <- pc_survey %>% 
  left_join(visit_tbl, by=c("location", "visitDate")) %>%
  left_join(pc_location, by=c("location")) %>%
  left_join(lu_species, by=c("original_species"="SpeciesID"))  %>%
  left_join(WT_spTbl, by=c("Species"="species_code")) %>%
  mutate(species = WT_spTbl$species_code[match(Species, WT_spTbl$species_code)])

## Fix or fill missing species
# 2 species don't have common name that match: "MYWA", YSFL"
# 5 others are unfound: "SCJU" "TTWO" "UNRA" "YWAR" "CAGO"  
data_flat <- data_flat  %>%
  mutate(species = case_when(original_species == "MYWA" ~ "YRWA",
                             original_species == "YSFL" ~ "NOFL",
                             original_species == "SCJU" ~ "DEJU",
                             original_species == "YWAR" ~ "YEWA",
                             original_species == "TTWO" ~ "ATTW",
                             original_species == "UNRA" ~ "URPT",
                             original_species == "CAGO" ~ "CANG",
                             TRUE ~ species),                                      
         scientificname = WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)])

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
dr<- drive_get(paste0("toUpload/",organization))
#Set GoogleDrive id
if (nrow(drive_ls(as_id(dr), pattern = dataset_code)) == 0){
  dr_dataset_code <-drive_mkdir(dataset_code, path = as_id(dr), overwrite = NA)
} else {
  dr_dataset_code <- drive_ls(as_id(dr), pattern = dataset_code)
}
dr_ls <- drive_ls(as_id(dr), pattern = dataset_code)

#---LOCATION
WTlocation <- c("location", "latitude", "longitude")

# Remove duplicated location
location_tbl <- pc_location[!duplicated(pc_location[,WTlocation]), WTlocation] 
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), row.names = FALSE, na = "")
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 

#---VISIT
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
write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_extended.csv")), quote = FALSE, row.names = FALSE, na = "")
extended_out <- file.path(out_dir, paste0(dataset_code,"_extended.csv"))
drive_upload(media = extended_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_extended.csv"), overwrite = TRUE) 

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


