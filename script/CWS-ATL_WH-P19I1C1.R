# Title: "Translate Newfounland Wildsape historical data - Hydro Quebec Romaine River forest bird surveys"
# Source dataset is an excel spreadsheet
# Author: "Melina Houle"
# Date: "March 26, 2025"
# NOte on translation:
##    - 9 observations are the same bird seeing in another distance band /duration interval during the same survey.Information taken in Note with the mention "Même que".
##    - Birds acronyms aren't the same that what's WildTrax used. Need to fix using species scientific names. 
##    - Delete 466 observations that occur using playback. Those used period 5,6 and 7.
#----------------------------------------------
#update.packages()
library(dplyr) # mutate, %>%
library(readxl) #read_excel, read_xls
library(stringr) #str_replace_all
library(googledrive) #drive_get, drive_mkdir, drive_ls, drive_upload
library(googlesheets4)

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
WT_spTbl <- read.csv(file.path("./lookupTables/species_codes.csv"))

WT_durMethTbl <- read.csv(file.path("./lookupTables/duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path("./lookupTables/distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path("./lookupTables/duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path("./lookupTables/distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

organization <- "CWS-ATL"
dataset <- "Hydro Quebec Romaine River forest bird surveys"
dataset_code <- "WH_P19I1C1"
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
    filter(dataset_code =="WH_P19I1C1") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  data_db <- gd.list %>%
    filter(name =="P19 I1 C1 Hydro Quebec Romaine River forest bird surveys.xls") %>%
    select("id")
  drive_download(as_id(as.character(data_db)), path = file.path(project_dir, "P19 I1 C1 Hydro Quebec Romaine River forest bird surveys.xls"))
}

data <- file.path(project_dir, "P19 I1 C1 Hydro Quebec Romaine River forest bird surveys.xls")
#--------------------------------------------------------------
#       LOAD
#--------------------------------------------------------------
raw_visit <- read_xls(data, sheet = "Site Survey General", skip=3)
names(raw_visit)<-str_replace_all(names(raw_visit), c(" " = "_"))

raw_survey <- read_xls(data, sheet = "Site Survey Obs", skip=3)
names(raw_survey)<-str_replace_all(names(raw_survey), c(" " = "_"))

lu_species <-  read_xls(data, sheet = "SpeciesTable")
names(lu_species)<-str_replace_all(names(lu_species), c(" " = "_"))
#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### Visit/Location TABLE ####
############################
s_visit <- raw_visit %>%
  dplyr::select(Site_ID, Station_ID, `Date_________(yyyy-mm-dd)`, Observer, `Time_Start_____hh:mm`, `Latitude_(DD)`, `Longitude_(DD)`)  %>%
  dplyr::rename(Date = `Date_________(yyyy-mm-dd)`,
                starttime = `Time_Start_____hh:mm`,
                latitude = `Latitude_(DD)`,
                longitude = `Longitude_(DD)`) %>%
  dplyr::mutate(location = paste(dataset_code, Site_ID, Station_ID, sep= ":"),
         visitDate = ifelse(is.na(Date), "1900-01-01", as.character(Date)),
         missingvisit = NA,
         rawObserver = Observer,
         observer = Observer,
         survey_time = starttime,
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
         utmZone = NA,
         time_zone = NA,       
         data_origin = NA,
         missinginvisit = NA,
         survey_year = substr(Date, 1, 4),
         missinginlocations = NA)

    ####### CHECK MAPPING 
    #canada <- st_read("E:/MelinaStuff/BAM/GIS_layer/CanadaLAEA.shp")
    #bnd <- st_transform(canada, crs= st_crs(4269)) %>% filter(NAME == "Newfoundland and Labrador / Terre-Neuve-et-Labrador")
    #plot(bnd$geometry)
    #xy <- s_visit[,c("longitude", "latitude")]
    #spdf <- SpatialPointsDataFrame(coords = xy, data = s_visit,
    #                              proj4string = CRS("+proj=longlat +ellps=GRS80 +towgs84=0.0,0.0,0.0,0.0,0.0,0.0,0.0 +no_defs"))
    #pc <- st_as_sf(spdf, coords = c("longitude", "latitude"), crs = UTM)
    #plot(pc$geometry, col = "red", add= TRUE)


################################
#### Update master_observer ####
################################
unique_observers <- s_visit %>%
  select(Observer) %>% 
  distinct() %>%
  filter(!is.na(Observer)) %>% # Exclude rows where Observer is NA
  mutate(
    observer_name = Observer,
    observer_id = Observer
  )

# Create the append_obs data frame
append_obs <- unique_observers %>%
  select(observer_id, observer_name) %>%
  mutate(
    organization = "CWS-ATL",
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
##unique(raw_survey$Note)

# Only keep first obsevration of a bird.
raw_survey_filter <- raw_survey %>% 
  dplyr::filter(!grepl("même que", Note, ignore.case = TRUE))
  
pc_survey <- raw_survey %>% 
  dplyr::filter (Period < 5) %>% #delete playback period (5-6-7)
  dplyr::select(-Sample_ID, -Visit_ID, -Species_Group, -`Semi-circle`, -No._Young) %>%
  dplyr::rename(site = Site_ID, 
                station = Station_ID,
                Date = `Date_________(yyyy-mm-dd)`,
                site = Site_ID,
                ind_count = Count) %>%
  mutate(organization = organization,
         project = dataset,
         original_species = Species,
         visitDate = ifelse(is.na(Date), "1900-01-01", as.character(Date)),
         location = paste(dataset_code, site, station, sep= ":"),
         distanceMethod = "0m-50m-INF",
         distanceband = case_when(`DRL/IPA` == "DRL" ~ "0m-50m",
                                   `DRL/IPA` == "IPA" ~ "50m-INF",
                                  TRUE ~ "UNKNOWN"),
         durationMethod = "0-20min",
         durationinterval = case_when(Period == 1  ~ "0-5min",
                                      Period == 2 ~ "5-10min",
                                      Period == 3 ~ "10-15min",
                                      Period == 4 ~ "15-20min",
                                      TRUE ~ "UNKNOWN"),
         isHeard = case_when(Behaviour == "CHA"  ~ "Yes",
                             Behaviour == "CRI" ~ "Yes",
                             Behaviour == "DEF" ~ "Yes",
                             Behaviour == "TAM"~ "Yes",
                             TRUE ~ "DNC"),
         isSeen = case_when(Behaviour == "ALI"  ~ "Yes",
                            Behaviour == "PAR" ~ "Yes",
                            Behaviour == "PER" ~ "Yes",
                            Behaviour == "QUE"~ "Yes",
                            Behaviour == "SUR" ~ "Yes",
                            Behaviour == "TRA" ~ "Yes",
                            Behaviour == "VOL"~ "Yes",
                            Note == grepl("^Vu", Note)~ "Yes",
                            TRUE ~ "DNC"),
         missingindetections = NA,
         raw_distance_code = `DRL/IPA`,
         raw_duration_code = Period,
         #Behaviour
         originalBehaviourData = Behaviour,
         pc_vt = case_when(Behaviour == "CHA"  ~ "Song",
                           Behaviour == "CRI"  ~ "Call",
                           Behaviour == "DEF"  ~ "Fight call",
                           Behaviour == "PAR"  ~ "Song",
                           Behaviour == "QUE"  ~ "Fight call",
                           Behaviour == "TAM"  ~ "Non vocal",
                           TRUE ~ "DNC"),
         pc_vt_detail =case_when(Behaviour == "PAR"  ~ "Courtship",
                                Behaviour == "TAM"  ~ "Drumming",
                                TRUE ~ "DNC"),
         age = case_when(grepl("juvénile", Note, ignore.case = TRUE)  ~ "Juvenile",
                         grepl("adulte", Note, ignore.case = TRUE)~ "Adult",
                         TRUE ~ "DNC"),
         fm = case_when(`No._females` > 0  & `No._Males` > 0  ~ "Male/Female",
                        `No._females` > 0  & `No._Males`== 0  ~ "Female",
                        `No._females` == 0  & `No._Males` > 0  ~ "Male",
                        TRUE ~ "DNC"),
         group = case_when(Atlas_Code == "P"  ~ "Pair",
                           Atlas_Code == "C"  ~ "Pair",
                           Note == "Plus de 2" ~ "Group",
                           TRUE ~ "DNC"),
         flyover = "No",
         displaytype = case_when(Behaviour == "PAR"  ~ "Courtship",
                                 TRUE ~ "DNC"),
         nestevidence = "DNC",
         behaviourother = `Atlas_Code`) %>% 
  filter(!is.na(original_species)) 


# Test species using scientific name
species_compatible <- lu_species %>%
  filter(TaxonGroup == "Birds",
         SpeciesID  %in% raw_survey$Species) %>%
  left_join(WT_spTbl, by=c("Scientific_Name"="scientific_name")) %>%
  filter(is.na(species_code)) 

# Fix species using scientific name
lu_species_fix <- lu_species %>%
  filter(TaxonGroup == "Birds",
         SpeciesID  %in% raw_survey$Species,
         SpeciesName != "Doubled-crested Cormorant",
         SpeciesName != "Three-toed Woodpecker") %>%
  mutate(Scientific_Name = case_when(Scientific_Name == "Carduelis tristis" ~ "Spinus tristis",
                                     Scientific_Name == "Dendroica castanea" ~ "Setophaga castanea",
                                     Scientific_Name == "Ceryle alcyon" ~ "Megaceryle alcyon",
                                     Scientific_Name == "Dendroica striata" ~ "Setophaga striata",
                                     Scientific_Name == "Dendroica virens" ~ "Setophaga virens",
                                     Scientific_Name == "Dendroica tigrina" ~ "Setophaga tigrina",
                                     Scientific_Name == "Carduelis flammea" ~ "Acanthis flammea",
                                     Scientific_Name == "Picoides pubescens" ~ "Dryobates pubescens",
                                     Scientific_Name == "Picoides villosus" ~ "Dryobates villosus",
                                     Scientific_Name == "Dendroica magnolia" ~ "Setophaga magnolia",
                                     Scientific_Name == "Oporornis philadelphia" ~ "Geothlypis philadelphia",
                                     Scientific_Name == "Vermivora ruficapilla" ~ "Leiothylpis ruficapilla",
                                     Scientific_Name == "Seiurus noveboracensis" ~ "Parkesia noveboracensis",
                                     Scientific_Name == "Vermivora celata" ~ "Leiothlypis celata",
                                     Scientific_Name == "Seiurus aurocapillus" ~ "Seiurus aurocapilla",
                                     Scientific_Name == "Dendroica palmarum" ~ "Setophaga palmarum",
                                     Scientific_Name == "Carduelis pinus" ~ "Spinus pinus",
                                     Scientific_Name == "Carpodacus purpureus" ~ "Haemorhous purpureus",
                                     Scientific_Name == "Regulus calendula" ~ "Corthylio calendula",
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
  left_join(lu_species_fix, by=c("original_species"="SpeciesID"))  %>%
  left_join(WT_spTbl, by=c("Scientific_Name"="scientific_name")) %>%
  mutate(species = case_when(Species  == "UNWO" ~ "UNWO", 
                             Species  == "UNWA" ~ "UNWA",
                             Species  == "BHVI" ~ "BHVI", 
                             TRUE ~ WT_spTbl$species_code[match(Scientific_Name, WT_spTbl$scientific_name)]),
         scientificname = Scientific_Name,
         surveyDateTime = paste(visitDate, starttime))

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
WTlocation <- c("location", "latitude", "longitude")

# Remove duplicated location
location_tbl <- data_flat[!duplicated(data_flat[,WTlocation]), WTlocation] 
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


