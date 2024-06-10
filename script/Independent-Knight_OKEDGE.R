# ---
# title: "Translate Okanagan Edge Effects data"
# author: "Elly Knight"
# date: "July 14, 2022"
# Note on translation:
# --  Fix distance band > 100m as UNKNOWN to fit protocol
# --- Fix distance band > 250m as UNKNOWN to fit protocol

library(tidyverse)
library(readxl)
library(sf)
library(googledrive)
library(googlesheets4)

############################
## IMPORT ####
############################
## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

organization <- "Independent-Knight"
dataset_code <- "OKEDGE"

WTpj_Tbl <- read_sheet("https://docs.google.com/spreadsheets/d/1fqifS_E5O_IpW1B-UG_xthr9hzY6FIek-nFjCrt1G0w", sheet = "project")
lu <- "./lookupTables"
WT_spTbl <- read.csv(file.path("./lookupTables/species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path("./lookupTables/duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path("./lookupTables/distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path("./lookupTables/duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path("./lookupTables/distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

#set working folder
project_dir <- file.path(wd, "project", dataset_code)
if (!dir.exists(project_dir)) {
  dir.create(project_dir)
}
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

#--------------------------------------------------------------
#       LOAD
#--------------------------------------------------------------
dat_source <- "OK Database - PC data - Elly Knight.csv"
pc_source <- "OK Database - PC info - Elly Knight.csv"
plot_source <- "OK Database - plot info - Elly Knight.csv"
site_source <- "OK Database - site info - Elly Knight.csv"

if (length(list.files(project_dir)) ==0) {
  pid <- WTpj_Tbl %>%
    filter(dataset_code =="OKEDGE") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  dat <- gd.list %>%
    filter(name =="OK Database - PC data - Elly Knight.csv") %>%
    select("id")
  drive_download(as_id(as.character(dat)), path = file.path(project_dir, dat_source))
  pc <- gd.list %>%
    filter(name =="OK Database - PC info - Elly Knight.csv") %>%
    select("id")
  drive_download(as_id(as.character(pc)), path = file.path(project_dir, pc_source))
  plot <- gd.list %>%
    filter(name =="OK Database - plot info - Elly Knight.csv") %>%
    select("id")
  drive_download(as_id(as.character(plot)), path = file.path(project_dir, plot_source))
  site <- gd.list %>%
    filter(name =="OK Database - site info - Elly Knight.csv") %>%
    select("id")
  drive_download(as_id(as.character(site)), path = file.path(project_dir, site_source))
}

site <- read.csv(file.path(project_dir, site_source))
plot <- read.csv(file.path(project_dir, plot_source))
pc <- read.csv(file.path(project_dir, pc_source)) 
dat <- read.csv(file.path(project_dir, dat_source))

############################
#### LOCATION TABLE ####
############################

#Read in template
#location.temp <- read_xlsx("template/Template-WT-1Location.xlsx")

#Tidy raw data
location.ok <- left_join(plot, site)

#Reproject to lat long
location.nad83 <- location.ok %>% 
  st_as_sf(coords=c("UTM.E", "UTM.N"), crs=26911) %>% 
  st_transform(crs=4326) %>% 
  st_coordinates() %>% 
  data.frame() %>% 
  dplyr::rename(longitude = X, latitude = Y)

#Format
location.all <- location.ok %>% 
  cbind(location.nad83) %>% 
  mutate(location = paste0("OKEDGE:", str_sub(Name, 1, 3), ":", Plot),
         elevationMeters = NA,
         bufferRadiusMeters = NA,
         isHidden = FALSE,
         trueCoordinates = TRUE,
         comments = NA,
         internal_wildtrax_id = NA,
         internal_update_ts = NA)

############################
#### VISIT TABLE ####
############################

#Read in template
#visit.temp <- read_xlsx("template/Template-WT-2Visit.xlsx", sheet=1)[,-c(12:18)]

#Tidy raw data
visit.ok <- pc %>% 
  dplyr::filter(!is.na(Site),
                !is.na(Round))

#Format
visit.all <- visit.ok %>% 
  mutate(SiteChar = case_when(nchar(Site)==1 ~ paste0("00", Site),
                          nchar(Site)==2 ~ paste0("0", Site),
                          nchar(Site)==3 ~ paste0(Site)),
         location = paste0("OKEDGE:", SiteChar, ":", Plot),
         visitDate = Date, 
         snowDepthMeters = NA,
         waterDepthMeters = NA,
         crew = NA,
         bait = "NONE",
         accessMethod = NA,
         landFeatures = NA, 
         comments = Comments, 
         wildtrax_internal_update_ts = NA,
         wildtrax_internal_lv_id = NA)
  
############################
#### SURVEY TABLE ####
############################
#Check species codes
lu.sp <- read.csv("lookupTables/species_codes.csv")
survey.sp <- dplyr::filter(dat, !Species %in% lu.sp$species_code)
table(survey.sp$Species)

#Tidy raw data
survey.ok<- dat %>% 
  dplyr::filter(!is.na(Site)) %>% 
  dplyr::select(-ID, -Time) %>% 
  #rename(ObsTime = Time) %>% 
  full_join(visit.ok %>% 
              dplyr::select(-ID)) %>%
  mutate(abundance = case_when(Species=="BHCOx50?" ~ 50,
                               !is.na(Species)~ 1, 
                               is.na(Species) ~ 0)) %>% 
  mutate(Observer = ifelse(Observer=="eck", "ECK", Observer),
         species= case_when(Species=="CEWA" ~ "CEDW",
                            Species=="BHCOx50?" ~ "BHCO",
                            Species=="TRSW" ~ "TRES",
                            Species=="raptor" ~ "URPT",
                            Species %in% c("SPSP", "spsp", "CCSP?", "GRSP?") ~ "UNSP",
                            Species %in% c("DOSP", "HUSP", "") ~ "UNBI",
                            Species=="GRPA" ~ "GRAP",
                            Species=="ECDO" ~ "EUCD",
                            !is.na(Species) ~ Species,
                            is.na(Species) ~ "NONE"))

#Check species codes again
survey.sp <- dplyr::filter(survey.ok, !species %in% lu.sp$species_code)
table(survey.sp$species)

#Get and create lookup tables
lu.obs <- survey.ok %>% 
  dplyr::select(Observer) %>% 
  unique()  %>%
  dplyr::mutate(row = row_number(),
         observer = paste0("OKEDGE_0", row))

# Set >100m and >250m as UNKNOWN
lu.db <- survey.ok %>% 
  dplyr::select(Distance) %>% 
  unique() %>% 
  arrange(Distance) %>% 
  mutate(distanceband = c("UNKNOWN",
                          "0m-10m",
                          "500m-INF",
                          "UNKNOWN",
                          "500m-INF",
                          "UNKNOWN",
                          "500m-INF",
                          "500m-INF",
                          "0m-10m",
                          "10m-20m",
                          "100m-200m",
                          "20m-50m",
                          "200m-300m",
                          "200m-300m",
                          "300m-400m",
                          "400m-500m",
                          "50m-75m",
                          "500m-INF",
                          "75m-100m",
                          "UNKNOWN"))

#Format
survey.all <- survey.ok %>% 
  left_join(lu.obs) %>% 
  left_join(lu.db) %>% 
  dplyr::mutate(SiteChar = case_when(nchar(Site)==1 ~ paste0("00", Site),
                              nchar(Site)==2 ~ paste0("0", Site),
                              nchar(Site)==3 ~ paste0(Site)),
         location = paste0("OKEDGE:", SiteChar, ":", Plot),
         Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%S"),
         Time = as.character(format(Time, format = "%H:%M:%S")),
         Time = case_when(is.na(Time) ~ "00:00:01",
                          !is.na(Time)~ Time),
         #Time = as.character(str_sub(Time, -8, -1)),
         surveyDateTime = as.character(paste(Date, Time)),
         durationMethod = "0-10min",
         distanceMethod = "0m-10m-20m-50m-75m-100m-200m-300m-400m-500m-INF",
         durationinterval = "0-10min",
         isHeard = ifelse(Activity %in% c("Singing", "Calling", "chipping", "Defend", "Drumming", "Chick beg call", "singing", "Display", "drumming"), "Yes", "No"),
         isSeen = ifelse(!'Visual.' %in% c("", "`", NA), "Yes", "No")) %>% 
  dplyr::rename(comments = Comments)


############################
#### EXTENDED TABLE ####
############################
#Get and create lookup tables
lu.vt <- survey.all %>% 
  dplyr::select(Activity) %>% 
  unique() %>% 
  mutate(pc_vt = c("Song", "Call", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Call", "Call", "Non-vocal", "Call", "Song", "Song", "Non-vocal", NA),
         pc_vt_detail=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Drumming", NA, NA, "Display", "Drumming", NA))

lu.agefm <- survey.all %>% 
  dplyr::select(Sex) %>% 
  unique() %>% 
  mutate(age = c("Adult", NA, "Adult", "Adult", "Juvenile", "Fledgling", "Adult", NA, "Adult", NA),
         fm = c("Male", NA, "Female", "Female", "Unknown", "Unknown", "Unknown", "NA", "Unknown", NA)) 

#Format
extended.all <- survey.all %>% 
  left_join(location.all %>% 
              dplyr::select(-Distance, -comments)) %>% 
  left_join(lu.sp %>% 
              dplyr::rename(species = species_code)) %>% 
  left_join(lu.vt) %>% 
  left_join(lu.agefm) %>% 
  dplyr::rename(site = SiteChar,
         station = Plot,
         easting = UTM.E,
         northing = UTM.N,
         rawObserver = Observer,
         original_species = Species,
         scientificname = scientific_name,
         raw_distance_code = Distance,
         originalBehaviourData = Activity) %>% 
  mutate(organization = organization,
         project = dataset_code,
         missinginlocations = NA,
         time_zone = "MDT",
         utmZone = 11,
         data_origin = NA,
         missinginvisit = NA,
         pkey_dt = paste0(location,":", surveyDateTime, ":", observer),
         survey_year = as.numeric(str_sub(Date, 1, 4)),
         survey_time = str_sub(Time, 1, 5),
         raw_duration_code = NA,
         missingindetections = NA,
         group=NA,
         flyover = ifelse(originalBehaviourData=="Fly over", "Yes", "No"),
         displaytype = NA,
         nestevidence=NA,
         behaviourother = NA,
         atlas_breeding_code = NA)
         
############################
## EXPORT ####
############################
#Extract GoogleDrive id to store output
dr<- drive_get(paste0("DataTransfered/",organization), shared_drive= "BAM_Core")

if (nrow(drive_ls(as_id(dr), pattern = dataset_code)) == 0){
  dr_dataset_code <-drive_mkdir(dataset_code, path = as_id(dr), overwrite = NA)
} else {
  dr_dataset_code <- drive_ls(as_id(dr), pattern = dataset_code)
}
#Location
WTlocation <- c("location", "latitude", "longitude")

# Remove duplicated location
location_tbl <- location.all[!duplicated(location.all[,c("location")]), WTlocation] # 
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), row.names = FALSE, na = "")
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 


#Visit
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- visit.all[!duplicated(visit.all[,WTvisit]), WTvisit] 
write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), quote = FALSE, row.names = FALSE, na = "")
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 



#Survey
survey_tbl <- survey.all %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband,
           durationinterval, isHeard, isSeen, comments) %>% 
  dplyr::summarise(abundance = sum(abundance), .groups= "keep")
WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen")
survey_tbl <- survey_tbl[!duplicated(survey_tbl[,WTsurvey]), WTsurvey] 


write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), quote = FALSE, row.names = FALSE, na = "")
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv")) 

#Extend
extended.temp <- c("organization", "project", "location", "surveyDateTime", "species", "abundance","distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code", 
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
              "displaytype", "nestevidence", "behaviourother","comments")
extended.wt <- extended.all %>% 
  dplyr::select(all_of(extended.temp))
write.csv(extended.wt, file= file.path(out_dir, paste0(dataset_code,"_behavior.csv")), row.names = FALSE)
extended_out <- file.path(out_dir, paste0(dataset_code,"_behavior.csv"))
drive_upload(media = extended_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_behavior.csv")) 
