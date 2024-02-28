# ---
# title: "Translate GEM data from MNRF (Rob Rempel)"
# author: "Melina Houle"
# date: "31 Jan, 2024"
# Note on translation:
# -- The project uses the same plotID associated with different XY coordinates and the same coordinates 
# -- with differents plotID. WildTrax do not allow to have duplicates in location name neither its allows duplicates on 
# -- X, Y coordinates using different name. 
# -- We had to resolve the location name in order to make sure uniqueness. source plotID and IDENt2 were
# --  kept in the comments attributes to allow the user to retrieve the original value. 

library(tidyverse)
library(readxl)
library(sf)
library(googledrive)
library(plyr)
library(googlesheets4)
library(dplyr)


############################
## IMPORT ####
############################
## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

WTpj_Tbl <- read_sheet("https://docs.google.com/spreadsheets/d/1fqifS_E5O_IpW1B-UG_xthr9hzY6FIek-nFjCrt1G0w", sheet = "project")
WT_spTbl <- read.csv(file.path("./lookupTables/species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path("./lookupTables/duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path("./lookupTables/distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path("./lookupTables/duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path("./lookupTables/distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

organization = "GOV-ON"
dataset_code = "ONGEM"
project_name = "Forest Management Guideline Effectiveness Monitoring Program"
source_file <- "exportDataForBAM.xlsx"

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
data_db <- file.path(project_dir, source_file)
if (!file.exists(data_db)) {
  #Download from GoogleDrive
  drive_download("sourceData/exportDataForBAM.xlsx", path = file.path(project_dir, source_file))
}
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

#--------------------------------------------------------------
#       LOAD
#--------------------------------------------------------------
if (length(list.files(project_dir)) ==0) {
  pid <- WTpj_Tbl %>%
    filter(dataset_code =="ONGEM") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  detection_id <- gd.list %>%
    filter(name =="exportDataForBAM.xlsx") %>%
    select("id")
  drive_download(as_id(as.character(detection_id)), path = file.path(project_dir, "exportDataForBAM.xlsx"))
}

s_data <- read_xlsx(file.path(project_dir, "exportDataForBAM.xlsx"), sheet = "Sheet 1")
#--------------------------------------------------------------

#--------------------------------------------------------------
#       DATA PREP TO CREATE LOCATION NAME
#--------------------------------------------------------------
loc.unique <- s_data[!duplicated(s_data[,c("LATITUDE", "LONGITUDE", "plotID")]),]
loc.freqXY <- ddply(loc.unique, .(loc.unique$LATITUDE, loc.unique$LONGITUDE), nrow)
names(loc.freqXY) <- c("LATITUDE", "LONGITUDE", "FreqXY")
loc.freqPlot <- ddply(loc.unique, .(loc.unique$plotID), nrow)
names(loc.freqPlot) <- c("plotID", "FreqPlotID")
location <- merge(s_data, loc.freqXY, by=c("LATITUDE", "LONGITUDE"), all.x = TRUE)
location <- merge(location, loc.freqPlot, by="plotID", all.x = TRUE)

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
#Read in template
locations <- location  %>%
  dplyr::mutate(organization = organization,
                project = project_name,
                location = case_when(FreqXY ==1 & FreqPlotID ==1 ~ paste("ONGEM", plotID, sep = ":"), 
                                     FreqXY >1 & grepl(".*[A-Za-z]$", plotID) ~ paste("ONGEM", substr(plotID, 1, nchar(plotID)-1), sep = ":"),
                                     FreqXY >1 & !grepl(".*[A-Za-z]$", plotID) ~ paste("ONGEM", IDENT2, sep = ":"),
                                     FreqPlotID >1 ~ paste("ONGEM", IDENT2, sep=":")),
                northing = NA,
                easting = NA,
                elevationMeters = NA,
                bufferRadiusMeters = NA,
                isHidden = NA,
                trueCoordinates = NA,
                internal_wildtrax_id = NA,
                internal_update_ts = NA,
                site = NA,
                station = NA, 
                utmZone = NA, 
                missinginlocations = NA) %>%
  dplyr::rename(latitude = LATITUDE,
                longitude = LONGITUDE) 


############################
#### VISIT TABLE ####
############################
visit_flat <- locations %>% 
  mutate(visitDate = str_sub(Date, 1, 10),
         snowDepthMeters = NA,  #derived from location at upload
         waterDepthMeters = NA,  #derived from location at upload
         landFeatures = NA,  #ARUs attributes
         crew = NA,   #ARUs attributes
         bait = "None",
         accessMethod = NA,  #ARUs attributes
         comments = paste0("original plotID: ", plotID),
         wildtrax_internal_update_ts = NA,  #created during the upload
         wildtrax_internal_lv_id = NA, #created during the upload
         time_zone = NA,
         data_origin = NA,
         missinginvisit = NA,
         survey_year = Year,
         ## observer, observer_raw
         observer = "NA",
         rawObserver = "NA",
         # survey_time, surveyDateTime
         survey_time = ifelse(is.na(TimeStr), "00:00:01", as.character(TimeStr, format = "%H:%M:%S")),
         surveyDateTime = paste(as.character(visitDate), survey_time),
         ## pkey_dt -- Concatenate, separated by colons: [location]:[visitDate]_[survey_time]:[localobservercode]; can also use raw observer or observer ID if needed
         pkey_dt = paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), observer, sep=":")
         )

############################
#### SURVEY TABLE ####
############################

survey <- visit_flat %>% 
  mutate(distanceMethod = "0m-INF",
         durationMethod = "0-10min",
         ## Species, species_old, comments, scientificname
         original_species = Species,
         species = Species,
         scientificname = WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)],
         isHeard = "Yes",
         isSeen = "No",
         count = MaxCount,
         distanceband = "0m-INF",
         durationinterval = "0-10min",
         raw_distance_code = MAXDIS,
         raw_duration_code = MAXDUR,
         missingindetections = NA,
         originalBehaviourData = NA,
         pc_vt = "DNC",
         pc_vt_detail = "DNC",
         age = "DNC",
         fm =  "DNC",
         group =  "DNC",
         flyover = "DNC",
         displaytype = "DNC",
         nestevidence = "DNC",
         behaviourother = "DNC",
         comments = paste0("Source plotID=", plotID, " & source IDENT2=", IDENT2))

# CHECK
print(unique(survey$distanceMethod[!(survey$distanceMethod %in% WT_distMethTbl$distance_method_type)]))
print(unique(survey$durationMethod[!(survey$durationMethod %in% WT_durMethTbl$duration_method_type)]))
print(unique(survey$species[!(survey$species %in% WT_spTbl$species_code)]))
print(unique(survey$distanceband[!(survey$distanceband %in% WT_distBandTbl$distance_band_type)]))
print(unique(survey$durationinterval[!(survey$durationinterval %in% WT_durBandTbl$duration_interval_type)]))
#print(unique(survey$comments))
print(survey[is.na(survey$latitude),])
print(survey[is.na(survey$visitDate),])
print(survey[is.na(survey$survey_time),])
print(survey[is.na(survey$count),])
print(survey[is.na(survey$species),])


############################
## EXPORT ####
############################
#Extract GoogleDrive id to store output
dr<- drive_get(paste0("toUpload/",organization), shared_drive = "BAM_Core")

if (nrow(drive_ls(as_id(dr), pattern = dataset_code)) == 0){
  dr_dataset_code <-drive_mkdir(dataset_code, path = as_id(dr), overwrite = NA)
} else {
  dr_dataset_code <- drive_ls(as_id(dr), pattern = dataset_code)
}

#---LOCATION
WTlocation <- c("location", "latitude", "longitude")

# Remove duplicated location
location_tbl <- locations[!duplicated(locations[,WTlocation]), WTlocation] 
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), row.names = FALSE, na = "")
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 


#---VISIT
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- visit_flat[!duplicated(visit_flat[,WTvisit]), WTvisit] 
write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), quote = FALSE, row.names = FALSE, na = "")
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 


#---SURVEY
survey_tbl <- survey %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband,
           durationinterval, isHeard, isSeen, comments) %>% 
  dplyr::summarise(abundance = sum(count), .groups= "keep")
WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen", "comments")
survey_tbl <- survey_tbl[!duplicated(survey_tbl[,WTsurvey]), WTsurvey] 
write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), quote = FALSE, row.names = FALSE, na = "")
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv")) 


#---EXTENDED
pc_extended <- survey %>% 
  dplyr::group_by(organization, project, location, surveyDateTime, species, count, distanceband, durationinterval, site, station, utmZone, easting, 
           northing, missinginlocations, time_zone, data_origin, missinginvisit, pkey_dt, survey_time,
           survey_year, rawObserver, original_species, scientificname, raw_distance_code, raw_duration_code, 
           originalBehaviourData, missingindetections, pc_vt, pc_vt_detail, age, fm, group, flyover, 
           displaytype, nestevidence, behaviourother, comments) %>% 
  dplyr::summarise(ind_count = sum(count), .groups= "keep")
Extended <- c("organization", "project", "location", "surveyDateTime", "species", "ind_count","distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code", 
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
              "displaytype", "nestevidence", "behaviourother","comments")
extended_tbl <- pc_extended[!duplicated(pc_extended[,Extended]), Extended] 
write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_extended.csv")), quote = FALSE, row.names = FALSE)
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



