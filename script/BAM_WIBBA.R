# ---
# title: "WIBBA Script"
# author: "Melina Houle"
# date created: "Feb 7, 2023"
# Note on translation:
# --- Source data are stored in a .xlsx format
# --- Metadata are found in sheet 2
# --- Tbl tblSurveyConditions INFO stores info on visit
#
# --- FIX DETAILS
# --- Fix species lookuptable. Survey data uses GRAJ while lookuptable provided uses CAJA
# ------3 occurences. Map speciescode = GRAJ to species = GRAJ. 

library(dplyr)
library(readxl)
library(googledrive)
library(readr)
library(googlesheets4)

## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

organization = "BAM"
dataset_code = "WIBBA"
project_name <- "Wisconsin Breeding Bird Atlas"
source_data <- "PointCountsMerged161718_09112020_noemails_withmetadatatab.xlsx"
species <- "IBP-AOS-LIST22.csv"
beh <- "breedingcode.xlsx"

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
lookup_dir <- file.path(project_dir, "lookupTable")
if (!dir.exists(lookup_dir)) {
  dir.create(lookup_dir)
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
    filter(dataset_code =="WIBBA") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  detection_id <- gd.list %>%
    filter(name =="PointCountsMerged161718_09112020_noemails_withmetadatatab.xlsx") %>%
    select("id")
  drive_download(as_id(as.character(detection_id)), path = file.path(project_dir, "PointCountsMerged161718_09112020_noemails_withmetadatatab.xlsx"))
  lookup_id <- gd.list %>%
    filter(name =="IBP-AOS-LIST22.csv") %>%
    select("id")
  drive_download(as_id(as.character(lookup_id)), path = file.path(project_dir, "IBP-AOS-LIST22.csv"), overwrite = TRUE)
  beh_id <- gd.list %>%
    filter(name =="breedingcode.xlsx") %>%
    select("id")
  drive_download(as_id(as.character(beh_id)), path = file.path(project_dir, "breedingcode.xlsx"), overwrite = TRUE)
}

#Connecte and load tables
#Read in template
data_flat <- read_xlsx(file.path(project_dir, "PointCountsMerged161718_09112020_noemails_withmetadatatab.xlsx"), sheet=1)
lu_species <-  read.csv(file.path(project_dir, "IBP-AOS-LIST22.csv"))
lu_beh <- read_xlsx(file.path(project_dir, "breedingcode.xlsx"), sheet=1)
#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
  ## Check if location have all x, y coordinates, Should be empty
  #survey_pc[is.na(survey_pc$longitude),]
  #survey_pc[is.na(survey_pc$latitude),]

data_flat <- data_flat %>% 
  mutate(organization = organization ,
         project = project_name,
         location = paste(dataset_code, pointid, sep= "_"),
         # If exists in source data
         site = NA,
         station = NA,
         utmZone = NA,
         easting = NA,
         northing = NA,
         missinginlocations = NA) %>% 
  dplyr::rename(visitDate = surveydate)

############################
#### VISIT TABLE ####
############################

pc_visit <- data_flat %>% 
  mutate(snowDepthMeters = NA, 
         waterDepthMeters = NA, 
         crew = NA, 
         bait = "NONE", 
         accessMethod = NA, 
         landFeatures = NA, 
         comments = NA,
         wildtrax_internal_update_ts = NA, 
         wildtrax_internal_lv_id= NA,
         time_zone = NA,
         data_origin = NA,
         missinginvisit = NA,
         survey_time = as.character(format(surveystarttime, format = "%H:%M:%S")),
         survey_year = substr(visitDate, 1, 4),
         observer = case_when(is.na(observerid) ~ "NA",
                              !is.na(observerid) ~ observerid),
         rawObserver = observerid,
         # survey_time, surveyDateTime
         surveyDateTime = paste(as.character(visitDate), survey_time),
         ## pkey_dt -- Concatenate, separated by colons: [location]:[visitDate]_[survey_time]:[localobservercode]; can also use raw observer or observer ID if needed
         pkey_dt = paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), observer, sep=":"),
         comments = NA)

############################
#### SURVEY TABLE ####
############################
lu_species <-  subset(lu_species, lu_species$SPEC %in% data_flat$speciescode)
lu_crosswalk <- merge(WT_spTbl, lu_species[,c("SPEC", "COMMONNAME")], by.x = "species_code", by.y ="SPEC" )
  #print(subset(lu_crosswalk, lu_crosswalk$species_common_name != lu_crosswalk$COMMONNAME))

survey_pc <- pc_visit %>% 
  mutate(original_species = speciescode,
         species = lu_crosswalk$species_code[match(speciescode, lu_crosswalk$species_code)],
        scientificname = WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)],
        isHeard = case_when(detectioncode == "S" ~ "Yes",
                            detectioncode == "V" ~ "No",
                            detectioncode == "C" ~ "Yes",
                            detectioncode == "J" ~ "DNC",
                            detectioncode == "F" ~ "DNC"),
        isSeen = case_when(detectioncode == "S" ~ "No",
                           detectioncode == "V" ~ "Yes",
                           detectioncode == "C" ~ "No",
                           detectioncode == "J" ~ "DNC",
                           detectioncode == "F" ~ "DNC"),
        distanceMethod = "0m-50m-100m-INF",
        distanceband = case_when(distance == "1" ~ "0m-50m",
                                     distance == "2" ~ "50m-100m",
                                     distance == "3" ~ "100m-INF",
                                 is.na(distance) ~ "UNKNOWN"),
        durationMethod = "0-1-2-3-4-5-6-7-8-9-10min",
        durationinterval = case_when(minute == "0" ~ "0-1min",
                                     minute ==  "1" ~ "1-2min",
                                     minute == "2" ~ "2-3min",
                                     minute == "3" ~ "3-4min",
                                     minute == "4" ~ "4-5min",
                                     minute == "5" ~ "5-6min",
                                     minute == "6" ~ "6-7min",
                                     minute == "7" ~ "7-8min",
                                     minute == "8" ~ "8-9min",
                                     minute == "9" ~ "9-10min",
                                     is.na(minute) ~ "UNKNOWN"),
        comments = noteso,
        missingindetections = NA,
        originalBehaviourData = breedingcode,
        raw_duration_code = minute,
        raw_distance_code = distance,
        pc_vt = lu_beh$pc_vt[match(breedingcode, lu_beh$breedingcode)],
        pc_vt_detail = lu_beh$pc_vt_detail[match(breedingcode, lu_beh$breedingcode)],
        age = lu_beh$age[match(breedingcode, lu_beh$breedingcode)],
        fm =  lu_beh$fm[match(breedingcode, lu_beh$breedingcode)],
        group =  lu_beh$group[match(breedingcode, lu_beh$breedingcode)],
        flyover = lu_beh$flyover[match(breedingcode, lu_beh$breedingcode)],
        displaytype = lu_beh$displaytype[match(breedingcode, lu_beh$breedingcode)],
        nestevidence = lu_beh$nestevidence[match(breedingcode, lu_beh$breedingcode)],
        behaviourother = "DNC"
  )


# CHECK
print(unique(survey_pc$distanceMethod[!(survey_pc$distanceMethod %in% WT_distMethTbl$distance_method_type)]))
print(unique(survey_pc$durationMethod[!(survey_pc$durationMethod %in% WT_durMethTbl$duration_method_type)]))
print(unique(survey_pc$species[!(survey_pc$species %in% WT_spTbl$species_code)]))
# Fix Gray Jay
survey_pc$species[survey_pc$speciescode == "GRAJ"] <-"GRAJ"
print(unique(survey_pc$species[!(survey_pc$species %in% WT_spTbl$species_code)]))

print(unique(survey_pc$distanceband[!(survey_pc$distanceband %in% WT_distBandTbl$distance_band_type)]))
print(unique(survey_pc$durationinterval[!(survey_pc$durationinterval %in% WT_durBandTbl$duration_interval_type)]))
print(survey_pc[is.na(survey_pc$latitude),])
print(survey_pc[is.na(survey_pc$visitDate),])
print(pc_visit[is.na(pc_visit$survey_time),])
print(survey_pc[is.na(survey_pc$count),])
print(survey_pc[is.na(survey_pc$species),])

#--------------------------------------------------------------
#
#       EXPORT
#
#--------------------------------------------------------------
#Extract GoogleDrive id to store output
dr<- drive_get(paste0("DataTransfered/",organization), shared_drive= "BAM_Core")

if (nrow(drive_ls(as_id(dr), pattern = dataset_code)) == 0){
  dr_dataset_code <-drive_mkdir(dataset_code, path = as_id(dr), overwrite = NA)
} else {
  dr_dataset_code <- drive_ls(as_id(dr), pattern = dataset_code)
}

#---SURVEY
survey_tbl <- survey_pc %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband,
           durationinterval, isHeard, isSeen) %>% 
  dplyr::summarise(abundance = sum(count), .groups= "keep")
WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen")
survey_tbl <- survey_tbl[!duplicated(survey_tbl[,WTsurvey]), WTsurvey] 


write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), quote = FALSE, row.names = FALSE, na = "")
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv")) 

#---VISIT (only where observation exists)
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- survey_pc[!duplicated(survey_pc[,WTvisit]), WTvisit] 
write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), quote = FALSE, row.names = FALSE, na = "")
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 

#---LOCATION
WTlocation <- c("location", "latitude", "longitude")
location_tbl <- survey_pc[!duplicated(survey_pc[,WTlocation]), WTlocation] # 

write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), quote = FALSE, row.names = FALSE)
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 

#---EXTENDED
extended_tbl <- survey_pc %>% 
  group_by(organization, project, location, surveyDateTime, species, distanceband, durationinterval, site, station, utmZone, easting, 
           northing, missinginlocations, time_zone, data_origin, missinginvisit, pkey_dt, survey_time,
           survey_year, rawObserver, original_species, scientificname, raw_distance_code, raw_duration_code, 
           originalBehaviourData, missingindetections, pc_vt, pc_vt_detail, age, fm, group, flyover, 
           displaytype, nestevidence, behaviourother, comments) %>% 
  dplyr::summarise(ind_count = sum(count), .groups= "keep")

Extended <- c("organization", "project", "location", "surveyDateTime", "species", "ind_count","distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code", 
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
              "displaytype", "nestevidence", "behaviourother", "comments")
write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_behavior.csv")), quote = FALSE, row.names = FALSE)
extended_out <- file.path(out_dir, paste0(dataset_code,"_behavior.csv"))
drive_upload(media = extended_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_behavior.csv"), overwrite = TRUE) 

#---PROCESSING STATS
write_lines(paste0("Organization: ", organization), file.path(out_dir, paste0(dataset_code, "_stats.csv")))
write_lines(paste0("Project: ", dataset_code), file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
nrow_location <- paste0("Number of locations: ", nrow(location_tbl))
write_lines(nrow_location, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
nrow_visit <- paste0("Number of visit: ", nrow(visit_tbl))
write_lines(nrow_visit, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
nrow_survey <- paste0("Number of survey: ", nrow(survey_tbl))
write_lines(nrow_survey, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)


