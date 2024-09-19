# ---
# title: "Translate NRRI data"
# author: Siu Chung WU, Diego
# date: "July 15, 2024"
# Note on translation:
# Original projection of data set: EPSG 32615
# 48 rows not joinable with species values "Unidentified non-passerine" 
# Line 242 i group their abundance value, into a new column 'total abundance', also delete the second duplicates -> but then the behavior of the 2nd duplicates would be deleted

library(googledrive)
library(tidyr)
library(readr)
library(chron)
library(readxl)
library(plyr) #rbind.fill
library(dplyr)
library(sf)
library(googlesheets4)

## URL
#url <- "https://drive.google.com/drive/u/2/folders/1xIHrlaJ7-gmtYw18iXjfWMkop1JtKJb4"


## Initialize variables
#wd <- "C:/Users/asito/Desktop/ModellingProject/#BAM/WildTrax_Integration"
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
organization = "NRRI"
dataset_code = "BWCAW"
setwd(file.path(wd))

WTpj_Tbl <- read_sheet("https://docs.google.com/spreadsheets/d/1fqifS_E5O_IpW1B-UG_xthr9hzY6FIek-nFjCrt1G0w", sheet = "project")
lu <- "./lookupTables"
WT_spTbl <- read.csv(file.path(lu, "species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path(lu, "duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path(lu, "distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path(lu, "duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path(lu, "distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

project <- file.path("./project", dataset_code)
if (!dir.exists(project)) {
  dir.create(project)
}
dataDir <- file.path("./out", dataset_code)   
if (!dir.exists(dataDir)) {
  dir.create(dataDir)
}


#--------------------------------------------------------------
#       LOAD
#--------------------------------------------------------------
if (length(list.files(project)) ==0) {
  pid <- WTpj_Tbl %>%
    filter(dataset_code =="BWCAW") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  data_id <- gd.list %>%
    filter(name =="BWCAW_Data_BAM_6_2020.xlsx") %>%
    select("id")
  drive_download(as_id(as.character(data_id)), path = file.path(project, "BWCAW_Data_BAM_6_2020.xlsx"))
}

data_tbl <- read_excel(file.path(project, "BWCAW_Data_BAM_6_2020.xlsx"))

alltbl <- file.path(project, "BWCAW_Data_BAM_6_2020.xlsx")
location <- read_excel(alltbl, sheet = 1)
survey <- read_excel(alltbl, sheet = 2)
observation <- read_excel(alltbl, sheet = 3)

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------

############################
#### LOCATION TABLE ####
############################
data_flat <- location
data_flat$organization <- organization
data_flat$project <- dataset_code
data_flat$site <- sub("([^\\.]+)\\..*", "\\1", data_flat$'Station Name')
data_flat$station <- sub("^[^\\.]*\\.(.*)$", "\\1", data_flat$`Station Name`)
data_flat$station <- gsub("\\.", "", data_flat$station)
data_flat$location <- paste(dataset_code, data_flat$site, data_flat$station, sep=":")

# Not exist in source data
data_flat$elevationMeters <- NA
data_flat$bufferRadiusMeters <- NA
data_flat$isHidden <- NA
data_flat$trueCoordinates <- NA
data_flat$comments <- NA
data_flat$internal_wildtrax_id <- NA
data_flat$internal_update_ts <- NA

# If exists in source data
data_flat$utmZone	<- '15N'
data_flat$easting	<- data_flat$'X Coord'
data_flat$northing	<- data_flat$'Y Coord'
data_flat$missinginlocations <- NA

# correct projection from EPSG 32615 to EPSG 4269, and save them back to the original dataframe (data_flat)
xy_sf <- st_as_sf(data_flat, coords = c("easting", "northing"))
xy_sf <- st_set_crs(xy_sf, 32615)
xy_sf_4269 <- st_transform(xy_sf, crs = 4269)
xy_4269 <- st_coordinates(xy_sf_4269) 
data_flat$longitude <- round(xy_4269[,1], 5)
data_flat$latitude <- round(xy_4269[,2], 5)

# Plot location on top of canada boundary using utils
# - DONE


# options(digits = 7)
# View(location_tbl)

############################
#### VISIT TABLE ####
############################
head(data_flat)
head(survey)
data_flat <- merge(data_flat, survey, by.x= 'Station Name', by.y= 'Sation Name')

#survey attributes
data_flat$visitDate <- as.character(as.Date(data_flat$Date, format = "%Y-%m-%d"))
data_flat$snowDepthMeters <- NA
data_flat$waterDepthMeters <- NA
data_flat$crew <- NA
data_flat$bait <- "None"
data_flat$accessMethod <- NA
data_flat$landFeatures <- NA
data_flat$comments <- NA
data_flat$wildtrax_internal_update_ts <- NA
data_flat$wildtrax_internal_lv_id <- NA
data_flat$time_zone <- NA
data_flat$data_origin <- dataset_code
data_flat$missinginvisit <- NA
data_flat$survey_year <- sub("\\-.*", "", data_flat$visitDate) 

data_flat$survey_time <- sub(".*\\s", "", data_flat$Time)
data_flat$surveyDateTime <- paste(data_flat$visitDate, data_flat$survey_time)

##Observer (internal ID number): unique(data_flat$Observer)
data_flat$observer <- case_when(data_flat$Observer == "ez"  ~ "obs01",
                                data_flat$Observer == "cl"  ~ "obs02",
                                data_flat$Observer == "jdb"  ~ "obs03")

data_flat$pkey_dt<- paste(data_flat$location, paste0(gsub("-", "", as.character(data_flat$visitDate)),"_", gsub(":", "", data_flat$survey_time)), data_flat$observer, sep=":")
head(data_flat$pkey_dt)


############################
#### SURVEY TABLE ####
############################
data_flat <- merge(data_flat, observation, by= c("Station Name", "Date", "Time"))
head(data_flat)
isTRUE(is.na(data_flat$Species_Abbreviation)) # 'Should be FALSE

# determine appropriate 'distanceMethod' by unique(data_flat$Distance_Observed)
data_flat$distanceMethod <- "0m-50m-100m-INF"
# determine appropriate 'distanceMethod' by unique(data_flat$Interval_Observed)
data_flat$durationMethod<- "0-2-3-4-5-6-7-8-9-10min"

data_flat <- data_flat %>%
  mutate(distanceband = case_when(Distance_Observed == "detected outside 100m circle"  ~ "100m-INF",
                                  Distance_Observed == "detected within 50 meters of central point"  ~ "0m-50m",
                                  Distance_Observed == "detected between 50-100 meters from central point"  ~ "50m-100m",
                                  Distance_Observed == "dectected between 50-100 meters from central point"  ~ "50m-100m",
                                  TRUE ~ "UNKNOWN"),
         durationinterval= case_when(Interval_Observed == "detected within 0-2 minutes of survey" ~ "0-2min",
                                     Interval_Observed == "2-3 minutes" ~ "2-3min",
                                     Interval_Observed == "3-4 minutes" ~ "3-4min",
                                     Interval_Observed == "4-5 minutes" ~ "4-5min",
                                     Interval_Observed == "5-6 minutes" ~ "5-6min",
                                     Interval_Observed == "6-7 minutes" ~ "6-7min",
                                     Interval_Observed == "7-8 minutes" ~ "7-8min",
                                     Interval_Observed == "8-9 minutes" ~ "8-9min",
                                     Interval_Observed == "9-10 minutes" ~ "9-10min",
                                     TRUE ~ "UNKNOWN")
         )

# Visual check on meaning of "Species_Abbreviation"
check_species <- merge(WT_spTbl, data_flat[,c("Species_Common", "Species_Abbreviation")], by.x="species_code", by.y = "Species_Abbreviation")
check_species[!duplicated(check_species),]
#-- ALL abbreviation has the same meaning
#Translate
data_flat$species <- WT_spTbl$species_code[match(data_flat$Species_Abbreviation, WT_spTbl$species_code)]

# extract abbreviation with no match
missABV <- unique(data_flat$Species_Abbreviation[!(data_flat$Species_Abbreviation %in% WT_spTbl$species_code)])
# "UPBD" "UWPR" "UWAR" "USPA" "UTHR" "UDUC" "UNPS" "UOWL" "UFLY" "UVIR" "WPWA" "URAP" "UBLB" "UGUL"

miss_species <- data_flat %>%
  filter(Species_Abbreviation %in% missABV) %>%
  distinct(Species_Common, Species_Abbreviation)
print(miss_species)

data_flat <- data_flat %>%
  mutate(species = case_when(Species_Abbreviation == "UPBD"  ~ "UNPA",
                             Species_Abbreviation == "UWPR"  ~ "UNWO",
                             Species_Abbreviation == "UWAR"  ~ "UNWA",
                             Species_Abbreviation == "USPA"  ~ "UNSP",
                             Species_Abbreviation == "UTHR"  ~ "UNTH",
                             Species_Abbreviation == "UDUC"  ~ "UNDU",
                             Species_Abbreviation == "UNPS"  ~ "UNBI",
                             Species_Abbreviation == "UOWL"  ~ "UNOW",
                             Species_Abbreviation == "UFLY"  ~ "UNFI",
                             Species_Abbreviation == "UVIR"  ~ "UNVI",
                             Species_Abbreviation == "WPWA"  ~ "PAWA",
                             Species_Abbreviation == "URAP"  ~ "URPT",
                             Species_Abbreviation == "UBLB"  ~ "UNBL",
                             Species_Abbreviation == "UGUL"  ~ "UNGU",
                             TRUE ~ species)
  )


# determine appropriate behaviour by unique(data_flat$Behaviour)
if(is.null(data_flat$isHeard)) {
  data_flat$isHeard <- rep("No", nrow(data_flat))
}
data_flat$isHeard <- ifelse(grepl(".*Singing.*", data_flat$Behaviour), "Yes", data_flat$isHeard)
data_flat$isHeard <- ifelse(grepl(".*Calling.*", data_flat$Behaviour), "Yes", data_flat$isHeard)
data_flat$isHeard <- ifelse(grepl(".*Drumming.*", data_flat$Behaviour), "Yes", data_flat$isHeard)
data_flat$isHeard <- ifelse(grepl(".*Flyover.*", data_flat$Behaviour), "DNC", data_flat$isHeard)
data_flat$isHeard <- ifelse(grepl(".*Simultaneous.*", data_flat$Behaviour), "Yes", data_flat$isHeard)
data_flat$isHeard <- ifelse(grepl(".*Observed.*", data_flat$Behaviour), "No", data_flat$isHeard)

# check result unique(paste(data_flat$Behaviour, data_flat$isHeard))

if(is.null(data_flat$isSeen)) {
  data_flat$isSeen <- rep("No", nrow(data_flat))
}
data_flat$isSeen <- ifelse(grepl(".*Observed.*", data_flat$Behaviour), "Yes", data_flat$isSeen)
data_flat$isSeen <- ifelse(grepl(".*Simultaneous.*", data_flat$Behaviour), "Yes", data_flat$isSeen)
data_flat$isSeen <- ifelse(grepl(".*Flyover.*", data_flat$Behaviour), "DNC", data_flat$isSeen)

# check result unique(paste(data_flat$Behaviour, data_flat$isSeen))


# determine appropriate species by unique(data_flat$Count), any(is.na(data_flat$Count))
data_flat$ind_count <-data_flat$Count
# any(is.na(data_flat$abundance))

#### Are they real duplicates
# create 'isDuplicate' column for signaling the duplicated column 
WTspecies <- c("surveyDateTime", "location", "species", "distanceband", "durationinterval")
duplicates <- duplicated(data_flat[WTspecies]) | duplicated(data_flat[WTspecies], fromLast = TRUE)
data_flat$isDuplicate <- duplicates
print(data_flat[data_flat$isDuplicate == TRUE, c("surveyDateTime", "location", "distanceband", "durationinterval", "Species_Abbreviation", "observer", "ind_count", "isDuplicate")])
print(nrow(data_flat[data_flat$isDuplicate == TRUE, ])) # 1725 duplicated record # View(data_flat)
#extract 1 to check
data_flat[data_flat$surveyDateTime =="2011-05-25 09:05:00" & data_flat$species=="WTSP"& data_flat$distanceband=="100m-INF" & data_flat$durationinterval=="0-2min",]
head(observation)
observation[observation$Species_Abbreviation=="WTSP"& 
              observation$Distance_Observed=="detected outside 100m circle" & 
              observation$Interval_Observed=="detected within 0-2 minutes of survey",]

# Validation 
print(unique(data_flat$species[!(data_flat$species %in% WT_spTbl$species_code)]))
print(unique(data_flat$durationinterval[!(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))
print(unique(data_flat$distanceband[!(data_flat$distanceband %in% WT_distBandTbl$distance_band_type)]))
print(unique(data_flat$durationinterval[!(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))

#---EXTENDED
data_flat$rawObserver <- data_flat$Observer
data_flat$original_species <- data_flat$Species_Common
data_flat$scientificname <- WT_spTbl$scientific_name[match(data_flat$species, WT_spTbl$species_code)]
data_flat$raw_distance_code <- data_flat$Distance_Observed
data_flat$raw_duration_code <- data_flat$Interval_Observed
data_flat$originalBehaviourData <- data_flat$Behaviour
data_flat$missingindetections <- "DNC"
data_flat$age <- "DNC"
data_flat$fm <- "DNC"
data_flat$group <- "DNC"
data_flat$flyover <- ifelse(data_flat$Behaviour == "Flyover", "Yes", "No") 
data_flat$displaytype <- "DNC"
data_flat$nestevidence <- "DNC"
data_flat$behaviourother <- "DNC"
data_flat$atlas_breeding_code <- "DNC"

data_flat <- data_flat %>%
  mutate(pc_vt = case_when(Behaviour =="Singing" ~ "Song",
                           Behaviour =="Calling" ~ "Song",
                           Behaviour =="Drumming" ~ "Non-Vocal",
                           Behaviour =="Observed" ~ "NA",
                           Behaviour =="Simultaneous" ~ "S-C",
                           TRUE ~ "DNC" ),
         pc_vt_detail= case_when(Behaviour =="Drumming" ~ "Drumming",
                                 TRUE ~ "NA" ))
  
#--------------------------------------------------------------
#
#       EXPORT
#
#--------------------------------------------------------------
dr<- drive_get(paste0("toUpload/",organization), shared_drive = "BAM_Core")
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

WTlocation <- c("location", "latitude", "longitude")

# Remove duplicated location
location_tbl <- no_flyover[!duplicated(no_flyover[,WTlocation]), WTlocation] 
write.csv(location_tbl, file= file.path(dataDir, paste0(dataset_code,"_location.csv")), row.names = FALSE, na = "")
location_out <- file.path(dataDir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 

#---VISIT
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- no_flyover[!duplicated(no_flyover[,WTvisit]), WTvisit] # 

write.csv(visit_tbl, file= file.path(dataDir, paste0(dataset_code,"_visit.csv")), row.names = FALSE, na = "")
visit_out <- file.path(dataDir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 

#---SURVEY
survey_tbl <- no_flyover %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband, durationinterval, isHeard, isSeen, comments) %>%
  dplyr::summarise(abundance = sum(ind_count), .groups= "keep")

WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen", "comments")
survey_tbl <- survey_tbl[!duplicated(survey_tbl[,WTsurvey]), WTsurvey] # 

write.csv(survey_tbl, file= file.path(dataDir, paste0(dataset_code,"_survey.csv")), row.names = FALSE, na = "")
survey_out <- file.path(dataDir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv"), overwrite = TRUE) 

#---EXTENDED
Extended <- c("organization", "project","location", "surveyDateTime", "species", "ind_count", "distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code", 
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
              "displaytype", "nestevidence", "behaviourother")
extended_tbl <- data_flat[!duplicated(data_flat[,Extended]), Extended] 
write.csv(extended_tbl, file.path(dataDir, paste0(dataset_code, "_behavior.csv")), quote = FALSE, row.names = FALSE, na = "")
extended_out <- file.path(dataDir, paste0(dataset_code,"_behavior.csv"))
drive_upload(media = extended_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_behavior.csv"), overwrite = TRUE) 


# ---PROCESSING STATS, no. of locations, visits, and surveys
file_name <- file.path(dataDir, paste0(dataset_code, "_stats.csv"))
con <- file(file_name, open = "a")
writeLines(paste0("Organization: ", organization), con)
writeLines(paste0("Project: ", dataset_code), con)
nrow_location <- paste0("Number of locations: ", nrow(location_tbl))
writeLines(nrow_location, con)
nrow_visit <- paste0("Number of visit: ", nrow(visit_tbl))
writeLines(nrow_visit, con)
nrow_survey <- paste0("Number of survey: ", nrow(survey_tbl))
writeLines(nrow_survey, con)
close(con)
