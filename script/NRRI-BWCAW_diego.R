# ---
# title: "Translate NRRI data"
# author: Siu Chung WU, Diego
# date: "July 15, 2024"
# Note on translation:
# Original projection of data set: EPSG 32615
# 48 rows not joinable with species values "Unidentified non-passerine" 
# Line 242 i group their abundance value, into a new column 'total abundance', also delete the second duplicates -> but then the behaviour of the 2nd duplicates would be deleted

library(googledrive)
library(tidyr)
library(readr)
library(chron)
library(readxl)
library(plyr) #rbind.fill
library(dplyr)
library(sf)


## URL
#url <- "https://drive.google.com/drive/u/2/folders/1xIHrlaJ7-gmtYw18iXjfWMkop1JtKJb4"


## Initialize variables
wd <- "C:/Users/asito/Desktop/ModellingProject/#BAM/WildTrax_Integration"
organization = "NRRI"
dataset_code = "NRRI-BWCAW"
setwd(file.path(wd))


lu <- "./lookupTables"
WT_spTbl <- read.csv(file.path(lu, "species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path(lu, "duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path(lu, "distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path(lu, "duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path(lu, "distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

project <- file.path("./project", dataset_code)
dataDir <- file.path(wd,"project",dataset_code,"data")   # where files would be downloaded
out_dir <- file.path(wd, "out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

XYtbl <- file.path(dataDir,"BWCAW_Data_BAM_6_2020.xlsx")
location <- read_excel(XYtbl, sheet = 1)
survey <- read_excel(XYtbl, sheet = 2)
observation <- read_excel(XYtbl, sheet = 3)


#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------

############################
#### LOCATION TABLE ####
############################
location <- read_excel(XYtbl, sheet = 1)

data_flat <- location

data_flat$organization <- organization
data_flat$project <- dataset_code
data_flat$site <- sub("([^\\.]+)\\..*", "\\1", data_flat$'Station Name')
data_flat$station <- sub("^[^\\.]*\\.(.*)$", "\\1", data_flat$`Station Name`)

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
data_flat$`longitude` <- round(xy_4269[,1], 5)
data_flat$`latitude` <- round(xy_4269[,2], 5)

#---LOCATION
data_flat$location <- paste(dataset_code, data_flat$site, data_flat$station, sep=":")
WTlocation <- c("location", "longitude", "latitude")
location_tbl <- data_flat[!duplicated(data_flat[,WTlocation]), WTlocation] 

# options(digits = 7)
# View(location_tbl)

############################
#### VISIT TABLE ####
############################
survey <- read_excel(XYtbl, sheet = 2)
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
data_flat$observer <- case_when(data_flat$Observer == "ez"  ~ "NRRI-BWCAW_01",
                                data_flat$Observer == "cl"  ~ "NRRI-BWCAW_02",
                                data_flat$Observer == "jdb"  ~ "NRRI-BWCAW_03")

data_flat$pkey_dt<- paste(data_flat$location, paste0(gsub("-", "", as.character(data_flat$visitDate)),"_", gsub(":", "", data_flat$survey_time)), data_flat$observer, sep=":")
head(data_flat$pkey_dt)


WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
data_flat <- data_flat[!duplicated(data_flat[,WTvisit]),] 
visit_tbl <- data_flat

############################
#### SURVEY TABLE ####
############################

observation <- read_excel(XYtbl, sheet = 3)
observation$visitDate <- as.character(as.Date(observation$Date, format = "%Y-%m-%d"))
observation$survey_time <- sub(".*\\s", "", observation$Time)
head(observation)
data_flat <- merge(data_flat, observation, by.x= c("Station Name", "visitDate", "survey_time"), by.y= c("Station Name", "visitDate", "survey_time"))
head(data_flat)
colSums(is.na(data_flat)) > 0 # 'Species_Abbreviation' should be FALSE


# determine appropriate 'distanceMethod' by unique(data_flat$Distance_Observed)
data_flat$distanceMethod <- "0m-50m-100m-INF"

# Initialize to empty strings if not already existing
if(is.null(data_flat$distanceband)) {
  data_flat$distanceband <- rep("", nrow(data_flat))
}
data_flat$distanceband <- ifelse(grepl(".*50.*", data_flat$Distance_Observed), "0m-50m", data_flat$distanceband)
data_flat$distanceband <- ifelse(grepl(".*100.*", data_flat$Distance_Observed), "100m-INF", data_flat$distanceband)
data_flat$distanceband <- ifelse(grepl(".*50-100.*", data_flat$Distance_Observed), "50m-100m", data_flat$distanceband)
# check result unique(paste(data_flat$Distance_Observed, data_flat$distanceband))
print(unique(data_flat$distanceband[(data_flat$distanceband %in% WT_distBandTbl$distance_band_type)]))



# determine appropriate 'distanceMethod' by unique(data_flat$Interval_Observed)
data_flat$durationMethod<- "0-2-3-4-5-6-7-8-9-10min"

# Initialize to empty strings if not already existing
if(is.null(data_flat$durationinterval)) {
  data_flat$durationinterval <- rep("", nrow(data_flat))
}
data_flat$durationinterval <- ifelse(grepl(".*0-2.*", data_flat$Interval_Observed), "0-2min", data_flat$durationinterval)
data_flat$durationinterval <- ifelse(grepl(".*2-3.*", data_flat$Interval_Observed), "2-3min", data_flat$durationinterval)
data_flat$durationinterval <- ifelse(grepl(".*3-4.*", data_flat$Interval_Observed), "3-4min", data_flat$durationinterval)
data_flat$durationinterval <- ifelse(grepl(".*4-5.*", data_flat$Interval_Observed), "4-5min", data_flat$durationinterval)
data_flat$durationinterval <- ifelse(grepl(".*5-6.*", data_flat$Interval_Observed), "5-6min", data_flat$durationinterval)
data_flat$durationinterval <- ifelse(grepl(".*6-7.*", data_flat$Interval_Observed), "6-7min", data_flat$durationinterval)
data_flat$durationinterval <- ifelse(grepl(".*7-8.*", data_flat$Interval_Observed), "7-8min", data_flat$durationinterval)
data_flat$durationinterval <- ifelse(grepl(".*8-9.*", data_flat$Interval_Observed), "8-9min", data_flat$durationinterval)
data_flat$durationinterval <- ifelse(grepl(".*9-10.*", data_flat$Interval_Observed), "9-10min", data_flat$durationinterval)
# check result unique(paste(data_flat$Interval_Observed, data_flat$durationinterval))
print(unique(data_flat$durationinterval[(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))



# determine appropriate species by unique(data_flat$Species_Abbreviation)
# length(unique(data_flat$Species_Abbreviation[(data_flat$Species_Abbreviation %in% WT_spTbl$species_code)])) # 101 in the list 
data_flat$species <- WT_spTbl$species_code[match(data_flat$Species_Abbreviation, WT_spTbl$species_code)]

# length(unique(data_flat$Species_Abbreviation[!(data_flat$Species_Abbreviation %in% WT_spTbl$species_code)])) # 14 not in list, most of them are 'unidentified, 1 suspected typo
# unique(data_flat$Species_Common[!(data_flat$Species_Abbreviation %in% WT_spTbl$species_code)]) # among the 14 code not in list, the 'common name' are shown
# join is performed by removing special characters and case effects
data_flat_cleaned <- tolower(gsub("[^[:alnum:]]", "", data_flat$Species_Common))
WT_spTbl_cleaned <- tolower(gsub("[^[:alnum:]]", "", WT_spTbl$species_common_name))
data_flat$species <- WT_spTbl$species_code[match(data_flat_cleaned, WT_spTbl_cleaned)]

print(length(data_flat$Species_Common[is.na(data_flat$Species_Abbreviation)])) # 0
print(length(data_flat$Species_Common[is.na(data_flat$species)])) # 48 rows not joinable with values "Unidentified non-passerine"
# data_flat$species[is.na(data_flat$species)] <- "NONE" # For these records we call it "NONE"



# determine appropriate behaviour by unique(data_flat$Behaviour)
if(is.null(data_flat$isHeard)) {
  data_flat$isHeard <- rep("No", nrow(data_flat))
}
data_flat$isHeard <- ifelse(grepl(".*Singing.*", data_flat$Behaviour), "Yes", data_flat$isHeard)
data_flat$isHeard <- ifelse(grepl(".*Calling.*", data_flat$Behaviour), "Yes", data_flat$isHeard)
data_flat$isHeard <- ifelse(grepl(".*Drumming.*", data_flat$Behaviour), "Yes", data_flat$isHeard)
data_flat$isHeard <- ifelse(grepl(".*Flyover.*", data_flat$Behaviour), "Yes", data_flat$isHeard)
data_flat$isHeard <- ifelse(grepl(".*Simultaneous.*", data_flat$Behaviour), "Yes", data_flat$isHeard)
# check result unique(paste(data_flat$Behaviour, data_flat$isHeard))

if(is.null(data_flat$isSeen)) {
  data_flat$isSeen <- rep("No", nrow(data_flat))
}
data_flat$isSeen <- ifelse(grepl(".*Observed.*", data_flat$Behaviour), "Yes", data_flat$isSeen)
data_flat$isSeen <- ifelse(grepl(".*Simultaneous.*", data_flat$Behaviour), "Yes", data_flat$isSeen)
# check result unique(paste(data_flat$Behaviour, data_flat$isSeen))


# determine appropriate species by unique(data_flat$Count), any(is.na(data_flat$Count))
data_flat$abundance <-data_flat$Count
# any(is.na(data_flat$abundance))


# create 'isDuplicate' column for signaling the duplicated column 
WTspecies <- c("surveyDateTime", "location", "species", "distanceband", "durationinterval")
duplicates <- duplicated(data_flat[WTspecies]) | duplicated(data_flat[WTspecies], fromLast = TRUE)
data_flat$isDuplicate <- duplicates
print(data_flat[data_flat$isDuplicate == TRUE, c("surveyDateTime", "location", "distanceband", "durationinterval", "Species_Abbreviation", "observer", "abundance", "isDuplicate")])
print(nrow(data_flat[data_flat$isDuplicate == TRUE, ])) # 1725 duplicated record # View(data_flat)


# group the abundance value of duplicated rows, into a new column 'total abundance', also delete the second duplicates 
data_flat <- data_flat %>%
  group_by(surveyDateTime, location, species, distanceband, durationinterval) %>%
  mutate(total_abundance = ifelse(isDuplicate == TRUE, sum(abundance), abundance)) %>%
  filter(!duplicated(paste(surveyDateTime, location, species, distanceband, durationinterval))) %>%
  ungroup() %>%
  select(-isDuplicate) # Optional: Remove the 'isDuplicate' column if no longer needed

# View(data_flat[data_flat$total_abundance != data_flat$abundance, ]) # visualised rows with grouped abundance




# Delete duplicated in the eye of the survey table *total abundance is used
WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "total_abundance", "isHeard", "isSeen", "comments")
survey_tbl <- data_flat[!duplicated(data_flat[,WTsurvey]), WTsurvey] 
# check NULL result by, colSums(is.na(survey_tbl)) > 0
# print(data_flat[is.na(data_flat$species), ])
# survey_tbl[survey_tbl$total_abundance ==0, ], <0 rows>




############################
##EXPORT
############################

write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), na = "", row.names = FALSE)
# survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
# drive_upload(media = survey_out, path = as_id(dr_ls), name = paste0(dataset_code,"_survey.csv"), overwrite = TRUE) 


#Only select visit with observation
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")
visit_tbl <- visit_tbl[visit_tbl$pkey_dt %in% data_flat$pkey_dt, WTvisit]
write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), na = "", row.names = FALSE)
# visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
# drive_upload(media = visit_out, path = as_id(dr_ls), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 


location_tbl <- subset(location_tbl,location_tbl$location %in% survey_tbl$location)
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), na = "", row.names = FALSE)
# location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
# drive_upload(media = location_out, path = as_id(dr_ls), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 









#---EXTENDED
Extended <- c("organization", "project","location", "surveyDateTime", "species", "distanceband", "durationinterval", "site", 
              "station", "utmZone", "easting", "northing", "missinginlocations", "time_zone", "data_origin", 
              "missinginvisit", "pkey_dt", "survey_time", "survey_year", "rawObserver", "original_species", 
              "scientificname", "raw_distance_code", "raw_duration_code", "originalBehaviourData", 
              "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
              "displaytype", "nestevidence", "behaviourother", "atlas_breeding_code")

extended_tbl <- data_flat[!duplicated(data_flat[,Extended]), Extended] 
write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_extended.csv")), na = "", row.names = FALSE)









# ---PROCESSING STATS, no. of locations, visits, and surveys
file_name <- file.path(out_dir, paste0(dataset_code, "_stats.csv"))
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

















