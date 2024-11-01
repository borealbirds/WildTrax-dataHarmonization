# ---
# title: "Translate USFWS data"
# author: Siu Chung WU, Diego
# date: "October 30, 2024"
# Note on translation:
# 


library(googledrive)
library(tidyr)
library(readr)
library(chron)
library(readxl)
library(plyr) 
library(dplyr)
library(sf)


## URL
url <- "https://drive.google.com/drive/u/1/folders/13SM653A94HRYk15_B4CfckIlepy5otc5"


## Initialize variables
wd <- "C:/Users/asito/Desktop/ModellingProject/#BAM/WildTrax_Integration"
organization = "BAM"
dataset_code = "ONF"
setwd(file.path(wd))


lu <- "./lookupTables"
WT_spTbl <- read.csv(file.path(lu, "species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path(lu, "duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path(lu, "distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path(lu, "duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path(lu, "distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

project <- file.path("./project", dataset_code)
if (!dir.exists(project)) {
  dir.create(project, recursive = TRUE)
}

dataDir <- file.path(wd,"project",dataset_code,"data")   # where files would be downloaded
if (!dir.exists(dataDir)) {
  dir.create(dataDir, recursive = TRUE)
}

out_dir <- file.path(wd, "out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE) 
}


#--------------------------------------------------------------
#
#       LOAD
#
#--------------------------------------------------------------

drive_auth()
folder_id <- sub(".*folders/", "", url)
files_in_folder <- drive_ls(as_id(folder_id))

# Filter for CSV files
csv_files <- files_in_folder %>% 
  filter(grepl("\\.csv$", name))


# Specify the CSV file name holding location information 
location_file <- "Ottawa NF Sampling Units Lat Long.csv"

location_csv <- csv_files %>% 
  filter(name == location_file)
location <- read.csv(drive_download(location_csv$id[1], overwrite = TRUE)$local_path)


# Specify the CSV file name holding observation information 
location_file <- "Ottawa NF Sampling Units Lat Long.csv"

location_csv <- csv_files %>% 
  filter(name == location_file)
location <- read.csv(drive_download(location_csv$id[1], overwrite = TRUE)$local_path)


# Specify the CSV file name holding observation information 
observation_file <- "Ottawa NF Point Count Observations.csv"

observation_csv <- csv_files %>% 
  filter(name == observation_file)
observation <- read.csv(drive_download(observation_csv$id[1], overwrite = TRUE)$local_path)

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------

############################
#### LOCATION TABLE ####
############################
data_flat <- observation
data_flat <- merge(data_flat, location, by.x= 'Point', by.y= 'Short.Name')

data_flat$organization <- organization
data_flat$project <- dataset_code
data_flat$site <- data_flat$Transect
data_flat$station <- data_flat$Point

# Not exist in source data
data_flat$elevationMeters <- NA
data_flat$bufferRadiusMeters <- NA
data_flat$isHidden <- NA
data_flat$trueCoordinates <- NA
data_flat$comments <- NA
data_flat$internal_wildtrax_id <- NA
data_flat$internal_update_ts <- NA
data_flat$utmZone	<- NA
data_flat$easting	<- NA
data_flat$northing	<- NA
data_flat$missinginlocations <- NA
data_flat$longitude <- data_flat$Longitude
data_flat$latitude <- data_flat$Latitude

# # correct projection from EPSG 32615 to EPSG 4269, and save them back to the original dataframe (data_flat)
# xy_sf <- st_as_sf(data_flat, coords = c("easting", "northing"))
# xy_sf <- st_set_crs(xy_sf, 32616)
# xy_sf_4269 <- st_transform(xy_sf, crs = 4269)
# xy_4269 <- st_coordinates(xy_sf_4269) 
# data_flat$longitude <- round(xy_4269[,1], 5)
# data_flat$latitude <- round(xy_4269[,2], 5)



#---LOCATION
data_flat$location <- paste(dataset_code, data_flat$site, data_flat$station, sep=":")

# options(digits = 7)
# View(location_tbl)

############################
#### VISIT TABLE ####
############################

#survey attributes
data_flat$visitDate <- data_flat$Date
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
data_flat$survey_time <- sub(".*\\s", "", data_flat$Start.Time)
data_flat$surveyDateTime <- paste(data_flat$visitDate, data_flat$survey_time)

##Observer (internal ID number): unique(data_flat$Researcher)
data_flat$observer <- case_when(data_flat$Researcher == "Koch, Katie"  ~ "obs01")

data_flat$pkey_dt<- paste(data_flat$location, paste0(gsub("-", "", as.character(data_flat$visitDate)),"_", gsub(":", "", data_flat$survey_time)), data_flat$observer, sep=":")
head(data_flat$pkey_dt)


############################
#### SURVEY TABLE ####
############################

colSums(is.na(data_flat)) > 0 # 'Spp' should be FALSE

# determine appropriate 'distanceMethod' by unique(data_flat$Distance.Bin)
data_flat$distanceMethod <- "0m-50m-100m-INF"

# Initialize to empty strings if not already existing
if(is.null(data_flat$distanceband)) {
  data_flat$distanceband <- rep("", nrow(data_flat))
}
data_flat$distanceband <- ifelse(grepl(".*<50.*", data_flat$Distance.Bin), "0m-50m", data_flat$distanceband)
data_flat$distanceband <- ifelse(grepl(".*50 to 100.*", data_flat$Distance.Bin), "50m-100m", data_flat$distanceband)
data_flat$distanceband <- ifelse(grepl(".*>100.*", data_flat$Distance.Bin), "100m-INF", data_flat$distanceband)
# check result unique(paste(data_flat$Distance.Bin, data_flat$distanceband))
print(unique(data_flat$distanceband[(data_flat$distanceband %in% WT_distBandTbl$distance_band_type)]))


# determine appropriate 'distanceMethod' by unique(data_flat$Time.Bin)
data_flat$durationMethod<- "0-3-5-10min"

# Initialize to empty strings if not already existing
if(is.null(data_flat$durationinterval)) {
  data_flat$durationinterval <- rep("", nrow(data_flat))
}
data_flat$durationinterval <- ifelse(grepl(".*0_3min.*", data_flat$'Time.Bin'), "0-3min", data_flat$durationinterval)
data_flat$durationinterval <- ifelse(grepl(".*3_5min.*", data_flat$'Time.Bin'), "3-5min", data_flat$durationinterval)
data_flat$durationinterval <- ifelse(grepl(".*5_10min.*", data_flat$'Time.Bin'), "5-10min", data_flat$durationinterval)
# check result unique(paste(data_flat$Time.Bin, data_flat$durationinterval))
print(unique(data_flat$durationinterval[(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))


# unique(data_flat$Spp[!(data_flat$Spp %in% WT_spTbl$species_code)])
data_flat$species <- WT_spTbl$species_code[match(data_flat$Spp, WT_spTbl$species_code)]

print(length(data_flat$species[is.na(data_flat$species)])) # 0, the newly build column
print(length(data_flat$species[is.na(data_flat$Scientific.Name)])) # 0, the original column holding species Scientific Name
print(length(data_flat$species[is.na(data_flat$Spp)]))  # 0, the original column holding species code
# data_flat$species[is.na(data_flat$species)] <- "NONE" # For these records we call it "NONE"




# determine appropriate behaviour by unique(data_flat$'Detection.Cue')
if(is.null(data_flat$isHeard)) {
  data_flat$isHeard <- rep("DNC", nrow(data_flat))
}
data_flat$isHeard <- ifelse(grepl(".*S.*", data_flat$Detection.Cue), "Yes", data_flat$isHeard)
data_flat$isHeard <- ifelse(grepl(".*C.*", data_flat$Detection.Cue), "Yes", data_flat$isHeard)
data_flat$isHeard <- ifelse(grepl(".*O.*", data_flat$Detection.Cue), "No", data_flat$isHeard)
data_flat$isHeard <- ifelse(grepl(".*NR.*", data_flat$Detection.Cue), "DNC", data_flat$isHeard)
# check result unique(paste(data_flat$Detection.Cue, data_flat$isHeard))


if(is.null(data_flat$isSeen)) {
  data_flat$isSeen <- rep("DNC", nrow(data_flat))
}
data_flat$isHeard <- ifelse(grepl(".*S.*", data_flat$Detection.Cue), "No", data_flat$isSeen)
data_flat$isHeard <- ifelse(grepl(".*C.*", data_flat$Detection.Cue), "No", data_flat$isSeen)
data_flat$isHeard <- ifelse(grepl(".*O.*", data_flat$Detection.Cue), "Yes", data_flat$isSeen)
data_flat$isHeard <- ifelse(grepl(".*NR.*", data_flat$Detection.Cue), "DNC", data_flat$isSeen)
# check result unique(paste(data_flat$Detection.Cue, data_flat$isSeen))


# determine appropriate species by unique(data_flat$Count), any(is.na(data_flat$Count))
data_flat$ind_count <-data_flat$Count
# any(is.na(data_flat$ind_count))


# create 'isDuplicate' column for signaling the duplicated column , *For information only
WTspecies <- c("surveyDateTime", "location", "species", "distanceband", "durationinterval")
duplicates <- duplicated(data_flat[WTspecies]) | duplicated(data_flat[WTspecies], fromLast = TRUE)
data_flat$isDuplicate <- duplicates
print(data_flat[data_flat$isDuplicate == TRUE, c("surveyDateTime", "location", "distanceband", "durationinterval", "species", "observer", "ind_count", "isDuplicate")])
print(nrow(data_flat[data_flat$isDuplicate == TRUE, ])) # 264 duplicated record





# Delete duplicated in the eye of the survey table *abundance is used
# WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
#               "durationinterval", "abundance", "isHeard", "isSeen", "comments")
# survey_tbl <- data_flat[!duplicated(data_flat[,WTsurvey]), WTsurvey]
# check NULL result by, colSums(is.na(survey_tbl)) > 0
# print(data_flat[is.na(data_flat$species), ])
# survey_tbl[survey_tbl$abundance ==0, ], should be 0


# Validations: species code, duration band, distance band, all in specification
print(unique(data_flat$species[!(data_flat$species %in% WT_spTbl$species_code)]))
print(unique(data_flat$durationinterval[!(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))
print(unique(data_flat$distanceband[!(data_flat$distanceband %in% WT_distBandTbl$distance_band_type)]))
print(unique(data_flat$durationinterval[!(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))

############################
#### SURVEY TABLE ####
############################

#---EXTENDED
data_flat$rawObserver <- data_flat$observer
data_flat$original_species <- data_flat$Spp
data_flat$scientificname <- WT_spTbl$scientific_name[match(data_flat$species, WT_spTbl$species_code)]
data_flat$raw_distance_code <- data_flat$Distance.Bin
data_flat$raw_duration_code <- data_flat$Time.Bin
data_flat$originalBehaviourData <- data_flat$Detection.Cue
data_flat$missingindetections <- "DNC"
data_flat$age <- "DNC"
data_flat$fm <- "DNC"
data_flat$group <- "DNC"
data_flat$flyover <- ifelse(data_flat$Observation.Note=="Flyover", "Yes", "No")
data_flat$displaytype <- "DNC"
data_flat$nestevidence <- "DNC"
data_flat$behaviourother <- "DNC"
data_flat$atlas_breeding_code <- "DNC"

data_flat <- data_flat %>%
  mutate(pc_vt = case_when(Detection.Cue =="S" ~ "Song",
                           Detection.Cue =="C" ~ "Song",
                           Detection.Cue =="O" ~ "NA",
                           Detection.Cue =="NR" ~ "NA",
                           TRUE ~ "DNC" ),
         pc_vt_detail= case_when(TRUE ~ "NA" ))



############################
##EXPORT
############################


# Create sub folder in 'toUpload' with the organization name
dr<- drive_get("toUpload/", shared_drive = "BAM_Core")
to_upload_contents <- drive_ls(as_id(dr)) # print(to_upload_contents)
cws_folder <- to_upload_contents[to_upload_contents$name == organization, ]
if (nrow(cws_folder) == 0) {
  cws_folder <- drive_mkdir(organization, path = as_id(dr))
}

# Create sub folder in 'toUpload/organisation' with the dataset name
dr<- drive_get(paste0("toUpload/",organization), shared_drive = "BAM_Core")
folder_list <- drive_ls(as_id(dr), pattern = dataset_code) # print(folder_list)

if (nrow(folder_list[folder_list$name == dataset_code, ]) == 0){
  dr_dataset_code <-drive_mkdir(dataset_code, path = as_id(dr), overwrite = NA)
  print(paste("Folder", dataset_code, "created successfully."))
} else {
  dr_dataset_code <- folder_list[folder_list$name == dataset_code, ]
} # print(drive_ls(as_id(dr)))


# flyover (flying) data singled out
no_flyover<- data_flat %>%
  filter(!flyover == "Yes")

# yes_flyover<- data_flat %>%
#   filter(flyover == "Yes")
# print(yes_flyover)



#---LOCATION
# Remove duplicated location
WTlocation <- c("location", "longitude", "latitude")
location_tbl <- no_flyover[!duplicated(no_flyover[,WTlocation]), WTlocation]
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), row.names = FALSE, na = "")
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 


#---VISIT
# Delete duplicated based on WildtTrax attributes (double observer on the same site, same day).
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")
visit_tbl <- no_flyover[!duplicated(no_flyover[,WTvisit]), WTvisit] # 
write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), row.names = FALSE, na = "")
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 


#---SURVEY
# Delete duplicated in the eye of the survey table *abundance is used

survey_tbl <- no_flyover %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband, durationinterval, isHeard, isSeen, comments) %>%
  dplyr::summarise(abundance = sum(ind_count), .groups= "keep")

WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen", "comments")
survey_tbl <- survey_tbl[!duplicated(survey_tbl[,WTsurvey]), WTsurvey] # 
write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), row.names = FALSE, na = "")
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv"), overwrite = TRUE) 



# check NULL result by, colSums(is.na(survey_tbl)) > 0
# survey_tbl[survey_tbl$abundance ==0, ], should be 0


# below lines to highlight the grouped lines, which are not selected in survey_tbl 
# x <- no_flyover %>%
#   group_by(surveyDateTime, location, species, distanceband, durationinterval, isHeard, isSeen, comments) %>%
#   mutate(abundance = ifelse(isDuplicate == TRUE, sum(ind_count), ind_count)) %>%
#   filter(!duplicated(paste(surveyDateTime, location, species, distanceband, durationinterval))) %>%
#   ungroup()
# View(x[x$abundance != x$ind_count, ]) # visualized rows with grouped ind_count



#---EXTENDED
# "atlas_breeding_code" still kept
# the operation of grouping "ind_count" into "abundance" is not done
Extended <- c("organization", "project", "location", "surveyDateTime", "species", "ind_count", "distanceband", "durationinterval", "site", "station", "utmZone", "easting",
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code",
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover",
              "displaytype", "nestevidence", "behaviourother", "atlas_breeding_code")

extended_tbl <- data_flat[!duplicated(data_flat[,Extended]), Extended]
write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_behavior.csv")), na = "", row.names = FALSE)
extended_out <- file.path(out_dir, paste0(dataset_code,"_behavior.csv"))
drive_upload(media = extended_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_behavior.csv"), overwrite = TRUE)




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

