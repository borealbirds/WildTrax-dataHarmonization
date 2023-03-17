# ---
# title: "Translate Atlas data"
# author: "Melina Houle"
# date: "February 1, 2022"
# Note on translation:
# -- NatureCounts has info on Sex and LifeStage, but those were empty for Atlas data
# -- ProtocolCode determine duration and distance protocol. QCATlas used to old method (0-INF, 0-5min), All others follow 0-INF, 0-3-5min.
# -- Only select one observer randomly when 2 observers recorded observation the same day, same site, same time
# -- Observation descriptor determine the distance band and duration interval
# -- lookupTables/lu_NatureCount_ATLAS_species.xlsx needs to be created prior to run the script


# --- FIX DETAILS
# -- Time is missing. Was replaced by 00:00:01
# ------- 52717 visit
# -- Some species do not match WT species_code
# ------- 1 obs species_id == 700 (Aythya sp.) SET TO "UNSC" 
# ------- 1 obs species_id == 21110  SET TO "UNBI"
# ------- 1 obs species_id == 40154 (Mallard x American Black Duck (hybrid)) SET TO "ABDU"   
# ------- 1 obs species_id == 40289 (Circus cyaneus) SET TO "NOHA"   
# ------- 1 obs species_id == 41254 (Mallard/American Black Duck) SET TO "ABDU"   
# ------- 5 obs species_id == 41773 (American Three-toed/Black-backed Woodpecker) SET TO "UNWO" 
# ------- 1 obs species_id == 44245 (Philadelphia/Warbling Vireo) SET TO "UNVI"   
# ------- 220 obs is.na(species_id) with abundance 0 SET TO "NONE"     
# ------- 5 obs is.na(species_id) with abundance >0 SET TO "UNBI"     

# -- Some observations have species code without abundance. (No answer from Nature Count about that issue). Need to delete
# ------- 4849 obs deleted



## Update reading:  https://birdstudiescanada.github.io/NatureCounts_IntroTutorial/index.html
update.packages()
install.packages("remotes")
remotes::install_github("BirdStudiesCanada/naturecounts")
library(naturecounts)
library(tidyverse)
library(plyr) #rbind.fill
library(reshape2) # melt
library(readxl)
library(readr)
library(dplyr)

## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

organization <- "BAM"
dataset_code <- "NatureCounts_ATLAS"
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

# Check if lookupTable exists
ATLAS_spTbl <- file.path(project_dir, "lookupTables/lu_NatureCount_ATLAS_species.xlsx")
if (!file.exists(ATLAS_spTbl)) {
  stop(print("You need to provide a species lookup table for translation"))
} else {
  ATLAS_spId <- as.data.frame(read_excel(ATLAS_spTbl, sheet = "species_name", col_names = TRUE))
}

out_dir <- file.path("./out", dataset_code)
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}
# Source credential
config <- "./config.R"
source(config)


if(file.exists(file.path(project_dir, "s_data.RData"))){
  load(file.path(project_dir, "s_data.RData"))
} else {
  #Load source data from birdCanada server
  dataList <- c("NFATLAS1PC", 
                "ONATLAS3PC", 
                "QCATLAS_NORTH_PC",
                "QCATLAS2PC",
                "SKATLAS1PC")
  s_data <- nc_data_dl(collections = dataList, fields_set = "extended", request_id = r_id, username = natureCounts_username, info = "download Atlas")
  #backup
  save(s_data, file = file.path(project_dir, "s_data.RData"))
}

#Source columns needed
NameList  <- c("collection", "InstitutionCode", "Locality", "SamplingEventIdentifier", "SurveyAreaIdentifier", "latitude",
               "longitude","UTMZone", "UTMEasting", "UTMNorthing", "survey_year", "survey_month", "survey_day", "TimeCollected", 
               "species_id", "ScientificName", "CommonName", "ProtocolCode","ObservationCount", "ObservationDescriptor","ObservationCount2", 
               "ObservationDescriptor2", "ObservationCount3",  "ObservationDescriptor3", "ObservationCount4","ObservationDescriptor4",  
               "ObservationCount5",  "ObservationDescriptor5",  "ObservationCount6", "ObservationDescriptor6", "ObservationCount7",
               "ObservationDescriptor7", "ObservationCount8", "ObservationDescriptor8", "NoObservations", "Sex", "LifeStage")

data_flat<- s_data[names(s_data) %in% NameList]

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
data_flat$organization <- organization
data_flat$project <-ifelse(data_flat$collection == "QCATLAS2PC", "QC-BBATLAS",
                                ifelse(data_flat$collection == "NFATLAS1PC", "NF-BBATLAS",
                                ifelse(data_flat$collection == "ONATLAS3PC", "ON-BBATLAS",
                                ifelse(data_flat$collection == "QCATLAS_NORTH_PC", "QCNORTH_BBATLAS",
                                ifelse(data_flat$collection == "SKATLAS1PC", "SK_BBATLAS", "UNKNOWN")))))
data_flat$project_full_name <-ifelse(data_flat$collection == "QCATLAS2PC", "Quebec_Breeding_Bird_Atlas_(2010-2014)",
                           ifelse(data_flat$collection == "NFATLAS1PC", "Newfoundland_Breeding_Bird_Atlas_(2020-2024)",
                                  ifelse(data_flat$collection == "ONATLAS3PC", "Ontario_Breeding_Bird_(2021-2025)",
                                         ifelse(data_flat$collection == "QCATLAS_NORTH_PC", "Quebec_Breeding_Bird_Atlas_(northern_project)",
                                                ifelse(data_flat$collection == "SKATLAS1PC", "Saskatchewan_Breeding_Bird_Atlas_(2017-2021)", "UNKNOWN")))))  
data_flat$site <- gsub("^.* ", "", data_flat$Locality)
data_flat$station <- ifelse(data_flat$SamplingEventIdentifier  == "" | nchar(data_flat$SamplingEventIdentifier) >15 , sub(".*?-","", data_flat$SamplingEventIdentifier), data_flat$SamplingEventIdentifier)
data_flat$station <- gsub(" ", "_", data_flat$station)
data_flat$location <- paste(data_flat$project, data_flat$site, data_flat$station, sep=":")
data_flat$latitude <- data_flat$latitude
data_flat$longitude <- data_flat$longitude
#data_flat$elevationMeters <- NA
#data_flat$bufferRadiusMeters <- NA
#data_flat$isHidden <- NA
#data_flat$trueCoordinates <- NA
#data_flat$comments <- paste0("Collection:", data_flat$collection)
#data_flat$internal_wildtrax_id <- NA
#data_flat$internal_update_ts <- NA

# If exists in source data
data_flat$utmZone	<- data_flat$UTMZone
data_flat$easting	<- data_flat$UTMEasting
data_flat$northing	<- data_flat$UTMNorthing
data_flat$missinginlocations <- NA


############################
#### VISIT TABLE ####
############################
# Check is day, month, year are valid. Should be empty
data_flat[is.na(data_flat$survey_year),]
data_flat[is.na(data_flat$survey_month),]
data_flat[is.na(data_flat$survey_day),]

#check for outside value. Should be fixed or removed if present
unique(data_flat$survey_year)
unique(data_flat$survey_month)
unique(data_flat$survey_day)

# Set date
data_flat$visitDate <- as.Date(with(data_flat, paste(survey_year, survey_month, survey_day, sep = "-")),format = "%Y-%m-%d")

data_flat$snowDepthMeters <- NA
data_flat$waterDepthMeters <- NA
data_flat$crew <- NA
data_flat$bait <- "None"
data_flat$accessMethod <- NA
data_flat$landFeatures <- NA
data_flat$wildtrax_internal_update_ts <- NA
data_flat$wildtrax_internal_lv_id <- NA
data_flat$time_zone <- NA
data_flat$data_origin <- data_flat$collection
data_flat$missinginvisit <- NA
# Fix survey_time. When unknown, translate with 00:00:01
data_flat$survey_time[!is.na(data_flat$TimeCollected)] <-format(as.POSIXct(as.numeric((data_flat$TimeCollected[!is.na(data_flat$TimeCollected)])) * 3600, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")
data_flat$survey_time[is.na(data_flat$TimeCollected)] <- "00:00:01"  
unique(data_flat$survey_time)
## pkey_dt -- Concatenate, separated by colons: [location]:[visitDate]_[survey_time]:[localobservercode]; can also use raw observer or observer ID if needed
data_flat$observer <- "NA"
data_flat$pkey_dt<- paste(data_flat$location, paste0(gsub("-", "", as.character(data_flat$visitDate)),"_", gsub(":", "", data_flat$survey_time)), data_flat$observer, sep=":")
data_flat$survey_year <- data_flat$survey_year


############################
#### SURVEY TABLE ####
############################
# surveyDateTime
data_flat$surveyDateTime <- paste(data_flat$visitDate, data_flat$survey_time)
data_flat$comments[is.na(data_flat$TimeCollected)] <- "Survey time missing in source data"
#------------------------------
# Species
# Merge Atlas species list and species code
data_flat <-merge(data_flat, ATLAS_spId[,c("species_id","common_name", "species_code")], by ="species_id", all.x = TRUE)

# Translate species
data_flat$species <- data_flat$species_code
print(unique(data_flat$species_id[!(data_flat$species %in% WT_spTbl$species_code)])) # 700 21110 40154 40289 41254 41773 44245    NA

# Check what is missing
data_flat$species[data_flat$species_id == 700] <-"UNSC" # Aythya sp.  1 occurence
data_flat$species[data_flat$species_id == 21110] <-"UNBI"
data_flat$species[data_flat$species_id == 40154] <-"ABDU"   #Mallard x American Black Duck (hybrid)    1 occurrence
data_flat$species[data_flat$species_id == 40289] <-"NOHA"   # Circus cyaneus 1 occurrence
data_flat$species[data_flat$species_id == 41254] <-"ABDU"    # Mallard/American Black Duck  1 occurrence
data_flat$species[data_flat$species_id == 41773] <-"UNWO"    # American Three-toed/Black-backed Woodpecker  5 occurrences
data_flat$species[data_flat$species_id == 44245] <-"UNVI"    # Philadelphia/Warbling Vireo  1 occurrence
data_flat$species[data_flat$NoObservations == "NoObs"] <- "NONE"     # 220 occurrences
data_flat$species[is.na(data_flat$species_id) & !(is.na(data_flat$ObservationCount))] <- "UNBI"     # 5 occurrences

# Should be empty
print(unique(data_flat$species_id[!(data_flat$species %in% WT_spTbl$species_code)])) 
data_flat$original_species <- data_flat$species_id

## Split df according to protocol. 
#-- Point Count
data_flat_pc <- data_flat[data_flat$ProtocolCode =="PointCount",]

# ProtocolCode =="PointCount" uses old methods. 
data_flat_pc$distanceMethod <- "0m-INF"
data_flat_pc$durationMethod <- "0-5min"

data_flat_pc$abundance[data_flat_pc$species == "NONE"] <- 0
data_flat_pc$abundance[!(data_flat_pc$species == "NONE")] <- as.numeric(data_flat_pc$ObservationCount)

data_flat_pc$durationinterval <- "0-5min"
data_flat_pc$distanceband <-"0m-INF"
data_flat_pc$raw_distance_code <- data_flat_pc$ProtocolCode
data_flat_pc$raw_duration_code <- data_flat_pc$ProtocolCode


#--NORAC
data_flat_norac <- data_flat[data_flat$ProtocolCode =="NORAC",]

# Methodology 
data_flat_norac$distanceMethod <- "0m-INF"
data_flat_norac$durationMethod <- "0-3-5min"

#--species = NONE (we don't want to use melt on them)
norac_sp_none <- data_flat_norac[data_flat_norac$species == "NONE",]
norac_sp_none$distanceband <-"0m-INF"
norac_sp_none$durationinterval <- "UNKNOWN"
norac_sp_none$raw_distance_code <- norac_sp_none$ProtocolCode
norac_sp_none$raw_duration_code <- norac_sp_none$ProtocolCode
norac_sp_none$abundance <- 0

# -- with species
norac_sp <- data_flat_norac[!(data_flat_norac$species == "NONE"),]
norac_expanded <- melt(norac_sp, measure.vars = c("ObservationCount2","ObservationCount3"), value.name = "abundance")
norac_expanded$abundance <- as.numeric(norac_expanded$abundance)
norac_expanded$distanceband <-"0m-INF"
norac_expanded$durationinterval[norac_expanded$variable == "ObservationCount2" & norac_expanded$abundance >= 1] <- "0-3min"
norac_expanded$durationinterval[norac_expanded$variable == "ObservationCount3" & norac_expanded$abundance >= 1] <- "4-5min"
norac_expanded$raw_distance_code <- norac_expanded$ProtocolCode
norac_expanded$raw_duration_code <- norac_expanded$variable
# Delete false 0 created by melt
norac_expanded <- subset(norac_expanded, norac_expanded$abundance>0 )


#--Point Count 6 Interval
data_flat_pc6 <- data_flat[data_flat$ProtocolCode =="Point Count 6 Interval",]

# Methodology 
data_flat_pc6$distanceMethod <- "0m-50m-100m-INF"
data_flat_pc6$durationMethod <- "0-3-5min"

#--species = NONE (we don't want to use melt on them)
pc6_sp_none <- data_flat_pc6[data_flat_pc6$species == "NONE",]
pc6_sp_none$distanceband <-"UNKNOWN"
pc6_sp_none$durationinterval <- "UNKNOWN"
pc6_sp_none$raw_distance_code <- pc6_sp_none$ProtocolCode
pc6_sp_none$raw_duration_code <- pc6_sp_none$ProtocolCode
pc6_sp_none$abundance <- 0

# -- with species
pc6_sp <- data_flat_pc6[!(data_flat_pc6$species == "NONE"),]
pc6_expanded <- melt(pc6_sp, measure.vars = c("ObservationCount2","ObservationCount3","ObservationCount4","ObservationCount5","ObservationCount6","ObservationCount7"), value.name = "abundance")
pc6_expanded$abundance <- as.numeric(pc6_expanded$abundance)
pc6_expanded$durationinterval[pc6_expanded$variable == "ObservationCount2" & pc6_expanded$abundance >= 1] <- "0-3min"
pc6_expanded$distanceband[pc6_expanded$variable == "ObservationCount2" & pc6_expanded$abundance >= 1] <- "0m-50m"
pc6_expanded$durationinterval[pc6_expanded$variable == "ObservationCount3" & pc6_expanded$abundance >= 1] <- "0-3min"
pc6_expanded$distanceband[pc6_expanded$variable == "ObservationCount3" & pc6_expanded$abundance >= 1] <- "50m-100m"
pc6_expanded$durationinterval[pc6_expanded$variable == "ObservationCount4" & pc6_expanded$abundance >= 1] <- "0-3min"
pc6_expanded$distanceband[pc6_expanded$variable == "ObservationCount4" & pc6_expanded$abundance >= 1] <- "100m-INF"
pc6_expanded$durationinterval[pc6_expanded$variable == "ObservationCount5" & pc6_expanded$abundance >= 1] <- "4-5min"
pc6_expanded$distanceband[pc6_expanded$variable == "ObservationCount5" & pc6_expanded$abundance >= 1] <- "0m-50m"
pc6_expanded$durationinterval[pc6_expanded$variable == "ObservationCount6" & pc6_expanded$abundance >= 1] <- "4-5min"
pc6_expanded$distanceband[pc6_expanded$variable == "ObservationCount6" & pc6_expanded$abundance >= 1] <- "50m-100m"
pc6_expanded$durationinterval[pc6_expanded$variable == "ObservationCount7" & pc6_expanded$abundance >= 1] <- "4-5min"
pc6_expanded$distanceband[pc6_expanded$variable == "ObservationCount7" & pc6_expanded$abundance >= 1] <- "100m-INF"
pc6_expanded$raw_distance_code <- pc6_expanded$variable
pc6_expanded$raw_duration_code <- pc6_expanded$variable
# Delete false 0 created by melt
pc6_expanded <- subset(pc6_expanded, pc6_expanded$abundance>0 )

data_translated <- rbind.fill(data_flat_pc, norac_sp_none, norac_expanded, pc6_sp_none, pc6_expanded)

# check methods and interval
print(unique(data_translated$distanceMethod[!(data_translated$distanceMethod %in% WT_distMethTbl$distance_method_type)]))
print(unique(data_translated$durationMethod[!(data_translated$durationMethod %in% WT_durMethTbl$duration_method_type)]))
print(unique(data_translated$distanceband[!(data_translated$distanceband %in% WT_distBandTbl$distance_band_type)]))
print(unique(data_translated$durationinterval[!(data_translated$durationinterval %in% WT_durBandTbl$duration_interval_type)]))

data_translated$isHeard <- "Yes"
data_translated$isSeen <- "No"
data_translated$rawObserver <- NA

data_translated$scientificname <- data_translated$ScientificName
data_translated$originalBehaviourData <- NA
data_translated$missingindetections <- NA
data_translated$pc_vt <- "DNC"
data_translated$pc_vt_detail <- "DNC"
data_translated$age <- "DNC"
data_translated$fm <- "DNC"
data_translated$group <- "DNC"
data_translated$flyover <- "DNC"
data_translated$displaytype <- "DNC"
data_translated$nestevidence <- "DNC"
data_translated$behaviourother <- "DNC"
data_translated$atlas_breeding_code <- "DNC"

##EXPORT
WTlocation <- c("location", "latitude", "longitude")

location_tbl <- data_flat[!duplicated(data_flat[,WTlocation]),] 
out_translated <- split(location_tbl,location_tbl$project_full_name)
out_translated <- lapply(out_translated, setNames, nm = colnames(data_flat))
out_translated <- lapply(out_translated, function(x) { x[ ,WTlocation] })
for (df in names(out_translated)) write.csv(out_translated[[df]], file= file.path(out_dir, paste0(df,"_location.csv")), row.names = FALSE, na = "")
for (df in names(out_translated)) write_lines(paste0("Organization: ", organization), file.path(out_dir, paste0(df, "_stats.csv")))
for (df in names(out_translated)) write_lines(paste0("Project: ", df), file.path(out_dir, paste0(df, "_stats.csv")), append= TRUE)
for (df in names(out_translated)) write_lines(paste0("Number of locations: ", nrow(out_translated[[df]])), file.path(out_dir, paste0(df, "_stats.csv")), append= TRUE)


##EXPORT
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew",
             "bait", "accessMethod", "landFeatures", "comments", "wildtrax_internal_update_ts",
             "wildtrax_internal_lv_id")
visit_tbl <- data_flat[!duplicated(data_flat[,WTvisit]),] 
out_translated <- split(visit_tbl,visit_tbl$project_full_name)
out_translated <- lapply(out_translated, setNames, nm = colnames(data_flat))
out_translated <- lapply(out_translated, function(x) { x[ ,WTvisit] })
for (df in names(out_translated)) write.csv(out_translated[[df]], file= file.path(out_dir, paste0(df,"_visit.csv")), row.names = FALSE, na = "")
for (df in names(out_translated)) write_lines(paste0("Number of visit: ", nrow(out_translated[[df]])), file.path(out_dir, paste0(df, "_stats.csv")), append= TRUE)


## EXPORT
# Sum obs that are the same based on WildTrax field
survey_tbl <- data_translated %>% 
  group_by(project_full_name, location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband, durationinterval, isHeard, isSeen, comments) %>%
  summarise(abundance = sum(abundance), .groups= "keep")

WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen", "comments")
survey_tbl <- survey_tbl[!duplicated(survey_tbl[,WTsurvey]),] 
out_translated <- split(survey_tbl,survey_tbl$project_full_name)
out_translated <- lapply(out_translated, setNames, nm = colnames(survey_tbl))
out_translated <- lapply(out_translated, function(x) { x[ ,WTsurvey] })
for (df in names(out_translated)) write.csv(out_translated[[df]], file= file.path(out_dir, paste0(df,"_survey.csv")), row.names = FALSE, na = "")

############################
#### EXTENDED TABLE ####
############################
extended_tbl <- data_translated %>% 
  group_by(project_full_name, organization, project, location, surveyDateTime, species, distanceband, durationinterval, site, station, utmZone, easting, 
           northing, missinginlocations, time_zone, data_origin, missinginvisit, pkey_dt, survey_time,
           survey_year, rawObserver, original_species, scientificname, raw_distance_code, raw_duration_code, 
           originalBehaviourData, missingindetections, pc_vt, pc_vt_detail, age, fm, group, flyover, 
           displaytype, nestevidence, behaviourother, atlas_breeding_code) %>%
  dplyr::summarise(abundance = sum(abundance), .groups= "keep")

Extended <- c("organization", "project","location", "surveyDateTime", "species", "abundance", "distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code", 
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
              "displaytype", "nestevidence", "behaviourother", "atlas_breeding_code")
extended_tbl <- extended_tbl[!duplicated(extended_tbl[,Extended]),] 
out_translated <- split(extended_tbl,extended_tbl$project_full_name)
out_translated <- lapply(out_translated, setNames, nm = colnames(extended_tbl))
out_translated <- lapply(out_translated, function(x) { x[ ,Extended] })
for (df in names(out_translated)) write.csv(out_translated[[df]], file= file.path(out_dir, paste0(df,"_extended.csv")), row.names = FALSE, na = "")

#--------------------------------------------------------------

