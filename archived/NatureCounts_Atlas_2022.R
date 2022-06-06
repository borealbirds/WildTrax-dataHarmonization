# ---
# title: "Import Atlas data"
# author: "Melina Houle"
# date: "February 1, 2022"
# ---

## Update reading:  https://birdstudiescanada.github.io/NatureCounts_IntroTutorial/index.html
update.packages()
install.packages("remotes")
remotes::install_github("BirdStudiesCanada/naturecounts")
library(naturecounts)
library(tidyverse)
library(dplyr)
library(plyr) #rbind.fill
library(reshape2) # melt
library(readxl)
library(googledrive)

# Get config param
source("E:/MelinaStuff/BAM/config.R")

## Initialize variables
wd <- "E:/MelinaStuff/BAM/dataImport"
setwd(wd)
template <-"./template/BAM-WT_Transfer_flat.csv"
s_folder <- "./project/Atlas/data"
ATLAS_spTbl <- "./lookupTables/AtlasSpeciesList.xlsx"
dataset_code <- "NCATLAScombined"

#Load df template####
df <-read.csv(template)

##Load source data if txt are found locally
#fl = list.files(s_folder, pattern = "*.txt")
#s_data = lapply(fl, function(x) read.delim(file.path(s_folder,x), sep="\t", header=TRUE, fill = TRUE)) 


#Load source data from birdCanada server
dataList <- c("NFATLAS1PC", 
              "ONATLAS3PC", 
              "QCATLAS_NORTH_PC",
              "QCATLAS2PC",
              "SKATLAS1PC")

#Login info are source from config
s_data <- nc_data_dl(request_id = r_id, username = natureCounts_username, fields_set = "extended", info = "download Atlas")

#######################################################

#subset columns needed only
NameList  <- c("collection", "Locality", "SamplingEventIdentifier", "SurveyAreaIdentifier", "latitude",
               "longitude","UTMZone","UTMEasting", "UTMNorthing", "survey_year", "survey_month", "survey_day", "TimeCollected", 
               "species_id", "ScientificName", "CommonName", "ProtocolCode","ObservationCount", "ObservationDescriptor","ObservationCount2", 
               "ObservationDescriptor2", "ObservationCount3",  "ObservationDescriptor3", "ObservationCount4", 
               "ObservationDescriptor4", "ObservationCount5",  "ObservationDescriptor5",  "ObservationCount6", 
               "ObservationDescriptor6",  "ObservationCount7","ObservationDescriptor7", "ObservationCount8", "ObservationDescriptor8", "NoObservations")


data_flat<- s_data[names(s_data) %in% NameList]

# Translate df with species only (absence of species or scientifqueName ==N/A always equal survey site with no Observations)
data_flat <-data_flat[!is.na(data_flat$species_id),]
data_flat <-data_flat[!is.na(data_flat$ObservationCount),]

# Delete duplicate record based on location, date, time, species and abundance
data_flat <- data_flat[!duplicated(data_flat[,c("latitude","longitude","survey_year","survey_month","survey_day","TimeCollected","species_id","ObservationCount")]),]

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
#### SET  COLUMN ####
## organization
data_flat$organization <- sapply(data_flat$collection, switch, 
                                 NFATLAS1PC = "NF-ATLAS (BAM)", 
                                 ONATLAS3PC = "ON-ATLAS (BAM)",
                                 QCATLAS_NORTH_PC = "QC-ATLAS (BAM)",
                                 QCATLAS2PC = "QC-ATLAS (BAM)",
                                 SKATLAS1PC = "SK-ATLAS (BAM)")
## dataset code
data_flat$dataset_code <-data_flat$collection
## location
data_flat$site <- gsub("^.* ", "", data_flat$Locality)
data_flat$station <- sub(".*?-","", data_flat$SamplingEventIdentifier)
data_flat$location <- paste(data_flat$dataset_code, data_flat$site, data_flat$station, sep=":")
## UTM zone, Easting, Northing
data_flat$utm_zone <- data_flat$UTMZone 
data_flat$easting <- data_flat$UTMEasting
data_flat$northing <- data_flat$UTMNorthing 
## latitude, longitude
data_flat$latitude <- data_flat$latitude
data_flat$longitude <- data_flat$longitude
## bufferRadiusMeters, elevationMeters, isHidden, trueCoordinates, comments, internal_wildtrax_id, internal_update_ts
data_flat$bufferRadiusMeters <- NA
data_flat$elevationMeters <- NA
data_flat$isHidden <- NA
data_flat$trueCoordinates <- NA
data_flat$comments_location <- NA
data_flat$internal_wildtrax_id <- NA
data_flat$internal_update_ts <- NA
## visitDate
data_flat$visitDate <- format(as.Date(with(data_flat, paste(survey_year, survey_month, survey_day, sep = "-"))),format = "%Y-%m-%d")
## snowDepthMeters, waterDepthMeters, landFeatures, crew, bait, accessMethod, comments_visit,wildtrax_internal_update_ts, wildtrax_internal_lv_id
data_flat$snowDepthMeters <- NA
data_flat$waterDepthMeters <- NA
data_flat$landFeatures <- NA
data_flat$crew <- NA
data_flat$bait <- NA
data_flat$accessMethod <- NA
data_flat$comments_visit <- NA
data_flat$wildtrax_internal_update_ts <- NA
data_flat$wildtrax_internal_lv_id <- NA
# surveyDateTime, survey_time
data_flat$POSIXdatetime <- as.POSIXlt(as.Date(data_flat$visitDate), format = "%Y-%m-%d %H:%M:%S") 
data_flat$POSIXdatetime[!is.na(data_flat$TimeCollected)] <- data_flat$POSIXdatetime[!is.na(data_flat$TimeCollected)] + 3600*as.numeric(data_flat$TimeCollected[(!is.na(data_flat$TimeCollected))])
data_flat$surveyDateTime <-strftime(data_flat$POSIXdatetime)
data_flat$survey_time <- format(data_flat$POSIXdatetime, format = "%H:%M:%S")
## pkey_dt -- Note: Atlas don't have observer id. Left it out of pkey_dt 
data_flat$pkey_dt<- paste(data_flat$location, paste0(gsub("-", "", as.character(data_flat$visitDate)),"_", gsub(":", "", data_flat$survey_time)), sep=":")
#-----------------------------                                                     
#Delete extra field to avoid error using melt later
data_flat$POSIXdatetime <- NULL
#------------------------------

## observer, observer_raw
data_flat$observer <- NA
data_flat$observer_raw <- NA

## Distance and duration Methods
data_flat$distanceMethod <- "0m-INF"
data_flat$durationMethod <- "0-5min"

## Species, species_old, comments, scientificname
#Read and merge Atlas species list and species code
ATLAS_spId <- as.data.frame(read_excel(ATLAS_spTbl, sheet = "species_name", col_names = TRUE))
#Hard coded fix code not found in Species Table
data_flat[data_flat$species_id==40182, "species_id"] <- 880
#Merge ATLAS_spId df
data_flat <-merge(data_flat, ATLAS_spId[,c("species_id", "species_code")], by ="species_id", all.x = TRUE)
data_flat$species <- data_flat$species_code
data_flat$Species_Old <- data_flat$species_id
data_flat$scientificname <- data_flat$ScientificName
data_flat$comments <- NA
## isHeard isSeen
data_flat$isHeard <- "Yes"
data_flat$isSeen <- "No"

## abundance
#Split  according to protocol. 
#-- Point Count
data_flat_pc <- data_flat[data_flat$ProtocolCode =="PointCount",]
data_flat_pc$abundance <- data_flat_pc$ObservationCount   
data_flat_pc$durationinterval <- "UNKNOWN"
data_flat_pc$distanceband <-"0m-INF"
data_flat_pc$raw_distance_code <- data_flat_pc$ProtocolCode
data_flat_pc$raw_duration_code <- data_flat_pc$ProtocolCode

#--NORAC
data_flat_norac <- data_flat[data_flat$ProtocolCode =="NORAC",]
norac_expanded <- melt(data_flat_norac, measure.vars = c("ObservationCount2","ObservationCount3"), value.name = "abundance")
norac_expanded$durationinterval[norac_expanded$variable == "ObservationCount2" & norac_expanded$abundance >= 1] <- "0-3min"
norac_expanded$durationinterval[norac_expanded$variable == "ObservationCount3" & norac_expanded$abundance >= 1] <- "4-5min"
norac_expanded$distanceband <-"0m-INF"
norac_expanded$raw_distance_code <- norac_expanded$ProtocolCode
norac_expanded$raw_duration_code <- norac_expanded$variable

#-- Point Count 6 Interval
data_flat_pc6 <- data_flat[data_flat$ProtocolCode =="Point Count 6 Interval",]

pc6_expanded <- melt(data_flat_pc6, measure.vars = c("ObservationCount2","ObservationCount3","ObservationCount4","ObservationCount5","ObservationCount6","ObservationCount7"), value.name = "abundance")
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

data_translated <- rbind.fill(data_flat_pc,norac_expanded, pc6_expanded)

#delete abundance = 0 created by melt and durationinterval is na representing incidental observation. Not yet implemented in WildTrax
atlas_translated <- data_translated[data_translated$abundance != 0 | !is.na(data_translated$durationinterval), ]

#--------------------------------------------------------------
#
#       EXPORT
#
#--------------------------------------------------------------

#subset columns 
outputName  <- c("organization", "dataset_code", "location", "site", "station", "utm_zone", "easting", "northing", "latitude", 
                 "longitude", "bufferRadiusMeters", "elevationMeters", "isHidden", "trueCoordinates", "comments_location", 
                 "internal_wildtrax_id", "internal_update_ts", "visitDate", "snowDepthMeters", "waterDepthMeters", "landFeatures", 
                 "crew", "bait", "accessMethod", "comments_visit", "wildtrax_internal_update_ts", "wildtrax_internal_lv_id", 
                 "pkey_dt", "surveyDateTime", "survey_time", "observer", "observer_raw", "distanceMethod", "durationMethod", 
                 "species", "Species_Old", "scientificname", "isHeard", "isSeen", "abundance", "distanceband", "raw_distance_code", 
                 "durationinterval", "raw_duration_code", "comments") 


out <- match(outputName, names(atlas_translated))
out_translated <- atlas_translated[,out]

write.csv(out_translated, "E:/MelinaStuff/BAM/dataImport/out/WT_NCATLAScombined.csv")


## Export flat file to GoogleDrive
out_location <- drive_get("NatureCounts_Atlas/FLATtranslated")
drive_upload(file.path(wd,"out", "WT_NCATLAScombined.csv"),path=as_id(out_location$id), name = "WT_NCATLAScombined.csv", overwrite = TRUE)


## Export SourceDatato GoogleDrive
out_location <- drive_get("NatureCounts_Atlas/SourceData")
write.csv(s_data, "E:/MelinaStuff/BAM/dataImport/out/NCATLAScombined_raw.csv")
drive_upload(file.path(wd,"out", "NCATLAScombined_raw.csv"),path=as_id(out_location$id), name = "NCATLAScombined_raw.csv", overwrite = TRUE)

