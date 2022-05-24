# ---
# title: "Translate data to make them ready for WT upload"
# author: "Melina Houle"
# date: "March 8, 2022"
# ---

update.packages()
library(tidyverse)
library(dplyr)
library(plyr) #rbind.fill
library(reshape2) # melt
library(readxl)

## Initialize variables
wd <- "E:/MelinaStuff/BAM/dataImport"
setwd(wd)
template <-"./template/BAM-WT_Transfer_flat.csv"
s_folder <- "./project/Atlas/data"
WT_spTbl <- "./lookupTables/species_codes.csv"
ATLAS_spTbl <- "./lookupTables/AtlasSpeciesList.xlsx"
#ATLAS_spTbl <- "./lookupTables/XWalk_Atlas-WT.xlsx"

#Load df template####
df <-read.csv(template)

##Load source data if txt are found locally
#fl = list.files(s_folder, pattern = "*.txt")
#s_data = lapply(fl, function(x) read.delim(file.path(s_folder,x), sep="\t", header=TRUE, fill = TRUE)) 


#Load source data 
dataList <- c("")

# username: teegand, password: BAMProject
s_data <- 


#subset columns needed only
NameList  <- c("")

data_flat<- s_data[names(s_data) %in% NameList]

# Translate df with species only (absence of species or scientifqueName ==N/A always equal survey site with no Observations)
data_flat <-data_flat[!is.na(data_flat$species_id),]
data_flat <-data_flat[!is.na(data_flat$ObservationCount),]



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
data_flat$dataset_code <-data_flat$
## location
data_flat$site <- 
data_flat$station <- 
data_flat$station <- 
data_flat$location <- 
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
data_flat$visitDate <- as.Date(with(data_flat, paste(survey_year, survey_month, survey_day, sep = "-")),format = "%Y-%m-%d")
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
DateTime_f <- paste(data_flat$visitDate, '00:00:00')
data_flat$POSIXdatetime <- as.POSIXlt(DateTime_f, format = "%Y-%m-%d %H:%M:%S") 
data_flat$addTime <- data_flat$POSIXdatetime 
data_flat$addTime[!is.na(data_flat$TimeCollected)] <- data_flat$POSIXdatetime[!is.na(data_flat$TimeCollected)] + 3600*as.numeric(data_flat$TimeCollected[(!is.na(data_flat$TimeCollected))])
data_flat$surveyDateTime <-strftime(data_flat$addTime)
data_flat$survey_time <- format(data_flat$addTime, format = "%H:%M:%S")
## pkey_dt -- Note: Atlas don't have observer id. Left it out of pkey_dt 
data_flat$pkey_dt<- paste(data_flat$location, paste0(as.character(data_flat$visitDate),"_", data_flat$survey_time), sep=":")
#-----------------------------                                                     
#Delete extra field to avoid error using melt later
data_flat$POSIXdatetime <- NULL
data_flat$addTime <- NULL
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
data_flat[data_flat$SpeciesCode==40182, "species_id"] <- 880
#Merge ATLAS_spId df
data_flat <-merge(data_flat, ATLAS_spId[,c("species_id", "species_code")], by ="species_id", all.x = TRUE)
data_flat$species <- data_flat$species_code
#Fill comments with species name that don't have WT translation
data_flat$comments[data_flat$species_code=="NA"] <- data_flat$CommonName[data_flat$species_code=="NA"]
data_flat$Species_Old <- data_flat$species_id
data_flat$scientificname <- data_flat$ScientificName

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
data_flat_norac$sum_count <- as.numeric(data_flat_norac$ObservationCount2) + as.numeric(data_flat_norac$ObservationCount3)

data_flat_norac_in <- data_flat_norac[as.numeric(data_flat_norac$ObservationCount)==data_flat_norac$sum_count,]
norac_expanded <- melt(data_flat_norac_in, measure.vars = c("ObservationCount2","ObservationCount3"), value.name = "abundance")
norac_expanded$durationinterval[norac_expanded$variable == "ObservationCount2" & norac_expanded$abundance >= 1] <- "0-3min"
norac_expanded$durationinterval[norac_expanded$variable == "ObservationCount3" & norac_expanded$abundance >= 1] <- "4-5min"
norac_expanded$distanceband <-"0m-INF"
norac_expanded$raw_distance_code <- "0m-INF"
norac_expanded$raw_duration_code <- norac_expanded$variable

data_flat_norac_out <- data_flat_norac[as.numeric(data_flat_norac$ObservationCount)!=data_flat_norac$sum_count,]
data_flat_norac_out$ObservationIncidental <- as.numeric(data_flat_norac_out$ObservationCount) - data_flat_norac_out$sum_count
norac_incidental <- melt(data_flat_norac_out, measure.vars = c("ObservationCount2","ObservationCount3", "ObservationIncidental"), value.name = "abundance")
norac_incidental$durationinterval[norac_incidental$variable == "ObservationCount2" & norac_incidental$abundance >= 1] <- "0-3min"
norac_incidental$durationinterval[norac_incidental$variable == "ObservationCount3" & norac_incidental$abundance >= 1] <- "4-5min"
norac_incidental$durationinterval[norac_incidental$variable == "ObservationIncidental" & norac_incidental$abundance >= 1] <- NA
norac_incidental$distanceband <-"0m-INF"
norac_incidental$raw_distance_code <- "0m-INF"
norac_incidental$raw_duration_code <- norac_incidental$variable

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

data_translated <- rbind.fill(data_flat_pc,norac_expanded, norac_incidental, pc6_expanded)
#delete abundance = 0 created by melt and durationinterval is na representing incidental observation. Not yet implemented in WildTrax
atlas_translated <- data_translated[data_translated$abundance != 0 | !is.na(data_translated$durationinterval), ]

#--------------------------------------------------------------
#
#       EXPORT
#
#--------------------------------------------------------------

#subset columns 
outputName  <- c("organization", "dataset_code", "location", "site", "station", "utm_zone", "easting", "northing", "latitude", "longitude",
                 "bufferRadiusMeters", "elevationMeters", "isHidden", "trueCoordinates", "comments_location", "internal_wildtrax_id", "internal_update_ts",
                 "visitDate", "snowDepthMeters", "waterDepthMeters", "landFeatures", "crew", "bait", "accessMethod", "comments_visit", "wildtrax_internal_update_ts",
                 "wildtrax_internal_lv_id", "pkey_dt", "surveyDateTime", "survey_time", "observer", "observer_raw", "distanceMethod", "durationMethod", 
                 "species", "Species_Old", "scientificname", "isHeard", "isSeen", "abundance", "distanceband", "raw_distance_code", "durationinterval",
                 "raw_duration_code", "comments") 

#outputName  <- c("location", "latitude", "longitude", "bufferRadiusMeters", "elevationMeters",
#                 "isHidden", "trueCoordinates", "comments", "internal_wildtrax_id", "internal_update_ts",
#                 "visitDate", "snowDepthMeters", "waterDepthMeters", "landFeatures", "crew", "bait", "accessMethod",
#                 "comments", "wildtrax_internal_update_ts", "wildtrax_internal_lv_id", "surveyDateTime", "observer",
#                 "distanceMethod", "durationMethod", "species", "isHeard", "isSeen", "abundance", "distanceband",
#                 "durationinterval")


out <- match(outputName, names(atlas_translated))
out_translated <- atlas_translated[,out]

write.csv(out_translated, "E:/MelinaStuff/BAM/dataImport/out/WT_ATLAScombined_20220211.csv")




#--------------------------------------------------------------
#--------------------------------------------------------------
## TEST

apply(out_translated, 2, function(x) any(is.na(x))) #NAs present in count columns
apply(out_translated, 2, function(x) any(x=="")) #blanks present in species and locality (square) columns

data[out_translated$CommonName=="",]


data_flat[is.na(data_flat$latitude),]
data[out_translated$CommonName=="",]

## some data has no XY coordinates (1400 or something)
out_translated[is.na(out_translated$latitude),]
data_flat[is.na(data_flat$latitude),]

## one has no vistiDate and surveyDateTime
out_translated[is.na(out_translated$visitDate),]
data_flat[is.na(data_flat$visitDate),]

## 30 has an abundance of 0 but has a species identified
length(out_translated[is.na(out_translated$abundance),])
data_flat[is.na(data_flat$abundance),]

## In surveyDateTime, when time is unknown, it is filled by 00:00:00

