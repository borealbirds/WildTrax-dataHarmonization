# ---
# title: "Translate Atlas data"
# author: "Melina Houle"
# date: "February 1, 2022"
# Note on translation:
# -- NatureCounts has info on Sex and LifeStage, but those were empty for Atlas data
# -- ProtocolCode determine duration and distance protocol 
# -- Only select one observer randomly when 2 observers recorded observation the same day, same site, same time
# -- Observation descriptor determine the distance band and duration interval
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

## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

project_dir <- "NatureCounts_ATLAS"
project <- file.path("./project", project_dir)
WT_spTbl <- "./lookupTables/species_codes.csv"
ATLAS_spTbl <- "./lookupTables/lu_NatureCount_ATLAS_species.xlsx"
out_dir <- file.path("./out", project_dir)

# Source credential
config <- "./config.R"
source(config)

dir.create(out_dir, showWarnings = FALSE)

if(file.exists(file.path(project, "s_data.RData"))){
  load(file.path(project, "s_data.RData"))
} else {
  #Load source data from birdCanada server
  dataList <- c("NFATLAS1PC", 
                "ONATLAS3PC", 
                "QCATLAS_NORTH_PC",
                "QCATLAS2PC",
                "SKATLAS1PC")
  s_data <- nc_data_dl(collections = dataList, fields_set = "extended", request_id = r_id, username = natureCounts_username, info = "download Atlas")
  #backup
  save(s_data, file = "./project/NatureCounts_ATLAS/s_data.RData")
}

#Source columns needed
NameList  <- c("collection", "InstitutionCode", "Locality", "SamplingEventIdentifier", "SurveyAreaIdentifier", "latitude",
               "longitude","UTMZone", "UTMEasting", "UTMNorthing", "survey_year", "survey_month", "survey_day", "TimeCollected", 
               "species_id", "ScientificName", "CommonName", "ProtocolCode","ObservationCount", "ObservationDescriptor","ObservationCount2", 
               "ObservationDescriptor2", "ObservationCount3",  "ObservationDescriptor3", "ObservationCount4","ObservationDescriptor4",  
               "ObservationCount5",  "ObservationDescriptor5",  "ObservationCount6", "ObservationDescriptor6", "ObservationCount7",
               "ObservationDescriptor7", "ObservationCount8", "ObservationDescriptor8", "NoObservations", "Sex", "LifeStage")

data_flat<- s_data[names(s_data) %in% NameList]

# Translate df with species only (absence of species or scientifqueName ==N/A always equal survey site with no Observations)
data_flat <-data_flat[!is.na(data_flat$species_id),]
data_flat <-data_flat[!is.na(data_flat$ObservationCount),]


#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
data_flat$project_name <-data_flat$collection
data_flat$site <- gsub("^.* ", "", data_flat$Locality)
data_flat$station <- ifelse(data_flat$SamplingEventIdentifier  == "" | nchar(data_flat$SamplingEventIdentifier) >15 , sub(".*?-","", data_flat$SamplingEventIdentifier), data_flat$SamplingEventIdentifier)
data_flat$station <- gsub(" ", "_", data_flat$station)
data_flat$location <- paste(data_flat$project_name, data_flat$site, data_flat$station, sep=":")
data_flat$latitude <- data_flat$latitude
data_flat$longitude <- data_flat$longitude
data_flat$elevationMeters <- NA
data_flat$bufferRadiusMeters <- NA
data_flat$isHidden <- NA
data_flat$trueCoordinates <- NA
data_flat$comments <- NA
data_flat$internal_wildtrax_id <- NA
data_flat$internal_update_ts <- NA
# If exists in source data
data_flat$utmZone	<- data_flat$UTMZone
data_flat$easting	<- data_flat$UTMEasting
data_flat$northing	<- data_flat$UTMNorthing
data_flat$missinginlocations <- NA

##EXPORT
WTlocation <- c("location", "latitude", "longitude", "bufferRadiusMeters", "elevationMeters", "isHidden", "trueCoordinates", "comments")

location_tbl <- data_flat[!duplicated(data_flat[,WTlocation]),] 
out_translated <- split(location_tbl,location_tbl$collection)
out_translated <- lapply(out_translated, setNames, nm = colnames(data_flat))
out_translated <- lapply(out_translated, function(x) { x[ ,WTlocation] })
for (df in names(out_translated)) write.csv(out_translated[[df]], file= file.path(out_dir, paste0(df,"_location1.csv")), row.names = FALSE)


############################
#### VISIT TABLE ####
############################
data_flat$visitDate <- as.Date(with(data_flat, paste(survey_year, survey_month, survey_day, sep = "-")),format = "%Y-%m-%d")
data_flat$snowDepthMeters <- NA
data_flat$waterDepthMeters <- NA
data_flat$crew <- NA
data_flat$bait <- NA
data_flat$accessMethod <- NA
data_flat$landFeatures <- NA
data_flat$comments <- NA
data_flat$wildtrax_internal_update_ts <- NA
data_flat$wildtrax_internal_lv_id <- NA
data_flat$time_zone <- NA
data_flat$data_origin <- data_flat$InstitutionCode
data_flat$missinginvisit <- NA
data_flat$survey_time <- NA
data_flat$survey_time[!is.na(data_flat$TimeCollected)] <-format(as.POSIXct(as.numeric((data_flat$TimeCollected[!is.na(data_flat$TimeCollected)])) * 3600, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")
## pkey_dt -- Concatenate, separated by colons: [location]:[visitDate]_[survey_time]:[localobservercode]; can also use raw observer or observer ID if needed
data_flat$pkey_dt<- paste(data_flat$location, paste0(gsub("-", "", as.character(data_flat$visitDate)),"_", gsub(":", "", data_flat$survey_time)), sep=":")
data_flat$survey_year <- data_flat$survey_year

##EXPORT
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew",
             "bait", "accessMethod", "landFeatures", "comments", "wildtrax_internal_update_ts",
             "wildtrax_internal_lv_id")
visit_tbl <- data_flat[!duplicated(data_flat[,WTvisit]),] 
out_translated <- split(visit_tbl,visit_tbl$collection)
out_translated <- lapply(out_translated, setNames, nm = colnames(data_flat))
out_translated <- lapply(out_translated, function(x) { x[ ,WTvisit] })
for (df in names(out_translated)) write.csv(out_translated[[df]], file= file.path(out_dir, paste0(df,"_visit1.csv")), row.names = FALSE)


############################
#### SURVEY TABLE ####
############################
# surveyDateTime
data_flat$addTime <- ifelse(is.na(data_flat$survey_time), "00:01:01", data_flat$survey_time)
data_flat$surveyDateTime <- paste(data_flat$visitDate, data_flat$addTime)

#------------------------------
data_flat$distanceMethod <- "0m-INF"
data_flat$durationMethod <- "0-5min"
data_flat$observer <- NA
# Species
# Read and merge Atlas species list and species code
ATLAS_spId <- as.data.frame(read_excel(ATLAS_spTbl, sheet = "species_name", col_names = TRUE))
# When speciesCode is numeric, merge ATLAS_spId df
data_flat <-merge(data_flat, ATLAS_spId[,c("species_id","common_name", "species_code")], by ="species_id", all.x = TRUE)
# For now, fill comments with species name that don't have WT translation
data_flat$species <- data_flat$species_code
data_flat$comments[is.na(data_flat$survey_time)] <- "Survey time missig in source data"
data_flat$original_species <- data_flat$species_id

## Split df according to protocol. 
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
norac_expanded$distanceband <-"0m-INF"
norac_expanded$durationinterval[norac_expanded$variable == "ObservationCount2" & norac_expanded$abundance >= 1] <- "0-3min"
norac_expanded$durationinterval[norac_expanded$variable == "ObservationCount3" & norac_expanded$abundance >= 1] <- "4-5min"
norac_expanded$raw_distance_code <- norac_expanded$ProtocolCode
norac_expanded$raw_duration_code <- norac_expanded$variable


#--Point Count 6 Interval
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
data_translated <- data_translated[as.numeric(data_translated$abundance)>0,] #delete false 0 created by melt

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

## EXPORT
WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen", "comments")
survey_tbl <- data_translated[!duplicated(data_translated[,WTsurvey]),] 
out_translated <- split(survey_tbl,survey_tbl$collection)
out_translated <- lapply(out_translated, setNames, nm = colnames(data_translated))
out_translated <- lapply(out_translated, function(x) { x[ ,WTsurvey] })
for (df in names(out_translated)) write.csv(out_translated[[df]], file= file.path(out_dir, paste0(df,"_survey1.csv")), row.names = FALSE)

############################
#### EXTENDED TABLE ####
############################
Extended <- c("location", "surveyDateTime", "species", "utmZone", "easting", "northing", "missinginlocations", "time_zone", 
              "data_origin", "missinginvisit", "pkey_dt", "survey_time", "survey_year", "rawObserver", "original_species", 
              "scientificname", "raw_distance_code", "raw_duration_code", "originalBehaviourData", "missingindetections", 
              "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", "displaytype", "nestevidence", "behaviourother", 
              "atlas_breeding_code")
out_translated <- split(survey_tbl,survey_tbl$collection)
out_translated <- lapply(out_translated, setNames, nm = colnames(data_translated))
out_translated <- lapply(out_translated, function(x) { x[ ,Extended] })
for (df in names(out_translated)) write.csv(out_translated[[df]], file= file.path(out_dir, paste0(df,"_extended.csv")), row.names = FALSE)






#--------------------------------------------------------------

.f = function() {
## TEST
apply(out_translated, 2, function(x) any(is.na(x))) #NAs present in count columns
apply(out_translated, 2, function(x) any(x=="")) #blanks present in species and locality (square) columns

nrow(unique(data_flat[,c("latitude", "longitude","location") ])) #64367
temp <- data_flat[!duplicated(data_flat[,c("latitude", "longitude", "location")]),c("latitude", "longitude", "location")] 
tempfreq <- data.frame(table(temp$location))
tempfreq[tempfreq$Freq > 1,]

## Visit Test
length(unique(data_flat$location)) #68312

tempVisit <- data_flat[!duplicated(data_flat[,c("location","visitDate")]),c("visitDate", "location")] 
tempfreqVisit <- data.frame(table(tempVisit$location))
tempfreqVisit[tempfreqVisit$Freq > 1,]

## test survey
tempSurvey <- data_translated[duplicated(data_translated[,WTsurvey]),] 
nrow(tempSurvey)
sum(as.numeric(survey_tbl$abundance))
sum(as.numeric(s_data$ObservationCount),na.rm=TRUE)

data_translated[data_translated$pkey_dt=="ONATLAS3PC:17TQK31:228372-1:20210628_NA",]

data_flat[data_flat$location=="QCATLAS_NORTH_PC:17PA04:440797",]
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

}

