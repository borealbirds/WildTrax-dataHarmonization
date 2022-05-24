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

## Initialize variables
wd <- "E:/MelinaStuff/BAM/dataImport"
setwd(wd)
template <-"./template/BAM-WT_Transfer_flat.csv"
s_folder <- "./project/Atlas/data"
WT_spTbl <- "./lookupTables/species_codes.csv"
#ATLAS_spTbl <- "./lookupTables/AtlasSpeciesList.xlsx"
ATLAS_spTbl <- "./lookupTables/XWalk_Atlas-WT.xlsx"

#Load df template####
df <-read.csv(template)

#Load source data if txt are found locally
fl = list.files(s_folder, pattern = "*.txt")
s_data = lapply(fl, function(x) read.delim(file.path(s_folder,x), sep="\t", header=TRUE, fill = TRUE)) 


#Load source data from birdCanada server
dataList <- c("NFATLAS1PC", 
              "ONATLAS3PC", 
              "QCATLAS_NORTH_PC",
              "QCATLAS2PC",
              "SKATLAS1PC")
# username: teegand, password: BAMProject
s_data <- nc_data_dl(collections = dataList, request_id = 202454, username = "teeegand", info = "download Atlas")


#subset columns needed only
NameList  <- c("InstitutionCode", "CollectionCode", "SamplingEventIdentifier", "Locality","RouteIdentifier", "SurveyAreaIdentifier", "DecimalLatitude",
               "DecimalLongitude","YearCollected", "MonthCollected", "DayCollected", "TimeCollected", "SurveyAreaShape", 
               "EffortMeasurement1", "ScientificName", "SpeciesCode", "CommonName", "ObservationCount", "ObservationDescriptor","ObservationCount2", 
               "ObservationDescriptor2", "ObservationCount3",  "ObservationDescriptor3", "NoObservations")


for (i in 1:length(s_data)){  s_data[[i]]<- s_data[[i]][names(s_data[[i]]) %in% NameList]}
data_flat <-do.call(rbind.fill, s_data)

# Translate df with species only (absence of species or scientifqueName ==N/A always equal survey site with no Observations)
data_flat <-data_flat[data_flat$ScientificName!="" & data_flat$ScientificName!="N/A" ,]


#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
#### SET  COLUMN ####
data_flat$PC <-data_flat$CollectionCode
data_flat$SITE <- gsub("^.* ", "", data_flat$Locality)
data_flat$STN <- ifelse(data_flat$SurveyAreaIdentifier == "" | nchar(data_flat$SurveyAreaIdentifier) >15 , sub(".*?-","", data_flat$SamplingEventIdentifier), data_flat$SurveyAreaIdentifier)
data_flat$STN <- gsub(" ", "_", data_flat$STN)
data_flat$location <- paste(data_flat$PC, data_flat$SITE, data_flat$STN, sep=":")
data_flat$latitude <- data_flat$DecimalLatitude
data_flat$longitude <- data_flat$DecimalLongitude
data_flat$bufferRadiusMeters <- NA
data_flat$elevationMeters <- NA
data_flat$isHidden <- NA
data_flat$trueCoordinates <- NA
data_flat$comments <- NA
data_flat$internal_wildtrax_id <- NA
data_flat$internal_update_ts <- NA
data_flat$visitDate <- as.Date(with(data_flat, paste(YearCollected, MonthCollected, DayCollected, sep = "-")),format = "%Y-%m-%d")
data_flat$snowDepthMeters <- NA
data_flat$waterDepthMeters <- NA
data_flat$landFeatures <- NA
data_flat$crew <- NA
data_flat$bait <- NA
data_flat$accessMethod <- NA
data_flat$comments <- NA
data_flat$wildtrax_internal_update_ts <- NA
data_flat$wildtrax_internal_lv_id <- NA
# Time
DateTime_f <- paste(data_flat$visitDate, '00:00:00')
data_flat$POSIXdatetime <- as.POSIXlt(DateTime_f, format = "%Y-%m-%d %H:%M:%S") 
data_flat$addTime <- data_flat$POSIXdatetime 
data_flat$addTime[!is.na(data_flat$TimeCollected)] <- data_flat$POSIXdatetime[!is.na(data_flat$TimeCollected)] + 3600*data_flat$TimeCollected[(!is.na(data_flat$TimeCollected))]

data_flat$surveyDateTime <-strftime(data_flat$addTime)
#Delete extra field to avoir error using melt later
data_flat$POSIXdatetime <- NULL
data_flat$addTime <- NULL
#------------------------------

data_flat$observer <- NA
data_flat$distanceMethod <- "0m-INF"
data_flat$durationMethod <- "0-5min"

# Species
# Read and merge Atlas species list and species code
ATLAS_spId <- as.data.frame(read_excel(ATLAS_spTbl, sheet = "species_name"))

## 2 pattern: bdme used species_code in SpeciesCode column while naturecounts used species_id in SpeciesCode column
## First step is to split dataset and recover common name. 
data_flat_spid <- data_flat[grep("[0-9]+", data_flat$SpeciesCode), ]
data_flat_spcode <- data_flat[grep("[[:alpha:]]", data_flat$SpeciesCode), ]

# Hard coded fix old code not found in Species Table
data_flat_spid[data_flat_spid$SpeciesCode==14840, "SpeciesCode"] <- 40703
data_flat_spid[data_flat_spid$SpeciesCode==7590, "SpeciesCode"] <- 43362
data_flat_spid[data_flat_spid$SpeciesCode==40182, "SpeciesCode"] <- 880
# When speciesCode is numeric, merge ATLAS_spId df
atlas.spid <-merge(data_flat_spid, ATLAS_spId[,c("species_id","species_code")], by.x ="SpeciesCode", by.y = "species_id", all.x = TRUE)


# When speciesCode is 4-letter code, use CommonName
# Hard coded fix old code not found in Species Table (Gray Jay -> Canada Jay, Le Conte's Sparrow -> LeConte's SParrow). Fix it!!
data_flat_spcode[data_flat_spcode$CommonName=="Gray Jay", "CommonName"] <- "Canada Jay"
data_flat_spcode[data_flat_spcode$CommonName=="Le Conte's Sparrow", "CommonName"] <- "LeConte's Sparrow"
atlas.spcode <-merge(data_flat_spcode, ATLAS_spId[,c("common_name","species_code")], by.x ="CommonName", by.y = "common_name", all.x = TRUE)

# Rebuild dataflat
data_flat <-rbind(atlas.spid, atlas.spcode)

## test
#lol <-atlas_merge[atlas_merge$species_code=="NA", ]
#unique(lol$common_name)
#table(lol$common_name)
data_flat$species <- data_flat$species_code

# For now, fill comments with species name that don't have WT translation
data_flat$comments[data_flat$species=="NA"] <- data_flat$CommonName[data_flat$species=="NA"]
#----------------------------------

data_flat$isHeard <- "Yes"
data_flat$isSeen <- "No"
data_flat$distanceband <-"0m-INF"

#------------------------------------------------------
#  This section needs to be used once we know how to treat the time and time/distance protocol
#------------------------------------------------------
## Split df into known duration interval and unknown duration interval. known duration interval need to have rows duplicated 
## in ordr to assign abundance to the right interval. 
#df1 <-data_flat[which((is.na(data_flat$ObservationCount2) & is.na(data_flat$ObservationCount3)) |
#                        (data_flat$ObservationCount2 == 0 & data_flat$ObservationCount3 == 0)), ]
#df1$abundance <- df1$ObservationCount   
#df1$durationinterval <- "UNKNOWN"

#df2 <- data_flat[which(!is.na(data_flat$ObservationCount2) | !is.na(data_flat$ObservationCount3) &
#                         (data_flat$ObservationCount2 > 0 | data_flat$ObservationCount3 > 0)),]

#df2.expanded <- melt(df2, measure.vars = c("ObservationCount2","ObservationCount3"), value.name = "abundance")
#df2.expanded$durationinterval <-  "NA"
#df2.expanded$durationinterval[df2.expanded$variable == "ObservationCount2" & df2.expanded$abundance >= 1] <- "0-3min"
#df2.expanded$durationinterval[df2.expanded$variable == "ObservationCount3" & df2.expanded$abundance >= 1] <- "4-5min"



#data_translated <- rbind.fill(df1,df2.expanded)

#------------------------------------------------------
data_flat$durationinterval <- "UNKNOWN"
data_flat$abundance <-data_flat$ObservationCount

data_translated <- data_flat
  

#--------------------------------------------------------------
#
#       EXPORT
#
#--------------------------------------------------------------

#subset columns needed only
outputName  <- c("location", "latitude", "longitude", "bufferRadiusMeters", "elevationMeters",
                 "isHidden", "trueCoordinates", "comments", "internal_wildtrax_id", "internal_update_ts",
                 "visitDate", "snowDepthMeters", "waterDepthMeters", "landFeatures", "crew", "bait", "accessMethod",
                 "comments", "wildtrax_internal_update_ts", "wildtrax_internal_lv_id", "surveyDateTime", "observer",
                 "distanceMethod", "durationMethod", "species", "isHeard", "isSeen", "abundance", "distanceband",
                 "durationinterval")

out <- match(outputName, names(data_translated))
out_translated <- data_translated[,out]

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

