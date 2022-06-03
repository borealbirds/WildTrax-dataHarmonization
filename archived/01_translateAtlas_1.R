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
ATLAS_spTbl <- "./lookupTables/AtlasSpeciesList.xlsx"
#ATLAS_spTbl <- "./lookupTables/XWalk_Atlas-WT.xlsx"

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
# username: teegand, password: BAMProject
s_data <- nc_data_dl(collections = dataList, fields_set = "extended", request_id = 202454, username = "teegand", info = "download Atlas")


#subset columns needed only
NameList  <- c("collection", "Locality", "SamplingEventIdentifier", "SurveyAreaIdentifier", "latitude",
               "longitude","survey_year", "survey_month", "survey_day", "TimeCollected", 
               "species_id", "ProtocolCode","ObservationCount", "ObservationDescriptor","ObservationCount2", 
               "ObservationDescriptor2", "ObservationCount3",  "ObservationDescriptor3", "ObservationCount4", 
               "ObservationDescriptor4", "ObservationCount5",  "ObservationDescriptor5",  "ObservationCount6", 
               "ObservationDescriptor6","NoObservations")


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
data_flat$PC <-data_flat$collection
data_flat$SITE <- gsub("^.* ", "", data_flat$Locality)
data_flat$STN <- ifelse(data_flat$SurveyAreaIdentifier == "" | nchar(data_flat$SurveyAreaIdentifier) >15 , sub(".*?-","", data_flat$SamplingEventIdentifier), data_flat$SurveyAreaIdentifier)
data_flat$STN <- gsub(" ", "_", data_flat$STN)
data_flat$location <- paste(data_flat$PC, data_flat$SITE, data_flat$STN, sep=":")
data_flat$latitude <- data_flat$latitude
data_flat$longitude <- data_flat$longitude
data_flat$bufferRadiusMeters <- NA
data_flat$elevationMeters <- NA
data_flat$isHidden <- NA
data_flat$trueCoordinates <- NA
data_flat$comments <- NA
data_flat$internal_wildtrax_id <- NA
data_flat$internal_update_ts <- NA
data_flat$visitDate <- as.Date(with(data_flat, paste(survey_year, survey_month, survey_day, sep = "-")),format = "%Y-%m-%d")
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
data_flat$addTime[!is.na(data_flat$TimeCollected)] <- data_flat$POSIXdatetime[!is.na(data_flat$TimeCollected)] + 3600*as.numeric(data_flat$TimeCollected[(!is.na(data_flat$TimeCollected))])

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
ATLAS_spId <- as.data.frame(read_excel(ATLAS_spTbl, sheet = "species_name", col_names = TRUE))


# Hard coded fix code not found in Species Table
data_flat[data_flat$SpeciesCode==40182, "species_id"] <- 880

# When speciesCode is numeric, merge ATLAS_spId df
data_flat <-merge(data_flat, ATLAS_spId[,c("species_id","common_name", "species_code")], by ="species_id", all.x = TRUE)
data_flat$species <- data_flat$species_code
# Fill comments with species name that don't have WT translation
data_flat$comments[data_flat$species_code=="NA"] <- data_flat$common_name[data_flat$species_code=="NA"]
data_flat$isHeard <- "Yes"
data_flat$isSeen <- "No"
#######nrow(data_flat = 641480)
#------------------------------------------------------
#  This section needs to be used once we know how to treat the time and time/distance protocol
#------------------------------------------------------
## Split df according to protocol. 
#-- Point Count n =366226
data_flat_pc <- data_flat[data_flat$ProtocolCode =="PointCount",]
data_flat_pc$abundance <- data_flat_pc$ObservationCount   
data_flat_pc$durationinterval <- "UNKNOWN"
data_flat_pc$distanceband <-"0m-INF"

#--NORAC n = 246683
data_flat_norac <- data_flat[data_flat$ProtocolCode =="NORAC",]
norac_expanded <- melt(data_flat_norac, measure.vars = c("ObservationCount2","ObservationCount3"), value.name = "abundance")
norac_expanded$durationinterval[norac_expanded$variable == "ObservationCount2" & norac_expanded$abundance >= 1] <- "0-3min"
norac_expanded$durationinterval[norac_expanded$variable == "ObservationCount3" & norac_expanded$abundance >= 1] <- "4-5min"
norac_expanded$distanceband <-"0m-INF"

#-- Point Count 6 Interval n=28571
data_flat_pc6 <- data_flat[data_flat$ProtocolCode =="Point Count 6 Interval",]
pc6_expanded <- melt(data_flat_pc6, measure.vars = c("ObservationCount2","ObservationCount3","ObservationCount4","ObservationCount5","ObservationCount6"), value.name = "abundance")
norac_expanded$durationinterval[norac_expanded$variable == "ObservationCount2" & norac_expanded$variable >= 1] <- "0-3min"
norac_expanded$durationinterval[norac_expanded$variable == "ObservationCount3" & norac_expanded$variable$abundance >= 1] <- "4-5min"
norac_expanded$distanceband <-"0m-INF"

# TEST
data_flat_pc6[is.na(data_flat_pc6$ObservationCount2),]
data_flat_pc6$test1 <- as.numeric(data_flat_pc6$ObservationCount2) + as.numeric(data_flat_pc6$ObservationCount3) +
  as.numeric(data_flat_pc6$ObservationCount4) + as.numeric(data_flat_pc6$ObservationCount5) + as.numeric(data_flat_pc6$ObservationCount6)

data_flat_pc6$test2 <- as.numeric(data_flat_pc6$ObservationCount) - data_flat_pc6$test1

unique(data_flat_pc6$test2)
data_flat_pc6[as.numeric(data_flat_pc6$test2) >0,]

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

