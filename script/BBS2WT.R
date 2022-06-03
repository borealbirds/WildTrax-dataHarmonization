# ---
# title: "Translate BBS data"
# author: "Melina Houle"
# date: "Mai 26, 2022"
# Note on translation:
# -- RPID <200 : >200 are experimental
# -- QualityCurrentID = 1 : Meet BBS criteria on time, weather and route completion
# -- Only select one observer randomly when 2 observers recorded observation the same day, same site, same time
# -- Only select location where observations were recorded
# -- Only select visit where observation at stop level were recorded
# -- RouteDataID is a unique identifier of CountryNum, StateNum, Route, RPID and YEAR
# ---

library(tidyr)
library(readr)
library(chron)
library(readxl)

# USGS URL
url <- "https://www.sciencebase.gov/catalog/file/get/5ea04e9a82cefae35a129d65"
pFifty <- "?f=__disk__40%2Fe4%2F92%2F40e4925dde30ffd926b1b4d540b485d8a9a320ba"
pweather <- "?f=__disk__87%2Fb5%2F1d%2F87b51d999ae1ad18838aa60851e9bcff4498ac8d"


## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

dataset_code = "BAM-BBS"
lu <- "./lookupTables"
project <- file.path("./project", dataset_code)
dataDir <- file.path(wd,"project",dataset_code,"data")   # where files would be downloaded
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}
XYtbl <- file.path(dataDir,"stopsXY_december2020.csv")
f_weather <- file.path(dataDir,"weather.csv")


# Check params
if (!file.exists(file.path(XYtbl))) {
  stop("You must provide a XY table")
}

# Temporal range
minYear = 1966
maxYear = 2019

#Load lookup table
BBS_spTbl <- read_excel(file.path(lu,"BBS_BAM-Avian-Species.xlsx"), sheet = "BAM_Avian_Species")
WT_spTbl <- read.csv(file.path(lu,"species_codes.csv"), header= TRUE)

#######################################################

##                    DOWNLOAD

#######################################################
# Weather file
download.file(paste0(url, pweather), file.path(dataDir,"weather.zip"), method = "auto", mode = "wb") 
unzip(file.path(dataDir,"weather.zip"), exdir= dataDir)

# 50 StopData
download.file(paste0(url, pFifty), file.path(dataDir,"50-StopData.zip"), method = "auto", mode = "wb") 
unzip(file.path(dataDir,"50-StopData.zip"), exdir= dataDir)
folderName <- sub("(.*)\\..*$","\\1", "50-StopData")
version <- list.files(file.path(dataDir,folderName))
flist <- list.files(file.path(dataDir,folderName,version))
lapply(flist,  function(x) {unzip(file.path(dataDir,folderName,version,x), exdir= dataDir)})

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
BBSstop <- read.csv(XYtbl, header = TRUE, sep=",")

data_flat <- BBSstop
names(data_flat)[names(data_flat) == 'SS'] <- 'location'
names(data_flat)[names(data_flat) == 'Y_Lat'] <- 'latitude'
names(data_flat)[names(data_flat) == 'X_Long'] <- 'longitude'
data_flat$site <- sub(".*[:]([^.]+)[:].*", "\\1", data_flat$location)
data_flat$station <- sub("(?:[^-]*:){2}(.*)", "\\1", data_flat$location)
data_flat$elevationMeters <- NA
data_flat$bufferRadiusMeters <- NA
data_flat$isHidden <- NA
data_flat$trueCoordinates <- NA
data_flat$comments <- NA
data_flat$internal_wildtrax_id <- NA
data_flat$internal_update_ts <- NA

# If exists in source data
data_flat$utmZone	<- NA
data_flat$easting	<- NA
data_flat$northing	<- NA
data_flat$missinginlocations <- NA

#---LOCATION
WTlocation <- c("location", "latitude", "longitude", "bufferRadiusMeters", "elevationMeters", "isHidden", "trueCoordinates", "comments")
location_tbl <- data_flat[!duplicated(data_flat[,WTlocation]), WTlocation] # 

############################
#### VISIT TABLE ####
############################
##Load source weather file and subset temporally using year range and geographically using location 
fweather <- read.csv(f_weather, header = TRUE)
fweather <-subset(fweather, fweather$CountryNum %in% data_flat$CountryNum & fweather$StateNum %in% data_flat$StateNum & fweather$Route %in% data_flat$Route)
rangeYear <- c(minYear:maxYear)
weather <-subset(fweather, fweather$Year %in% rangeYear)
# Drop experimental protocol (RPID >199 are experimental) and survey that do not meet BBS weather, date, time, and route completion criteria (QualityCurrentID = 0)
weather <-subset(weather, weather$RPID < 200)
weather <-subset(weather, weather$QualityCurrentID ==1)

# Apply information of the route at the stop level and keep only the ones for which we have coordinates
### Add stop1:stop50 prior to transpose the matrix
x <- 1:50
namevector <- paste0("Stop", x)
weather[,namevector] <- NA
weathergather <- gather(weather, key=Stops, value =stop_no, Stop1:Stop50)
weathergather$stop_no<- parse_number(weathergather$Stops)
#data_flat <- merge(data_flat, weathergather, by.x= c("CountryNum", "StateNum", "Route", "station"), by.y= c("CountryNum", "StateNum", "Route", "Stops"), all.x = TRUE)
data_flat <- merge(data_flat, weathergather, by.x= c("CountryNum", "StateNum", "Route", "station"), by.y= c("CountryNum", "StateNum", "Route", "Stops"))
#**NOTE: There are 1650 stops location not found in weather. Those are non_active routes or routes planned that haven't been surveyed yet.
#survey attributes
data_flat$visitDate <- as.character(as.Date(with(data_flat, paste(Year,Month,Day, sep = "-")),format = "%Y-%m-%d"))
data_flat$snowDepthMeters <- NA
data_flat$waterDepthMeters <- NA
data_flat$crew <- NA
data_flat$bait <- NA
data_flat$accessMethod <- NA
data_flat$landFeatures <- NA
data_flat$comments <- "Include only visit with known XY locations"
data_flat$wildtrax_internal_update_ts <- NA
data_flat$wildtrax_internal_lv_id <- NA
data_flat$time_zone <- NA
data_flat$data_origin <- "USGS download"
data_flat$missinginvisit <- NA
data_flat$survey_year <- sub("\\-.*", "", data_flat$visitDate) 

# Generate time at stop level using StartTime and EndTime of route survey
# Mark NA as 0
data_flat$StartTime_BAM <- ifelse(is.na(data_flat$StartTime), 0, data_flat$StartTime)
data_flat$EndTime_BAM <- ifelse(is.na(data_flat$EndTime), 0, data_flat$EndTime)
# Convert Start and End time
leadTime_st <- mapply(function(x, y) paste0(rep(x, y, times = 1), collapse = ""), 0, 4 - nchar(data_flat$StartTime_BAM))
data_flat$StartTime_BAM <- paste0(leadTime_st, data_flat$StartTime_BAM)
data_flat$StartTime_BAM <- times(format(strptime(data_flat$StartTime_BAM, format="%H%M"), format = "%H:%M:%S"))
leadTime_et <- mapply(function(x, y) paste0(rep(x, y, times = 1), collapse = ""), 0, 4 - nchar(data_flat$EndTime_BAM))
data_flat$EndTime_BAM <- paste0(leadTime_et, data_flat$EndTime_BAM)
data_flat$EndTime_BAM <- times(format(strptime(data_flat$EndTime_BAM, format="%H%M"), format = "%H:%M:%S"))
### Time incremental for 50 stop (Only when QualityCurrentID ==1, i.e., meet BBS weather, date, time, or route completion criteria).
data_flat$time_int <- data_flat$EndTime_BAM - data_flat$StartTime_BAM
data_flat$time_inc <- data_flat$time_int / 50
data_flat$survey_time <- as.character(data_flat$StartTime_BAM + ((data_flat$stop_no - 1)*data_flat$time_inc))
# Assign survey_time =="00:01:01" when does not meet one or more BBS weather, date, time, or route completion criteria
data_flat$survey_time[is.na(data_flat$survey_time)] <- "00:01:01"
data_flat$surveyDateTime <- paste(data_flat$visitDate, data_flat$survey_time)
data_flat$comments_visit <- paste0("route time interval: ", data_flat$StartTime, "-", data_flat$EndTime)
##Observer (internal ID number)
data_flat$observer <- data_flat$ObsN
data_flat$pkey_dt<- paste(data_flat$location, paste0(gsub("-", "", as.character(data_flat$visitDate)),"_", gsub(":", "", data_flat$survey_time)), data_flat$observer, sep=":")

WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
data_flat <- data_flat[!duplicated(data_flat[,WTvisit]),] 
visit_tbl <- data_flat

############################
#### SURVEY TABLE ####
############################
#data_flat <- visit_tbl
##Protocol duration 4 (3 min)
data_flat$durationMethod <- "0-3min"
##Protocol distance 22 (0-400m)
data_flat$distanceMethod <- "0m-400m"
#Species
fifties = c("fifty1.csv","fifty2.csv","fifty3.csv","fifty4.csv","fifty5.csv","fifty6.csv","fifty7.csv","fifty8.csv","fifty9.csv","fifty10.csv")
outPtCount <- lapply(fifties, function(x){
  path2fifty = file.path(dataDir,x)
  fifty <- read.csv(file = path2fifty, header = TRUE)
  # Subset temporal and geographical range at the route level using RouteDataID
  fifty <-subset(fifty, fifty$RouteDataID %in% data_flat$RouteDataID)

  # if fifty not empty -> transpose
  if(!nrow(fifty)==0){
    fiftygather <- gather(fifty, key=stop_no, value = count, Stop1:Stop50)
    fiftygather$stop_no <- gsub("Stop", "", fiftygather$stop_no)
    ## Delete 0 abundance
    fiftygather<- fiftygather[which(fiftygather$count!=0), ] 
    #---------
    ## Merge to data_flat 
    #---------
    fifty_BAM <-merge(data_flat, fiftygather, by=c("RouteDataID", "stop_no"))
    #Species
    fifty_BAM$species <- BBS_spTbl$WT_Species_Code[match(fifty_BAM$AOU, BBS_spTbl$BBS_Number)]
    fifty_BAM$scientificname <- WT_spTbl$scientific_name[match(fifty_BAM$species, WT_spTbl$species_code)]
    fifty_BAM$original_species <- fifty_BAM$AOU
    #Distance (0-400m)
    fifty_BAM$distanceband <- "0m-400m"
    #Duration (3mins)
    fifty_BAM$durationinterval <- "0-3min"
    #ABUND
    fifty_BAM$abundance <- fifty_BAM$count
    #HEARD
    fifty_BAM$isHeard <- "DNC"
    #SEEN
    fifty_BAM$isSeen <- "DNC"
    #comments
    fifty_BAM$comments <- ""
    fifty_BAM$rawObserver <- fifty_BAM$ObsN
    fifty_BAM$raw_distance_code <- "During the count, every bird seen within a 0.25-mile radius or heard is recorded."
    fifty_BAM$raw_duration_code <- "At each stop, a 3-minute point count is conducted."
    fifty_BAM$originalBehaviourData <- NA
    fifty_BAM$missingindetections <- NA
    fifty_BAM$pc_vt <- "DNC"
    fifty_BAM$pc_vt_detail <- "DNC"
    fifty_BAM$age <- "DNC"
    fifty_BAM$fm <- "DNC"
    fifty_BAM$group <- "DNC"
    fifty_BAM$flyover <- "DNC"
    fifty_BAM$displaytype <- "DNC"
    fifty_BAM$nestevidence <- "DNC"
    fifty_BAM$behaviourother <- "DNC"
    fifty_BAM$atlas_breeding_code <- "DNC"
    
    #---------------------
    
    return(fifty_BAM)
    }
  })
data_flat <-do.call(rbind, outPtCount) # 1966-2019


############################
##EXPORT
############################
#---SURVEY
WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen", "comments")
survey_tbl <- data_flat[!duplicated(data_flat[,WTsurvey]), WTsurvey] 
write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), row.names = FALSE)

#Only select visit with observation
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")
visit_tbl <- visit_tbl[visit_tbl$pkey_dt %in% data_flat$pkey_dt, WTvisit]
write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), row.names = FALSE)

location_tbl <- subset(location_tbl,location_tbl$location %in% survey_tbl$location)
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), row.names = FALSE)

#---EXTENDED
Extended <- c("location", "surveyDateTime", "species", "utmZone", "easting", "northing", "missinginlocations", "time_zone", 
              "data_origin", "missinginvisit", "pkey_dt", "survey_time", "survey_year", "rawObserver", "original_species", 
              "scientificname", "raw_distance_code", "raw_duration_code", "originalBehaviourData", "missingindetections", 
              "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", "displaytype", "nestevidence", "behaviourother", 
              "atlas_breeding_code")
extended_tbl <- data_flat[!duplicated(data_flat[,Extended]), Extended] 
write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_extended.csv")), row.names = FALSE)



