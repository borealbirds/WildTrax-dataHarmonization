
library(tidyr)
library(readr)
library(chron)
library(readxl)
#####################################################
#SET params
# USGS URL

url <- "https://www.sciencebase.gov/catalog/file/get/5ea04e9a82cefae35a129d65"
pFifty <- "?f=__disk__40%2Fe4%2F92%2F40e4925dde30ffd926b1b4d540b485d8a9a320ba"
pweather <- "?f=__disk__87%2Fb5%2F1d%2F87b51d999ae1ad18838aa60851e9bcff4498ac8d"

## Initialize variables
dataset_code = "BAM-BBS"
wd <- "E:/MelinaStuff/BAM/dataImport"
lu <- "E:/MelinaStuff/git/BAM/BAMTools/data_translation/lookupTables"
dataDir <- file.path(wd,"project",dataset_code,"data")           # where files would be downloaded
lookupDir <-file.path(wd,"lookupTables") # where lookup tables are found
outDir <- file.path(wd,"out")    # where output dataframe will be exported
XYtbl <- file.path(dataDir,"stopsXY_december2020.csv")

# Check params
if (!dir.exists(file.path(dataDir))) {
  dir.create(file.path(dataDir))
}

# Temporal range
minYear = 1966
maxYear = 2019

#Load lookup table
spList <- read_excel(file.path(lookupDir,"BBS_BAM-Avian-Species.xlsx"), sheet = "BAM_Avian_Species")
WT_spTbl <- read.csv(file.path(wd,"WT_PointCount_Codes/species_codes.csv"), header= TRUE)

#######################################################

##                    DOWNLOAD

#######################################################
# Weather file
download.file(paste0(url, pweather), file.path(dataDir,"weather.zip"), method = "auto", mode = "wb") 
unzip(file.path(dataDir,"weather.zip"), exdir= dataDir)
fweather <- file.path(dataDir,"weather.csv")

# 50 StopData
download.file(paste0(url, pFifty), file.path(dataDir,"50-StopData.zip"), method = "auto", mode = "wb") 
unzip(file.path(dataDir,"50-StopData.zip"), exdir= dataDir)
folderName <- sub("(.*)\\..*$","\\1", "50-StopData")
version <- list.files(file.path(dataDir,folderName))
flist <- list.files(file.path(dataDir,folderName,version))
lapply(flist,  function(x) {unzip(file.path(dataDir,folderName,version,x), exdir= dataDir)})

#######################################################

##                    TRANSLATE

#######################################################
##Load location
BBSstop <- read.csv(XYtbl, header = TRUE, sep=",")

##Load lookup table to extract Prov/States
#regionCodes <- read.csv(file = unlist(lookupTables[names(lookupTables)=='regionCodes']), header = TRUE, sep=",")

## Location attributes
data_flat <- BBSstop
names(data_flat)[names(data_flat) == 'SS'] <- 'location'
names(data_flat)[names(data_flat) == 'Y_Lat'] <- 'latitude'
names(data_flat)[names(data_flat) == 'X_Long'] <- 'longitude'

#Wildtrax field for uploader
data_flat$bufferRadiusMeters <- NA
data_flat$elevationMeters <- NA
data_flat$isHidden <- NA
data_flat$trueCoordinates <- NA
data_flat$comments_location <- NA
data_flat$internal_wildtrax_id <- NA
data_flat$internal_update_ts <- NA
# BAM keeper
data_flat$organization <- "Canada WildlifE Service and USGS"
names(data_flat)[names(data_flat) == "PCODE"] <- "dataset_code"
data_flat$site <- sub(".*[:]([^.]+)[:].*", "\\1", data_flat$location)
data_flat$station <- sub("(?:[^-]*:){2}(.*)", "\\1", data_flat$location)
data_flat$utm_zone <- NA
data_flat$easting <- NA
data_flat$northing <- NA  

##Load source weather file and subset temporally using year range and geographically using location 
fweather <- read.csv(fweather, header = TRUE)
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
data_flat <- merge(data_flat, weathergather, by.x= c("CountryNum", "StateNum", "Route", "station"), by.y= c("CountryNum", "StateNum", "Route", "Stops"), all.x = TRUE)
#**NOTE: There are 1650 stops location not found in weather. Those are non_active routes or routes planned that haven't been surveyed yet.

#survey attributes
data_flat$visitDate <- as.Date(with(data_flat, paste(Year,Month,Day, sep = "-")),format = "%Y-%m-%d")
data_flat$snowDepthMeters <- NA
data_flat$waterDepthMeters <- NA
data_flat$landFeatures <- NA
data_flat$crew <- NA
data_flat$bait <- NA
data_flat$accessMethod <- NA
data_flat$wildtrax_internal_update_ts <- NA
data_flat$wildtrax_internal_lv_id <- NA

# Generate time at stop level using StartTime and EndTime of route survey
# Mark NA as 0
data_flat$StartTime_BAM <- data_flat$StartTime
data_flat$EndTime_BAM <- data_flat$EndTime
data_flat$StartTime_BAM[is.na(data_flat$StartTime_BAM)] <- 0
data_flat$EndTime_BAM[is.na(data_flat$EndTime_BAM)] <- 0
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
##Protocol distance 22 (0-400m)
data_flat$distanceMethod <- "0m-400m"
##Protocol duration 4 (3 min)
data_flat$durationMethod <- "0-3min"

#Species
fifties = c("fifty1.csv","fifty2.csv","fifty3.csv","fifty4.csv","fifty5.csv","fifty6.csv","fifty7.csv","fifty8.csv","fifty9.csv","fifty10.csv")
outPtCount <- lapply(fifties, function(x){
  path2fifty = file.path(dataDir,x)
  fifty <- read.csv(file = path2fifty, header = TRUE)
  
  # Subset temporal and geographical range at the route level
  fifty <-subset(fifty, fifty$Year %in% rangeYear)
  fifty <-subset(fifty, fifty$CountryNum %in% data_flat$CountryNum & fifty$StateNum %in% data_flat$StateNum & fifty$Route %in% data_flat$Route)
  
  # RPID < 200 meet BBS weather, date, time, or route completion criteria
  fifty <-subset(fifty, fifty$RPID < 200)
  
  # if fifty not empty -> transpose
  if(!nrow(fifty)==0){
    fiftygather <- gather(fifty, key=stop_no, value = count, Stop1:Stop50)
    fiftygather$stop_no <- gsub("Stop", "", fiftygather$stop_no)
    ## Delete 0 abundance
    fiftygather<- fiftygather[which(fiftygather$count!=0), ] 
    #---------
    ## Merge to data_flat 
    #---------
    fifty_BAM <-merge(fiftygather, data_flat, by=c("CountryNum", "StateNum", "Route", "Year", "stop_no"))
    #Species
    fifty_BAM$species <- spList$WT_Species_Code[match(fifty_BAM$AOU, spList$BBS_Number)]
    fifty_BAM$scientificname <- WT_spTbl$scientific_name[match(fifty_BAM$species, WT_spTbl$species_code)]
    fifty_BAM$Species_Old <- fifty_BAM$AOU
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
    #---------------------
    
    outputName  <- c("organization", "dataset_code", "site", "station", "location", "utm_zone", "easting", "northing", "latitude", "longitude",
                     "bufferRadiusMeters", "elevationMeters", "isHidden", "trueCoordinates", "comments_location", "internal_wildtrax_id", "internal_update_ts",
                     "visitDate", "snowDepthMeters", "waterDepthMeters", "landFeatures", "crew", "bait", "accessMethod", "comments_visit", "wildtrax_internal_update_ts",
                     "wildtrax_internal_lv_id", "pkey_dt", "surveyDateTime", "survey_time", "observer", "distanceMethod", "durationMethod", 
                     "species", "Species_Old", "scientificname", "isHeard", "isSeen", "abundance", "distanceband", "durationinterval",
                     "comments") 
    out <- match(outputName, names(fifty_BAM))
    out_translated <- fifty_BAM[,out]
    
    return(out_translated)
    }
  })
  

out_translated <-do.call(rbind, outPtCount) # 1966-2019

##Export local copy
write.csv(out_translated, file.path(outDir, paste0(dataset_code, ".csv")))


## Export flat file to GoogleDrive
out_location <- drive_get("BBS/FLATtranslated")
drive_upload(file.path(outDir, paste0(dataset_code, ".csv")),path=as_id(out_location$id),type = 'spreadsheet', name = paste0(dataset_code, ".csv"), overwrite = TRUE)

