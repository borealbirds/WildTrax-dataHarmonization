# ---
# title: "Create species list "
# author: "Melina Houle"
# date: "November 7, 2022"
# Note:
#     The script need to run prior to do the translation in order to create the 
#     species conversion between AOU code and WT species_code. It loops thought 
#     all fifties files to extract AOU code. It then recovers WT species_code 
#     using English_Common_Name. Because of the syntax, 45 species don't get a  
#     WT species_code assigned. They need to be manually assigned.

library(googledrive)
library(tidyr)
library(readr)
library(chron)
library(readxl)
library(plyr) #rbind.fill
library(dplyr)
 

##USGS URL
#url <- "https://www.sciencebase.gov/catalog/file/get/5ea04e9a82cefae35a129d65"
#pFifty <- "?f=__disk__40%2Fe4%2F92%2F40e4925dde30ffd926b1b4d540b485d8a9a320ba"
#pweather <- "?f=__disk__87%2Fb5%2F1d%2F87b51d999ae1ad18838aa60851e9bcff4498ac8d"


## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
organization = "BAM"
dataset_code = "BAM-BBS"
setwd(file.path(wd))


lu <- "./lookupTables"
WT_spTbl <- read.csv(file.path(lu, "species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")

project <- file.path("./project", dataset_code)
dataDir <- file.path(wd,"project",dataset_code,"data")   # where files would be downloaded
out_dir <- file.path(wd, "out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

XYtbl <- file.path(dataDir,"stopsXY_july2022.csv")
f_weather <- file.path(dataDir,"weather.csv")
fiftystop <- file.path(dataDir,"50-StopData.zip")
#species_lu <- file.path(project, "lookupTables/BBS_BAM-Avian-Species.xlsx")
BBS_spTbl <- read.csv(file.path(project, "lookupTables/SpeciesList.csv"), header= TRUE)

#Download from GoogleDrive
if (!file.exists(XYtbl)) {
  drive_download("sourceData/stopsXY_july2022.csv", path = dataDir)
}
if (!file.exists(f_weather)) {
  drive_download("sourceData/weather.zip", path = f_weather)
  unzip(f_weather, exdir= dataDir)
  f_weather <- file.path(dataDir, "weather.csv")
} 
if (!file.exists(fiftystop)) {
  drive_download("sourceData/50-StopData.zip", path = fiftystop)
  unzip(file.path(dataDir,"50-StopData.zip"), exdir= dataDir)
  folderName <- sub("(.*)\\..*$","\\1", "50-StopData")
  version <- list.files(file.path(dataDir,folderName))
  flist <- list.files(file.path(dataDir,folderName,version))
  lapply(flist,  function(x) {unzip(file.path(dataDir,folderName,version,x), exdir= dataDir)})
  
}
if (!file.exists(species_lu)) {
  drive_download("lookupTable/BBS_BAM-Avian-Species.xlsx", path = species_lu)
}

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------

data_flat <- read.csv(XYtbl, header = TRUE, sep=",")
fweather <- read.csv(f_weather, header = TRUE)

##Load source weather file and subset temporally using year range and geographically using location 
# subset only those with lat/long
weather <-subset(fweather, fweather$CountryNum %in% data_flat$CountryNum & fweather$StateNum %in% data_flat$StateNum & fweather$Route %in% data_flat$Route)

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
data_flat <- merge(data_flat, weathergather, by.x= c("CountryNum", "StateNum", "Route", "Stop"), by.y= c("CountryNum", "StateNum", "Route", "stop_no"))

#Species
fifties <- c("fifty1.csv","fifty2.csv","fifty3.csv","fifty4.csv","fifty5.csv","fifty6.csv","fifty7.csv","fifty8.csv","fifty9.csv","fifty10.csv")
AOU_list <- c()
AOU_list <- lapply(fifties, function(x){
  path2fifty = file.path(dataDir,x)
  fifty <- read.csv(file = path2fifty, header = TRUE)
  # Subset temporal and geographical range at the route level using RouteDataID
  fifty <-subset(fifty, fifty$RouteDataID %in% data_flat$RouteDataID)
  AOU <- unique(fifty$AOU)
  print(length(AOU))
  AOU_list <- append(AOU_list, AOU)
  return(AOU_list)
})

out_AOU <-unlist(AOU_list) 
out_AOU <- unique(out_AOU)

BBS_spTbl <- subset(BBS_spTbl,BBS_spTbl$AOU %in% out_AOU)
BBS_spTbl$WT_species_code <- WT_spTbl$species_code[match(BBS_spTbl$English_Common_Name, WT_spTbl$species_common_name)]

write.csv(BBS_spTbl,file.path(dataDir, "speciesList.csv"), na = "", row.names = FALSE)


