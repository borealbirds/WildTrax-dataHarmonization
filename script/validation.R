####VALIDATION
library(tidyverse)
library(dplyr)
library(plyr) #rbind.fill
library(reshape2) # melt
library(readxl)
library(googledrive)
library(RODBC)
library(googlesheets4)
library(remotes)
remotes::install_github("ABbiodiversity/wildrtrax")
library(wildrtrax) #to download data from wildtrax

## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

fileType <- ".mdb"
source_data <- "Landbird_Surveys_2016.mdb"
pcode <- "NLBMS2011-16"
tbllocation <- "tblStations"
tblvisit <- "tblSurveyConditions"
tblsurvey <- "tblData_TimeDetection"
tblobserver <- "tblObservers"
tblspecies <- "tblBird_Names"


WT_projectName <-"Newfoundland-Labrador Landbird Surveys 2011-2016"


#set working folder
validation_dir <- file.path(wd, "validation", dataset_code)
if (!dir.exists(validation_dir)) {
  dir.create(validation_dir)
}

#--------------------------------------------------------------
#       LOAD SOURCE FILE 
#--------------------------------------------------------------
WTpj_Tbl <- read_sheet("https://docs.google.com/spreadsheets/d/1fqifS_E5O_IpW1B-UG_xthr9hzY6FIek-nFjCrt1G0w", sheet = "project")
if (length(list.files(validation_dir)) ==0) {
  pid <- WTpj_Tbl %>%
    filter(dataset_code ==pcode) %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  data_id <- gd.list %>%
    filter(name ==source_data) %>%
    select("id")
  drive_download(as_id(as.character(data_id)), path = file.path(validation_dir, source_data))
}

##    Connect according to source data
# A. Connecte and load tables
if (fileType ==".mdb"){
  data_db <- file.path(validation_dir, source_data)
  con <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",data_db))
  pc_location <- sqlFetch(con, tbllocation)
  pc_visit <- sqlFetch(con, tblvisit)
  pc_survey <- sqlFetch(con, tblsurvey)
  pc_observer <- sqlFetch(con, tblobserver)
  lu_species <- sqlFetch(con, tblspecies)
}
  
#--------------------------------------------------------------
#       LOAD WILDTRAX FILE 
#--------------------------------------------------------------
source("./config.R")
wt_auth()

#Retrieve project_id
WT_pj <- wt_get_download_summary(sensor_id = "PC")
pj_id <- WT_pj[WT_pj$project==WT_projectName, "project_id"]
WT_main <- wt_download_report(project_id = pj_id, sensor = "PC", weather_cols = F, report="main")
write.csv(WT_main, file.path(validation_dir, paste0("WT_", pcode, ".csv")))
#######################################################
##   Validation 
#######################################################
print(paste0("Number of source birds obs: ", pc_survey)
print(paste0("Number of WT birds obs: ",))












WT_spTbl <- read.csv(file.path("./lookupTables/species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path("./lookupTables/duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path("./lookupTables/distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path("./lookupTables/duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path("./lookupTables/distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

