# Title: "Translate Ontario Forest Bird Monitoring Program 1987-2017"
# Source dataset is an excel spreadsheet
# Author: "Melina Houle"
# Date: "February 20, 2024"
# Note on translation:
# -- SpeciesID ==ZERO: No species observe. Delete 4 obs.
# -- Some species aren't define in the species list provided by the FBMP, but are described in other documentation and they fit the WildTrax definition. 
# -- in source data, Count is underestimate. Some column split the count into protocol. Protocol columns are filled but the total count haven't been reported in the count colum. 
# -- That do not affect our translation of count since when protocol is present, count is derived from the proper protocol column. 

#update.packages()
library(googlesheets4)
library(dplyr) # mutate, %>%
library(terra)
library(googledrive) #drive_get, drive_mkdir, drive_ls, drive_upload
library(stringr)
library(readr)

## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

organization <- "CWS-ONT"
dataset_code <- "ONFBMP1987-19"

project_dir <- file.path("./project", dataset_code)
if (!dir.exists(project_dir)) {
  dir.create(project_dir)
}
out_dir <- file.path("./out", dataset_code)   
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

# Lookup Table
WTpj_Tbl <- read_sheet("https://docs.google.com/spreadsheets/d/1fqifS_E5O_IpW1B-UG_xthr9hzY6FIek-nFjCrt1G0w", sheet = "project")
lu <- "./lookupTables"
WT_spTbl <- read.csv(file.path(lu, "species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path(lu, "duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path(lu, "distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path(lu, "duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path(lu, "distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

#--------------------------------------------------------------
#       LOAD
#--------------------------------------------------------------
if (length(list.files(project_dir)) ==0) {
  pid <- WTpj_Tbl %>%
    filter(dataset_code =="ONFBMP1987-19") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  detection_8717 <- gd.list %>%
    filter(name =="m2.alljoined.csv") %>%
    select("id")
  drive_download(as_id(as.character(detection_8717)), path = file.path(project_dir, "m2.alljoined.csv"))
  detection_2018 <- gd.list %>%
    filter(name =="m2_2018.alljoined.csv") %>%
    select("id")
  drive_download(as_id(as.character(detection_2018)), path = file.path(project_dir, "m2_2018.alljoined.csv"))
  detection_2019 <- gd.list %>%
    filter(name =="m2_2019.alljoined.csv") %>%
    select("id")
  drive_download(as_id(as.character(detection_2019)), path = file.path(project_dir, "m2_2019.alljoined.csv"))
  speciesList <- gd.list %>%
    filter(name =="FBMP_species_list.csv") %>%
    select("id")
  drive_download(as_id(as.character(speciesList)), path = file.path(project_dir, "FBMP_species_list.csv"))
}

detection_87 <- read.csv(file.path(project_dir, "m2.alljoined.csv"), fileEncoding="UTF-8-BOM")
detection_18 <- read.csv(file.path(project_dir, "m2_2018.alljoined.csv"), fileEncoding="UTF-8-BOM")
detection_19 <- read.csv(file.path(project_dir, "m2_2019.alljoined.csv"), fileEncoding="UTF-8-BOM")
#Append
detection <- rbind.fill(detection_87, detection_18, detection_19)

# load species list
speciesList <- read.csv(file.path(project_dir, "FBMP_species_list.csv"), fileEncoding="UTF-8-BOM")

#--------------------------------------------------------------
#
#       Fix species list
#
#--------------------------------------------------------------
# create specieslist
spIDdetect <- unique(detection$SpeciesID) 

spToCheck <- subset(spIDdetect, !(spIDdetect %in% speciesList$SpeciesID))
#"GBHE" "NESP" "GRHE" "RNEP" "NOFL" "UNBU"  

species_check <- merge(speciesList, WT_spTbl, by.x ="SpeciesID", by.y ="species_code", all.x = TRUE)
species_diff <- species_check %>%
  filter(English_Name != species_common_name | is.na(species_common_name)) %>%
  select(SpeciesID, English_Name, species_common_name)

# Fix detection prior to strat processing
detection <- detection %>%
  mutate(SpeciesID = case_when(SpeciesID == "GNBH" ~ "GRHE",
                               SpeciesID == "GTBH" ~ "GBHE",
                               SpeciesID == "RPHE" ~ "RNEP",
                               SpeciesID == "SCJU" ~ "DEJU",
                               SpeciesID == "STSP" ~ "NESP",
                               SpeciesID == "YSFL" ~ "NOFL",
                               TRUE ~ SpeciesID),)

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION           ####
############################
#Format
detection <- detection[!is.na(detection$Latitude),]
detection <- detection[!is.na(detection$Longitude),]
detection <- detection[detection$SpeciesID != "ZERO",]

# REPROJECT
spLocation <- vect(detection, geom=c("Longitude", "Latitude"), crs="epsg:4269")
spLocation_pj <- as.data.frame(project(spLocation,"EPSG:4326"),  geom = "XY") # WILDTRAX

pc_location <- spLocation_pj %>%
  dplyr::rename(longitude = x,
                latitude = y) %>%
  mutate(location = paste(dataset_code, SiteNumb_Station, sep= ":"))

############################
#### VISIT TABLE ####
############################
pc_visit <- pc_location %>% 
  select(location, longitude, latitude, Station, SiteNumber, Easting, Northing, UTM_Zone, Date, Time, VolunteerID, SpeciesID, Count, First5_In, First5_Out, Second5_In, Second5_Out, Year) %>% 
  mutate(visitDate = as.character(Date),
         rawObserver = as.character(VolunteerID),
         observer = paste0("obs",VolunteerID),
         time2 = as.character(as.POSIXct(Time, format = "%H:%M:%S")),
         survey_time = format(as.POSIXct(time2), format = "%H:%M:%S"),
         snowDepthMeters= NA,
         waterDepthMeters = NA,
         crew = NA,
         bait = "NONE",
         accessMethod = NA,
         landFeatures = NA,
         wildtrax_internal_update_ts = NA,
         wildtrax_internal_lv_id = NA,
         comments = NA,
         utmZone = UTM_Zone,
         time_zone = NA,       
         data_origin = NA,
         missinginvisit = NA,
         survey_year = sub("\\-.*", "", visitDate))

############################
#### SURVEY TABLE ####
############################
survey <- pc_visit %>% 
  mutate(organization = organization,
         project = dataset_code,
         original_species = SpeciesID,
         surveyDateTime = paste(visitDate, survey_time),
         pkey_dt = paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), observer, sep=":"),
         raw_distance_code = NA,
         raw_duration_code = NA,
         durationMethod = "0-5-10min",
         distanceMethod = "0m-100m-INF", 
         species = SpeciesID, 
         scientificname = WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)],
         missingindetections = NA,
         isHeard = NA,
         isSeen = NA)

## Split df according to protocol. 
#no info on protocol
pc_survey_noinfo <- survey %>%
  filter(First5_In ==0 & First5_Out ==0 & Second5_In ==0 & Second5_Out ==0)

pc_survey_noinfo <- pc_survey_noinfo %>% 
  mutate(durationinterval = "UNKNOWN",
         distanceband = "UNKNOWN",
         ind_count = Count
  ) 

# with protocol
pc_survey_winfo <- survey %>%
  filter(First5_In !=0 | First5_Out !=0 | Second5_In !=0 | Second5_Out !=0)


ff<- pc_survey_winfo %>% 
  mutate(test = First5_In + First5_Out + Second5_In + Second5_Out) %>%
  select(pkey_dt, species, Count, test) %>%
  filter(Count != test)

subset(detection, detection$SiteNumb_Station =="FBMP_248_1" & detection$Date =="2018-06-06" & detection$SpeciesID =="SCTA")




detection_expanded <- melt(as.data.table(pc_survey_winfo), measure.vars = c("First5_In","First5_Out", "Second5_In","Second5_Out"), value.name = "ind_count")
detection_expanded <- subset(detection_expanded, detection_expanded$ind_count>0)
detection_expanded <- detection_expanded %>%
  mutate(durationinterval = case_when(variable == "First5_In" ~ "0-5min",
                                      variable == "First5_Out" ~ "0-5min",
                                      variable == "Second5_In" ~ "5-10min",
                                      variable == "Second5_Out" ~ "5-10min"),
         distanceband = case_when(variable == "First5_In" ~ "0m-100m",
                                  variable == "First5_Out" ~ "100m-INF",
                                  variable == "Second5_In" ~ "0m-100m",
                                  variable == "Second5_Out" ~ "100m-INF"))
#-- Extended 
data_bind <- rbind.fill(pc_survey_noinfo, detection_expanded)

pc_survey <- data_bind %>%
  mutate(originalBehaviourData = NA,
         age = NA,
         fm = NA,
         group = NA,
         flyover = NA,
         nestevidence = NA,
         displaytype = NA,
         pc_vt = NA,
         pc_vt_detail = NA,
         behaviourother = NA,
         comments = NA,
         site = SiteNumber,
         station = Station,
         easting= Easting ,
         northing = Northing,
         original_species= NA,
         raw_distance_code = variable,
         raw_duration_code=variable, 
         originalBehaviourData= NA)

## CHECK
print(unique(pc_survey$distanceMethod[!(pc_survey$distanceMethod %in% WT_distMethTbl$distance_method_type)]))
print(unique(pc_survey$durationMethod[!(pc_survey$durationMethod %in% WT_durMethTbl$duration_method_type)]))
print(unique(pc_survey$species[!(pc_survey$species %in% WT_spTbl$species_code)]))
print(unique(pc_survey$original_species[!(pc_survey$species %in% WT_spTbl$species_code)]))

print(unique(pc_survey$durationinterval[!(pc_survey$durationinterval %in% WT_durBandTbl$duration_interval_type)]))
print(unique(pc_survey$distanceband[!(pc_survey$distanceband %in% WT_distBandTbl$distance_band_type)]))

#--------------------------------------------------------------
#
#       EXPORT
#
#--------------------------------------------------------------
dr<- drive_get(paste0("toUpload/",organization), shared_drive= "BAM_Core")

#Set GoogleDrive id
if (nrow(drive_ls(as_id(dr), pattern = dataset_code)) == 0){
  dr_dataset_code <-drive_mkdir(dataset_code, path = as_id(dr), overwrite = NA)
} else {
  dr_dataset_code <- drive_ls(as_id(dr), pattern = dataset_code)
}
dr_ls <- drive_ls(as_id(dr), pattern = dataset_code)

#---LOCATION
WTlocation <- c("location", "latitude", "longitude")

# Remove duplicated location
location_tbl <- pc_survey[!duplicated(pc_survey[,WTlocation]), WTlocation] 
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), row.names = FALSE, na = "")
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 

#---VISIT
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- pc_survey[!duplicated(pc_survey[,WTvisit]), WTvisit] # 

write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), row.names = FALSE, na = "")
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 

#---SURVEY
survey_tbl <- pc_survey %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband, durationinterval, isHeard, isSeen, comments) %>%
  dplyr::summarise(abundance = sum(ind_count), .groups= "keep")

write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), row.names = FALSE, na = "")
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv"), overwrite = TRUE) 

#---EXTENDED
extended_tbl <- pc_survey %>% 
  group_by(organization, project,location, surveyDateTime, species, ind_count, distanceband, durationinterval, site, station, utmZone, easting, 
           northing, time_zone, data_origin, missinginvisit, pkey_dt, survey_time,
           survey_year, rawObserver, original_species, scientificname, raw_distance_code, raw_duration_code, 
           originalBehaviourData, missingindetections, pc_vt, pc_vt_detail,age, fm, group, flyover, 
           displaytype, nestevidence, behaviourother) %>%
  dplyr::summarise(abundance = sum(ind_count), .groups= "keep")

write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_extended.csv")), quote = FALSE, row.names = FALSE, na = "")
extended_out <- file.path(out_dir, paste0(dataset_code,"_extended.csv"))
drive_upload(media = extended_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_extended.csv"), overwrite = TRUE) 

#---PROCESSING STATS
write_lines(paste0("Organization: ", organization), file.path(out_dir, paste0(dataset_code, "_stats.csv")))
write_lines(paste0("Project: ", dataset_code), file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
nrow_location <- paste0("Number of locations: ", nrow(location_tbl))
write_lines(nrow_location, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
nrow_visit <- paste0("Number of visit: ", nrow(visit_tbl))
write_lines(nrow_visit, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
nrow_survey <- paste0("Number of survey: ", nrow(survey_tbl))
write_lines(nrow_survey, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)
nrow_extended <- paste0("Number of extended: ", nrow(extended_tbl))
write_lines(nrow_extended, file.path(out_dir, paste0(dataset_code, "_stats.csv")), append= TRUE)


WTextended <- c("organization", "project","location", "surveyDateTime", "species", "ind_count", 
                "distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
                "northing", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
                "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", 
                "raw_duration_code",  "originalBehaviourData", "missingindetections", "pc_vt", 
                "pc_vt_detail","age", "fm", "group", "flyover", "displaytype", "nestevidence", "behaviourother")

bbb <- pc_survey[duplicated(pc_survey[,WTextended]), WTextended]

subset (pc_survey, pc_survey$pkey_dt =="ONFBMP1987-19:FBMP_210_1:20180531_NA:obs62" & pc_survey$species == "REVI")
subset (survey, survey$pkey_dt =="ONFBMP1987-19:FBMP_210_1:20180531_NA:obs62" & survey$species == "REVI")
subset(detection, detection$Date == "2018-05-31" & detection$SiteNumb_Station == "FBMP_210_1" & detection$SpeciesID=="REVI")
