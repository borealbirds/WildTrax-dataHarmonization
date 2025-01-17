# ---
# title: "Translate LSLBO data"
# author: Siu Chung WU, Diego
# date: "January 6, 2025"
# Note on translation:
# metadata: https://docs.google.com/document/d/1vA6qnp6zGH8SvK8JN6AXAOGsVDePkLJW/edit
# library(reshape2) applied to melt the data - distanceband marked in different row in original data "TimeBand1", "TimeBand2", "TimeBand3"...
# 'Station' : NA
# new durationMethod = "0-2-4-6-8-10min" / durationinterval to be added
# species code "HOWR" is thought and transfer as "NHWR", (according to "LSLBO-Vanderwell2018-21" dataset) 
# "master_observer" sheet updated -> Should we add a column named 'update date'? so that we can track the last updat, and avoid accidental delete
# how to rename the 'behaviour' table?

library(googledrive)
library(tidyr)
library(readr)
library(chron)
library(readxl)
library(plyr) 
library(dplyr)
library(sf)
library(lubridate)
library(stringr)
library(googlesheets4)
library(reshape2) # melt

## URL
url <- "https://drive.google.com/drive/u/1/folders/1zwPL6m9rN0uxRQQNpIJEpwDCccSqS0vW"

## Initialize variables
wd <- "C:/Users/asito/Desktop/ModellingProject/#BAM/WildTrax_Integration"
organization = "LSLBO"
dataset_code = "TOSL2017"
setwd(file.path(wd))

drive_auth()
WTpj_Tbl <- read_sheet("https://docs.google.com/spreadsheets/d/1fqifS_E5O_IpW1B-UG_xthr9hzY6FIek-nFjCrt1G0w", sheet = "project")
observer_Tbl <-  read_sheet("https://docs.google.com/spreadsheets/d/1ffV8T2Q6XWossIRmZR7Mtd0WbxW3tLSeDv7vchicdvs", sheet = "master_observer.csv")

lu <- "./lookupTables"
WT_spTbl <- read.csv(file.path(lu, "species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path(lu, "duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path(lu, "distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path(lu, "duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path(lu, "distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

project <- file.path("./project", dataset_code)
if (!dir.exists(project)) {
  dir.create(project, recursive = TRUE)
}

dataDir <- file.path(wd,"project",dataset_code,"data")   # where files would be downloaded
if (!dir.exists(dataDir)) {
  dir.create(dataDir, recursive = TRUE)
}

out_dir <- file.path(wd, "out", dataset_code)    # where output data frame will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE) 
}


#--------------------------------------------------------------
#
#       DOWNLOAD FILE FROM DRIVE 
#
#--------------------------------------------------------------

if (length(list.files(dataDir)) ==0) {
  pid <- WTpj_Tbl %>%
    filter(dataset_code =="TOSL2017") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  location <- gd.list %>%
    filter(name =="Slave Lake Point Counts 2017.xlsx") %>%
    select("id")
  drive_download(as_id(as.character(location)), path = file.path(dataDir, "Slave Lake Point Counts 2017.xlsx"))
}

observation <- read_excel(file.path(dataDir, "Slave Lake Point Counts 2017.xlsx"), sheet = 1)
meta <- read_excel(file.path(dataDir, "Slave Lake Point Counts 2017.xlsx"), sheet = 2)
location <- read_excel(file.path(dataDir, "Slave Lake Point Counts 2017.xlsx"), sheet = 3)

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------

############################
#### LOCATION TABLE ####
############################
data_flat <- observation 
data_flat <- merge(data_flat, meta, 
                   by.x = c('Point','Day','Month','Year'), 
                   by.y = c('Point','Day','Month','Year'), 
                   all.x = TRUE)

View(data_flat)  # 434 obs. of  40 variables

data_flat <- merge(data_flat, location, 
                   by.x = c('Point'), 
                   by.y = c('Point'), 
                   all.x = TRUE)

print(str(data_flat))  # 434 obs. of  47 variables


# Find rows in observation that were not joined with data_flat 
# unmatched_rows <- anti_join(data_flat, observation, by = "Point")
# print(unmatched_rows) 

data_flat <- data_flat %>%
  mutate(organization = organization,
         project = dataset_code,
         station = NA,
         location = paste(dataset_code, Point, sep=":"),
         longitude = Long,
         latitude = Lat) %>%
  rename(site = Point,
         comments = Comments.x)

data_flat <- data_flat %>%
  mutate(elevationMeters = NA,
         bufferRadiusMeters = NA,
         isHidden = NA,
         trueCoordinates = NA,
         internal_wildtrax_id = NA,
         internal_update_ts = NA,
         utmZone	= NA,
         easting	= NA,
         northing	= NA,
         missinginlocations = NA)


# options(digits = 7)
# View(location_tbl)

############################
#### VISIT TABLE ####
############################

data_flat <- data_flat %>%
  mutate(visitDate = format(as.Date(with(data_flat, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")),
         snowDepthMeters = NA,
         waterDepthMeters = NA,
         crew = NA,
         bait = "None",
         accessMethod = NA,
         landFeatures = HabitatVariable1,
         wildtrax_internal_update_ts = NA,
         wildtrax_internal_lv_id = NA,
         time_zone = NA,
         data_origin = dataset_code,
         missinginvisit = NA,
         survey_year = sub("\\-.*", "", visitDate),
         survey_time = sub(".*\\s", "", Start),
         surveyDateTime = paste(visitDate, survey_time),
         observer = case_when(Observer == "RNP"  ~ "obs01",
                              Observer == "NDK" ~ "obs02",
                              is.na(Observer)   ~ "NA"),
         pkey_dt= paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), observer, sep=":")
  ) 

str(data_flat)  # 434 obs. of  80 variables 

################################
#### Update master_observer ####
################################
unique_observers <- data_flat %>%
  select(Observer, observer) %>% 
  distinct() %>%
  filter(!is.na(Observer)) %>% # Exclude rows where Observer is NA
  mutate(
    observer_name = Observer,
    observer_id = observer
  )

# Create the append_obs data frame
append_obs <- unique_observers %>%
  select(observer_id, observer_name) %>%
  mutate(
    organization = "LSLBO",
    project = dataset_code
  ) %>%
  select(organization, project, observer_id, observer_name)


# Identify rows in append_obs that are not in observer_Tbl
new_rows <- anti_join(append_obs, observer_Tbl, 
                      by = c("organization", "project", "observer_id", "observer_name"))

# # Combine new rows with the existing observer_Tbl
if(nrow(new_rows)>1){
  updated_observer_Tbl <- bind_rows(observer_Tbl, new_rows)
  write_csv(updated_observer_Tbl, file.path(wd, "master_observer.csv"), append = FALSE)
  dr<- drive_get("WildTrax-dataHarmonization", shared_drive = "BAM_Core")
  observer_Tbl <- file.path(wd, "master_observer.csv")
  drive_upload(media = observer_Tbl, path = as_id(dr), name = "master_observer", type = "spreadsheet", overwrite = TRUE)
}

############################
#### SURVEY TABLE ####
############################

any(is.na(data_flat$Spp)) # Should be FALSE
# unique(data_flat$Spp[(data_flat$Spp %in% WT_spTbl$species_code)])   #51 species NOT in WildTrax
# unique(data_flat$Spp[!(data_flat$Spp %in% WT_spTbl$species_code)])  #1 species NOT in WildTrax

# extract abbreviation with no match 
missABV <- unique(data_flat$Spp[!(data_flat$Spp %in% WT_spTbl$species_code)])
print(missABV)

# extract the common name and scientific name of unmatched species code
miss_species <- data_flat %>%
  filter(Spp %in% missABV) %>%
  # select(Spp, Common.AOU2021, Scientific.AOU2021) %>%
  distinct()
print(miss_species$Spp)

# search matched common name and scientific name in WildTrax species list
# print(WT_spTbl %>% filter(species_common_name %in% miss_species$Common.AOU2021)) 
# print(WT_spTbl %>% filter(scientific_name %in% miss_species$Scientific.AOU2021)) 


# determine appropriate behaviour by unique(data_flat$Status)
View(data_flat)

data_expanded <- melt(data_flat, measure.vars = c("Time1Band1", "Time1Band2", "Time1Band3", "Time1Over",
"Time2Band1", "Time2Band2", "Time2Band3", "Time2Over",
"Time3Band1", "Time3Band2", "Time3Band3", "Time3Over",
"Time4Band1", "Time4Band2", "Time4Band3", "Time4Over",
"Time5Band1", "Time5Band2", "Time5Band3", "Time5Over"), value.name = "ind_count")
# original number: 434
# str(data_expanded)  # 8680 (434x20) obs. of  62 (80-20+2) variables

data_expanded$ind_count <- as.numeric(data_expanded$ind_count)

data_expanded <- data_expanded %>%
  filter(ind_count>0) %>% #delete false 0 created by melt
  mutate(distanceMethod = "0m-50m-100m-INF",
         distanceband = case_when(
           grepl("Band1$", variable) ~ "0m-50m",
           grepl("Band2$", variable) ~ "50m-100m",
           grepl("Band3$", variable) ~ "100m-INF",
           grepl("Over$", variable) ~ "UNKNOWN",
           TRUE ~ NA_character_
         ),
         durationMethod = "0-2-4-6-8-10min",
         durationinterval = case_when(
           grepl("^Time1", variable) ~ "0-2min",
           grepl("^Time2", variable) ~ "2-4min",
           grepl("^Time3", variable) ~ "4-6min",
           grepl("^Time4", variable) ~ "6-8min",
           grepl("^Time5", variable) ~ "8-10min",
           TRUE ~ "0-10min"
         ),
         species = case_when(Spp =="HOWR" ~ "NHWR",
                             TRUE ~ Spp),
         isHeard = "DNC",
         isSeen = "DNC"
         )
str(data_expanded) # 577 obs. of  69 variables

# validations
any(is.na(data_flat$ind_count)) # Should be FALSE
print(unique(data_expanded$durationinterval[(data_expanded$durationinterval %in% WT_durBandTbl$duration_interval_type)]))
print(unique(data_expanded$distanceband[(data_expanded$distanceband %in% WT_distBandTbl$distance_band_type)]))
print(unique(data_expanded$species[!(data_expanded$species %in% WT_spTbl$species_code)]))

# # create 'isDuplicate' column for signaling the duplicated column , *For information only
# WTspecies <- c("surveyDateTime", "location", "species", "distanceband", "durationinterval")
# duplicates <- duplicated(data_flat[WTspecies]) | duplicated(data_flat[WTspecies], fromLast = TRUE)
# data_flat$isDuplicate <- duplicates
# print(data_flat[data_flat$isDuplicate == TRUE, c("surveyDateTime", "location", "distanceband", "durationinterval", "species", "observer", "ind_count", "isDuplicate")])
# print(nrow(data_flat[data_flat$isDuplicate == TRUE, ])) # 1532 duplicated record


############################
#### BEHAVIOR TABLE ####
############################

data_expanded <- data_expanded %>%
  mutate(rawObserver  = Observer,
         original_species = Spp,
         scientificname = WT_spTbl$scientific_name[match(data_expanded$species, WT_spTbl$species_code)],
         raw_distance_code = variable,
         raw_duration_code = variable,
         originalBehaviourData = NA,
         missingindetections = "DNC",
         pc_vt = "DNC", 
         pc_vt_detail = NA,
         age = NA,
         fm = NA,
         group = case_when(
           ind_count >=5 ~ "Yes",
           TRUE ~ "No"
         ),
         flyover = case_when(
           grepl("Over$", variable) ~ "Yes",
           TRUE ~ "No"
         ),
         displaytype = "DNC",
         nestevidence = "DNC",
         behaviourother = "DNC",
         atlas_breeding_code = "DNC"
  )

############################
##EXPORT
############################

# Create sub folder in 'toUpload' with the organization name
dr<- drive_get("toUpload/", shared_drive = "BAM_Core")
to_upload_contents <- drive_ls(as_id(dr)) # print(to_upload_contents)
cws_folder <- to_upload_contents[to_upload_contents$name == organization, ]
if (nrow(cws_folder) == 0) {
  cws_folder <- drive_mkdir(organization, path = as_id(dr))
}

# Create sub folder in 'toUpload/organisation' with the dataset name
dr<- drive_get(paste0("toUpload/",organization), shared_drive = "BAM_Core")
beh_dr<- drive_get("behavior/", shared_drive = "BAM_Core")
folder_list <- drive_ls(as_id(dr), pattern = dataset_code) # print(folder_list)

if (nrow(folder_list[folder_list$name == dataset_code, ]) == 0){
  dr_dataset_code <-drive_mkdir(dataset_code, path = as_id(dr), overwrite = NA)
  print(paste("Folder", dataset_code, "created successfully."))
} else {
  dr_dataset_code <- folder_list[folder_list$name == dataset_code, ]
} # print(drive_ls(as_id(dr)))


# flyover (flying) data filtered out
no_flyover<- data_expanded %>%
  filter(!flyover == "Yes")
View(no_flyover)

yes_flyover<- data_expanded %>%
  filter(flyover == "Yes")
View(yes_flyover)


#---LOCATION
# Remove duplicated location
WTlocation <- c("location", "longitude", "latitude")
location_tbl <- no_flyover[!duplicated(no_flyover[,WTlocation]), WTlocation]
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), row.names = FALSE, na = "")
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 


#---VISIT
# Delete duplicated based on WildtTrax attributes (double observer on the same site, same day).
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")
visit_tbl <- no_flyover[!duplicated(no_flyover[,WTvisit]), WTvisit] # 
write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), row.names = FALSE, na = "")
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 


#---SURVEY
# Delete duplicated in the eye of the survey table
survey_tbl <- no_flyover %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband, durationinterval, isHeard, isSeen, comments) %>%
  dplyr::summarise(abundance = sum(ind_count), .groups= "keep")

WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen", "comments")
survey_tbl <- survey_tbl[!duplicated(survey_tbl[,WTsurvey]), WTsurvey] # 
write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), row.names = FALSE, na = "")
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv"), overwrite = TRUE) 

# below lines to highlight the grouped lines, which are not selected in survey_tbl 
# x <- no_flyover %>% 
#  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband, durationinterval, isHeard, isSeen, comments) %>%
#  dplyr::summarise(abundance = sum(ind_count), .groups= "keep") %>%
#  ungroup()
# View(x[x$abundance != x$ind_count, ]) # visualized rows with grouped ind_count


#---EXTENDED
# "atlas_breeding_code" still kept
# the operation of grouping "ind_count" into "abundance" is not done
Extended <- c("organization", "project", "location", "surveyDateTime", "species", "ind_count", "distanceband", "durationinterval", "site", "station", "utmZone", "easting",
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code",
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover",
              "displaytype", "nestevidence", "behaviourother", "atlas_breeding_code")

extended_tbl <- data_expanded[!duplicated(data_expanded[,Extended]), Extended]
write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_behavior.csv")), na = "", row.names = FALSE)
extended_out <- file.path(out_dir, paste0(dataset_code,"_behavior.csv"))
drive_upload(media = extended_out, path = as_id(beh_dr), name = paste0(dataset_code,"_behavior.csv"), overwrite = TRUE)


# ---PROCESSING STATS, no. of locations, visits, and surveys
file_name <- file.path(out_dir, paste0(dataset_code, "_stats.csv"))
con <- file(file_name, open = "a")
writeLines(paste0("Organization: ", organization), con)
writeLines(paste0("Project: ", dataset_code), con)
nrow_location <- paste0("Number of locations: ", nrow(location_tbl))
writeLines(nrow_location, con)
nrow_visit <- paste0("Number of visit: ", nrow(visit_tbl))
writeLines(nrow_visit, con)
nrow_survey <- paste0("Number of survey: ", nrow(survey_tbl))
writeLines(nrow_survey, con)
close(con)
