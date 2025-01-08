# ---
# title: "Translate LSLBO data"
# author: Siu Chung WU, Diego
# date: "December 13, 2024"
# Note on translation:
# metadata: https://docs.google.com/document/d/1pJQ1qsW_31QMRTCek-6yE72VcKU8dja_/edit
# Only Site number, No Station number -> apply Station number to Site number
# dataset_code with "_", should we change it?
# in original data, 1 row can record more than 1 bird in two distance band, extra operation made to split those rows 
# few rows record 'displaytype' in the column 'Status' with 'D' or 'DD', 'displaytype' of those row become 'Yes'
# 'flyover' is not separated from 'observed' in original record

library(googledrive)
library(googlesheets4)
library(tidyr)
library(readr)
library(chron)
library(readxl)
library(plyr) 
library(dplyr)
library(sf)
library(lubridate)
library(stringr)
source("./config.R")


## Initialize variables
organization = "LSLBO"
dataset_code = "Vanderwell2018-21"
setwd(file.path(wd))
data_db <- "AllBirdCounts.csv"

WTpj_Tbl <- read_sheet("https://docs.google.com/spreadsheets/d/1fqifS_E5O_IpW1B-UG_xthr9hzY6FIek-nFjCrt1G0w", sheet = "project")
observer_Tbl <-  read_sheet("https://docs.google.com/spreadsheets/d/1yp0tjhQC7EJ_WqgP_vUNPfvgwwBhMDeSex4OghoIS3s", sheet = "master_observer")

lu <- "./lookupTables"
WT_spTbl <- read.csv(file.path(lu, "species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path(lu, "duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path(lu, "distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path(lu, "duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path(lu, "distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

## Path
dataDir <- file.path("./project", dataset_code)
if (!dir.exists(project)) {
  dir.create(project, recursive = TRUE)
}

out_dir <- file.path("./out", dataset_code)    # where output data frame will be exported
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
    filter(dataset_code =="Vanderwell2018-21") %>%
    select("GSharedDrive location")
  #Download from GoogleDrive
  gd.list <- drive_ls(as.character(pid))
  detection_id <- gd.list %>%
    filter(name =="AllBirdCounts.csv") %>%
    select("id")
  drive_download(as_id(as.character(detection_id)), path = file.path(dataDir, data_db))
  site_id <- gd.list %>%
    filter(name =="AllSiteDescription.csv") %>%
    select("id")
  drive_download(as_id(as.character(site_id)), path = file.path(dataDir, "AllSiteDescription.csv"))
}

BirdCounts_ori <- read.csv(file.path(dataDir, data_db))
SiteDescription_ori <- read.csv(file.path(dataDir, "AllSiteDescription.csv"))


#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
data_flat <- merge(BirdCounts_ori, SiteDescription_ori, 
                   by = c('Site', 'X_dd', 'Y_dd', 'Age', 'Type', 'Min_Edge_m', 'Date', 'Crew', 'Comments'), 
                   all.x = TRUE)

# Find rows in observation_ori that were not joined with data_flat 
# unmatched_rows <- anti_join(data_flat, SiteDescription_ori, by = "Site")
# print(unmatched_rows) 

data_flat <- data_flat %>%
  mutate(organization = organization,
         project = dataset_code,
         station = NA,
         location = paste(dataset_code, Site, sep=":"),
         longitude = X_dd,
         latitude = Y_dd) %>%
  rename(site = Site,
         comments = Comments)
     

# Not exist in source data will be set NA
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
# all rows contain 'Time'
# na_rows <- data_flat[is.na(data_flat$Start.Time), ]
# print(na_rows)

data_flat <- data_flat %>%
  mutate(visitDate = format(as.Date(Date, format = "%Y-%m-%d"), "%Y-%m-%d"),
         snowDepthMeters = NA,
         waterDepthMeters = NA,
         crew = NA,
         bait = "None",
         accessMethod = NA,
         landFeatures = Site.Description,
         comments = NA,
         wildtrax_internal_update_ts = NA,
         wildtrax_internal_lv_id = NA,
         time_zone = NA,
         data_origin = dataset_code,
         missinginvisit = NA,
         survey_year = sub("\\-.*", "", visitDate),
         survey_time = sub(".*\\s", "", Start.Time),
         surveyDateTime = paste(visitDate, survey_time),
         observer = case_when(Crew == "NDK/RNP"  ~ "obs01",
                              Crew == "RNP/MMM" ~ "obs02",
                              Crew == "RGK/NDK" ~ "obs03",
                              Crew == "NDK/RGK"  ~ "obs04",
                              Crew == "RNP/NDK" ~ "obs05",
                              Crew == "BPR/SLS" ~ "obs06",
                              Crew == "SLS/BPR"  ~ "obs07",
                              Crew == "RNP/CLC" ~ "obs08",
                              Crew == "SLS/CLC" ~ "obs09",
                              Crew == "BPR/RNP" ~ "obs10",
                              Crew == "RNP/BPR" ~ "obs11",
                              Crew == "RNP/GBH"  ~ "obs12",
                              Crew == "BPR/CLC" ~ "obs13",
                              Crew == "SLS/RNP" ~ "obs14",
                              Crew == "RNP/SLS" ~ "obs15",
                              is.na(Crew)   ~ "NA"),
         pkey_dt= paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), observer, sep=":")
  ) 

################################
#### Update master_observer ####
################################
unique_observers <- data_flat %>%
  select(Crew, observer) %>% 
  distinct() %>%
  filter(!is.na(Crew)) %>% # Exclude rows where Observer is NA
  mutate(
    observer_name = Crew,
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

# Combine new rows with the existing observer_Tbl
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

any(is.na(data_flat$Species.Code)) # Should be FALSE

# determine appropriate 'distanceMethod' as stated in 'metadata' sheet
data_flat$distanceMethod <- "0m-50m-100m-INF"


# 451 rows recording birds in more than one distance band
columns_to_check <- c("Within.50", "X51.100", "Over.100")
rows_with_multiple_nonzero <- data_flat[rowSums(data_flat[columns_to_check] != 0) > 1, ]
rows_with_multiple_nonzero

# 13 rows recording birds in all three distance band
# columns_to_check <- c("Within.50", "X51.100", "Over.100")
# rows_with_multiple_nonzero <- data_flat[rowSums(data_flat[columns_to_check] != 0) > 2, ]
# rows_with_multiple_nonzero

################################################################################################################
# Create a function to split rows with multiple non-zero values
# original number: 3438, expanded number should be around 3438 + 451 + 13 = 3902
################################################################################################################
split_rows <- function(row, columns) {
  # Identify the non-zero columns
  non_zero_cols <- which(row[columns] != 0)
  
  # Create a list of rows, each with only one non-zero value
  new_rows <- lapply(non_zero_cols, function(col) {
    new_row <- row
    new_row[columns] <- 0          # Set all columns to 0
    new_row[columns[col]] <- row[columns[col]]  # Retain only the current non-zero value
    return(new_row)
  })
  
  # Combine the rows into a data frame
  do.call(rbind, new_rows)
}

# Apply the function to all problematic rows
duplicated_rows <- do.call(rbind, apply(rows_with_multiple_nonzero, 1, split_rows, columns = columns_to_check))

# Combine the new rows with the original data, excluding the problematic rows
final_data <- rbind(
  data_flat[rowSums(data_flat[columns_to_check] != 0) <= 1, ],  # Rows without issues
  duplicated_rows  # The modified duplicated rows
)

# View the resulting data frame
final_data

# 0 rows recording birds in more than one distance band
# columns_to_check <- c("Within.50", "X51.100", "Over.100")
# rows_with_multiple_nonzero <- final_data[rowSums(final_data[columns_to_check] != 0) > 1, ]
# rows_with_multiple_nonzero

data_flat <- final_data
################################################################################################################

# Initialize to empty strings if not already existing
data_flat <- data_flat %>%
  mutate(distanceband = case_when(
    Within.50 != 0 ~ "0m-50m",
    X51.100 != 0 ~ "50m-100m",    
    Over.100 != 0 ~ "100m-INF",
    str_detect(Comments, '< 50 M') ~ "0m-50m",
    str_detect(Comments, '~ 30 M') ~ "0m-50m",
    str_detect(Comments, '51-100 M') ~ "50m-100m",
    str_detect(Comments, '~ 80 M') ~ "50m-100m",
    str_detect(Comments, '> 100 M') ~ "100m-INF",
    str_detect(Comments, '~ 108 M') ~ "100m-INF",
    str_detect(Comments, '~170 M') ~ "100m-INF",
    str_detect(Comments, '~175 M') ~ "100m-INF",
    Outside.Time != 0 ~ "UNKNOWN",
    TRUE ~ NA
  ))
# check result unique(paste(data_flat$Distance, data_flat$distanceband))
# print(unique(data_flat$distanceband[(data_flat$distanceband %in% WT_distBandTbl$distance_band_type)]))



# determine appropriate 'durationMethod' as stated in 'metadata' sheet
data_flat$durationMethod<- "0-5min"

# Initialize to empty strings if not already existing
# str(data_flat %>% filter(Quadrant %in% c("7"))) # nrows 140
data_flat <- data_flat %>%
  mutate(durationinterval = case_when(
    Outside.Time != 0 ~ "UNKNOWN",
    TRUE ~ "0-5min"
  ))
# check result unique(paste(data_flat$Outside.Time, data_flat$durationinterval))
print(unique(data_flat$durationinterval[(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))

# unique(data_flat$Species.Code[(data_flat$Species.Code %in% WT_spTbl$species_code)]) 
# unique(data_flat$Species.Code[!(data_flat$Species.Code %in% WT_spTbl$species_code)])  #4 species NOT in WildTrax
data_flat$species <- WT_spTbl$species_code[match(data_flat$Species.Code, WT_spTbl$species_code)]

# extract abbreviation with no match 
missABV <- unique(data_flat$Species.Code[!(data_flat$Species.Code %in% WT_spTbl$species_code)])

# extract the common name and scientific name of unmatched species code
miss_species <- data_flat %>%
  filter(Species.Code %in% missABV) %>%
  select(Species.Code, Common.AOU2021, Scientific.AOU2021) %>%
  distinct()
print(miss_species)

# search matched common name and scientific name in WildTrax species list
# print(WT_spTbl %>% filter(species_common_name %in% miss_species$Common.AOU2021)) 
# print(WT_spTbl %>% filter(scientific_name %in% miss_species$Scientific.AOU2021)) 

# applied correct codes to miss_species
data_flat <- data_flat %>%
  mutate(species = case_when(Species.Code == "CAGO"  ~ "CANG", # for 'Canada Goose'
                             Species.Code == "SCJU"  ~ "JUNHYE", # for 'Junco hyemalis hyemalis'
                             Species.Code == "WPWA"  ~ "PAWA", # for 'Setophaga palmarum'
                             Species.Code == "HOWR"  ~ "NHWR", # for 'Troglodytes aedon'
                             TRUE ~ species)
  )


# Check
print(length(data_flat$species[is.na(data_flat$species)])) # 0, the newly build column
# print(length(data_flat$species[is.na(data_flat$Scientific.AOU2021)])) # 0, the original column holding species Scientific Name
print(length(data_flat$species[is.na(data_flat$Species.Code)]))  # 0, the original column holding species code
# head(data_flat[!(data_flat$Species.Code %in% WT_spTbl$species_code),]) # visual check rows we changed species code


# determine appropriate behaviour by unique(data_flat$Status)
data_flat <- data_flat %>%
  mutate(isHeard = case_when(
    grepl(".*S.*", Status) ~ "Yes",
    grepl(".*C.*", Status) ~ "Yes",
    grepl(".*O.*", Status) ~ "No",
    grepl(".*D.*", Status) ~ "No",
    grepl(".*N.*", Status) ~ "No",
    grepl(".*DD.*", Status) ~ "No",
    grepl(".*F.*", Status) ~ "No",
    NA ~ "DNC", 
    TRUE ~ "DNC"
  ))
# check result unique(paste(data_flat$Status, data_flat$isHeard))

data_flat <- data_flat %>%
  mutate(isSeen = case_when(
    grepl(".*S.*", Status) ~ "No",
    grepl(".*C.*", Status) ~ "No",
    grepl(".*O.*", Status) ~ "Yes",
    grepl(".*D.*", Status) ~ "Yes",
    grepl(".*N.*", Status) ~ "Yes",
    grepl(".*DD.*", Status) ~ "Yes",
    grepl(".*F.*", Status) ~ "Yes",
    NA ~ "DNC", 
    TRUE ~ "DNC"
  ))
# check result unique(paste(data_flat$Status, data_flat$isSeen))


# ind_count
# Ensure all the distance band columns are numeric
data_flat <- data_flat %>%
  mutate(
    Within.50 = as.numeric(Within.50),
    X51.100 = as.numeric(X51.100),
    Over.100 = as.numeric(Over.100),
    Outside.Time = as.numeric(Outside.Time)
  )

# Update ind_count
data_flat <- data_flat %>%
  mutate(ind_count = case_when(
    Within.50 != 0 ~ Within.50,
    X51.100 != 0 ~ X51.100,
    Over.100 != 0 ~ Over.100,
    Outside.Time != 0 ~ Outside.Time,
    TRUE ~ 0
  ))

# any(is.na(data_flat$ind_count))
# Print rows where ind_count > 1
#rows_with_ind_count_gt_1 <- data_flat %>% filter(ind_count > 1)
# print(rows_with_ind_count_gt_1)


# # create 'isDuplicate' column for signaling the duplicated column , *For information only
# WTspecies <- c("surveyDateTime", "location", "species", "distanceband", "durationinterval")
# duplicates <- duplicated(data_flat[WTspecies]) | duplicated(data_flat[WTspecies], fromLast = TRUE)
# data_flat$isDuplicate <- duplicates
# print(data_flat[data_flat$isDuplicate == TRUE, c("surveyDateTime", "location", "distanceband", "durationinterval", "species", "observer", "ind_count", "isDuplicate")])
# print(nrow(data_flat[data_flat$isDuplicate == TRUE, ])) # 1532 duplicated record


# Delete duplicated in the eye of the survey table *abundance is used
# WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
#               "durationinterval", "abundance", "isHeard", "isSeen", "comments")
# survey_tbl <- data_flat[!duplicated(data_flat[,WTsurvey]), WTsurvey]
# check NULL result by, colSums(is.na(survey_tbl)) > 0
# print(data_flat[is.na(data_flat$species), ])
# survey_tbl[survey_tbl$abundance ==0, ], should be 0


# Validations: species code, duration band, distance band, all in specification
print(unique(data_flat$species[!(data_flat$species %in% WT_spTbl$species_code)]))
print(unique(data_flat$durationinterval[!(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))
print(unique(data_flat$distanceband[!(data_flat$distanceband %in% WT_distBandTbl$distance_band_type)]))
print(unique(data_flat$durationinterval[!(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))


############################
#### SURVEY TABLE ####
############################

#---EXTENDED
data_flat$rawObserver <- data_flat$Crew
data_flat$original_species <- data_flat$Species.Code
data_flat$scientificname <- WT_spTbl$scientific_name[match(data_flat$species, WT_spTbl$species_code)]
data_flat$raw_distance_code <- NA
data_flat$raw_duration_code <- NA
data_flat$originalBehaviourData <- data_flat$Status
data_flat$missingindetections <- "DNC"
# data_flat$age <- NA
data_flat$age <- ifelse(
  grepl("FEMALE", data_flat$Comments, ignore.case = TRUE), 
  "Adult", 
  ifelse(
    grepl("MALE", data_flat$Comments, ignore.case = TRUE), 
    "Adult", 
    ifelse(
      grepl("YOUNG", data_flat$Comments, ignore.case = TRUE), 
      "Juvenile", 
      "DNC"
    )
  )
)
# data_flat$fm <- NA
data_flat$fm <- ifelse(
  grepl("FEMALE", data_flat$Comments, ignore.case = TRUE), 
  "Female", 
  ifelse(
    grepl("MALE", data_flat$Comments, ignore.case = TRUE), 
    "Male", 
    "DNC"
  )
)
data_flat$group <- ifelse(grepl("flock", data_flat$Comments, ignore.case = TRUE), "flock", "No")
data_flat$flyover <- "DNC"
# data_flat$displaytype <- "DNC"
data_flat$displaytype <- ifelse(
  grepl("\\bD\\b|\\bDD\\b", data_flat$Status, ignore.case = TRUE), 
  "Yes", 
  "DNC"
)
# data_flat$nestevidence <- "DNC"
data_flat$nestevidence <- ifelse(
  grepl("N", data_flat$Status, ignore.case = TRUE), 
  "Yes", 
  "DNC"
  )
data_flat$behaviourother <- "DNC"
data_flat$atlas_breeding_code <- "DNC"
# unique(data_flat$Comments) 

data_flat <- data_flat %>%
  mutate(
    pc_vt = case_when(
      grepl(".*S.*", Status) ~ "Sing",
      grepl(".*C.*", Status) ~ "Calls",
      grepl(".*O.*", Status) ~ "None-vocal",
      grepl(".*D.*", Status) ~ "None-vocal",
      grepl(".*N.*", Status) ~ "None-vocal",
      grepl(".*DD.*", Status) ~ "None-vocal",
      grepl(".*F.*", Status) ~ "None-vocal",
      TRUE ~ "DNC"  
    ),
    pc_vt_detail = "NA"  
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
folder_list <- drive_ls(as_id(dr), pattern = dataset_code) # print(folder_list)

if (nrow(folder_list[folder_list$name == dataset_code, ]) == 0){
  dr_dataset_code <-drive_mkdir(dataset_code, path = as_id(dr), overwrite = NA)
  print(paste("Folder", dataset_code, "created successfully."))
} else {
  dr_dataset_code <- folder_list[folder_list$name == dataset_code, ]
} # print(drive_ls(as_id(dr)))


# flyover (flying) data singled out
no_flyover<- data_flat %>%
  filter(!flyover == "Yes")

yes_flyover<- data_flat %>%
  filter(flyover == "Yes")
print(yes_flyover)



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
# Delete duplicated in the eye of the survey table *abundance is used

survey_tbl <- no_flyover %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband, durationinterval, isHeard, isSeen, comments) %>%
  dplyr::summarise(abundance = sum(ind_count), .groups= "keep")

WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen", "comments")
survey_tbl <- survey_tbl[!duplicated(survey_tbl[,WTsurvey]), WTsurvey] # 
write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), row.names = FALSE, na = "")
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv"), overwrite = TRUE) 



# check NULL result by, colSums(is.na(survey_tbl)) > 0
# survey_tbl[survey_tbl$abundance ==0, ], should be 0


# below lines to highlight the grouped lines, which are not selected in survey_tbl 
# x <- no_flyover %>%
#   group_by(surveyDateTime, location, species, distanceband, durationinterval, isHeard, isSeen, comments) %>%
#   mutate(abundance = ifelse(isDuplicate == TRUE, sum(ind_count), ind_count)) %>%
#   filter(!duplicated(paste(surveyDateTime, location, species, distanceband, durationinterval))) %>%
#   ungroup()
# View(x[x$abundance != x$ind_count, ]) # visualized rows with grouped ind_count



#---EXTENDED
# "atlas_breeding_code" still kept
# the operation of grouping "ind_count" into "abundance" is not done
Extended <- c("organization", "project", "location", "surveyDateTime", "species", "ind_count", "distanceband", "durationinterval", "site", "station", "utmZone", "easting",
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code",
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover",
              "displaytype", "nestevidence", "behaviourother", "atlas_breeding_code")

extended_tbl <- data_flat[!duplicated(data_flat[,Extended]), Extended]
write.csv(extended_tbl, file.path(out_dir, paste0(dataset_code, "_behavior.csv")), na = "", row.names = FALSE)
extended_out <- file.path(out_dir, paste0(dataset_code,"_behavior.csv"))
drive_upload(media = extended_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_behavior.csv"), overwrite = TRUE)




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
