# ---
# title: "Translate CWS-ATL data"
# author: Siu Chung WU, Diego
# date: "November 8, 2024"
# Note on translation:
# 1/371 survey does not contain 'Time' -> changed to 09:00:00 as agreed internally
# 1/371 survey does not contain 'Obs' Observer -> change to 'NA'
# 7/2952 observations do not contain species 'Code' and 'Quadrant' -> deleted
# nrows 140 with 'Quadrant' %in% c("7") - Before/after 10-min period (if not detected on pc)
# 140 survey comments interpreted in 6 lists of keywords, after all regular treatments


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


## URL
url <- "https://drive.google.com/drive/u/1/folders/13PkkvZ06tjZjDUZTji0SzMbJrqHifc9H"

## Initialize variables
wd <- "C:/Users/asito/Desktop/ModellingProject/#BAM/WildTrax_Integration"
organization = "CWS-ATL"
dataset_code = "LHMM"
setwd(file.path(wd))

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

out_dir <- file.path(wd, "out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE) 
}


#--------------------------------------------------------------
#
#       LOAD - this part uses modified Excel Sheets
#
#--------------------------------------------------------------

# ## semi-finished excel sheets on Google Drive
# drive_auth()
# folder_id <- sub(".*folders/", "", url)
# files_in_folder <- drive_ls(as_id(folder_id))
# 
# # Filter for xlsx files
# xlsx_files <- files_in_folder %>% 
#   filter(grepl("\\.xlsx$", name))
# 
# # Specify the xlsx file name holding observation information 
# survey_file <- "KeithLewisMealyMts_LG.xlsx"
# survey_xlsx <- xlsx_files %>% 
#   filter(name == survey_file)
# 
# # Locations located in first 2 sheets 
# location1 <- read_excel(drive_download(survey_xlsx$id[1], overwrite = TRUE)$local_path, sheet = 1)
# location2 <- read_excel(drive_download(survey_xlsx$id[1], overwrite = TRUE)$local_path, sheet = 2)
# location <- bind_rows(location1, location2[-1, ]) # Remove the first row, of the binded second dataframe
# # adjust Locations dataframe - with duplicate
# location <- location %>%
#   slice(-1) %>%                                  # Remove the first row, now used as column names
#   setNames(as.character(location[1, ])) %>%      # Set the first row as column names
#   select(1:5) %>%                                # Keep only the first 5 columns
#   mutate(SS = sub(":", "", SS))              # 'SS' column should have only one ':'
# # View(location) - duplicate exists, # rows 192
# 
# # delete duplicate
# location <- location %>%  distinct()
# # View(location) - duplicate exists, # rows 116
# 
# 
# # Surveys located in next 2 sheets 
# survey1 <- read_excel(drive_download(survey_xlsx$id[1], overwrite = TRUE)$local_path, sheet = 3)
# survey2 <- read_excel(drive_download(survey_xlsx$id[1], overwrite = TRUE)$local_path, sheet = 4)
# # adjust Surveys dataframe, bind only the first 5 columns
# survey1 <- survey1 %>%
#   mutate(across(everything(), as.character))
# survey2 <- survey2 %>%
#   mutate(across(everything(), as.character))
# survey <- bind_rows(
#   survey1 %>% select(1:15),
#   survey2 %>% select(1:15)
# )
# 
# 
# # Observations located in next 2 sheets 
# observation1 <- read_excel(drive_download(survey_xlsx$id[1], overwrite = TRUE)$local_path, sheet = 5)
# observation2 <- read_excel(drive_download(survey_xlsx$id[1], overwrite = TRUE)$local_path, sheet = 6)
# observation <- bind_rows(observation1, observation2)
# # adjust Observations dataframe 
# observation <- observation %>%
#   select(1:9)                  # Keep only the first 9 columns
# 
# 
# # metadata located in next 1 sheet 
# metadata <- read_excel(drive_download(survey_xlsx$id[1], overwrite = TRUE)$local_path, sheet = 7)
# 
# 
# # Site information is also attached 
# point_file <- "points.xlsx"
# point_xlsx <- xlsx_files %>% 
#   filter(name == point_file)
# point <- read_excel(drive_download(point_xlsx$id[1], overwrite = TRUE)$local_path, sheet = 1)
# point$SS <- paste0(point$Location, point$'Site Number', ":", sprintf("%03d", point$Point_id))


#--------------------------------------------------------------
#
#       EXTARCT FILE IN ORIGINAL MDB
#
#--------------------------------------------------------------

## semi-finished excel sheets on Google Drive
location_ori_path <- paste0(wd, "/project/LHMM/data/", "points_LHMM_DiegoFromMdb.xlsx")
location_ori <- read_excel(location_ori_path)

survey_ori_path <- paste0(wd, "/project/LHMM/data/", "point counts_LHMM_DiegoFromMdb.xlsx")
survey_ori <- read_excel(survey_ori_path)

observation_ori_path <- paste0(wd, "/project/LHMM/data/", "sightings_LHMM_DiegoFromMdb.xlsx")
observation_ori <- read_excel(observation_ori_path)

species_ori_path <- paste0(wd, "/project/LHMM/data/", "bird species names_LHMM_DiegoFromMdb.xlsx")
species_ori <- read_excel(species_ori_path)



#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------

############################
#### LOCATION TABLE ####
############################
data_flat <- survey_ori # rows 372
data_flat <- merge(data_flat, location_ori, by.x= 'Point_id', by.y= 'Point_id', all.x = TRUE)  # rows 372
data_flat <- merge(data_flat, observation_ori, by.x= 'pc_id', by.y= 'pc_id', all.x = TRUE)  # rows 2957/2964 joined

# Find rows in observation_ori that were not joined with data_flat 
# unmatched_rows <- anti_join(observation_ori, data_flat, by = "pc_id")
# print(unmatched_rows) 
# 'pc_id' 304 not included in 'survey' table

data_flat <- merge(data_flat, species_ori, by.x= 'Species_id', by.y= 'Species_id', all.x = TRUE)  # rows 2957 with species names

data_flat$organization <- organization
data_flat$project <- dataset_code
data_flat$site <- data_flat$Location
data_flat$station <- data_flat$'Site Number'


# Not exist in source data will be set NA
data_flat$elevationMeters <- data_flat$Elevation
data_flat$bufferRadiusMeters <- NA
data_flat$isHidden <- NA
data_flat$trueCoordinates <- NA
data_flat$comments <- data_flat$'Habitat Type'
data_flat$internal_wildtrax_id <- NA
data_flat$internal_update_ts <- NA
data_flat$utmZone	<- data_flat$Zone
data_flat$easting	<- data_flat$Easting
data_flat$northing	<- data_flat$Northing
data_flat$missinginlocations <- NA
# data_flat$longitude <- data_flat$Longitude
# data_flat$latitude <- data_flat$Latitude


# 5 rows don't have coordinates - removed
# data_flat_complete <- data_flat[is.na(data_flat$easting) & is.na(data_flat$northing), ]
# str(data_flat_complete)


# 2952 rows now
data_flat <- data_flat[!is.na(data_flat$easting) & !is.na(data_flat$northing), ]
# # correct projection from EPSG 32621 to EPSG 4269, and save them back to the original dataframe (data_flat)
xy_sf <- st_as_sf(data_flat, coords = c("easting", "northing"))
xy_sf <- st_set_crs(xy_sf, 32621)
xy_sf_4269 <- st_transform(xy_sf, crs = 4269)
xy_4269 <- st_coordinates(xy_sf_4269)
data_flat$longitude <- round(xy_4269[,1], 5)
data_flat$latitude <- round(xy_4269[,2], 5)


#---LOCATION
data_flat$location <- paste(dataset_code, data_flat$site, data_flat$station, sep=":")

# options(digits = 7)
# View(location_tbl)

############################
#### VISIT TABLE ####
############################

#survey attributes
data_flat$visitDate <- format(as.Date(data_flat$Date, format = "%Y-%m-%d"), "%Y-%m-%d") 
data_flat$snowDepthMeters <- NA
data_flat$waterDepthMeters <- NA
data_flat$crew <- NA
data_flat$bait <- "None"
data_flat$accessMethod <- NA
data_flat$landFeatures <- data_flat$'Habitat Type'
data_flat$comments <- data_flat$'Comment.y'
data_flat$wildtrax_internal_update_ts <- NA
data_flat$wildtrax_internal_lv_id <- NA
data_flat$time_zone <- NA
data_flat$data_origin <- dataset_code
data_flat$missinginvisit <- NA
data_flat$survey_year <- sub("\\-.*", "", data_flat$visitDate) 


# 1 survery / 10 observations, do not contain 'Time'
# na_rows <- data_flat[is.na(data_flat$Time), ]
# print(na_rows)
# it is thought to be at 09:00:00, as discussed internally, reference: MealyMt-Proofing-Checklist.xlsx
# https://docs.google.com/spreadsheets/d/1uNnc15UOQ9Z-m7_fERosEjfKgwdbd7JD/edit?gid=459589097#gid=459589097

# so, Replace NA values in the Time column with '09:00:00'
data_flat$Time[is.na(data_flat$Time)] <- '1899-12-31 09:00:00'
data_flat$survey_time <- sub(".*\\s", "", data_flat$Time)
# print(na_rows <- data_flat[is.na(data_flat$survey_time), ])


data_flat$surveyDateTime <- paste(data_flat$visitDate, data_flat$survey_time)
# na_rows <- data_flat %>% filter(is.na(survey_time))
# print(na_rows)


# Observer (internal ID number): unique(data_flat$Observer) 
data_flat$observer <- case_when(
  data_flat$Observer == "DF"  ~ "obs01",
  data_flat$Observer == "KPL" ~ "obs02",
  data_flat$Observer == "BMS" ~ "obs03",
  is.na(data_flat$Observer)   ~ "NA"
)
# 1 survey does not contain 'Observer'
# print(na_rows <- data_flat %>% filter(is.na(observer)))



data_flat$pkey_dt<- paste(data_flat$location, paste0(gsub("-", "", as.character(data_flat$visitDate)),"_", gsub(":", "", data_flat$survey_time)), data_flat$observer, sep=":")
# print(na_rows <- data_flat %>% filter(is.na(pkey_dt)))



############################
#### SURVEY TABLE ####
############################


# 2952 rows in observation 
# print(na_rows <- data_flat %>% filter(is.na(Code))) 
# 7 rows do not contain species 'Code' and 'Quadrant' -> deleted
data_flat <- data_flat %>% filter(!is.na(Code)) # 2945 rows in cleaned data_flat 


any(is.na(data_flat$Code)) # Should be FALSE

# determine appropriate 'distanceMethod' as stated in 'metadata' sheet
# str(data_flat %>% filter(Quadrant %in% c("5"))) # nrows 425
data_flat$distanceMethod <- "0m-100m-INF"

# Initialize to empty strings if not already existing
data_flat <- data_flat %>%
  mutate(distanceband = case_when(
    Quadrant == 5 ~ "100m-INF",
    TRUE ~ "0m-100m"
  ))
# check result unique(paste(data_flat$Distance, data_flat$distanceband))
print(unique(data_flat$distanceband[(data_flat$distanceband %in% WT_distBandTbl$distance_band_type)]))



# determine appropriate 'distanceMethod' as stated in 'metadata' sheet
data_flat$durationMethod<- "0-10min"

# Initialize to empty strings if not already existing
# str(data_flat %>% filter(Quadrant %in% c("7"))) # nrows 140
data_flat <- data_flat %>%
  mutate(durationinterval = case_when(
    TRUE ~ "0-10min"
  ))
# check result unique(paste(data_flat$Period, data_flat$durationinterval))
print(unique(data_flat$durationinterval[(data_flat$durationinterval %in% WT_durBandTbl$duration_interval_type)]))


# unique(data_flat$Code[(data_flat$Code %in% WT_spTbl$species_code)])  #37 species in WildTrax
# unique(data_flat$Code[!(data_flat$Code %in% WT_spTbl$species_code)])  #9 species NOT in WildTrax
data_flat$species <- WT_spTbl$species_code[match(data_flat$Code, WT_spTbl$species_code)]

# extract abbreviation with no match - "CORE" "SASP" "UFIN" "UNID" "TRSW" "DUCK" "AMTR" "UMER" "NOSH"
missABV <- unique(data_flat$Code[!(data_flat$Code %in% WT_spTbl$species_code)])

# extract the common name and scientific name of unmatched species code
miss_species <- data_flat %>%
  filter(Code %in% missABV) %>%
  select(Code, `Species Names`, `Latin Name`) %>%
  distinct()
print(miss_species)

# search matched common name and scientific name in WildTrax species list
# print(WT_spTbl %>% filter(species_common_name %in% miss_species$'Species Names')) 
# print(WT_spTbl %>% filter(scientific_name %in% miss_species$'Latin Name')) 

# applied correct codes to miss_species
data_flat <- data_flat %>%
  mutate(species = case_when(Code == "CORE"  ~ "REDP", # for 'Acanthis flammea' as indicated in the science paper 'https://doi.org/10.1139/cjz-2014-0309'
                             Code == "UNID"  ~ "UNBI", # for 'Unidentified Species'
                             Code == "UFIN"  ~ "UNFI", # for 'Unidentified Finch'
                             Code == "DUCK"  ~ "UNDU", # for 'Unidentified Duck'
                             Code == "SASP"  ~ "SAVS", # for 'Savannah Sparrow'
                             Code == "TRSW"  ~ "TRES",  # for 'Tree Swallow'
                             Code == "AMTR"  ~ "ATSP", # for 'American Tree Sparrow'                            
                             Code == "UMER"  ~ "UNME", # for 'Unidentified Merganser'                              
                             Code == "NOSH"  ~ "NSHR", # for 'Northern Shrike'
                             TRUE ~ species)
  )

# Check
print(length(data_flat$species[is.na(data_flat$species)])) # 0, the newly build column
# print(length(data_flat$species[is.na(data_flat$`Species Names`)])) # 0, the original column holding species Scientific Name
print(length(data_flat$species[is.na(data_flat$Code)]))  # 0, the original column holding species code
# head(data_flat[!(data_flat$Code %in% WT_spTbl$species_code),]) # visual check rows we changed species code



# determine appropriate behaviour by unique(data_flat$Voc)
data_flat <- data_flat %>%
  mutate(isHeard = case_when(
    grepl(".*S.*", Voc) ~ "Yes",
    grepl(".*C.*", Voc) ~ "Yes",
    grepl(".*Q.*", Voc) ~ "No",
    grepl(".*N.*", Voc) ~ "DNC",
    grepl(".*m.*", Voc) ~ "DNC",
    grepl(".*u.*", Voc) ~ "DNC",
    grepl(".*2.*", Voc) ~ "DNC",
    grepl(".*f.*", Voc) ~ "DNC",    
    grepl(".*w.*", Voc) ~ "DNC",
    NA ~ "DNC", 
    TRUE ~ "DNC"
  ))
# check result unique(paste(data_flat$Voc, data_flat$isHeard))
# understand what the codes mean:
# print(data_flat %>% filter(Voc %in% c("m")))
# print(data_flat %>% filter(Voc %in% c("u")))
# print(data_flat %>% filter(Voc %in% c("2")))
# print(data_flat %>% filter(Voc %in% c("f")))
# print(data_flat %>% filter(Voc %in% c("w")))


data_flat <- data_flat %>%
  mutate(isSeen = case_when(
    grepl(".*S.*", Voc) ~ "No",
    grepl(".*C.*", Voc) ~ "No",
    grepl(".*Q.*", Voc) ~ "Yes",
    grepl(".*N.*", Voc) ~ "DNC",
    grepl(".*m.*", Voc) ~ "DNC",
    grepl(".*u.*", Voc) ~ "DNC",
    grepl(".*2.*", Voc) ~ "DNC",
    grepl(".*f.*", Voc) ~ "DNC",    
    grepl(".*w.*", Voc) ~ "DNC",
    NA ~ "DNC", 
    TRUE ~ "DNC"
  ))
# check result unique(paste(data_flat$Voc, data_flat$isSeen))


# determine appropriate species by unique(data_flat$Code), any(is.na(data_flat$Code))
data_flat$ind_count <- ifelse(!is.na(data_flat$Code), 1, 0)
# data_flat$ind_count <-data_flat$Count
# any(is.na(data_flat$ind_count))


# create 'isDuplicate' column for signaling the duplicated column , *For information only
WTspecies <- c("surveyDateTime", "location", "species", "distanceband", "durationinterval")
duplicates <- duplicated(data_flat[WTspecies]) | duplicated(data_flat[WTspecies], fromLast = TRUE)
data_flat$isDuplicate <- duplicates
print(data_flat[data_flat$isDuplicate == TRUE, c("surveyDateTime", "location", "distanceband", "durationinterval", "species", "observer", "ind_count", "isDuplicate")])
print(nrow(data_flat[data_flat$isDuplicate == TRUE, ])) # 1532 duplicated record


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
data_flat$rawObserver <- data_flat$Observer
data_flat$original_species <- data_flat$Code
data_flat$scientificname <- WT_spTbl$scientific_name[match(data_flat$species, WT_spTbl$species_code)]
data_flat$raw_distance_code <- NA
data_flat$raw_duration_code <- NA
data_flat$originalBehaviourData <- data_flat$Voc
data_flat$missingindetections <- "DNC"
#data_flat$age <- ifelse(data_flat$Observation.Note %in% c("M","F", "P"), "Adult", "DNC")  
data_flat$age <- case_when(
  data_flat$Sex %in% c("M", "F", "P") ~ "Adult",
  data_flat$Sex %in% c("N", "Y") ~ "Juvenile",
  data_flat$Sex %in% c("U") | is.na(data_flat$Sex) ~ "DNC",
  TRUE ~ "DNC"
) # unique(paste(data_flat$Sex, data_flat$age))
#data_flat$fm <- ifelse(data_flat$Observation.Note=="Male", "Male", ifelse(data_flat$Observation.Note=="Female", "Female","DNC"))
data_flat$fm <- case_when(
  data_flat$Sex %in% c("M") ~ "Male",
  data_flat$Sex %in% c("F") ~ "Female",
  data_flat$Sex %in% c("P", "N", "Y", "U") | is.na(data_flat$Sex) ~ "DNC",
  TRUE ~ "DNC"
) # unique(paste(data_flat$Sex, data_flat$fm))




data_flat$group <- "DNC"
data_flat$flyover <- "DNC"
data_flat$displaytype <- "DNC"
data_flat$nestevidence <- "DNC"
data_flat$behaviourother <- "DNC"
data_flat$atlas_breeding_code <- "DNC"

data_flat <- data_flat %>%
  mutate(
    pc_vt = case_when(
      grepl(".*S.*", Voc) ~ "Sing",
      grepl(".*C.*", Voc) ~ "Calls",
      grepl(".*Q.*", Voc) ~ "NA",
      grepl(".*N.*", Voc) ~ "NA",
      grepl(".*m.*", Voc) ~ "DNC",
      grepl(".*u.*", Voc) ~ "DNC",
      grepl(".*2.*", Voc) ~ "DNC",
      grepl(".*f.*", Voc) ~ "DNC",    
      grepl(".*w.*", Voc) ~ "DNC",
      TRUE ~ "DNC"  
    ),
    pc_vt_detail = "NA"  
  )


##############################################
#### Using information in 'Comment' field ####
##############################################

#Step1: Visualize all types of 'Comment' # 140 rows
unique(data_flat$Comment)

# set up keyword list and update:
# 1. abundance: "flock", "several", "individuals", "birds"
keywords <- c("flock", "several", "individuals", "birds")
print(data_flat %>% filter(str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE))) %>% select (Comment, ind_count)) # 24 rows
# update
data_flat <- data_flat %>%
  mutate(
    extracted_number = ifelse(
      str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE)),
      as.numeric(str_extract(Comment, "\\d+")),  
      NA  
    ),
    #ind_count = ifelse(!is.na(extracted_number), extracted_number, ind_count),
    ind_count = case_when(
      !is.na(extracted_number) ~ extracted_number,
      TRUE ~ ind_count  
    ),
    #Comment = ifelse(str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE)), NA, Comment)
  )  %>%
  select(-extracted_number) 
# confirm replacement: 
print(data_flat %>% filter(str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE)))  %>% select (Comment, ind_count)) 



# 2. isSeen : "observed", "seen", "viewed", "moving", "obs", "visual", "appear", "identified", "confirmed"
keywords <- c("observed", "seen", "viewed", "moving", "obs", "visual", "appear", "identified", "confirmed")
print(data_flat %>% filter(str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE))) %>% select (Comment, isSeen)) # 27 rows
# update
data_flat <- data_flat %>%
  mutate(
    isSeen = case_when(
      str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE)) ~ "Yes",
      TRUE ~ isSeen
    ),
    #Comment = ifelse(str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE)), NA, Comment)
  )
# confirm replacement :
print(data_flat %>% filter(str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE))) %>% select (Comment, isSeen)) 


# 3. isHeard : "sound", "chirp", "vocal", "heard", "call", "sang"
keywords <- c("sound", "chirp", "vocal", "heard", "call", "sang")
print(data_flat %>% filter(str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE))) %>% select (Comment, isHeard)) # 21 rows
# update
data_flat <- data_flat %>%
  mutate(
    isHeard = case_when(
      str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE)) ~ "Yes",
      TRUE ~ isHeard
    ),
    #Comment = ifelse(str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE)), NA, Comment)
  )
# confirm replacement : print(data_flat %>% filter(isHeard == "Yes") %>% select (Comment, isHeard)) 



# 4. flyover: 'flyover', 'fly', 'flew', 'overhead'
keywords <- c("flyover", "fly", "flew", "overhead")
print(data_flat %>% filter(str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE))) %>% select (Comment, flyover)) # 22 rows
# update
data_flat <- data_flat %>%
  mutate(
    flyover = case_when(
      str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE)) ~ "Yes",
      TRUE ~ flyover
    ),
    #Comment = ifelse(str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE)), NA, Comment)
  )
# confirm replacement : 
print(data_flat %>% filter(str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE))) %>% select (Comment, flyover))



# 5. sex : "male", "female"
keywords <- c("male", "female")
print(data_flat %>% filter(str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE))) %>% select (Comment, Sex)) # 3 rows
# update
data_flat <- data_flat %>%
  mutate(
    Sex = case_when(
      str_detect(Comment, regex("male", ignore_case = TRUE)) ~ "Male",
      str_detect(Comment, regex("female", ignore_case = TRUE)) ~ "Female",
      TRUE ~ Sex
    ),
    #Comment = ifelse(str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE)), NA, Comment)
  )
# confirm replacement :
print(data_flat %>% filter(str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE)))  %>% select(Comment, Sex))


# 6. corrected species : 
keywords <- unique(WT_spTbl$species_code)
keywords <- keywords[nchar(keywords) == 4]
keywords <- keywords[keywords != "EACH"] # the species code 'EACH' also shown in 'Comment' as a common word, it causes wrong species replacement and should be manually removed
pattern <- paste0("\\b(", paste(keywords, collapse = "|"), ")\\b")

print(data_flat %>% filter(str_detect(Comment, regex(pattern, ignore_case = TRUE)))  %>% select (Comment, Code)) # 25 rows
# update
data_flat <- data_flat %>%
  mutate(
    extracted_code = str_extract(Comment, regex(pattern, ignore_case = TRUE)),
    #Code = ifelse(!is.na(extracted_code), toupper(extracted_code), Code),
    Code = case_when(
      !is.na(extracted_code) ~ toupper(extracted_code),
      TRUE ~ Code
    ),
    #Comment = ifelse(str_detect(Comment, regex(paste(keywords, collapse = "|"), ignore_case = TRUE)), NA, Comment)
  ) %>%
  select(-extracted_code)
# confirm replacement :
print(data_flat %>% filter(str_detect(Comment, regex(pattern, ignore_case = TRUE)))  %>% select(Comment, Code))



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
