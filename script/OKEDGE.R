# ---
# title: "Translate Okanagan Edge Effects data"
# author: "Elly Knight"
# date: "July 14, 2022"
# Note on translation:
# -- 
# ---

library(tidyverse)
library(readxl)
library(sf)

############################
## IMPORT ####
############################

site <- read.csv("project/OK Database - site info - Elly Knight.csv")
plot <- read.csv("project/OK Database - plot info - Elly Knight.csv")
pc <- read.csv("project/OK Database - PC info - Elly Knight.csv") 
dat <- read.csv("project/OK Database - PC data - Elly Knight.csv")

############################
#### LOCATION TABLE ####
############################

#Read in template
location.temp <- read_xlsx("template/Template-WT-1Location.xlsx")

#Tidy raw data
location.ok <- left_join(plot, site)

#Reproject to lat long
location.nad83 <- location.ok %>% 
  st_as_sf(coords=c("UTM.E", "UTM.N"), crs=26911) %>% 
  st_transform(crs=4326) %>% 
  st_coordinates() %>% 
  data.frame() %>% 
  rename(longitude = X, latitude = Y)

#Format
location.all <- location.ok %>% 
  cbind(location.nad83) %>% 
  mutate(location = paste0("OKEDGE:", str_sub(Name, 1, 3), ":", Plot),
         elevationMeters = NA,
         bufferRadiusMeters = NA,
         isHidden = FALSE,
         trueCoordinates = TRUE,
         comments = NA,
         internal_wildtrax_id = NA,
         internal_update_ts = NA)

############################
#### VISIT TABLE ####
############################

#Read in template
visit.temp <- read_xlsx("template/Template-WT-2Visit.xlsx", sheet=1)[,-c(12:18)]

#Tidy raw data
visit.ok <- pc %>% 
  dplyr::filter(!is.na(Site),
                !is.na(Round))

#Format
visit.all <- visit.ok %>% 
  mutate(SiteChar = case_when(nchar(Site)==1 ~ paste0("00", Site),
                          nchar(Site)==2 ~ paste0("0", Site),
                          nchar(Site)==3 ~ paste0(Site)),
         location = paste0("OKEDGE:", SiteChar, ":", Plot),
         visitDate = Date, 
         snowDepthMeters = NA,
         waterDepthMeters = NA,
         crew = NA,
         bait = "NONE",
         accessMethod = NA,
         landFeatures = NA, 
         comments = Comments, 
         wildtrax_internal_update_ts = NA,
         wildtrax_internal_lv_id = NA)
  
############################
#### SURVEY TABLE ####
############################

#Read in template
survey.temp <- read_xlsx("template/Template-WT-3Surveys.xlsx", sheet=1)

#Check species codes
lu.sp <- read.csv("lookupTables/species_codes.csv")
survey.sp <- dplyr::filter(dat, !Species %in% lu.sp$species_code)
table(survey.sp$Species)

#Tidy raw data
survey.ok<- dat %>% 
  dplyr::filter(!is.na(Site)) %>% 
  dplyr::select(-ID) %>% 
  rename(ObsTime = Time) %>% 
  full_join(visit.ok %>% 
              dplyr::select(-ID)) %>%
  mutate(abundance = ifelse(Species=="BHCOx50?", 50, 1)) %>% 
  mutate(Observer = ifelse(Observer=="eck", "ECK", Observer),
         species= case_when(Species=="CEWA" ~ "CEDW",
                            Species=="BHCOx50?" ~ "BHCO",
                            Species=="TRSW" ~ "TRES",
                            Species=="raptor" ~ "URPT",
                            Species %in% c("SPSP", "spsp", "CCSP?", "GRSP?") ~ "UNSP",
                            Species %in% c("DOSP", "HUSP", "") ~ "UNBI",
                            Species=="GRPA" ~ "GRAP",
                            Species=="ECDO" ~ "EUCD",
                            !is.na(Species) ~ Species))

#Check species codes again
survey.sp <- dplyr::filter(survey.ok, !species %in% lu.sp$species_code)
table(survey.sp$species)

#Get and create lookup tables
lu.obs <- survey.ok %>% 
  dplyr::select(Observer) %>% 
  unique() %>% 
  mutate(row = row_number(),
         observer = paste0("OKEDGE_0", row))

lu.db <- survey.ok %>% 
  dplyr::select(Distance) %>% 
  unique() %>% 
  arrange(Distance) %>% 
  mutate(distanceband = c("UNKNOWN",
                          "0m-10m",
                          "500m-INF",
                          "100m-INF",
                          "500m-INF",
                          "100m-INF",
                          "500m-INF",
                          "500m-INF",
                          "0m-10m",
                          "10m-20m",
                          "100m-200m",
                          "20m-50m",
                          "200m-300m",
                          "200m-300m",
                          "300m-400m",
                          "400m-500m",
                          "50m-75m",
                          "500m-INF",
                          "75m-100m",
                          "UNKNOWN"))

#Format
survey.all <- survey.ok %>% 
  left_join(lu.obs) %>% 
  left_join(lu.db) %>% 
  mutate(SiteChar = case_when(nchar(Site)==1 ~ paste0("00", Site),
                              nchar(Site)==2 ~ paste0("0", Site),
                              nchar(Site)==3 ~ paste0(Site)),
         location = paste0("OKEDGE:", SiteChar, ":", Plot),
         Time = str_sub(Time, -8, -1),
         surveyDateTime = paste(Date, Time),
         durationMethod = "0-10min",
         distanceMethod = "0m-10m-20m-50m-75m-100m-200m-300m-400m-500m-INF",
         durationinterval = "0-10min",
         isHeard = ifelse(Activity %in% c("Singing", "Calling", "chipping", "Defend", "Drumming", "Chick beg call", "singing", "Display", "drumming"), "Yes", "No"),
         isSeen = ifelse(!'Visual.' %in% c("", "`", NA), "Yes", "No")) %>% 
  rename(comments = Comments)


############################
#### EXTENDED TABLE ####
############################

#Read in template
extended.temp <- read_xlsx("template/Template-WT-4Extended.xlsx", sheet=1)

#Get and create lookup tables
lu.vt <- survey.all %>% 
  dplyr::select(Activity) %>% 
  unique() %>% 
  mutate(pc_vt = c("Song", "Call", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Call", "Call", "Non-vocal", "Call", "Song", "Song", "Non-vocal", NA),
         pc_vt_detail=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Drumming", NA, NA, "Display", "Drumming", NA))

lu.agefm <- survey.all %>% 
  dplyr::select(Sex) %>% 
  unique() %>% 
  mutate(age = c("Adult", NA, "Adult", "Adult", "Juvenile", "Fledgling", "Adult", NA, "Adult", NA),
         fm = c("Male", NA, "Female", "Female", "Unknown", "Unknown", "Unknown", "NA", "Unknown", NA)) 

#Format
extended.all <- survey.all %>% 
  left_join(location.all %>% 
              dplyr::select(-Distance, -comments)) %>% 
  left_join(lu.sp %>% 
              rename(species = species_code)) %>% 
  left_join(lu.vt) %>% 
  left_join(lu.agefm) %>% 
  rename(site = SiteChar,
         station = Plot,
         utmZone = 11,
         easting = UTM.E,
         northing = UTM.N,
         rawObserver = Observer,
         original_species = Species,
         scientificname = scientific_name,
         rawDistanceCode = Distance,
         originalBehaviourData = Activity) %>% 
  mutate(missinginlocations = NA,
         time_zone = "MDT",
         data_origin = NA,
         missinginvisit = NA,
         pkey_dt = paste0(location,":", surveyDateTime, ":", observer),
         survey_year = as.numeric(str_sub(Date, 1, 4)),
         survey_time = str_sub(Time, 1, 5),
         rawDurationCode = NA,
         missingindetections = NA,
         group=NA,
         flyover = ifelse(originalBehaviourData=="Fly over", "Yes", "No"),
         displaytype = NA,
         nestevidence=NA,
         behaviourother = NA,
         atlas_breeding_code = NA)
         
############################
## EXPORT ####
############################

#Location
location.wt <- location.all %>% 
  dplyr::select(colnames(location.temp))
write.csv(location.wt, "out/OKEDGE_location.csv", row.names = FALSE)

#Visit
visit.wt <- visit.all %>% 
  dplyr::select(colnames(visit.temp))
write.csv(visit.wt,  "out/OKEDGE_visit.csv", row.names = FALSE)

#Survey
survey.wt <- survey.all %>% 
  dplyr::select(colnames(survey.temp))
write.csv(survey.wt, "out/OKEDGE_survey.csv", row.names = FALSE)

#Extend
extended.wt <- extended.all %>% 
  dplyr::select(colnames(extended.temp))
write.csv(extended.wt, "out/OKEDGE_extended.csv", row.names = FALSE)
