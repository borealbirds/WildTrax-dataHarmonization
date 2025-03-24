# Title: "Translate Newfounland Wildsape historical data - IRMR River Valley"
# Source dataset is an excel spreadsheet
# Author: "Melina Houle"
# Date: "February 28, 2023"
#  BUG: 29 visit don't have survey. We assume the site was visited but no obs. 
#update.packages()
library(dplyr) # mutate, %>%
#library(utils) #read.csv
library(readxl) #read_excel
library(stringr) #str_replace_all
library(sf) #st_crs, st_as_sf, st_transform, st_drop_geometry
library(purrr) #map
library(plyr) #rbind.fill
library(googledrive) #drive_get, drive_mkdir, drive_ls, drive_upload
library(sp)
library(sf)
library(reshape2) # melt
library(readr) #write_lines

## Initialize variables
wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

WT_spTbl <- read.csv(file.path("./lookupTables/species_codes.csv"))
colnames(WT_spTbl) <- c("species_common_name", "species_code", "scientific_name")
WT_durMethTbl <- read.csv(file.path("./lookupTables/duration_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distMethTbl <- read.csv(file.path("./lookupTables/distance_method_codes.csv"), fileEncoding="UTF-8-BOM")
WT_durBandTbl <- read.csv(file.path("./lookupTables/duration_interval_codes.csv"), fileEncoding="UTF-8-BOM")
WT_distBandTbl <- read.csv(file.path("./lookupTables/distance_band_codes.csv"), fileEncoding="UTF-8-BOM")

organization <- "CWS-ATL"
dataset <- "Wildspace_Historical_River_Valley_Ecosystem_Project"
dataset_code <- "WH_IEMR"
lu <- "./lookupTables"
project <- file.path("./project", dataset_code)
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}
project_dir <- file.path(wd, "project", dataset_code)
if (!dir.exists(project_dir)) {
  dir.create(project_dir)
}
data_db <- file.path(project_dir, "P5 I1 C1 IEMR River Valley.xlsx")
if (!file.exists(data_db)) {
  #Download from GoogleDrive
  drive_download("sourceData/P5 I1 C1 IEMR River Valley.xlsx", path = file.path(project_dir, "P5 I1 C1 IEMR River Valley.xlsx"))
}
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

#--------------------------------------------------------------
#       LOAD
#--------------------------------------------------------------
raw_visit <- read_xlsx(data_db, sheet = "Site Survey General", skip=3)
names(raw_visit)<-str_replace_all(names(raw_visit), c(" " = "_"))

raw_survey <- read_xlsx(data_db, sheet = "Site Survey Obs", skip=3)
names(raw_survey)<-str_replace_all(names(raw_survey), c(" " = "_"))

lu_observer <- read_xlsx(data_db, sheet = "Survey Crew", skip=3)
names(lu_observer)<-str_replace_all(names(lu_observer), c(" " = "_"))

lu_species <-  read_xlsx(data_db, sheet = "SpeciesTable")
names(lu_species)<-str_replace_all(names(lu_species), c(" " = "_"))

#--------------------------------------------------------------
#
#       TRANSLATE
#
#--------------------------------------------------------------
############################
#### LOCATION TABLE ####
############################
#Format
pc_location <- raw_visit %>% 
  select(Site_ID, Station_ID, Station_Easting, Station_Northing, `Date_________(yyyy-mm-dd)`, Observer, `Time_Start_____hh:mm`)  %>%
  dplyr::rename(northing = Station_Northing,
                easting = Station_Easting,
                site = Site_ID, 
                station = Station_ID,
                Date = `Date_________(yyyy-mm-dd)`,
                Time = `Time_Start_____hh:mm`) %>%
  mutate(location = paste(dataset_code, station, sep= ":"),
         missinginlocations = NA)

    ####### CHECK MAPPING 
    #canada <- st_read("E:/MelinaStuff/BAM/GIS_layer/CanadaLAEA.shp")
    #bnd <- st_transform(canada, crs= st_crs(2961))
    #plot(bnd$geometry)
    #xy <- pc_location[,c("longitude", "latitude")]
    #spdf <- SpatialPointsDataFrame(coords = xy, data = pc_location,
    #                              proj4string = CRS("+proj=longlat +ellps=GRS80 +towgs84=0.0,0.0,0.0,0.0,0.0,0.0,0.0 +no_defs"))
    #pc <- st_as_sf(spdf, coords = c("longitude", "latitude"), crs = UTM)
    #plot(pc$geometry, add= TRUE)

# REPROJECT
crs_utm20N <- st_crs(2961) # Data CRS: NAD83 UTM20N
crs_WT <- st_crs(4386) # WILDTRAX

pc_location <- st_as_sf(pc_location, coords = c("easting", "northing"), remove = FALSE)
st_crs(pc_location) <- crs_utm20N
pc_location_WT <- st_transform(pc_location, crs_WT)

s_location <- pc_location_WT %>%
  mutate(latitude = unlist(map(pc_location_WT$geometry,2)),
         longitude = unlist(map(pc_location_WT$geometry,1)))

st_drop_geometry(s_location)
s_location$geometry <- NULL

############################
#### VISIT TABLE ####
############################
s_visit <- s_location %>% 
  mutate(visitDate = ifelse(is.na(Date), "1900-01-01", as.character(Date)),
         missingvisit = NA,
         rawObserver = toupper(Observer),
         observer = case_when(rawObserver == "TN"  ~ "P5L1C1_obs01",
                              rawObserver == "GJ"  ~ "P5L1C1_obs02",
                              rawObserver == "KH"  ~ "P5L1C1_obs03",
                              rawObserver == "SMB"  ~ "P5L1C1_obs04",
                              rawObserver == "IS"  ~ "P5L1C1_obs05",
                              rawObserver == "CW"  ~ "P5L1C1_obs06",
                              rawObserver == "SS"  ~ "P5L1C1_obs07",
                              rawObserver == "LE" ~ "P5L1C1_obs08"),
         #time = as.character(format(as.POSIXct(sprintf("%04.0f", Time), format='%H%M'), format = "%H:%M:%S")),
         survey_time = as.character(format(s_location$Time, format = "%H:%M:%S")),
         snowDepthMeters= NA,
         waterDepthMeters = NA,
         crew = NA,
         bait = "NONE",
         accessMethod = NA,
         landFeatures = NA,
         wildtrax_internal_update_ts = NA,
         wildtrax_internal_lv_id = NA,
         comments = NA,
         utmZone = "utm20N",
         time_zone = NA,       
         data_origin = NA,
         missinginvisit = NA,
         survey_year = substr(Date, 1, 4))

#---VISIT
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double point count on the same site, same day). 
visit_tbl <- s_visit[!duplicated(s_visit[,WTvisit]), ] # 

############################
#### SURVEY TABLE ####
############################
# Filter species lookupTable. Delete duplicated rows
lu_species <- lu_species %>%
  filter(TaxonGroup == "Birds",
         !SpeciesName == "Blue-headed Vireo",
         !SpeciesName == "Three-toed Woodpecker")

pc_survey <- raw_survey %>% 
  dplyr::rename(station = Station_ID,
                Date = `Date_________(yyyy-mm-dd)`) %>%
  mutate(organization = organization,
         project = dataset,
         original_species = Species ,
         visitDate = ifelse(is.na(Date), "1900-01-01", as.character(Date)),
         location = paste(dataset_code, station, sep= ":"),
         unk_50 = `_50M_Total` - `_50M_0-3MIN` - `_50M_3-5MIN`,
         unk_UNL = `Unlim_Total` - `_+50M_0-3MIN` - `_+50M_3-5MIN`,
         unk_ALL = Count - `_50M_Total` - `Unlim_Total` - Fly_Total) %>%
  melt(measure.vars = c("_50M_0-3MIN","_50M_3-5MIN", "unk_50", "_+50M_0-3MIN", "_+50M_3-5MIN", "unk_UNL", "unk_ALL", "Fly_Total"), value.name = "ind_count") %>%
  dplyr::filter(!ind_count == 0)

s_data <- pc_survey %>%
  left_join(visit_tbl, by = c("station", "visitDate", "location")) %>%
  left_join(lu_species, by=c("original_species"="SpeciesID"))  %>%
  left_join(WT_spTbl, by=c("Species"="species_code"))

pc_survey <- s_data %>% 
  dplyr::rename(protocol = variable) %>%
  dplyr::mutate(survey_time = ifelse(is.na(survey_time), "00:00:01", survey_time),
         pkey_dt = paste(location, paste0(gsub("-", "", as.character(visitDate)),"_", gsub(":", "", survey_time)), observer, sep=":"),
         surveyDateTime = paste(visitDate, survey_time),
         species = ifelse(Species %in% c("unk", "UNK"), "UNBI",
                          WT_spTbl$species_code[match(Species, WT_spTbl$species_code)]),
         scientificname = WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)],
         #         scientificname = ifelse(Speccode %in% c("SPEC1", "SPEC2", "SPEC3"), NA,
         #                          WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)]),
         distanceMethod = "0m-50m-INF",
         distanceband = case_when(protocol == "_50M_0-3MIN" ~ "0m-50m",
                                  protocol == "_50M_3-5MIN" ~ "0m-50m",
                                  protocol == "unk_50" ~ "0m-50m",
                                  protocol == "_+50M_0-3MIN" ~ "50m-INF",
                                  protocol == "_+50M_3-5MIN" ~ "50m-INF",
                                  protocol == "unk_UNL" ~ "50m-INF",
                                  protocol == "unk_ALL" ~ "UNKNOWN"),
         durationMethod = "0-3-5min",
         durationinterval = case_when(protocol == "_50M_0-3MIN" ~ "0-3min",
                                      protocol == "_50M_3-5MIN" ~ "3-5min",
                                      protocol == "unk_50" ~ "UNKNOWN",
                                      protocol == "_+50M_0-3MIN" ~ "0-3min",
                                      protocol == "_+50M_3-5MIN" ~ "3-5min",
                                      protocol == "unk_UNL" ~ "UNKNOWN",
                                      protocol == "unk_ALL" ~ "UNKNOWN"),
         isHeard = "Yes",
         isSeen = "DNC",
         missingindetections = NA,
         raw_distance_code = protocol,
         raw_duration_code = protocol,
         #Behaviour
         originalBehaviourData = NA,
         pc_vt = NA,
         pc_vt_detail = NA,
         age = NA,
         fm = NA,
         group = NA,
         flyover = ifelse(protocol == "Fly_Total", "Yes", "No"),
         displaytype = NA,
         nestevidence = NA,
         behaviourother = NA,
         comments = NA)

## Fix or fill missing species
# 3 species don't have common name that match: "MYWA", YSFL", "GULL"
# 5 others are unfound: "SCJU" "YWAR" "TTWO" "YPWA" "GCKi"
pc_survey <- pc_survey  %>%
  mutate(species = case_when(original_species == "MYWA" ~ "YRWA",
                             original_species == "YSFL" ~ "NOFL",
                             original_species == "GULL" ~ "UNGU",
                             original_species == "SCJU" ~ "DEJU",
                             original_species == "YWAR" ~ "YEWA",
                             original_species == "TTWO" ~ "ATTW",
                             original_species == "YPWA" ~ "PAWA",
                             original_species == "GCKi" ~ "GCKI",
                             TRUE ~ species),                                      
         scientificname = WT_spTbl$scientific_name[match(species, WT_spTbl$species_code)],
  )
## CHECK
print(unique(pc_survey$distanceMethod[!(pc_survey$distanceMethod %in% WT_distMethTbl$distance_method_type)]))
print(unique(pc_survey$durationMethod[!(pc_survey$durationMethod %in% WT_durMethTbl$duration_method_type)]))
print(unique(pc_survey$species[!(pc_survey$species %in% WT_spTbl$species_code)]))
print(unique(pc_survey$original_species[!(pc_survey$species %in% WT_spTbl$species_code)]))

print(unique(pc_survey$species[!(pc_survey$species %in% WT_spTbl$species_code)]))
print(unique(pc_survey$durationinterval[!(pc_survey$durationinterval %in% WT_durBandTbl$duration_interval_type)]))
print(unique(pc_survey$distanceband[!(pc_survey$distanceband %in% WT_distBandTbl$distance_band_type)]))


#--------------------------------------------------------------
#
#       EXPORT
#
#--------------------------------------------------------------
dr<- drive_get(paste0("toUpload/",organization))
#Set GoogleDrive id
if (nrow(drive_ls(as_id(dr), pattern = dataset_code)) == 0){
  dr_dataset_code <-drive_mkdir(dataset_code, path = as_id(dr), overwrite = NA)
} else {
  dr_dataset_code <- drive_ls(as_id(dr), pattern = dataset_code)
}
dr_ls <- drive_ls(as_id(dr), pattern = dataset_code)

#---LOCATION
no_flyover <- pc_survey %>%
  filter(!flyover == "Yes")

WTlocation <- c("location", "latitude", "longitude")

# Remove duplicated location
location_tbl <- no_flyover[!duplicated(no_flyover[,WTlocation]), WTlocation] 
write.csv(location_tbl, file= file.path(out_dir, paste0(dataset_code,"_location.csv")), row.names = FALSE, na = "")
location_out <- file.path(out_dir, paste0(dataset_code,"_location.csv"))
drive_upload(media = location_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_location.csv"), overwrite = TRUE) 

#---VISIT
WTvisit <- c("location", "visitDate", "snowDepthMeters", "waterDepthMeters", "crew", "bait", "accessMethod", "landFeatures", "comments", 
             "wildtrax_internal_update_ts", "wildtrax_internal_lv_id")

#Delete duplicated based on WildtTrax attributes (double observer on the same site, same day). 
visit_tbl <- no_flyover[!duplicated(no_flyover[,WTvisit]), WTvisit] # 

write.csv(visit_tbl, file= file.path(out_dir, paste0(dataset_code,"_visit.csv")), row.names = FALSE, na = "")
visit_out <- file.path(out_dir, paste0(dataset_code,"_visit.csv"))
drive_upload(media = visit_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_visit.csv"), overwrite = TRUE) 

#---SURVEY
survey_tbl <- no_flyover %>% 
  group_by(location, surveyDateTime, durationMethod, distanceMethod, observer, species, distanceband, durationinterval, isHeard, isSeen, comments) %>%
  dplyr::summarise(abundance = sum(ind_count), .groups= "keep")

WTsurvey <- c("location", "surveyDateTime", "durationMethod", "distanceMethod", "observer", "species", "distanceband",
              "durationinterval", "abundance", "isHeard", "isSeen", "comments")

write.csv(survey_tbl, file= file.path(out_dir, paste0(dataset_code,"_survey.csv")), row.names = FALSE, na = "")
survey_out <- file.path(out_dir, paste0(dataset_code,"_survey.csv"))
drive_upload(media = survey_out, path = as_id(dr_dataset_code), name = paste0(dataset_code,"_survey.csv"), overwrite = TRUE) 

#---EXTENDED
Extended <- c("organization", "project","location", "surveyDateTime", "species", "ind_count", "distanceband", "durationinterval", "site", "station", "utmZone", "easting", 
              "northing", "missinginlocations", "time_zone", "data_origin", "missinginvisit", "pkey_dt", "survey_time",
              "survey_year", "rawObserver", "original_species", "scientificname", "raw_distance_code", "raw_duration_code", 
              "originalBehaviourData", "missingindetections", "pc_vt", "pc_vt_detail", "age", "fm", "group", "flyover", 
              "displaytype", "nestevidence", "behaviourother")
extended_tbl <- pc_survey[!duplicated(pc_survey[,Extended]), Extended] 
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


