WildTrax upload is a 3 steps process. 




blabla


<a name=Location_tbl></a>
### LOCATION TABLE

The **LOCATION** attributes identify the geographic extent of the site.

| Values   | Format   | Description   |
| :------- | :-------------- | :-------------- |
| location     | Text | The physical place on the landscape where the data was collected. Created using the concatenation of  [datasetCode]:[site]:[station], unless otherwise specified |
| latitude     | Decimal degrees | NAD83, convert if otherwise |
| longitude     | Decimal degrees | NAD83, convert if otherwise |
| elevationMeters     | Numeric | Elevation in meters. NULL if not collected. The upload will fill it  |
| bufferRadiusMeters     | Numeric | Radius of the buffer around the location (in meters). Use only if points need to be masked. NULL otherwise |
| isHidden     | Logical | t if points need to be masked  |
| trueCoordinates     | Logical | t if coordinates are not buffered |
| comments     | Text | Any comments related to locations. As needed |
| internal_wildtrax_id     | Numeric | Generated during the upload. Leave it blank |
| internal_update_ts     | Text | Generated during the upload. Leave it blank |


### VISIT TABLE

The **VISIT** attributes identify the date the survey was performed.
| Values   | Format   | Description   |
| :------- | :-------------- | :-------------- |
| location     | Text | The physical place on the landscape where the data was collected. Created using the concatenation of  [datasetCode]:[site]:[station], unless otherwise specified |
| visitDate     | Text | The date of the survey (YYYY-MM-DD) |
| snowDepthMeters     | Numeric | Generated during the upload. Leave it blank |
| waterDepthMeters     | Numeric | Generated during the upload. Leave it blank  |
| crew     | Text | Leave blank. ARUs field |
| bait     | Text | Use "None" for point count data  |
| accessMethod     | Text | Leave blank. ARUs field |
| landFeatures     | Text | Leave blank. ARUs field |
| comments     | Text | Any comments related to visit. As needed |
| wildtrax_internal_update_ts     | Text | Generated during the upload. Leave it blank |
| wildtrax_internal_lv_id     | Text | Generated during the upload. Leave it blank |

### SURVEY TABLE

The **SURVEY** attributes identify protocols, species, abundance, and time of the observations

| Values   | Format   | Description   |
| :------- | :-------------- | :-------------- |
| location     | Text | The physical place on the landscape where the data was collected. Created using the concatenation of  [datasetCode]:[site]:[station], unless otherwise specified |
| surveyDateTime     | Text | YYYY-MM-DD HH:MM:SS, Concatenation of  visitDate  survey_time; separated by space |
| durationMethod     | Text | The duration method used the count-remove species from the survey. Refer to duration_method_codes table  |
| distanceMethod     | Text | The distance band separation method used. Refer to distance_method_codes table   |
| observer     | Text | The observer code who conducted the survey. When observer name are provided in the source data, we create a lookup table where observer name get a serial number assigned using this format:  [Dataset Code]_[serial number] |
| species     | Text | AOU code used by WildTrax. See species codes table  |
| distanceband     | Text | The distance band the species was detected in. Refer to distance_band_codes table   |
| durationinterval     | Text | The duration interval the species was detected in. Refer to duration_interval_codes table  |
| abundance     | Numeric | Number of individual of a species with the same date, time, observer, isHeard, isSeen, distanceband and durationinterval information |
| isHeard     | Text | Was / were the bird(s) detected using a visual method (Yes, No or DNC). If no behaviour data, fill in as DNC except for NONE = null |
| isSeen     | Text | Was / were the bird(s) detected using an auditory method (Yes, No or DNC). If no behaviour data, fill in as DNC except for NONE = null |
| comments     | Text | Any comments related to survey. As needed |


### EXTENDED TABLE

The **EXTENDED** attributes are information BAM wants to keep but are not implemented in WildTrax

| Values   | Format   | Description   |
| :------- | :-------------- | :-------------- |
| site     | Text | Site is any part of the location name that groups the stations, if they are multiple parts, separate by hyphens (dashes) |
| station     | Text | Individual survey location, if there are multiple parts, separate by hyphens (dashes) |
| utm_zone     | Text | Include if the source coordinates were not using NAD83  |
| easting     | Text |  Source X if coordinates were transposed   |
| northing     | Text | Source Y if coordinates were transposed |
| missinginlocations     | Text | Information on missing locations  |
| time_zone     | Text | Include if in source data, use three letter codes, e.g. MDT, MST   |
| data_origin     | Text | Source of the data if differ from the organization  |
| missinginvisit     | Numeric | Information missing from the visit |
| pkey_dt     | Text | Concatenatation of  location:visitDate_survey_time:observer; separated by colons |
| survey_year     | Text | Year the data were collected |
| survey_time     | Text | HH:MM, 24 hour format |
| rawObserver     | Text | Which ever format the raw observer data came in |
| original_species     | Text | Species code provided in the source data if it differ from  WildTrax code, field is null where source data  match WildTrax species list |
| scientificname     | Text | Binomial classification (Genus, species) of the species |
| rawDistanceCode     | Text |  Original distance band used in source file or source protocol |
| rawDurationCode     | Text | Original duration interval used in source file or source protocol |
| originalBehaviourData     | Text | Any original behaviour codes at end of file |
| missingindetections     | Text | Comments on missing information |
| pc_vt     | Text | Vocalization type, same options as for ARU vocalization type, fill in if available. Refer to lookup table |
| pc_vt_detail     | Text | further detail on vocalization type, E.g Drumming, Counter Singing, fill in if available |
| age     | Text | Age related information. Refer to lookup table |
| fm     | Text | Sex related information. Refer to lookup table |
| group     | Text | Group related information. Refer to lookup table |
| flyover     | Text | Yes/No field for flyover. Refer to lookup table |
| displaytype     | Text | any display related information. Refer to lookup table |
| nestevidence     | Text | Nest related information. Refer to lookup table |
| behaviourother     | Text | Any behaviour that does not fit in the other columns |
| atlas_breeding_code     | Text | Fill in atlas code letters if these are part of original data |


