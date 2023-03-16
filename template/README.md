
observer: 
### EXTENDED TABLE

The **EXTENDED** attributes are information BAM wants to keep but are not implemented in WildTrax

| Values   | Format   | Description   |
| :------- | :-------------- | :-------------- |
| location     | Text | The physical place on the landscape where the data was collected. Created using the concatenation of  [datasetCode]:[site]:[station], unless otherwise specified |
| surveyDateTime     | Text | YYYY-MM-DD HH:MM:SS, Concatenation of  visitDate  survey_time; separated by space |
| species     | Text | AOU code used by WildTrax. See species codes table  |
| distanceband     | Text | The distance band the species was detected in. Refer to distance_band_codes table   |
| durationinterval     | Text | The duration interval the species was detected in. Refer to duration_interval_codes table  |
| site     | Text | Site is any part of the location name that groups the stations, if they are multiple parts, separate by hyphens (dashes) |
| station     | Text | Individual survey location, if there are multiple parts, separate by hyphens (dashes) |
| utmZone     | Text | Include if the source coordinates were not using NAD83  |
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


