WildTrax upload is a 3 steps process. 




blabla


<a name=Location_tbl></a>
### LOCATION TABLE

The **LOCATION** attribute identifies the geographic extent of the site.

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

The **VISIT** attribute identifies the date the survey was performed.
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

The **SURVEY** attribute identifies protocols, species, abundance, and time of the observations

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
