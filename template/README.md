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
| comments     | Text | Any comments related to locations. As needed |
| wildtrax_internal_update_ts     | Text | Generated during the upload. Leave it blank |
| wildtrax_internal_lv_id     | Text | Generated during the upload. Leave it blank |

### SURVEY TABLE

The **STANDARD_TYPE** attribute identifies the kind of inventory that was produced for an area. The name, abbreviation, or acronym usually becomes the name used to identify an inventory. For example, Alberta had a series of successive forest inventories called Phase 1, Phase 2, and Phase 3. As inventories became more inclusive of attributes other than just the trees, they became known as vegetation inventories, for example, the Alberta Vegetation Inventory or AVI. The inventory type along with a version number usually identifies an inventory.

| Values         | Description        |
| :------------- | :-------------- |
| Alphanumeric   | Inventory name or type of inventory |
| UNKNOWN_VALUE  | Inventory name or type of inventory is unknown |
