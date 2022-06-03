WildTrax upload is a 3 steps process. 







<a name=Location_tbl></a>
### LOCATION TABLE

The **OWNER_NAME** attribute identifies who owns the land covered by the inventory.

| Values   | Format   | Description   |
| :------- | :-------------- | :-------------- |
| location     | Text | The physical place on the landscape where the data was collected. Created using the concatenation of  [datasetCode]:[site]:[station], unless otherwise specified |
| latitude     | Decimal degrees | NAD83, convert if otherwise |
| longitude     | Decimal degrees | NAD83, convert if otherwise |
| elevationMeters     | Numeric | Elevation in meters. NULL if not collected. The upload will fill it  |
| bufferRadiusMeters     | Numeric | Radius of the buffer around the location (in meters). Use only if points need to be masked. NULL otherwise |
| isHidden     | Logical | t if points need to be masked  |
| trueCoordinates     | Logical | t if coordinates are not buffered |
| comments     | TEXT | Any comments related to locations. As needed |
| internal_wildtrax_id     | Numeric | Generated during the upload. Leave it blank |
| internal_update_ts     | Text | Generated during the upload. Leave it blank |


### SURVEY TABLE

The **STANDARD_TYPE** attribute identifies the kind of inventory that was produced for an area. The name, abbreviation, or acronym usually becomes the name used to identify an inventory. For example, Alberta had a series of successive forest inventories called Phase 1, Phase 2, and Phase 3. As inventories became more inclusive of attributes other than just the trees, they became known as vegetation inventories, for example, the Alberta Vegetation Inventory or AVI. The inventory type along with a version number usually identifies an inventory.

| Values         | Description        | Format        |
| :------------- | :-------------- |
| Alphanumeric   | Inventory name or type of inventory |
| UNKNOWN_VALUE  | Inventory name or type of inventory is unknown |

### VISIT TABLE

The **STANDARD_TYPE** attribute identifies the kind of inventory that was produced for an area. The name, abbreviation, or acronym usually becomes the name used to identify an inventory. For example, Alberta had a series of successive forest inventories called Phase 1, Phase 2, and Phase 3. As inventories became more inclusive of attributes other than just the trees, they became known as vegetation inventories, for example, the Alberta Vegetation Inventory or AVI. The inventory type along with a version number usually identifies an inventory.

| Values         | Description        |
| :------------- | :-------------- |
| Alphanumeric   | Inventory name or type of inventory |
| UNKNOWN_VALUE  | Inventory name or type of inventory is unknown |
