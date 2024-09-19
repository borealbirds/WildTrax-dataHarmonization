###################################################
###################################################

## MAP output prior to upload

###################################################
###################################################

library(geodata)
library(terra)

#Set PARAM (dataset code and extent)
pc_location <-vect(data_flat, geom=c("longitude", "latitude"), crs="EPSG:4326", keepgeom=FALSE)
extent_polygon <- as.polygons(ext(pc_location), crs=crs(pc_location))
# DOWNLOAD PROPER EXTENT
canada <- gadm(country = "CAN", level = 1, path = "E:/MelinaStuff/BAM/data/geodata")

# Check if it falls inside
plot(canada) 
points(pc_location,col = "blue")
plot(extent_polygon, add= TRUE, border = "red", lwd= 2)
dev.off()

