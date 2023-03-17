# ---
# title: "Template translation script"
# author: "Melina Houle"
# date: "March 8, 2022"
# ---

library(sp)
library(sf)

DD <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


wd <- "E:/MelinaStuff/BAM/WildTrax/WT-Integration"
setwd(wd)

organization = "BAM"
dataset_code <- "BAM-BBS"

location_f <- list.files(file.path("E:/MelinaStuff/BAM/temp/"), pattern = '.location.csv')

#location_f <- list.files(file.path(wd, "out", dataset_code), pattern = '.location.csv')
loc <- lapply(location_f, function(x) {
  print(x)
  location_tbl <- read.csv(file.path("E:/MelinaStuff/BAM/temp", x), sep=",", header = TRUE, stringsAsFactors = FALSE)
  #location_tbl <- read.csv(file.path(wd, "out", dataset_code, x), sep=",", header = TRUE, stringsAsFactors = FALSE)
  return(location_tbl)
})
loc_bind <-do.call(rbind, loc) 

loc_bind <- subset(loc_bind, !(is.na(loc_bind$latitude)))

coordinates(loc_bind) <- c("longitude", "latitude")
proj4string(loc_bind) <- DD

pc <- st_as_sf(loc_bind, coords = c("longitude", "latitude"), crs = DD)

## Map canada extent
f_canada <-"E:/MelinaStuff/BAM/NationalModelv4.1/runNationalModel/data/basemaps/CanadaLAEA.shp"
canada<- st_read(f_canada)
canada_DD <-st_transform(canada, DD)
plot(st_geometry(pc), lwd=3)

plot(st_geometry(canada_DD), border="grey", add = TRUE)


ylims <- c(34.67, 83.11)
xlims <- c(-168, -52.66)
box_coords <- tibble(x = xlims, y = ylims) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(DD)

#get the bounding box of the two x & y coordintates, make sfc
bounding_box <- st_bbox(box_coords) %>% st_as_sfc()
plot(bounding_box, add = TRUE)

diff_st<- st_intersects(pc, bounding_box) 
finalpts<-pc[is.na(diff_st)] 
