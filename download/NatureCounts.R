# ---
# title: "Download Nature Counts data"
# author: "Elly Knight"
# date: "March 9, 2026"
# Notes: 

library(tidyverse)
library(naturecounts)

root <- "G:/Shared drives/BAM_AvianData/Harmonization"

#1. Get list of projects accessible ----
proj <- nc_count(username = "ecknight", show="all")
req <- nc_requests(username = "ecknight")

#2. Download ----
bcmmp <- nc_data_dl(username = "ecknight", request_id = c(265595), info="waterbird models")
glmmp <- nc_data_dl(username = "ecknight", request_id = c(265584), info="waterbird models", collection = "MMPBIRDS")

#3. Save ----
save(bcmmp, file=file.path(root, "Waterfowl", "Data", "NatureCounts", "BCMMP.Rdata"))
save(glmmp,  file=file.path(root, "Waterfowl", "Data", "NatureCounts", "GLMMP.Rdata"))
