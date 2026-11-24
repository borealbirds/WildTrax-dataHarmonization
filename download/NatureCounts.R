# ---
# title: "Download Nature Counts data"
# author: "Elly Knight"
# date: "March 9, 2026"
# Notes:

library(tidyverse)
library(naturecounts)

root <- "G:/Shared drives/BAM_AvianData/Harmonization"

#1. Get list of projects accessible ----
proj.ek <- nc_count(username = "ecknight", show = "all")
req.ek <- nc_requests(username = "ecknight")

proj.bam <- nc_count(username = "bamp", show = "all")
req.bam <- nc_requests(username = "bamp")

#2. Download ----
bcmmp <- nc_data_dl(
  username = "ecknight",
  request_id = c(265595),
  info = "waterbird models"
)
glmmp <- nc_data_dl(
  username = "ecknight",
  request_id = c(265584),
  info = "waterbird models",
  collection = "MMPBIRDS"
)

njars <- nc_data_dl(
  username = "bamp",
  request_id = c(288508)
)

#3. Save ----
save(
  bcmmp,
  file = file.path(root, "Waterfowl", "Data", "NatureCounts", "BCMMP.Rdata")
)
save(
  glmmp,
  file = file.path(root, "Waterfowl", "Data", "NatureCounts", "GLMMP.Rdata")
)
save(
  njars,
  file = file.path(
    root,
    "Others-dataHarmonization",
    "sourceData",
    "NightjarSurvey",
    "CanadianNightjarSurvey.Rdata"
  )
)
