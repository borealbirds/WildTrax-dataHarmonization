# ---
# title: "Harmonize Canadian Nightjar Survey data for BAMDataset"
# author: "Elly Knight & Codex"
# date: "November 23, 2026"
# ---
# Source data are downloaded from NatureCounts by download/NatureCounts.R.
#
# This dataset is not currently destined for WildTrax. The harmonized CSV is
# staged on BAM_AvianData for ingestion into BAMDataset by a different
# repository. Do not add a WildTrax upload step until the required WildTrax
# functionality has been revised.
#
# Harmonization decisions:
# - Retain every source survey event. If an event has no nightjar or owl after
#   taxon filtering and duplicate removal, represent it with species = "NONE"
#   and abundance = 0. This includes events containing only incidental taxa.
# - Retain nightjars (Caprimulgiformes) and owls (Strigiformes); discard other
#   incidentally reported taxa. Map unidentified owl (Strigiformes sp.) to UNOW.
# - Remove every individual marked Duplicate = Y. Inspection showed that the
#   flag is not consistently limited to only the second record in a pair.
# - NatureCounts minute codes mean C = call, V = visual, N = not detected,
#   W = wingboom/wing-generated sound, and U = unknown detection type.
# - Retain only the first detection of each individual for aggregation: use the
#   earliest minute whose code is not N, then sum individuals by event, species,
#   first-detection minute/type, and distance band. If no usable detection code
#   exists, use first_detection_type = "UNKNOWN".
# - Translate first detections to the established BAM behavior fields:
#     C: isHeard Yes, isSeen No,  pc_vt Call,      pc_vt_detail DNC
#     V: isHeard No,  isSeen Yes, pc_vt Non-Vocal, pc_vt_detail DNC
#     W: isHeard Yes, isSeen No,  pc_vt Song,      pc_vt_detail Wing boom
#     U/UNKNOWN/NONE: DNC for all four behavior fields.
# - If W occurs in any of the six minutes, retain "Wingboom detected" in
#   comments even when W was not the individual's first detection.
# - Translate Near to 0m-100m and Far to 100m-INF. Missing or ambiguous
#   near/far values become UNKNOWN; the distance method is 0m-100m-INF.
# - SiteCode is the stable route-level site ID for random effects; Locality is
#   the station ID. Preserve visit-specific coordinates without averaging so
#   downstream geospatial extraction uses the recorded survey coordinates.
# - Retain the NatureCounts Collector value as observer_id.
# - Leave missing survey times and datetimes as NA; do not use a WildTrax time
#   placeholder because this output is staged directly for BAMDataset.
# - The source has only one generic stationary Nightjars protocol identifier
#   and no playback/broadcast field. Do not infer call-playback use without an
#   external year/site protocol rule.

library(dplyr)
library(readr)
library(stringr)

# Configuration -----------------------------------------------------------

root <- "G:/Shared drives/BAM_AvianData/Harmonization"
dataset_code <- "CanadianNightjarSurvey"

source_file <- file.path(
  root,
  "Others-dataHarmonization",
  "sourceData",
  "NightjarSurvey",
  paste0(dataset_code, ".Rdata")
)

output_file <- Sys.getenv(
  "BAM_NIGHTJAR_OUTPUT",
  unset = file.path(
    root,
    "Others-dataHarmonization",
    "toBAMDataset",
    paste0(dataset_code, ".csv")
  )
)

species_lookup_file <- file.path("lookupTables", "species_codes.csv")

retained_orders <- c("Caprimulgiformes", "Strigiformes")
valid_minute_codes <- c("C", "V", "N", "W", "U")

# Helpers -----------------------------------------------------------------

decimal_hour_to_time <- function(x) {
  decimal_hour <- suppressWarnings(as.numeric(x))
  seconds <- round(decimal_hour * 60 * 60)

  if_else(
    is.na(seconds),
    NA_character_,
    sprintf(
      "%02d:%02d:%02d",
      (seconds %/% 3600) %% 24,
      (seconds %% 3600) %/% 60,
      seconds %% 60
    )
  )
}

required_columns <- c(
  "record_id",
  "collection",
  "project_id",
  "protocol_id",
  "SiteCode",
  "latitude",
  "longitude",
  "survey_year",
  "survey_month",
  "survey_day",
  "Locality",
  "ProjectCode",
  "ProtocolType",
  "ProtocolCode",
  "SamplingEventIdentifier",
  "SurveyAreaIdentifier",
  "TimeObservationsStarted",
  "Collector",
  "NoObservations",
  "AllIndividualsReported",
  "AllSpeciesReported",
  "SpeciesCode",
  "CommonName",
  "ScientificName",
  "OrderTaxon",
  "ObservationCount",
  paste0("ObservationCount", 2:7),
  paste0("ObservationDescriptor", 2:7),
  "ObservationCount8",
  "ObservationDescriptor8",
  "ObservationCount9",
  "ObservationDescriptor9"
)

# Load and validate -------------------------------------------------------

if (!file.exists(source_file)) {
  stop(
    "Source file not found. Run download/NatureCounts.R first: ",
    source_file
  )
}

source_environment <- new.env(parent = emptyenv())
loaded_objects <- load(source_file, envir = source_environment)

if (!"njars" %in% loaded_objects || !is.data.frame(source_environment$njars)) {
  stop("The source RData file must contain the data frame `njars`.")
}

njars <- source_environment$njars
missing_columns <- setdiff(required_columns, names(njars))

if (length(missing_columns) > 0) {
  stop(
    "The NatureCounts download is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

if (!all(njars$collection == "NIGHTJAR", na.rm = TRUE)) {
  stop("The source contains records outside the NIGHTJAR collection.")
}

if (!all(njars$ProtocolCode == "Nightjars", na.rm = TRUE)) {
  stop("The source contains an unexpected NatureCounts protocol.")
}

expected_minute_descriptors <- paste("Minute", 1:6)
minute_descriptor_columns <- paste0("ObservationDescriptor", 2:7)

for (i in seq_along(minute_descriptor_columns)) {
  observed <- unique(na.omit(njars[[minute_descriptor_columns[i]]]))
  if (!identical(observed, expected_minute_descriptors[i])) {
    stop(
      minute_descriptor_columns[i],
      " does not consistently represent ",
      expected_minute_descriptors[i],
      "."
    )
  }
}

# Harmonize ---------------------------------------------------------------

species_lookup <- read_csv(species_lookup_file, show_col_types = FALSE) %>%
  select(
    standard_species_code = species_code,
    scientific_name_lookup = scientific_name
  ) %>%
  filter(!is.na(scientific_name_lookup)) %>%
  distinct(scientific_name_lookup, .keep_all = TRUE)

harmonized <- njars %>%
  mutate(
    dataset_code = dataset_code,
    sampling_event_id = SamplingEventIdentifier,
    survey_area_id = SurveyAreaIdentifier,
    site_id = SiteCode,
    station_id = na_if(str_trim(Locality), ""),
    has_precise_coordinates = !is.na(latitude) & !is.na(longitude),
    survey_date = sprintf(
      "%04d-%02d-%02d",
      survey_year,
      survey_month,
      survey_day
    ),
    # Missing source times remain NA rather than receiving a WildTrax sentinel.
    survey_time = decimal_hour_to_time(TimeObservationsStarted),
    survey_datetime = if_else(
      is.na(survey_time),
      NA_character_,
      paste(survey_date, survey_time)
    ),
    # NatureCounts Collector IDs are complete and stable within survey events.
    observer_id = as.character(Collector),
    source_no_observations = coalesce(NoObservations == "NoObs", FALSE),
    is_no_observation = source_no_observations,
    original_species_code = as.character(SpeciesCode),
    original_common_name = CommonName,
    scientific_name = ScientificName,
    taxonomic_order = OrderTaxon,
    abundance = if_else(
      is_no_observation,
      0L,
      suppressWarnings(as.integer(ObservationCount))
    ),
    minute_1 = str_trim(ObservationCount2),
    minute_2 = str_trim(ObservationCount3),
    minute_3 = str_trim(ObservationCount4),
    minute_4 = str_trim(ObservationCount5),
    minute_5 = str_trim(ObservationCount6),
    minute_6 = str_trim(ObservationCount7),
    source_duplicate_flag = coalesce(
      str_to_upper(str_trim(ObservationCount8)) == "Y",
      FALSE
    ),
    # Distance is stored by NatureCounts in ObservationCount9 rather than the
    # standard DistanceFromObserver fields, which are empty in this download.
    distance_band = case_when(
      is_no_observation ~ "UNKNOWN",
      is.na(ObservationCount9) ~ "UNKNOWN",
      str_trim(ObservationCount9) == "" ~ "UNKNOWN",
      str_to_lower(str_trim(ObservationCount9)) == "near" ~ "0m-100m",
      str_to_lower(str_trim(ObservationCount9)) == "far" ~ "100m-INF",
      TRUE ~ "UNKNOWN"
    ),
    is_retained_taxon = taxonomic_order %in% retained_orders
  ) %>%
  left_join(
    species_lookup,
    by = c("scientific_name" = "scientific_name_lookup")
  )

# Capture the complete event universe before taxon and duplicate filtering.
# This allows focal-group absences to remain represented even when a survey
# contains only incidental taxa or only detections flagged as duplicates.
survey_events <- harmonized %>%
  group_by(sampling_event_id) %>%
  slice(1) %>%
  ungroup()

duplicate_records_removed <- harmonized %>%
  filter(is_retained_taxon, source_duplicate_flag) %>%
  nrow()

retained_detections <- harmonized %>%
  # Duplicate is a source observation flag. Remove all flagged individuals;
  # do not try to infer which member of an apparent pair should be discarded.
  filter(is_retained_taxon, !source_duplicate_flag) %>%
  mutate(
    is_no_observation = FALSE,
    species = if_else(
      scientific_name == "Strigiformes sp.",
      "UNOW",
      standard_species_code
    )
  )

events_with_detections <- unique(retained_detections$sampling_event_id)

none_events <- survey_events %>%
  filter(!sampling_event_id %in% events_with_detections) %>%
  mutate(
    is_no_observation = TRUE,
    original_species_code = NA_character_,
    original_common_name = NA_character_,
    scientific_name = NA_character_,
    taxonomic_order = NA_character_,
    standard_species_code = NA_character_,
    species = "NONE",
    abundance = 0L,
    minute_1 = NA_character_,
    minute_2 = NA_character_,
    minute_3 = NA_character_,
    minute_4 = NA_character_,
    minute_5 = NA_character_,
    minute_6 = NA_character_,
    source_duplicate_flag = FALSE,
    distance_band = "UNKNOWN",
    is_retained_taxon = FALSE
  )

harmonized <- bind_rows(retained_detections, none_events)

observed_minute_codes <- harmonized %>%
  select(minute_1:minute_6) %>%
  unlist(use.names = FALSE) %>%
  unique() %>%
  na.omit()

unexpected_minute_codes <- setdiff(observed_minute_codes, valid_minute_codes)
if (length(unexpected_minute_codes) > 0) {
  stop(
    "Unexpected minute-level detection codes: ",
    paste(unexpected_minute_codes, collapse = ", ")
  )
}

if (anyDuplicated(harmonized$record_id)) {
  stop("The retained source contains duplicate NatureCounts record IDs.")
}

unmatched_species <- harmonized %>%
  filter(
    !is_no_observation,
    !is.na(scientific_name),
    is.na(species)
  ) %>%
  distinct(original_species_code, original_common_name, scientific_name)

if (nrow(unmatched_species) > 0) {
  warning(
    nrow(unmatched_species),
    " source taxa could not be matched to lookupTables/species_codes.csv."
  )
  print(unmatched_species)
}

# Derive first detection --------------------------------------------------
# Each source row represents one individual (ObservationCount is always 1 for
# retained detections). C, V, W, and U count as detections; N does not.

minute_columns <- paste0("minute_", 1:6)
minute_matrix <- as.matrix(harmonized[minute_columns])
is_detection <- !is.na(minute_matrix) & minute_matrix != "N"
has_first_detection <- rowSums(is_detection) > 0

first_detection_minute <- max.col(is_detection, ties.method = "first")
first_detection_minute[!has_first_detection] <- NA_integer_

first_detection_type <- rep(NA_character_, nrow(harmonized))
detected_rows <- which(has_first_detection)
first_detection_type[detected_rows] <- minute_matrix[
  cbind(detected_rows, first_detection_minute[detected_rows])
]

harmonized <- harmonized %>%
  mutate(
    first_detection_minute = first_detection_minute,
    first_detection_type = case_when(
      is_no_observation ~ NA_character_,
      is.na(first_detection_type) ~ "UNKNOWN",
      TRUE ~ first_detection_type
    ),
    # Follow the behavior-field conventions used by other scripts in this
    # repository. A wingboom is treated as Song with detail "Wing boom".
    isHeard = case_when(
      first_detection_type %in% c("C", "W") ~ "Yes",
      first_detection_type == "V" ~ "No",
      TRUE ~ "DNC"
    ),
    isSeen = case_when(
      first_detection_type == "V" ~ "Yes",
      first_detection_type %in% c("C", "W") ~ "No",
      TRUE ~ "DNC"
    ),
    pc_vt = case_when(
      first_detection_type == "C" ~ "Call",
      first_detection_type == "V" ~ "Non-Vocal",
      first_detection_type == "W" ~ "Song",
      TRUE ~ "DNC"
    ),
    pc_vt_detail = case_when(
      first_detection_type == "W" ~ "Wing boom",
      TRUE ~ "DNC"
    ),
    duration_method = "0-1-2-3-4-5-6min",
    duration_interval = case_when(
      first_detection_minute == 1 ~ "0-1min",
      first_detection_minute == 2 ~ "1-2min",
      first_detection_minute == 3 ~ "2-3min",
      first_detection_minute == 4 ~ "3-4min",
      first_detection_minute == 5 ~ "4-5min",
      first_detection_minute == 6 ~ "5-6min",
      TRUE ~ "UNKNOWN"
    ),
    distance_method = "0m-100m-INF",
    # This flag examines all minutes, independently of first detection.
    wingboom_detected = rowSums(minute_matrix == "W", na.rm = TRUE) > 0,
    protocol_type = ProtocolType,
    protocol_code = ProtocolCode,
    all_individuals_reported = AllIndividualsReported,
    all_species_reported = AllSpeciesReported
  )

# Aggregate individual detections ----------------------------------------
# Preserve the first-detection stratum while summing individual records.

harmonized <- harmonized %>%
  group_by(
    dataset_code,
    sampling_event_id,
    survey_area_id,
    site_id,
    station_id,
    latitude,
    longitude,
    has_precise_coordinates,
    survey_date,
    survey_time,
    survey_datetime,
    observer_id,
    protocol_type,
    protocol_code,
    all_individuals_reported,
    all_species_reported,
    source_no_observations,
    is_no_observation,
    original_species_code,
    original_common_name,
    scientific_name,
    taxonomic_order,
    species,
    first_detection_minute,
    first_detection_type,
    isHeard,
    isSeen,
    pc_vt,
    pc_vt_detail,
    duration_method,
    duration_interval,
    distance_method,
    distance_band
  ) %>%
  summarise(
    abundance = sum(abundance),
    source_record_count = n(),
    source_record_ids = paste(record_id, collapse = ";"),
    comments = if_else(
      any(wingboom_detected),
      "Wingboom detected",
      NA_character_
    ),
    .groups = "drop"
  ) %>%
  arrange(
    survey_date,
    sampling_event_id,
    species,
    first_detection_minute,
    first_detection_type,
    duration_interval,
    distance_band
  )

# QA ----------------------------------------------------------------------

if (any(harmonized$abundance < 0 | is.na(harmonized$abundance))) {
  stop("The harmonized output contains invalid abundance values.")
}

if (any(harmonized$is_no_observation & harmonized$species != "NONE")) {
  stop("A no-observation row has a species code other than NONE.")
}

if (any(harmonized$is_no_observation & harmonized$abundance != 0)) {
  stop("A no-observation row has non-zero abundance.")
}

message("Source rows: ", nrow(njars))
message("Output rows: ", nrow(harmonized))
message("Sampling events: ", n_distinct(harmonized$sampling_event_id))
message("No-observation rows: ", sum(harmonized$is_no_observation))
message("Detection rows: ", sum(!harmonized$is_no_observation))
message("Individuals represented: ", sum(harmonized$abundance))
message("Duplicate nightjar/owl records removed: ", duplicate_records_removed)
message("Rows with wingboom comments: ", sum(!is.na(harmonized$comments)))
message("Rows missing survey time: ", sum(is.na(harmonized$survey_time)))

# Export ------------------------------------------------------------------

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
write_csv(harmonized, output_file, na = "")
message("Wrote: ", output_file)
