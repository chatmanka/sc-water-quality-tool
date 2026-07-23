# ══════════════════════════════════════════════════════════════════════════
# SC WATER QUALITY DATA TOOL
# Kate Chatman for the S.C. Sea Grant Consortium — SC Water Monitoring Portal (WMP) project
#
# Purpose:  Replaces the manual EPA Water Quality Portal (WQP) workflow for
#           the SC WMP. The old process required downloading very large raw
#           CSVs from the EPA site, trimming 181 raw columns down to 17,
#           joining station coordinates, assigning parameter categories, and
#           reformatting for ArcGIS. Process was hours to days of work per update, and
#           too large for Excel. Shu-Mei Huang requested a streamlined cleaning code,
#           several months later this Shiny app was built to simply the process for
#           non-R code users. This app performs the entire pipeline in the browser
#           (download → clean → categorize → export) with no coding required. 
#
# Data source: EPA Water Quality Portal (https://www.waterqualitydata.us/),
#           accessed via the WQX 3.0 REST API. Results are pulled for all of
#           South Carolina (FIPS state code US:45), water media only.
#           Parameters and QA/QC protocol provided by Shu-Mei Huang at S.C. Sea Grant Consortium.
#
# Pipeline overview (see the matching numbered sections below):
#   [1] Lookup table    — maps EPA CharacteristicName → WMP parameter Category
#   [2] Column mapping  — maps raw WQX 3.0 field names → clean output names
#   [3] Downloaders     — chunked Result pulls + one Station (coordinates) pull
#   [4] Chunk builder   — splits the request into calendar-year pieces
#   [5] Cleaner         — trim, join coordinates, categorize, QC coordinates
#   [6] UI              — date pickers, progress bar, summary, preview, export
#   [7] Server          — connects pipeline to the user interface, reactively
#
# Output:   A cleaned, analysis-ready CSV with the 17 WMP data columns plus
#           four QC columns (FalseLatitude, FalseLongitude, flag_negLat,
#           flag_coordOutOfRange), an optional coordinate-error flag report
#           formatted for submission to WQX@epa.gov, and a separate
#           unresolved-coordinate report for records a simple swap could not
#           fix (these should NOT be sent to EPA until reviewed).
#
# Column names follow WQX 3.0 conventions (not the legacy WMP headers) per
# project team decision, May 2026 — see WMP_Column_Name_Reference.docx for
# the six legacy -> new renames.
#
# Deployment: shinyapps.io (https://3ufznw-kate-chatman.shinyapps.io/sc-water-quality-tool/)
#
# Change log:
#   Jul 2026 — Added post-correction South Carolina bounding-box check
#              (flag_coordOutOfRange) after a 2024 test run found 4 sites
#              where a negative-latitude swap did not produce a valid SC
#              location. New "Unresolved Coordinate Report" panel/download.
#   Jul 2026 — Full annotation pass; added "Visit the EPA Water Quality
#              Portal" link button in the left panel (requested by S. Huang).
#   May 2026 — Beta release shared with WMP project team.
# ══════════════════════════════════════════════════════════════════════════

# ── Packages ──
library(shiny)   # web application framework — UI + reactive server
library(httr)    # HTTP client used to call the EPA WQP REST API (GET requests)
library(readr)   # fast CSV parsing (read_csv) and writing (write_csv)
library(dplyr)   # data manipulation verbs: mutate, filter, left_join, bind_rows
library(bslib)   # Bootstrap theming for Shiny (the "flatly" look and colors)
library(DT)      # interactive HTML data table used for the 200-row preview

# ─────────────────────────────────────────────
# [1] LOOKUP TABLE: CharacteristicName → Category
# ─────────────────────────────────────────────
# Assigns each EPA parameter to one of the 8 WMP display categories
# (Physical, water column / Nutrients / Bacteria-HAB / Biological /
# Alkalinity / Chemical, other / Weather / Physical). The category values
# come from the WMP project team's Summary_of_WQ_Parameters table.
#
# How it works: this is a *named character vector*. The names (left side)
# are EPA CharacteristicName values exactly as the WQP returns them —
# spelling, capitalization, and punctuation must match exactly. The values
# (right side) are the WMP Category assigned to that parameter. Lookup is a
# simple vector index: wq_lookup["Phosphorus"] returns "Nutrients".
#
# Why embedded in the script rather than read from a file: shinyapps.io
# deployments are simplest when self-contained — no external file to upload,
# version, or lose. To add a new parameter, add one line here in the form
#   "EPA CharacteristicName" = "Category",
# and redeploy. Any characteristic NOT listed here gets Category = NA and is
# dropped by the non-parameter filter in the cleaning step (section [5]).
wq_lookup <- c(
  "Temperature, water" = "Physical, water column",
  "Dissolved oxygen (DO)" = "Physical, water column",
  "pH" = "Alkalinity",
  "Salinity" = "Physical, water column",
  "Fecal Coliform" = "Bacteria/HAB",
  "Turbidity" = "Physical, water column",
  "Inorganic nitrogen (nitrate and nitrite)" = "Nutrients",
  "Tide stage (choice list)" = "Physical, water column",
  "Biochemical oxygen demand, standard conditions" = "Biological",
  "Phosphorus" = "Nutrients",
  "Alkalinity, total" = "Alkalinity",
  "Kjeldahl nitrogen" = "Nutrients",
  "Depth" = "Physical, water column",
  "Weather condition (WMO code 4501) (choice list)" = "Weather",
  "Enterococcus" = "Bacteria/HAB",
  "Nitrogen" = "Nutrients",
  "Temperature, air" = "Weather",
  "Ammonia" = "Nutrients",
  "Iron" = "Nutrients",
  "Specific conductance" = "Physical",
  "Wind direction (direction from, expressed 0-360 deg)" = "Weather",
  "Manganese" = "Chemical, other",
  "Lead" = "Chemical, other",
  "Copper" = "Chemical, other",
  "Zinc" = "Chemical, other",
  "Cadmium" = "Chemical, other",
  "Chromium" = "Chemical, other",
  "Nickel" = "Chemical, other",
  "Conductivity" = "Physical, water column",
  "Dissolved oxygen saturation" = "Physical, water column",
  "Escherichia coli" = "Bacteria/HAB",
  "Mercury" = "Chemical, other",
  "Magnesium" = "Chemical, other",
  "Calcium" = "Chemical, other",
  "RBP2, Weather Condition, Now (choice list)" = "Physical, water column",
  "Hardness, Ca, Mg" = "Chemical, other",
  "Total dissolved solids" = "Physical, water column",
  "Organic carbon" = "Biological",
  "Total suspended solids" = "Physical, water column",
  "Sodium" = "Chemical, other",
  "Potassium" = "Nutrients",
  "Chlorophyll a, corrected for pheophytin" = "Biological",
  "True color" = "Physical, water column",
  "Nitrate" = "Nutrients",
  "Depth, Secchi disk depth" = "Physical, water column",
  "Chloride" = "Chemical, other",
  "Phosphate-phosphorus as P" = "Nutrients",
  "Oxygen" = "Physical, water column",
  "Inorganic nitrogen (nitrate and nitrite) as N" = "Nutrients",
  "Nitrite" = "Nutrients",
  "Orthophosphate as P" = "Nutrients",
  "Fluoride" = "Chemical, other",
  "Ammonia as NH3" = "Nutrients",
  "Acidity, (H+)" = "Physical, water column",
  "Orthophosphate" = "Nutrients",
  "Ammonia and ammonium" = "Nutrients",
  "Sulfate" = "Chemical, other",
  "Total solids" = "Physical, water column",
  "Chlorophyll a" = "Biological",
  "Total Coliform" = "Bacteria/HAB",
  "Alkalinity" = "Alkalinity",
  "Stream flow, instantaneous" = "Physical, water column",
  "Precipitation" = "Weather",
  "Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)" = "Nutrients",
  "Total fixed solids" = "Physical, water column",
  "Suspended Sediment Concentration (SSC)" = "Physical, water column",
  "Silica" = "Nutrients",
  "Carbon dioxide" = "Physical, water column",
  "Light, incident" = "Weather",
  "Flow" = "Physical, water column",
  "Sodium adsorption ratio [(Na)/(sq root of 1/2 Ca + Mg)]" = "Chemical, other",
  "Sodium, percent total cations" = "Chemical, other",
  "Arsenic" = "Chemical, other",
  "Organic Nitrogen" = "Nutrients",
  "Alkalinity, Carbonate as CaCO3" = "Alkalinity",
  "Bromide" = "Chemical, other",
  "Height, gage" = "Physical, water column",
  "Bicarbonate" = "Alkalinity",
  "Selenium" = "Chemical, other",
  "Fecal Streptococcus Group Bacteria" = "Bacteria/HAB",
  "Barometric pressure" = "Weather",
  "Aluminum" = "Chemical, other",
  "Ammonia-nitrogen" = "Nutrients",
  "Pheophytin a" = "Biological",
  "Cobalt" = "Chemical, other",
  "Barium" = "Chemical, other",
  "Carbonate" = "Alkalinity",
  "Silver" = "Chemical, other",
  "Total Kjeldahl nitrogen" = "Nutrients",
  "Chlorophyll b" = "Biological",
  "Chlorophyll" = "Biological",
  "Nitrate + Nitrite" = "Nutrients",
  "Total Nitrogen, mixed forms" = "Nutrients",
  "Total Phosphorus, mixed forms" = "Nutrients",
  "Nitrate as N" = "Nutrients",
  "Nitrite as N" = "Nutrients",
  "Ammonium" = "Nutrients",
  "Inorganic nitrogen" = "Nutrients",
  "Total Kjeldahl nitrogen (Organic N & NH3)" = "Nutrients",
  "Soluble Reactive Phosphorus (SRP)" = "Nutrients",
  "Phosphate-phosphorus" = "Nutrients",
  "Inorganic phosphorus" = "Nutrients",
  "Organic phosphorus" = "Nutrients",
  "Orthophosphate as PO4" = "Nutrients",
  "Silicon" = "Nutrients",
  "Silicate" = "Nutrients",
  "Ferrous ion" = "Nutrients",
  "Chlorophyll a, uncorrected for pheophytin" = "Biological",
  "Phytoplankton" = "Biological",
  "Chemical oxygen demand" = "Biological",
  "Oxidation reduction potential (ORP)" = "Chemical, other",
  "Geosmin" = "Bacteria/HAB",
  "Microcystins/nodularin" = "Bacteria/HAB",
  "Microcystin" = "Chemical, other",
  "Microcystins" = "Chemical, other",
  "Total microcystins plus nodularins" = "Bacteria/HAB",
  "Cylindrospermopsin" = "Chemical, other",
  "Temperature" = "Physical, water column",
  "Hardness, carbonate" = "Alkalinity",
  "Calcium carbonate" = "Alkalinity",
  "Alkalinity, carbonate" = "Alkalinity"
)

# ─────────────────────────────────────────────
# [2] COLUMN MAPPING: WQX 3.0 raw → clean output names
# ─────────────────────────────────────────────
# The WQX 3.0 "fullPhysChem" profile returns 181 raw columns with API-style
# names (e.g., "Org_FormalName"). We keep only the 14 result columns the WMP
# needs and rename them to the output names below. (The other 3 of the 17
# WMP columns — LatitudeMeasure, LongitudeMeasure, and Category — are added
# later: coordinates come from the Station join and Category from the lookup
# table, both in section [5].)
#
# Format: names (left) = raw WQX 3.0 API column; values (right) = clean
# output column name. Only columns listed here survive the trim.
#
# NOTE ON LEGACY NAMES: six output names differ from the legacy WMP headers
# (e.g., legacy "ResultMeasure/MeasureUnitCode" → "ResultMeasureUnit").
# Per project decision (May 2026) we keep the new WQX 3.0-style names, since
# they match what users see downloading raw data from EPA, and legacy names
# containing "/" are invalid as ArcGIS field names anyway. If a rename back
# to legacy were ever needed, this vector is where it would happen.
col_map_results <- c(
  "Org_FormalName"                  = "OrganizationFormalName",
  "Activity_StartDate"              = "ActivityStartDate",
  "Activity_StartTime"              = "ActivityStartTime",
  "Activity_DepthHeightMeasure"     = "ActivityDepthHeightMeasure_Value",
  "Activity_DepthHeightMeasureUnit" = "ActivityDepthHeightMeasure_Unit",
  "Project_Name"                    = "ProjectName",
  "Location_Identifier"             = "MonitoringLocationIdentifier",
  "SampleCollectionMethod_Name"     = "SampleCollectionMethod",
  "Result_Characteristic"           = "CharacteristicName",
  "Result_SampleFraction"           = "ResultSampleFractionText",
  "Result_Measure"                  = "ResultMeasureValue",
  "Result_MeasureUnit"              = "ResultMeasureUnit",
  "DataQuality_ResultComment"       = "ResultCommentText",
  "ResultAnalyticalMethod_Name"     = "ResultAnalyticalMethod"
)

# ─────────────────────────────────────────────
# [3a] HELPER: download one annual chunk of Result data from the WQP
# ─────────────────────────────────────────────
# Requests ONE calendar year (or partial year) of SC results. We never ask
# EPA for the full date range in a single request: pulls beyond ~1 year
# routinely time out on EPA's side (an early full-range test died at 53 MB).
# The server loop in section [7] calls this once per year and stitches the
# chunks together. Returns a data frame of raw results, or NULL if the year
# had no data / failed to parse (the caller treats NULL as "skip year").
download_wqp_chunk <- function(start_date, end_date) {
  # WQX 3.0 Result endpoint — the modern API path (note "/wqx3/" in the URL)
  base_url <- "https://www.waterqualitydata.us/wqx3/Result/search"

  # Query parameters, passed to GET() below:
  params <- list(
    statecode    = "US:45",         # FIPS code for South Carolina — statewide pull
    sampleMedia  = "Water",         # water samples only (excludes air, sediment, tissue)
    # EPA requires dates as MM-DD-YYYY; format() converts from R's ISO dates
    startDateLo  = format(as.Date(start_date), "%m-%d-%Y"),   # range start
    startDateHi  = format(as.Date(end_date),   "%m-%d-%Y"),   # range end
    mimeType     = "csv",           # ask for CSV rather than JSON/Excel
    dataProfile  = "fullPhysChem",  # the 181-column physical/chemical profile
    # Repeated "providers" keys are intentional: httr sends the parameter
    # twice, which is how the API expects multi-value filters. This requests
    # both source systems EPA aggregates: NWIS (USGS) and STORET (states/EPA).
    providers    = "NWIS",
    providers    = "STORET"
  )

  # Make the HTTP request. timeout(600) allows up to 10 minutes — busy years
  # (100k+ rows) can be slow on EPA's side. tryCatch converts a low-level
  # network failure into a readable error message for the status box.
  resp <- tryCatch(
    GET(base_url, query = params, timeout(600)),
    error = function(e) stop(paste("Network error:", conditionMessage(e)))
  )

  # Non-200 HTTP status (e.g., 500 from EPA) → stop with the code visible
  if (http_error(resp)) stop(paste("EPA portal error:", status_code(resp)))

  # Extract the response body as text (a CSV string)
  raw_text <- content(resp, as = "text", encoding = "UTF-8")
  # A body under 100 characters is just a header row / empty response —
  # treat as "no data for this year" rather than an error
  if (nchar(raw_text) < 100) return(NULL)

  # Parse the CSV. Every column is read as character on purpose:
  # read_csv guesses types per chunk, and a column that looks numeric in one
  # year but has text in another would make bind_rows() fail when chunks are
  # combined. Reading everything as text keeps all years type-consistent; we know
  # what type everything is and so we are less likely to have errors,
  # numeric conversion happens later, only where needed (i.e. coordinates).
  # name_repair = "unique" guards against duplicate raw column names.
  tryCatch(
    read_csv(I(raw_text), col_types = cols(.default = col_character()),
             show_col_types = FALSE, name_repair = "unique"),
    error = function(e) NULL   # unparseable response → treat as empty year
  )
}

# ─────────────────────────────────────────────
# [3b] HELPER: download the station table (site lat/lon coordinates)
# ─────────────────────────────────────────────
# The Result data identifies each record only by MonitoringLocationIdentifier
# (EPA's legacy site ID) — it carries no coordinates. This pulls the separate
# Station table for all SC monitoring sites so the cleaner (section [5]) can
# join lat/lon onto every result. Downloaded ONCE per run (site locations
# don't change year to year), regardless of how many annual chunks run. If the site
# locations ever did change, or you needed to update station locations, it would go here.
download_stations <- function() {
  resp <- tryCatch(
    # NOTE: this is the legacy "/data/" Station endpoint, not "/wqx3/" —
    # it reliably returns the full SC site list with coordinates
    GET("https://www.waterqualitydata.us/data/Station/search",
        query = list(
          countrycode = "US",       # required scoping parameter
          statecode   = "US:45",    # South Carolina FIPS code
          mimeType    = "csv",      # CSV response
          zip         = "no",       # plain CSV, not a zipped archive
          # Repeated "providers" keys (see [3a]) — stations come from three
          # source systems here, including STEWARDS (USDA-ARS sites)
          providers   = "NWIS",
          providers   = "STEWARDS",
          providers   = "STORET"
        ),
        timeout(300)),   # 5-minute cap; the station table is much smaller than results
    error = function(e) stop(paste("Station download error:", conditionMessage(e)))
  )

  if (http_error(resp)) stop(paste("Station endpoint error:", status_code(resp)))

  # Parse to a data frame — all-character for the same type-safety reason as [3a]
  raw_text <- content(resp, as = "text", encoding = "UTF-8")
  stations <- read_csv(I(raw_text), col_types = cols(.default = col_character()),
                       show_col_types = FALSE, name_repair = "unique")

  # Log actual column names so we can see what the API returned.
  # EPA has changed station field names between API versions before; this
  # message (visible in the shinyapps.io log) is the first place to look if
  # the coordinate join in section [5] ever starts failing.
  message("Station columns received: ", paste(names(stations), collapse = " | "))
  stations
}

# ─────────────────────────────────────────────
# [4] HELPER: split the requested date range into calendar-year chunks
# ─────────────────────────────────────────────
# Turns one user-selected range into a list of ≤1-year pieces, one per
# calendar year, so each API request stays small enough not to time out
# (the whole reason the download is chunked — see [3a]). Example:
# 2023-06-15 → 2025-02-01 becomes three chunks:
#   2023-06-15 → 2023-12-31, 2024-01-01 → 2024-12-31, 2025-01-01 → 2025-02-01.
build_chunks <- function(start_date, end_date) {
  start    <- as.Date(start_date)
  end      <- as.Date(end_date)
  yr_start <- as.integer(format(start, "%Y"))   # first calendar year in range
  yr_end   <- as.integer(format(end,   "%Y"))   # last calendar year in range
  chunks   <- list()
  for (yr in yr_start:yr_end) {
    chunks[[length(chunks) + 1]] <- list(
      # First chunk starts at the user's chosen start date; every later
      # chunk starts Jan 1 of its year
      start = if (yr == yr_start) start else as.Date(paste0(yr, "-01-01")),
      # Last chunk ends at the user's chosen end date; every earlier
      # chunk ends Dec 31 of its year
      end   = if (yr == yr_end)   end   else as.Date(paste0(yr, "-12-31"))
    )
  }
  chunks   # list of {start, end} pairs consumed by the server loop in [7]
}

# ─────────────────────────────────────────────
# SOUTH CAROLINA COORDINATE BOUNDING BOX
# ─────────────────────────────────────────────
# Used by the post-correction validation check in [5e] below. A generous box
# around SC's actual borders (mainland lat ≈ 32.0–35.2°N, lon ≈ -83.4 to
# -78.5°W), padded slightly to avoid false positives for legitimate
# near-border sites. Any record whose FINAL coordinates (after the negative-
# latitude swap correction) fall outside this box could not be fixed by a
# simple swap and needs a human to look at the source record.
SC_LAT_MIN <- 31.5
SC_LAT_MAX <- 35.5
SC_LON_MIN <- -84.0
SC_LON_MAX <- -78.0

# ─────────────────────────────────────────────
# [5] HELPER: clean and trim the raw data
# ─────────────────────────────────────────────
# This is the bulk of the pipeline and what Shu-Mei asked for first. 
# Takes the combined raw results (all chunks) and the station table,
# and returns the analysis-ready WMP dataset.  
# Five stages:
#   (a) trim 181 raw columns → 14, renamed via col_map_results
#   (b) join station lat/lon by MonitoringLocationIdentifier
#   (c) assign Category from the lookup table
#   (d) drop rows that aren't tracked WMP parameters
#   (e) detect and correct flipped coordinates (per the WMP QC protocol)
#   (f) validate corrected coordinates against SC's real geographic bounds,
#       flagging any that are still implausible even after correction
#       (added Jul 2026, after a 2024 test run found 4 sites where a simple
#       swap did not produce a valid South Carolina location)
# Output: 17 data columns + FalseLatitude, FalseLongitude, flag_negLat,
#         flag_coordOutOfRange.
clean_wqp_data <- function(results_df, stations_df) {

  # --- (a) Trim results to 14 columns ---
  # intersect() keeps only mapped columns that actually exist in this pull —
  # so if EPA ever drops a field, the app degrades gracefully (that column
  # is simply absent) instead of erroring on a missing name.
  res_cols        <- intersect(names(col_map_results), names(results_df))
  results_trimmed <- results_df[, res_cols, drop = FALSE]
  names(results_trimmed) <- col_map_results[res_cols]   # raw names → clean names

  # --- (b) Find station coordinate columns (handles both legacy and WQX 3.0 naming) ---
  # EPA's Station endpoint has used different ID/coordinate field names across
  # API versions. Rather than hard-coding one name and breaking when EPA
  # changes it, we search a list of known aliases in priority order and take
  # the first that exists ([1]). If none exists, the result is NA and we fall
  # into the warning branch below instead of crashing.
  stn <- names(stations_df)
  id_col  <- intersect(c("MonitoringLocationIdentifier", "Location_Identifier"), stn)[1]
  lat_col <- intersect(c("LatitudeMeasure", "Location_Latitude",
                          "LatitudeStandardized", "Location_LatitudeStandardized"), stn)[1]
  lon_col <- intersect(c("LongitudeMeasure", "Location_Longitude",
                          "LongitudeStandardized", "Location_LongitudeStandardized"), stn)[1]

  # Logged to the app console/logs so the join can be debugged after the fact
  message("Station join — ID: ", id_col, " | Lat: ", lat_col, " | Lon: ", lon_col)

  # --- Join lat/lon onto every result row ---
  if (!is.na(id_col) && !is.na(lat_col) && !is.na(lon_col)) {
    # Keep only the three needed station columns, standardize their names
    stations_trimmed <- stations_df[, c(id_col, lat_col, lon_col), drop = FALSE]
    names(stations_trimmed) <- c("MonitoringLocationIdentifier", "LatitudeMeasure", "LongitudeMeasure")
    # Deduplicate — keep first record per site. A site listed by multiple
    # providers would otherwise multiply result rows in the join below.
    stations_trimmed <- stations_trimmed[!duplicated(stations_trimmed$MonitoringLocationIdentifier), ]
    message("Unique stations: ", nrow(stations_trimmed))
    # left_join keeps ALL result rows; a site missing from the station table
    # gets NA coordinates rather than being silently dropped
    clean <- left_join(results_trimmed, stations_trimmed, by = "MonitoringLocationIdentifier")
  } else {
    # Coordinate columns not found (EPA schema change?) — proceed without
    # coordinates so the user still gets data, but warn loudly
    warning("Could not find lat/lon columns in station data — coordinates will be missing.")
    clean <- results_trimmed
  }

  # --- (c) Add Category from lookup table ---
  # Vectorized lookup: wq_lookup[CharacteristicName] returns the mapped WMP
  # category for each row, or NA when the characteristic isn't in the table
  if ("CharacteristicName" %in% names(clean)) {
    clean <- clean %>% mutate(Category = wq_lookup[CharacteristicName])
  }

  # --- (d) Remove non-parameters ---
  # Drop rows the WMP doesn't track: anything that didn't match the lookup
  # (Category is NA) or is explicitly categorized "Not a parameter". This is
  # where most of the raw-row reduction happens; the removed count is
  # reported in the final status message in section [7].
  if ("Category" %in% names(clean)) {
    clean <- clean %>% filter(!is.na(Category), Category != "Not a parameter")
  }

  # --- (e) Handle negative latitudes (flipped coordinates, per the WMP QC protocol) ---
  # KNOWN EPA DATA QUALITY ISSUE: some stations in the EPA database have
  # latitude and longitude entered in each other's fields. The giveaway is a
  # NEGATIVE latitude — impossible for South Carolina (lat ≈ +32 to +35),
  # while SC longitudes are always negative (≈ −78 to −83). So a negative
  # "latitude" is really the longitude, and the paired "longitude" (positive)
  # is really the latitude.
  #
  # The protocol (set by the WMP project lead): CORRECT the coordinates so
  # the point maps properly, PRESERVE the original wrong values in
  # FalseLatitude/FalseLongitude for transparency, and FLAG the record so a
  # report can be sent to WQX@epa.gov (the export panel builds that report).
  if ("LatitudeMeasure" %in% names(clean)) {
    clean <- clean %>%
      # Coordinates were read as text (see [3a]) — convert to numeric here,
      # the only place numeric math is needed
      mutate(
        LatitudeMeasure  = as.numeric(LatitudeMeasure),
        LongitudeMeasure = as.numeric(LongitudeMeasure)
      ) %>%
      # ORDER MATTERS inside this mutate(): each step below relies on the
      # values set by the previous ones. FalseLatitude must capture the bad
      # latitude BEFORE LatitudeMeasure is overwritten, and the final
      # LongitudeMeasure line reads FalseLatitude (not LatitudeMeasure,
      # which by then already holds the corrected value).
      mutate(
        # Flag flipped records (blank string = record is fine)
        flag_negLat = ifelse(!is.na(LatitudeMeasure) & LatitudeMeasure < 0,
                             "⚠ Coordinates flipped – corrected automatically", ""),
        # Preserve original wrong values in FalseLatitude / FalseLongitude
        FalseLatitude  = ifelse(!is.na(LatitudeMeasure) & LatitudeMeasure < 0,
                                LatitudeMeasure, NA_real_),
        FalseLongitude = ifelse(!is.na(LatitudeMeasure) & LatitudeMeasure < 0,
                                LongitudeMeasure, NA_real_),
        # Swap and correct: true lat = old lon (positive), true lon = old lat (already negative)
        LatitudeMeasure  = ifelse(!is.na(LatitudeMeasure) & LatitudeMeasure < 0,
                                  LongitudeMeasure, LatitudeMeasure),
        LongitudeMeasure = ifelse(!is.na(FalseLatitude),
                                  FalseLatitude, LongitudeMeasure)
      )
  }

  # --- (f) Validate corrected coordinates against SC's real geographic bounds ---
  # WHY THIS EXISTS: a 2024 test run found 4 sites where the record had a
  # negative "latitude" (triggering the swap above), but the swapped result
  # STILL wasn't a valid South Carolina coordinate — the underlying error was
  # something other than a simple transposition (e.g., a corrupted digit).
  # The swap logic can't know this on its own; it just swaps whenever it sees
  # a negative latitude. This step is a second, independent check: after all
  # correction is done, does the FINAL coordinate actually fall inside South
  # Carolina? If not, flag it for manual review rather than silently shipping
  # a wrong "corrected" point. This check runs on every record — not just
  # ones flagged by the swap step — so it would also catch a bad coordinate
  # that never triggered the negative-latitude rule in the first place.
  if (all(c("LatitudeMeasure", "LongitudeMeasure") %in% names(clean))) {
    clean <- clean %>%
      mutate(
        flag_coordOutOfRange = ifelse(
          !is.na(LatitudeMeasure) & !is.na(LongitudeMeasure) &
            (LatitudeMeasure  < SC_LAT_MIN | LatitudeMeasure  > SC_LAT_MAX |
             LongitudeMeasure < SC_LON_MIN | LongitudeMeasure > SC_LON_MAX),
          "⚠ Coordinates outside expected SC range – needs manual review", ""
        )
      )
  }

  # --- Final column order ---
  # The 17 WMP data columns in display order, with the two preserved-value
  # columns placed next to the coordinates they relate to, and the QC flag
  # last. intersect() below means any column that couldn't be built this run
  # is omitted rather than causing an error.
  final_cols <- c(
    "OrganizationFormalName", "ActivityStartDate", "ActivityStartTime",
    "ActivityDepthHeightMeasure_Value", "ActivityDepthHeightMeasure_Unit",
    "ProjectName", "MonitoringLocationIdentifier",
    "LatitudeMeasure", "LongitudeMeasure",
    "FalseLatitude", "FalseLongitude",
    "SampleCollectionMethod", "Category", "CharacteristicName",
    "ResultSampleFractionText", "ResultMeasureValue", "ResultMeasureUnit",
    "ResultCommentText", "ResultAnalyticalMethod", "flag_negLat",
    "flag_coordOutOfRange"
  )
  clean[, intersect(final_cols, names(clean)), drop = FALSE]
}

# ─────────────────────────────────────────────
# [6] UI — page layout, styling, and client-side helpers
# ─────────────────────────────────────────────
# Layout: a branded header bar, then a two-column body —
#   LEFT (width 4/12): date range picker → download/clean button with live
#     status + progress bar → export buttons → EPA source link (new, Jul 2026)
#   RIGHT (width 8/12): summary stat boxes + QC warning → 200-row preview table
ui <- fluidPage(

  # Bootstrap "flatly" theme; primary color matches SCSGC-style teal/blue
  theme = bs_theme(bootswatch = "flatly", primary = "#1a6b8a", font_scale = 0.95),

  tags$head(
    # Two small JavaScript handlers let the R server push live updates into
    # the page mid-computation (Shiny's default outputs only refresh between
    # reactive flushes, which would leave the UI frozen during a long
    # multi-chunk download). The server sends these via sendCustomMessage():
    #   'update_progress' — sets the width/label of the progress bar
    #   'update_status'   — replaces the text in the status box
    tags$script(HTML("
      Shiny.addCustomMessageHandler('update_progress', function(msg) {
        var bar = document.getElementById('progress_bar');
        if (bar) {
          bar.style.width = msg.value + '%';
          bar.textContent = msg.value + '%';
          bar.setAttribute('aria-valuenow', msg.value);
        }
      });
      Shiny.addCustomMessageHandler('update_status', function(msg) {
        var box = document.getElementById('status_box');
        if (box) box.innerHTML = msg.msg;
      });
    ")),
    # All page styling in one embedded stylesheet: header gradient, white
    # card panels, section labels, button colors, summary stat boxes, the
    # amber QC warning box, status box, and preview-table sizing
    tags$style(HTML("
      body { background-color: #f4f8fb; }
      .app-header {
        background: linear-gradient(135deg, #1a6b8a 0%, #0d4f69 100%);
        color: white; padding: 28px 36px 22px 36px;
        margin-bottom: 28px; border-radius: 0 0 8px 8px;
      }
      .app-header h2 { margin: 0 0 4px 0; font-weight: 700; font-size: 1.6em; }
      .app-header p  { margin: 0; opacity: 0.85; font-size: 0.95em; }
      .card-panel {
        background: white; border-radius: 8px; padding: 24px 28px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.07); margin-bottom: 20px;
      }
      .section-label {
        font-size: 0.78em; font-weight: 700; letter-spacing: 0.08em;
        color: #1a6b8a; text-transform: uppercase; margin-bottom: 12px;
        border-bottom: 2px solid #e8f2f7; padding-bottom: 6px;
      }
      .btn-download-main {
        background: #1a6b8a !important; border-color: #1a6b8a !important;
        color: white !important; font-weight: 600; padding: 10px 24px;
        font-size: 1em; width: 100%; border-radius: 6px;
      }
      .btn-download-main:hover { background: #14546e !important; }
      .btn-export {
        background: #2ecc71 !important; border-color: #27ae60 !important;
        color: white !important; font-weight: 600;
      }
      .stat-box {
        background: #f0f7fb; border-left: 4px solid #1a6b8a;
        border-radius: 4px; padding: 10px 16px; margin-bottom: 10px;
      }
      .stat-box .stat-num { font-size: 1.6em; font-weight: 700; color: #1a6b8a; }
      .stat-box .stat-lbl { font-size: 0.82em; color: #555; }
      .warn-box {
        background: #fff8e1; border-left: 4px solid #f39c12;
        border-radius: 4px; padding: 10px 16px; font-size: 0.88em; margin-top: 10px;
      }
      .info-note { font-size: 0.82em; color: #666; margin-top: 8px; line-height: 1.5; }
      #status_box {
        background: #eaf4fb; border-radius: 6px; padding: 12px 16px;
        font-size: 0.9em; color: #1a6b8a; font-weight: 500;
        margin-top: 12px; min-height: 42px;
      }
      .table-container { overflow-x: auto; font-size: 0.82em; }
    "))
  ),

  # ── Branded header bar ──
  div(class = "app-header",
    h2("🌊 SC Water Quality Data Tool"),
    p("South Carolina Sea Grant Consortium  ·  EPA Water Quality Portal  ·  Statewide SC Data")
  ),

  fluidRow(

    # ── Left panel: controls ────────────────────
    column(4,
      # Card 1: date range selection. Defaults to a single recent year
      # (2023) so a first-time user's test pull is fast (~1 minute) rather
      # than accidentally launching a 26-chunk, 40-minute full pull.
      div(class = "card-panel",
        div(class = "section-label", "📅 Date Range"),
        dateInput("start_date", "Start date:",
                  value = as.Date("2023-01-01"),
                  min = as.Date("1900-01-01"), max = Sys.Date(),   # no future dates
                  format = "mm/dd/yyyy"),
        dateInput("end_date", "End date:",
                  value = as.Date("2023-12-31"),
                  min = as.Date("1900-01-01"), max = Sys.Date(),
                  format = "mm/dd/yyyy"),
        p(class = "info-note",
          "💡 Data downloads one year at a time to avoid timeouts. ",
          "A 25-year pull (2000–present) runs 26 chunks and may take 20–40 minutes. ",
          "Each year completes independently.")
      ),
      # Card 2: the one-button pipeline trigger, plus the live status box
      # and progress bar that the JS handlers above keep updated during runs
      div(class = "card-panel",
        div(class = "section-label", "🔽 Download & Clean"),
        actionButton("go_btn", "Download & Clean Data", class = "btn-download-main"),
        div(id = "status_box", "Ready — select a date range and click Download."),
        tags$div(class = "progress mt-2",
          tags$div(id = "progress_bar",
                   class = "progress-bar progress-bar-striped progress-bar-animated",
                   role = "progressbar", style = "width: 0%;",
                   `aria-valuenow` = "0", `aria-valuemin` = "0", `aria-valuemax` = "100",
                   "0%")
        )
      ),
      # Card 3: export buttons are rendered server-side (export_ui) so they
      # only appear once there is data, and the flag-report button only
      # appears when flagged records exist
      div(class = "card-panel",
        div(class = "section-label", "📤 Export"),
        uiOutput("export_ui"),
        p(class = "info-note",
          "Output includes 17 columns: org name, date/time, depth, site ID, ",
          "lat/lon, collection method, category, characteristic, result value/unit, ",
          "comments, and analytical method.")
      ),

      # Card 4: link back to the original EPA Water Quality Portal.
      # ADDED Jul 2026 (feature request, S. Huang): gives WMP users a direct
      # path to the source site for other states, custom queries, or raw
      # downloads. tags$a styled as a button; target="_blank" opens a new
      # tab so users don't lose an in-progress download in this app.
      div(class = "card-panel",
        div(class = "section-label", "🔗 Data Source"),
        tags$a(
          href   = "https://www.waterqualitydata.us/",
          target = "_blank",
          class  = "btn btn-download-main",
          style  = "text-decoration:none; text-align:center; display:block;",
          "Visit the EPA Water Quality Portal ↗"
        ),
        p(class = "info-note",
          "This tool retrieves South Carolina data from the EPA Water Quality ",
          "Portal (WQP). Visit the original portal for other states, additional ",
          "data profiles, or advanced query options.")
      )
    ),

    # ── Right panel: results ────────────────────
    column(8,
      # Summary stat boxes + coordinate QC warning (both server-rendered),
      # then an interactive preview of the first 200 cleaned rows
      div(class = "card-panel",
        div(class = "section-label", "📊 Summary"),
        uiOutput("summary_ui"),
        uiOutput("warn_ui"),
        uiOutput("range_warn_ui")
      ),
      div(class = "card-panel",
        div(class = "section-label", "🔍 Data Preview (first 200 rows)"),
        div(class = "table-container", DT::dataTableOutput("preview_table"))
      )
    )
  )
)

# ─────────────────────────────────────────────
# [7] SERVER — reactive wiring
# ─────────────────────────────────────────────
server <- function(input, output, session) {

  # App state, held in reactive values so every output below updates
  # automatically whenever they change:
  cleaned_data <- reactiveVal(NULL)   # the cleaned data frame (NULL until a run completes)
  status_msg   <- reactiveVal("Ready — select a date range and click Download.")  # status box text
  prog_val     <- reactiveVal(0)      # progress bar percentage (0–100)

  # Whenever prog_val changes, push the new percentage to the JS progress
  # bar handler defined in the UI head
  observe({
    session$sendCustomMessage("update_progress", list(value = prog_val()))
  })

  # ── Summary stat boxes (right panel, top) ──
  # Recomputes whenever cleaned_data changes: total observations, unique
  # monitoring sites, categories present, and actual date range in the data
  output$summary_ui <- renderUI({
    df <- cleaned_data()
    if (is.null(df)) return(p(style = "color:#aaa;font-style:italic;",
                               "No data yet. Run a download to see results here."))
    n_rows  <- nrow(df)
    n_sites <- length(unique(df$MonitoringLocationIdentifier))
    n_cats  <- length(unique(df$Category[!is.na(df$Category)]))
    # suppressWarnings: any unparseable date strings become NA quietly
    d <- suppressWarnings(as.Date(df$ActivityStartDate[!is.na(df$ActivityStartDate)]))
    date_range <- if (length(d) > 0) paste(min(d), "to", max(d)) else "—"

    fluidRow(
      column(3, div(class="stat-box", div(class="stat-num", format(n_rows, big.mark=",")),
                    div(class="stat-lbl","Observations"))),
      column(3, div(class="stat-box", div(class="stat-num", format(n_sites, big.mark=",")),
                    div(class="stat-lbl","Monitoring Sites"))),
      column(3, div(class="stat-box", div(class="stat-num", n_cats),
                    div(class="stat-lbl","Categories"))),
      column(3, div(class="stat-box", div(class="stat-num", style="font-size:1em;", date_range),
                    div(class="stat-lbl","Date Range")))
    )
  })

  # ── Coordinate QC warning (right panel, under the stats) ──
  # Appears only when the run found flipped-coordinate records; explains
  # what was corrected, where originals are preserved, and the EPA
  # reporting step
  output$warn_ui <- renderUI({
    df <- cleaned_data()
    if (is.null(df) || !"flag_negLat" %in% names(df)) return(NULL)
    n <- sum(df$flag_negLat != "", na.rm = TRUE)   # count of flagged records
    if (n > 0) div(class = "warn-box",
      paste0("⚠ ", n, " record(s) had flipped coordinates (lat/lon swapped). ",
             "These have been automatically corrected in LatitudeMeasure and LongitudeMeasure. ",
             "Original wrong values are preserved in FalseLatitude and FalseLongitude columns. ",
             "Download the flag report and email to WQX@epa.gov to report the error. ",
             "(A small number of these may still be flagged below as unresolved — ",
             "exclude those from the EPA report until reviewed.)"))
  })

  # ── Out-of-range coordinate warning (right panel, under the swap warning) ──
  # ADDED Jul 2026, after a 2024 test run found 4 sites where swapping
  # lat/lon still didn't produce a valid SC location. Appears only when the
  # post-correction bounds check (section [5f] of clean_wqp_data) finds
  # records that need a human to look at them, rather than being silently
  # included in the "corrected" flag report.
  output$range_warn_ui <- renderUI({
    df <- cleaned_data()
    if (is.null(df) || !"flag_coordOutOfRange" %in% names(df)) return(NULL)
    n <- sum(df$flag_coordOutOfRange != "", na.rm = TRUE)
    if (n > 0) div(class = "warn-box", style = "border-left-color:#c0392b; background:#fdecea;",
      paste0("🛑 ", n, " record(s) still have implausible coordinates even after the ",
             "lat/lon correction above — these do NOT appear to be a simple swap and need ",
             "manual review before reporting to EPA. Original submitted values are preserved ",
             "in FalseLatitude/FalseLongitude where a swap was attempted. Download the ",
             "unresolved coordinate report below to investigate."))
  })

  # ── Export panel (left column, card 3) ──
  # Rendered server-side so buttons appear only when there is data to
  # export. The orange flag-report button renders only if flagged
  # coordinate records exist in this run.
  output$export_ui <- renderUI({
    df <- cleaned_data()
    if (is.null(df)) return(
      p(style="color:#aaa;font-style:italic;font-size:0.88em;",
        "Download data first to enable export."))
    
    n_flags <- if ("flag_negLat" %in% names(df)) 
      sum(df$flag_negLat != "", na.rm = TRUE) else 0
    n_range_flags <- if ("flag_coordOutOfRange" %in% names(df))
      sum(df$flag_coordOutOfRange != "", na.rm = TRUE) else 0
    
    tagList(
      downloadButton("download_csv", "⬇ Download Cleaned CSV",
                     class = "btn btn-success btn-export", style = "width:100%; margin-bottom:10px;"),
      if (n_flags > 0) tagList(
        downloadButton("download_flags", 
                       paste0("⚠ Download Coordinate Flag Report (", format(n_flags, big.mark=","), " records)"),
                       class = "btn btn-warning btn-export", 
                       style = "width:100%; background:#e67e22 !important; border-color:#d35400 !important; margin-bottom:10px;"),
        p(class = "info-note", style = "margin-top:6px;",
          "Send this file to ", tags$a("WQX@epa.gov", href="mailto:WQX@epa.gov"),
          " to report suspected coordinate errors in the EPA database. ",
          "Exclude any records that also appear in the unresolved report below.")
      ),
      # NEW (Jul 2026): separate button for records that failed the
      # post-correction SC bounds check — kept visually distinct (red) from
      # the orange "ready to report" flag button so the two are never
      # confused with each other
      if (n_range_flags > 0) tagList(
        downloadButton("download_range_flags",
                       paste0("🛑 Download Unresolved Coordinate Report (", format(n_range_flags, big.mark=","), " records)"),
                       class = "btn btn-danger btn-export",
                       style = "width:100%; background:#c0392b !important; border-color:#962d22 !important;"),
        p(class = "info-note", style = "margin-top:6px;",
          "These records could not be resolved by a simple lat/lon swap. ",
          "Review the source data before including them in any EPA report.")
      )
    )
  })

  # ── Download handler: the full cleaned CSV ──
  # Filename encodes the selected years, e.g.
  # SC_WaterQuality_2000_to_2026_cleaned.csv; na = "" writes blank cells
  # instead of the literal text "NA" (friendlier for Excel and ArcGIS)
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("SC_WaterQuality_",
             format(input$start_date, "%Y"), "_to_",
             format(input$end_date,   "%Y"), "_cleaned.csv")
    },
    content = function(file) write_csv(cleaned_data(), file, na = "")
  )

  # ── Download handler: coordinate flag report ──
  # Subsets to only the flagged records (with FalseLatitude/FalseLongitude
  # preserved), producing the file to email to WQX@epa.gov
  output$download_flags <- downloadHandler(
    filename = function() {
      paste0("SC_WaterQuality_negLat_flags_",
             format(input$start_date, "%Y"), "_to_",
             format(input$end_date,   "%Y"), ".csv")
    },
    content = function(file) {
      df <- cleaned_data()
      flags <- df[!is.na(df$flag_negLat) & df$flag_negLat != "", , drop = FALSE]
      write_csv(flags, file, na = "")
    }
  )

  # ── Download handler: unresolved coordinate report ──
  # ADDED Jul 2026. Subsets to records that failed the post-correction SC
  # bounds check (section [5f]) — these are NOT safe to include in the
  # WQX@epa.gov report as "corrected," since the swap did not produce a
  # plausible location. Kept as a fully separate file/button from
  # download_flags so the two are never accidentally combined.
  output$download_range_flags <- downloadHandler(
    filename = function() {
      paste0("SC_WaterQuality_coordOutOfRange_",
             format(input$start_date, "%Y"), "_to_",
             format(input$end_date,   "%Y"), ".csv")
    },
    content = function(file) {
      df <- cleaned_data()
      range_flags <- df[!is.na(df$flag_coordOutOfRange) & df$flag_coordOutOfRange != "", , drop = FALSE]
      write_csv(range_flags, file, na = "")
    }
  )

  # ── Preview table (right panel, bottom) ──
  # Shows only the first 200 rows: rendering a full multi-hundred-thousand
  # row pull in the browser would freeze the page, and 200 rows is plenty to
  # verify the cleaning worked before exporting the real file
  output$preview_table <- DT::renderDataTable({
    df <- cleaned_data()
    if (is.null(df)) return(NULL)
    preview <- head(df, 200)
    # Hide the flag columns from the preview when no previewed rows are
    # flagged — reduces visual noise (the columns are always in the CSV)
    if ("flag_negLat" %in% names(preview) && all(preview$flag_negLat == ""))
      preview <- preview[, names(preview) != "flag_negLat", drop = FALSE]
    if ("flag_coordOutOfRange" %in% names(preview) && all(preview$flag_coordOutOfRange == ""))
      preview <- preview[, names(preview) != "flag_coordOutOfRange", drop = FALSE]
    DT::datatable(preview,
      options = list(scrollX=TRUE, pageLength=10, dom="tp",
                     columnDefs=list(list(targets="_all", className="dt-left"))),
      rownames = FALSE, class = "compact stripe hover")
  })

  # ── Main pipeline (runs when "Download & Clean Data" is clicked) ────
  # Sequence: validate dates → download stations once → download each
  # annual chunk → combine → clean → publish to cleaned_data(), which
  # cascades to every output above. Wrapped in tryCatch so any failure
  # surfaces in the status box instead of crashing the session.
  observeEvent(input$go_btn, {

    # Input validation: both dates present and in the right order
    req(input$start_date, input$end_date)
    if (input$end_date < input$start_date) {
      showNotification("End date must be after start date.", type = "error")
      return()
    }

    cleaned_data(NULL)   # clear any previous run's results from the UI
    prog_val(2)          # nudge the progress bar so the user sees it start

    # Helper that BOTH stores the status message and pushes it straight to
    # the browser — sendCustomMessage is what lets the text update live
    # in the middle of the long-running loop below
    update_status <- function(msg) {
      status_msg(msg)
      session$sendCustomMessage("update_status", list(msg = msg))
    }

    chunks      <- build_chunks(input$start_date, input$end_date)  # annual pieces (see [4])
    n_chunks    <- length(chunks)   # for "chunk i of n" progress messages
    all_raw     <- list()           # accumulates each year's raw data frame
    n_raw_total <- 0                # running raw row count for status updates

    withProgress(message = "Downloading...", value = 0, {
      tryCatch({

        # Step 1: Stations (downloaded once per run — see [3b])
        update_status("📡 Downloading station coordinates...")
        incProgress(0.02, detail = "Fetching stations...")
        stations_raw <- download_stations()
        prog_val(8)
        incProgress(0.06, detail = "Stations ready.")

        # Step 2: Annual chunks — the loop that makes big pulls possible.
        # Each year downloads independently; one failed year is skipped
        # with a warning rather than aborting the whole run.
        for (i in seq_along(chunks)) {
          chunk     <- chunks[[i]]
          yr_label  <- format(chunk$start, "%Y")   # e.g. "2023", for messages
          # Progress math: stations took us to 8%; the chunk loop spans
          # 8%→90%, split evenly across the years
          chunk_pct <- round(8 + (i - 1) / n_chunks * 82)

          update_status(paste0("📥 Downloading ", yr_label,
                               " data... (chunk ", i, " of ", n_chunks, ")"))
          incProgress(1 / n_chunks * 0.75, detail = paste("Year:", yr_label))
          prog_val(chunk_pct)

          chunk_df <- tryCatch(
            download_wqp_chunk(chunk$start, chunk$end),
            error = function(e) { warning(paste(yr_label, "failed:", conditionMessage(e))); NULL }
          )

          if (!is.null(chunk_df) && nrow(chunk_df) > 0) {
            all_raw[[i]]  <- chunk_df
            n_raw_total   <- n_raw_total + nrow(chunk_df)
            update_status(paste0("✔ ", yr_label, " — ",
                                 format(nrow(chunk_df), big.mark=","), " rows | Total: ",
                                 format(n_raw_total, big.mark=","),
                                 " | Chunk ", i, " of ", n_chunks))
          } else {
            update_status(paste0("⚠ ", yr_label, ": no data returned, skipping."))
          }
        }

        # Step 3: Combine — stack all annual data frames into one.
        # This is why every chunk was read all-character in [3a]:
        # bind_rows would fail if the same column had different types
        # in different years.
        update_status("🔗 Combining all chunks...")
        prog_val(90)
        incProgress(0.05, detail = "Combining...")
        non_null <- Filter(Negate(is.null), all_raw)
        if (length(non_null) == 0) stop("No data returned for any year in the selected range.")
        results_combined <- bind_rows(non_null)

        # Step 4: Clean — the full trim/join/categorize/QC pipeline (see [5])
        update_status(paste0("🧹 Cleaning ", format(nrow(results_combined), big.mark=","), " rows..."))
        prog_val(93)
        incProgress(0.04, detail = "Cleaning...")
        clean <- clean_wqp_data(results_combined, stations_raw)

        # Done — publishing to cleaned_data() triggers the summary boxes,
        # QC warning, export buttons, and preview table all at once
        cleaned_data(clean)
        prog_val(100)
        incProgress(0.01, detail = "Done!")
        n_removed <- nrow(results_combined) - nrow(clean)
        update_status(paste0(
          "✅ Done! ", format(nrow(clean), big.mark=","),
          " cleaned records from ", n_chunks, " chunk(s). (",
          format(n_removed, big.mark=","), " non-parameter rows removed)"))

      }, error = function(e) {
        # Any error anywhere in the pipeline lands here: show it in the
        # status box AND as a popup notification, and reset the progress bar
        msg <- paste("❌ Error:", conditionMessage(e))
        status_msg(msg)
        prog_val(0)
        session$sendCustomMessage("update_status", list(msg = msg))
        showNotification(conditionMessage(e), type = "error", duration = 20)
      })
    })
  })

  # Keep the status box in sync with status_msg on normal reactive
  # flushes too (covers initial page load and post-run states)
  observe({
    session$sendCustomMessage("update_status", list(msg = status_msg()))
  })
}

# Launch the application
shinyApp(ui, server)
