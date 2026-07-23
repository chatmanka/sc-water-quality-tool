# SC Water Quality Data Tool

A browser-based Shiny application that automates retrieval, cleaning, and export of South Carolina water quality data from the EPA Water Quality Portal (WQP). Built for the **SC Water Monitoring Portal (WMP)**, a project of the [S.C. Sea Grant Consortium](https://www.scseagrant.org/).

**Live app:** https://3ufznw-kate-chatman.shinyapps.io/sc-water-quality-tool/

## The problem this solves

Updating the SC Water Monitoring Portal previously required manually downloading very large raw datasets from the [EPA Water Quality Portal](https://www.waterqualitydata.us/), then trimming 181 raw columns down to 17 usable ones, joining station coordinates, assigning parameter categories, and reformatting for ArcGIS — a process that took hours to days per update and produced files too large for Excel.

This tool performs the entire **download → clean → categorize → export** pipeline in the browser. No coding required: select a date range, click one button, export an analysis-ready CSV.

## Features

- **Direct API access** — calls the EPA WQP WQX 3.0 REST API for all of South Carolina (statewide, water media, NWIS + STORET providers)
- **Chunked downloads** — data is retrieved one calendar year at a time to avoid EPA server timeouts, with a live progress bar and per-year status updates
- **Automated cleaning** — trims 181 raw columns to the 17-column WMP format and standardizes column names
- **Coordinate join** — automatically attaches station latitude/longitude from EPA's Station table by monitoring location identifier
- **Parameter categorization** — assigns each record to one of 8 WMP display categories using the project's parameter lookup table (embedded in the app)
- **Coordinate QC** — detects records with flipped latitude/longitude (a known EPA data quality issue), corrects them automatically, preserves the original values, and generates a flag report formatted for submission to WQX@epa.gov
- **Coordinate validation** — after correcting flipped coordinates, independently checks that the *result* actually falls within South Carolina's real geographic bounds. Records that still look implausible even after correction (a different, unresolved data error) are flagged separately and excluded from the EPA-ready report, so they can't be mistaken for a verified fix
- **One-click export** — cleaned CSV named by date range, with blank cells instead of "NA" for Excel/ArcGIS compatibility
- **Link to source** — a button back to the original EPA Water Quality Portal for other states, additional data profiles, or advanced queries

## Using the tool

1. Open the [live app](https://3ufznw-kate-chatman.shinyapps.io/sc-water-quality-tool/)
2. Select a start and end date. **Try one year first** — a single-year pull completes in about a minute. A full 2000–present pull runs ~26 annual chunks and may take 20–40 minutes.
3. Click **Download & Clean Data** and watch the progress bar
4. Review the summary statistics and 200-row preview
5. Click **Download Cleaned CSV** to export
6. If flipped-coordinate records were found, an orange button offers the **Coordinate Flag Report** — email this file to [WQX@epa.gov](mailto:WQX@epa.gov) to report the errors upstream
7. If any records still look geographically implausible *after* correction, a red button offers a separate **Unresolved Coordinate Report** — these need manual review before reporting to EPA, since a simple swap didn't resolve them (see Technical notes below)

For reference, a full January 2000 – May 2026 pull returns ~1.37 million cleaned records (~263 MB CSV). A 2024 test pull (53,694 records) found 1,851 flipped-coordinate records across 118 sites that were successfully corrected, and 41 records across 4 sites that remain unresolved.

## Output columns

The cleaned CSV contains the 17 WMP data columns plus three QC columns:

| Column | Contents |
|---|---|
| OrganizationFormalName | Reporting organization |
| ActivityStartDate | Sample date (ISO format, YYYY-MM-DD) |
| ActivityStartTime | Sample collection time |
| ActivityDepthHeightMeasure_Value | Sampling depth value |
| ActivityDepthHeightMeasure_Unit | Sampling depth unit |
| ProjectName | Monitoring project |
| MonitoringLocationIdentifier | EPA site ID |
| LatitudeMeasure / LongitudeMeasure | Site coordinates (corrected where flagged) |
| FalseLatitude / FalseLongitude | *(QC)* Original values for corrected records |
| SampleCollectionMethod | Field collection method |
| Category | WMP parameter category (8 categories) |
| CharacteristicName | EPA parameter name |
| ResultSampleFractionText | Sample fraction |
| ResultMeasureValue / ResultMeasureUnit | Result value and unit |
| ResultCommentText | Data quality comments |
| ResultAnalyticalMethod | Lab analytical method |
| flag_negLat | *(QC)* Flag for corrected coordinate records |
| flag_coordOutOfRange | *(QC)* Flag for records still implausible after correction — needs manual review |

### Note on column names (WQX 3.0)

Column names follow EPA's **WQX 3.0** conventions rather than the legacy WMP headers, so the tool's output matches what users see when downloading raw data directly from EPA. Six columns were renamed relative to the legacy format (e.g., legacy `ResultMeasure/MeasureUnitCode` → `ResultMeasureUnit`); legacy names containing `/` are also invalid as ArcGIS field names. See the code comments in `app.R` (section [2]) for the full mapping.

## Repository contents

- `app.R` — the complete application (UI + server + cleaning pipeline), fully annotated section by section
- `LICENSE` — MIT license

The app is self-contained: the parameter category lookup table and column mapping are embedded in `app.R`, so no external data files are required to run or deploy it.

## Running locally

Requires R with the following packages: `shiny`, `httr`, `readr`, `dplyr`, `bslib`, `DT`.

```r
install.packages(c("shiny", "httr", "readr", "dplyr", "bslib", "DT"))
shiny::runApp("app.R")
```

## Technical notes

- **Why chunked by year:** single requests covering multiple years routinely time out on EPA's side. Each year downloads independently; a failed year is skipped with a warning rather than aborting the run.
- **Why all columns are read as text:** `read_csv` guesses column types per chunk, and type mismatches between years would break `bind_rows()`. Reading everything as character keeps chunks type-consistent; numeric conversion happens only where needed (coordinates).
- **Coordinate QC logic:** a negative latitude is impossible in South Carolina (valid range ≈ +32° to +35°), so a negative value indicates latitude and longitude were entered in each other's fields. The tool swaps them, preserves the originals, and flags the record.
- **Coordinate validation (added Jul 2026):** the swap logic assumes the only problem is transposition, but a 2024 test run found 4 sites where swapping still didn't produce a valid South Carolina location — a different, unresolved error in the source data. The tool now independently checks every corrected coordinate against SC's real geographic bounds; anything still outside those bounds is flagged (`flag_coordOutOfRange`) and routed to a separate export, rather than being silently included in the "corrected and verified" report sent to EPA.
- **Station endpoint:** site coordinates come from EPA's Station table (legacy `/data/Station/search` endpoint), downloaded once per run and joined by `MonitoringLocationIdentifier`. The join uses flexible column detection to tolerate EPA field-name changes across API versions.

## Roadmap

- Investigate the root cause of the 4 sites flagged by `flag_coordOutOfRange` (likely a data entry error distinct from lat/lon transposition)
- Hosted feature layer testing in ArcGIS Enterprise (post-2000 dataset)
- Integration with the redesigned SC Water Monitoring Portal (ArcGIS Experience Builder)
- "Data volume by county" summary layer
- Long-term hosting evaluation (shinyapps.io tiers / Posit Cloud / institutional account)

## Attribution

Developed by **Kate Chatman**, S.C. Sea Grant Consortium, for the SC Water Monitoring Portal project. Parameter categories and the 17-column data format were established by the WMP project team.

Data source: [EPA Water Quality Portal](https://www.waterqualitydata.us/), a cooperative service of USGS, EPA, and the National Water Quality Monitoring Council. This tool is not affiliated with or endorsed by EPA.

## License

MIT — see [LICENSE](LICENSE).
