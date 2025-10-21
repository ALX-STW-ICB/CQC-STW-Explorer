# CQC STW Explorer

## Overview
CQC STW Explorer is an interactive analytics app built in R Shiny that visualises Care Quality Commission (CQC) ratings across Shropshire, Telford & Wrekin. It automatically cleans and standardises the national CQC data extract, maps each regulated location to its local authority and Integrated Care Board, and overlays these points on Lower Layer Super Output Areas (LSOAs) coloured by deprivation (IMD decile). The app lets users filter by rating, service type, ownership, or geography, explore trends between IMD and quality, and download filtered datasets for further analysis. Built for transparency and reproducibility, it runs entirely from the repository—no external dependencies or credentials—and demonstrates how open data and lightweight R visualisation can support strategic oversight and quality-improvement planning within the ICS.

## Key capabilities
- **End-to-end data hygiene** – Raw CQC location extracts are auto-detected, canonicalised to a consistent schema, de-duplicated, and enriched with any IMD indicators already present in the file.
- **Geospatial context** – Regulated locations are mapped onto the included `LSOA_2021_EW_BSC_V4.geojson` geometry (transformed to WGS84 and simplified on load) so the deprivation gradient is visible alongside service coverage.
- **Rich filtering** – Search by free text or filter by rating, regulated activity, service type, ownership, organisation type, local authority, ICB/ICS, inspection date, bed capacity, or IMD decile. Toggle to show only locations with coordinates and highlight LSOAs by IMD range.
- **Multiple analytical views** – Switch between high-level KPIs, a faceted leaflet map, an interactive data table, and IMD-vs-rating charts with correlation statistics.
- **Downloadable outputs** – Export the current filter state as CSV or capture the full session metadata (including resolution steps) as JSON for audit and reproducibility.
- **Self-contained execution** – All required data files (`locations_stw_filtered_merged.csv` and `LSOA_2021_EW_BSC_V4.geojson`) live in the repo and the app installs any missing CRAN packages on startup, so no manual dependency management is needed.

## Repository layout
```
.
├── app.R                          # Main Shiny application
├── locations_stw_filtered_merged.csv  # Default CQC locations extract scoped to STW
├── LSOA_2021_EW_BSC_V4.geojson    # Generalised LSOA polygons for England & Wales
├── manifest.json                  # Posit/ShinyApps manifest enumerating package versions
├── LICENSE                        # MIT license for the project
└── README.md                      # This document
```

## Getting started
### Prerequisites
- R 4.2 or newer (tested with R 4.5.1)
- Internet access on first run so the app can install any missing CRAN packages listed in `app.R`

### Launching the app
You can run the explorer directly from the repository root:

```r
# From within an interactive R session
shiny::runApp("app.R")
```

Or from a terminal using `Rscript`:

```bash
Rscript app.R
```

The app opens in your default browser. Because package installation is handled automatically, the first launch may take a minute while dependencies are installed.

### Using custom inputs
The application searches for input data in a "repo-first" order. Override the defaults with either command-line flags or environment variables:

| Purpose | CLI flag | Environment variable | Notes |
|---------|----------|----------------------|-------|
| CQC locations CSV | `--input /path/to/file.csv` | `CQC_CSV=/path/to/file.csv` | Must contain at least location name/postcode or latitude/longitude. Column names are canonicalised automatically. |
| LSOA geometry | `--lsoa_shp /path/to/LSOA.geojson` | `LSOA_SHP=/path/to/LSOA.geojson` | Accepts GeoPackage (`.gpkg`), GeoJSON (`.geojson`/`.json`), or ESRI Shapefile (`.shp`). If absent, the map still renders points but without polygons. |

If no valid dataset is found the app falls back to an in-memory demo dataset and displays a banner explaining the mode.

## Working with the data pipeline
- **Schema inference** – Column names are cleaned with `janitor::clean_names()` and mapped to canonical fields (e.g. `overall_rating`, `local_authority`, `icb_name`). Rating text and numeric scales are normalised to the official five-point CQC ladder.
- **Deduplication** – Records are sorted by most recent inspection date, then de-duplicated on `location_id`, or `location_name` + `postcode` when IDs are missing.
- **IMD handling** – Any IMD decile, rank, or score columns in the CSV are detected and standardised. The map tab also derives an IMD decile for each LSOA by taking the median of matched locations to spotlight areas needing attention.
- **Diagnostics** – A persistent banner summarises data provenance (CLI/env/repo/demo), domain filtering, deduplication counts, rating-source detection, and IMD coverage so analysts can trust what they see.

## Application tour
1. **Overview** – Headline KPIs for total locations, unique providers, rating distribution, plus bar charts for rating mix and top service or activity categories.
2. **Map** – Leaflet map with deprivation shading, popups showing inspection history, provider details, and quick links to CQC rating reports.
3. **Table** – Interactive `DT::datatable` view respecting all filters, with quick download of the current subset.
4. **IMD vs Rating** – Correlation-ready heatmaps, ridges, and scatterplots illustrating the relationship between deprivation and quality. Toggle whether to exclude "Not rated" locations.

## Refreshing the datasets
1. Download the latest CQC location extract (CSV) covering the desired geography.
2. Replace `locations_stw_filtered_merged.csv` with the new file (keeping the filename or updating the CLI/env settings accordingly).
3. If updated LSOA boundaries are available, replace `LSOA_2021_EW_BSC_V4.geojson` with the new geometry (GeoPackage, GeoJSON, or Shapefile are all supported).
4. Restart the app—no code changes required.

## Deployment hints
- **RStudio / Posit Workbench** – Open the project directory and click "Run App". The included `manifest.json` describes all packages for easy deployment to Posit Connect or ShinyApps.io.
- **Docker / headless servers** – Install R plus system dependencies for `sf` (e.g. `libgdal`, `libgeos`, `libproj`), clone this repository, and launch `Rscript app.R`.
- **Bookmarkable links** – Use the built-in "Copy shareable link" button to capture the current filter state and share with colleagues.

## Troubleshooting
- **Missing packages** – Ensure outbound HTTPS is allowed on first launch. Subsequent runs use the locally installed packages.
- **No polygons on the map** – Provide a valid LSOA geometry file via the CLI or environment variable. Without it, the map still plots point locations.
- **Unexpected ratings** – Check the banner to confirm which column supplied the overall rating and that the upstream file contains the expected values.

## License
This project is released under the [MIT License](LICENSE).
