# CASSN occurrences

This R package stages the CASSN metadata and Motus receiver data used for an occurrence build, then turns Wildlife Insights, NABat/SonoBat, and Motus inputs into one standardized occurrence CSV. The result is one row per occurrence, with a shared core and useful platform detail kept alongside it.

## Scope

The current package supports Wildlife Insights camera exports, stationary-acoustic NABat exports, local Motus receiver databases, and CASSN metadata snapshots. It can copy selected ingest metadata and canonical reference files into staging and synchronize the configured Motus receivers; WI and NABat exports are still staged manually. It writes CSV only and keeps taxonomically resolved records. Future integration with Wildlife SoundHub is planned once the platform provides an export process.

## Output metadata dictionary

The package includes `CASSN_Metadata_Mapping.xlsx`, which documents the occurrence output schema and current source-field mappings. After installation, locate it with:

``` r
system.file(
  "documentation",
  "CASSN_Metadata_Mapping.xlsx",
  package = "cassnoccurrences"
)
```

## Install

Install the package from GitHub:

``` r
install.packages("remotes")
remotes::install_github("john-imperato/cassn-occurrences")
```

To use `sync_motus_receivers()`, also install the optional Motus package:

``` r
install.packages(
  "motus",
  repos = c(
    birdscanada = "https://birdscanada.r-universe.dev",
    CRAN = "https://cloud.r-project.org"
  )
)
```

## Prepare the staged folder

``` text
staged_dir/
├── wildlife_insights/
│   ├── ORG_SITE_plotN_ML_YYYYMMDD/
│   │   ├── deployments.csv
│   │   └── images.csv
│   └── ORG_SITE_plotN_SA_YYYYMMDD/
│       ├── deployments.csv
│       └── images.csv
├── nabat/
│   └── ORG_SITE_plotN_BT_YYYYMMDD/
│       └── nabat_export.csv
├── motus/
│   ├── CTT-RECEIVER-ONE.motus
│   └── CTT-RECEIVER-TWO.motus
└── metadata_inputs/
    ├── sites.csv
    ├── motus.csv
    └── ORG_SITE_YYYYMMDD/         # one subfolder per deployment event
        ├── audio_file_metadata.csv
        └── image_file_metadata.csv   # optional sensor enrichment
```

Wildlife Insights and NABat exports are placed in the staged folder manually. Each subfolder is named with the exact CASSN deployment ID represented by its files. This makes multiple sites, plots, dates, and device types explicit. The pipeline recursively combines all deployment folders and stops if a Wildlife Insights deployment or NABat metadata match disagrees with its folder name.

`ORG` is the organization code, `SITE` is the CASSN site short name, and `plotN` is the plot identifier, such as `plot1`.

For each new occurrence build, `stage_metadata_inputs()` copies `sites.csv` and `motus.csv` plus each selected deployment event's audio and/or image metadata. These metadata folders do not need to be split by device deployment because each metadata row carries its exact `deployment_id`, and the pipeline reads them recursively. The staged copies remain fixed for a reproducible build.

`metadata_inputs/motus.csv` contains one row per physical receiver and the columns `m_station_id`, `receiver_serial`, and `site_short_name`. `sync_motus_receivers()` creates or updates one receiver database under `motus/` for every configured receiver. It may request Motus authentication on its first network call; later synchronizations are normally much faster than the initial download.

## Run a new occurrence build

After placing the Wildlife Insights and NABat exports in the staged directory, run:

``` r
library(cassnoccurrences)

staged_dir <- "/path/to/staged_dir"
deployment_dirs <- c(
  "/path/to/ORG_SiteOne_YYYYMMDD",
  "/path/to/ORG_SiteTwo_YYYYMMDD"
)
reference_dir <- "/path/to/CASSN/app_config"
output_csv <- "/path/to/cassn_occurrences.csv"

stage_metadata_inputs(staged_dir, deployment_dirs, reference_dir)
sync_motus_receivers(staged_dir)
occurrences <- write_occurrence_csv(staged_dir, output_csv)
```

Use a fresh staged folder for each new publication build. The three calls snapshot the selected CASSN metadata, synchronize the configured Motus receivers, and write the occurrence CSV.

## Rerun an existing staged snapshot

To reproduce or debug the same build without refreshing its metadata or Motus databases:

``` r
library(cassnoccurrences)
write_occurrence_csv(
  "/path/to/staged_dir",
  "/path/to/cassn_occurrences.csv"
)
```

The only data product is the CSV. The function invisibly returns the same occurrence table, with the output path and run counts attached as attributes. `occurrenceID` is a deterministic text identifier beginning with `CASSN-`, so spreadsheet and CSV readers cannot mistake it for a number. `occurrenceKey` retains the readable source-and-species identity from which it was derived.

## How the output is organized

The output has 82 columns. The first 35 form the shared occurrence record: identity, platform, taxon, time, coordinates, elevation, site, sampling method, sensor, and deployment information. The remaining fields preserve broadly useful source detail under unambiguous prefixes: 12 `wi_` fields, 15 `nabat_` fields, and 20 `m_` fields.

The shared core contains information needed to interpret an occurrence across platforms. Platform-specific fields are retained when they help someone assess an identification, trace it to its source, understand the observing hardware, or interpret the sampling context. Fields are omitted when they duplicate the shared core, describe internal platform processing or submission history, are consistently empty, contain low-level telemetry with little publication value, or cannot yet be interpreted consistently enough to publish.

The exact column names, order, types, and required status are defined in `R/schema.R`. Source mappings are implemented in `R/wi.R`, `R/nabat.R`, `R/motus.R`, and `R/enrich.R`. `R/staging.R` prepares the CASSN metadata snapshot, and `R/pipeline.R` runs the transformations, enrichment, schema conformance, identifier checks, and CSV write in that order.

## Occurrence and enrichment rules

- Wildlife Insights: remove blanks, humans, vehicles, and non-taxon labels; keep the most specific legitimate taxonomic rank supplied. ML and SA protocol names come from the CASSN deployment ID.
- NABat: prefer a manual species ID, fall back to the automated ID, and publish only codes that resolve to one species in the pinned NABat reference table.
- Motus: publish a receiver detection run only when `motusFilter == 1`, `speciesSci` is populated, and `ambigID` is empty. Motus is site-level, so shared `deploymentID` and `plot` stay blank.
- NABat rejoins ingest metadata by normalized `filename`, then `original_filename`. Motus joins `m_station_id` through `metadata_inputs/motus.csv` to `sites.csv`. WI and the other CASSN deployments use the canonical deployment ID and `sites.csv`.

The installed NABat reference was built from NABat's official Partner Portal workbook, `NABat_Species_Codes (updated 10-8-24).xlsx`. It includes the published single-species, grouping, frequency-class, and non-identification codes; only codes representing one species resolve to a publishable taxon.

## 
