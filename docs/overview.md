# Project Overview

This repository contains code developed by Rahil Amin for spatial analysis of species occurrences as part of a PhD project (Chapter 3). The current focus is on acquiring occurrence records, aligning them to spatial grids, and generating hexagonal grids at multiple spatial resolutions to support downstream ecological analyses (e.g., richness, occupancy, beta-diversity) across scales.

## Objectives

- Acquire species occurrence data from the Atlas of Living Australia (ALA) for a user-specified list of taxa.
- Filter and intersect occurrences with a study-area boundary to restrict analyses spatially and temporally.
- Snap occurrences to the nearest grid point to standardize locations for gridded analyses.
- Generate hexagonal grids at multiple resolutions to explore scale effects.
- Provide simple plotting helpers to visualize grid layouts.

## Current Data Flow

1. Input preparation
   - Study area geometry: a shapefile or GeoPackage for the analysis extent.
   - Species list: a data frame with a `Species.Name` column.
   - Optional occurrence CSVs and a grid CSV with `lon`/`lat` columns for snapping.
2. Data acquisition and filtering
   - Retrieve ALA records via the `galah` package (optionally filtering by first year).
   - Reproject to match the study-area CRS and intersect with the boundary.
3. Standardization to grids
   - Snap presence points to nearest grid coordinates for consistent gridded analyses.
4. Multi-scale grids
   - Build hexagonal grids at several cell sizes (km), intersected with the study area, for scale-sensitive analyses and mapping.

## Key Functions (in `R/beta_analysis_func.R`)

- `get_ALA_data(first_year, dTol, species_list, shp, output)`: Download and filter ALA occurrences for a list of species, intersect with a study-area polygon, and optionally write to CSV.
- `process_presence_data(grid, pres, crs_ref)`: Snap presence points to the nearest grid coordinate for standardized gridded analyses.
- `generate_unique_hex_sizes(max_hex_size_km, min_hex_size_km, reduction_factor)`: Create a unique sequence of hex sizes between provided bounds.
- `generate_hex_grid(area, current_cellsize_km)`: Build a hexagonal grid of a given cell size and intersect it with the study area.
- `create_variable_hex_grids(shapefile_path, max_hex_km, min_hex_km, reduction_factor, num_cores)`: Generate multiple hex grids (optionally in parallel) across resolutions.
- `plot_hex_grids(hex_grids_list)`: Quick visual check of generated grids.

## Dependencies

- Core spatial: `sf`, `terra`.
- Data wrangling: `dplyr`, `purrr`.
- Occurrence retrieval: `galah` (ALA API).
- Parallelization: `parallel` (for `mclapply`).

Note: Functions expect consistent coordinate reference systems (CRS) between inputs (study area, grids, and occurrences).

## Next Steps (planned)

- Convert this repo into a proper R package using `devtools`, with one-function-per-file organization under `R/`.
- Manage dependencies reproducibly with `renv`.
- Orchestrate data pipelines using `targets`.
- Add a Quarto site or notebooks for literate analysis and results communication.

Author: Rahil Amin

