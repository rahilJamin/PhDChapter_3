# PhDChapter_3

Spatial analysis of species occurrence data for a PhD project (Chapter 3) by Rahil Amin. The current code focuses on downloading and cleaning Atlas of Living Australia (ALA) records, aligning occurrences to grid systems, and creating hexagonal grids at multiple resolutions to support scale-sensitive biodiversity analyses.

**Status**: Early prototype functions under `R/`. A proper R package + reproducible pipeline (renv + targets) and Quarto docs are planned.

## What’s Here

- **ALA retrieval**: Pulls occurrence records for a list of species and intersects them with a study-area polygon.
- **Grid snapping**: Snaps presence points to nearest grid coordinates for standardized gridded analyses.
- **Multi-scale hex grids**: Builds hexagonal grids at several resolutions for scale exploration and mapping.
- **Docs**: High-level overview in `docs/overview.md`.

## Quick Start

- **Requirements**: R (≥4.2 recommended) with packages `sf`, `terra`, `dplyr`, `purrr`, `galah`, `parallel`.
- **Setup**:
  - Prepare a study-area geometry (e.g., shapefile or GeoPackage).
  - Prepare a species list data frame with a `Species.Name` column.
  - Optional: a grid CSV with `lon`/`lat` columns for snapping.
- **Use**:
  - Source functions: `source("R/beta_analysis_func.R")`.
  - Example sketch:
    ```r
    library(sf)
    # study area
    shp <- sf::read_sf("path/to/study_area.gpkg")

    # species list (data frame with a 'Species.Name' column)
    species_list <- data.frame(Species.Name = c("Isoodon obesulus", "Petaurus breviceps"))

    # ALA occurrences (optionally filter by year)
    occ <- get_ALA_data(first_year = 2000, species_list = species_list, shp = shp, dTol = 10000, output = "occ.csv")

    # Snap to grid (if you have a grid CSV with lon/lat columns)
    snapped <- process_presence_data(grid = "grid.csv", pres = "occ.csv", crs_ref = sf::st_crs(shp))

    # Multi-scale hex grids
    hexes <- create_variable_hex_grids("path/to/study_area.gpkg", max_hex_km = 100, min_hex_km = 10, reduction_factor = 0.9)
    ```

## Notes and Caveats

- Ensure all inputs share a consistent CRS. `get_ALA_data` assumes the ALA points are interpreted in the CRS of the study area.
- The `output` argument in `get_ALA_data` controls optional CSV writing.
- Plotting helpers are basic and intended for quick checks.

## Roadmap

- Convert to a `devtools` package with one-function-per-file organization.
- Add `renv` for reproducible dependencies.
- Build a `targets` pipeline for data acquisition and analysis.
- Publish Quarto docs for methods and results.

Author: Rahil Amin
