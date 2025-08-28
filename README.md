# PhDChapter_3

Spatial analysis of species occurrence data for a PhD project (Chapter 3) by Rahil Amin. The current code focuses on downloading and cleaning Atlas of Living Australia (ALA) records, aligning occurrences to grid systems, and creating hexagonal grids at multiple resolutions to support scale-sensitive biodiversity analyses.

**Status**: Early prototype functions under `R/`. A proper R package + reproducible pipeline (renv + targets) and Quarto docs are planned.

## What’s Here

- **ALA retrieval**: Pulls occurrence records for a list of species and intersects them with a study-area polygon.
- **Grid snapping**: Snaps presence points to nearest grid coordinates for standardized gridded analyses.
- **Multi-scale hex grids**: Builds hexagonal grids at several resolutions for scale exploration and mapping.
- **Docs**: High-level overview in `docs/overview.md`.

## Package Setup (devtools)

- Package metadata added (`DESCRIPTION`, `NAMESPACE`, `.Rbuildignore`).
- One-function-per-file organization with roxygen2 docs under `R/`.
- Backward-compatible shim remains at `R/beta_analysis_func.R` (sources the new files).

Use in an interactive session:

```r
# Option A: load functions without installing
devtools::load_all(".")

# Option B: install the package locally
devtools::install(dependencies = TRUE)
library(PhDChapter3)
```

## Quick Start

- **Requirements**: R (≥4.2 recommended) with packages `sf`, `terra`, `dplyr`, `purrr`, `galah`, `parallel`.
- **Setup**:
  - Prepare a study-area geometry (e.g., shapefile or GeoPackage).
  - Prepare a species list data frame with a `Species.Name` column.
  - Optional: a grid CSV with `lon`/`lat` columns for snapping.
- **Use**:
  - Load functions (see above) or source the shim: `source("R/beta_analysis_func.R")`.
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

## Reproducibility (renv)

- Initialize a project-local library:

```r
install.packages("renv")
renv::init()
renv::snapshot()
```

- Restore on another machine:

```r
renv::restore()
```

## Pipeline (targets)

- A small toy dataset is bundled under `inst/extdata/` and referenced by default in `_targets.R` via `system.file()`.
- Edit the `config` list in `_targets.R` with your file paths and options. To avoid network calls, keep `use_sample_occ = TRUE` (uses `inst/extdata/occ_sample.csv`).
- Run the pipeline:

```r
library(targets)
tar_make()
tar_visnetwork()  # optional: visualize pipeline
```

Outputs are written to `data/` (e.g., `occurrences.csv`, `snapped.csv`).

## Quarto Site

- A Quarto website scaffold is included. Render locally with:

```bash
quarto render
```

HTML outputs are written to `docs/` for easy publishing (e.g., GitHub Pages).

## Bootstrap

- Use `bootstrap.R` to set up `renv` and the basic helper packages:

```r
source("bootstrap.R")
# Then snapshot once your libs are installed
renv::snapshot()
```

## Changelog

See `docs/CHANGELOG.md` for a record of changes.

## Vignettes

After installing or loading with `devtools::load_all()`:

```r
browseVignettes("PhDChapter3")
# or open the quickstart directly
vignette("quickstart-toy-data", package = "PhDChapter3")
```

Author: Rahil Amin
