# Changelog

All notable changes to this project will be documented here.

## Unreleased
- Refactor CRS handling: pass `crs_ref` explicitly to `process_species_presences()`, `process_hex_grid()`, and `process_all_grids()`; update `_targets.R` accordingly.
- Add toy dataset under `inst/extdata/` (`study_area.geojson`, `species_list.csv`, `grid.csv`, `occ_sample.csv`) and configure `targets` to use it via `use_sample_occ`.
- Add `bootstrap.R` to install `renv` and `import`; prefer `pak` for installs.
- Update `.Rprofile` to enable `pak` as the package manager for `renv`.
- DESCRIPTION: add `Suggests: knitr, rmarkdown` and `VignetteBuilder: knitr`.
- Add vignette `vignettes/quickstart-toy-data.Rmd` demonstrating the toy workflow.
- Add devtools package scaffolding (`DESCRIPTION`, `NAMESPACE`, `.Rbuildignore`, `.Rprofile`).
- Add `targets` pipeline (`_targets.R`) and `data/` folder.
- Add Quarto website scaffold (`_quarto.yml`, `index.qmd`) targeting `docs/`.
- Create this `docs/CHANGELOG.md` to track changes.

## 2025-08-22
- Summarize existing R code and purpose.
- Create `docs/overview.md` with high-level description and objectives.
- Expand `README.md` with overview, quick start, notes, and roadmap.
- Fix `get_ALA_data` signature: `output = NULL` and write only if non-NULL via `utils::write.csv`.
- Clean up `get_ALA_data` docs and inputs; add light validation.
- Fix `plot_hex_grids` to use `sf::st_geometry` for plotting.
- Split functions into one-per-file with roxygen2 headers; add exports.
- Improve `process_presence_data` with column/CRS validation.
- Fix `potential_absences_from_sites` to use `anti_join(pres, ...)`.
