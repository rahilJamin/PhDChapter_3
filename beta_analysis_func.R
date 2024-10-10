
# Download ALA data -------------------------------------------------------

####
# Retrieve, filter and spatially intersect occurrence data for a class of species from the Atlas of Living Australia db
get_ALA_data <- function(first_year=NULL, dTol=10000, species_list, shp, output=output_file) {
  # This function retrieves occurrence data for a given class of species from the Atlas of Living Australia (ALA) db
  # and returns a filtered and intersected sf object containing the occurrence data.
  # The function also writes the occurrence data to a CSV file if write_csv is set to TRUE.
  # 
  # Arguments:
  #   class: the class of species to retrieve occurrence data for (default is "mammal")
  #   first_date: the earliest date to retrieve occurrence data for (default is 1970)
  #   dTol: the distance tolerance for simplifying the shapefile (default is 10000)
  #   species_list: a list of species to retrieve occurrence data for (default is NULL)
  #   shp: the shapefile to intersect the occurrence data with
  #   write_csv: whether to write the occurrence data to a CSV file (default is TRUE)
  # 
  # Returns:
  #   sp_df: a filtered and intersected sf object containing the occurrence data
  
  # Validate inputs
  
  stopifnot(is.list(species_list)) # species_list must be a list
  
  # Simplify shapefile and create convex hull
  shp <- st_simplify(st_union(shp), dTolerance = dTol) # Simplify shapefile to reduce processing time
  hull <- st_convex_hull(st_union(shp)) # Create convex hull of shapefile to reduce processing time
  
  sp_select <- species_list$Species.Name # Select species from species list
  
  # Define a function to handle the conditional logic of whether to filter by date (and ignore any date=NA entries)
  check_date_filter <- function(data, year_filter) {
    if (!is.null(year_filter)) {
      return(data |> galah_filter(year >= year_filter))
    }
    return(data) # If year_filter is NULL, return the data unchanged
  }
  sp_occ <- map(sp_select, function(sp) { # Loop through each species in the species list
    cat("Downloading records for", sp, ":\n")
    galah_call() |> # Initialize galah
      galah_identify(sp) |> # Identify the species
      check_date_filter(first_year) |> # Filter by date
      galah_polygon(hull) |> # Filter by convex hull
      galah_select(decimalLongitude, decimalLatitude, class, family, genus, species, vernacularName, eventDate,
                   dataResourceName) |>
      atlas_occurrences() # Retrieve occurrence data from ALA db
  })
  sp_df <- sp_occ %>% bind_rows() # Combine the occurrence data into a single data frame
  
  # Rename relevant columns and filter out rows with missing or imprecise longitude or latitude values
  sp_df <- sp_df %>% 
    rename(longitude = decimalLongitude,
           latitude = decimalLatitude,
           common_name = vernacularName,
           date = eventDate) %>%  # Rename relevant columns
    filter(!is.na(longitude) & !is.na(latitude))%>% # Filter out rows with missing longitude or latitude values
    filter(!(longitude %% 1 == 0 | latitude %% 1 == 0)) # Filter out rows with no lon-lat precision
  
  # Convert the data frame to an sf object and intersect it with the shapefile, then convert to a df
  sp_df <- st_as_sf(sp_df, coords = c("longitude", "latitude"), crs = st_crs(shp)) %>%
    st_intersection(shp) %>% # Intersect with shapefile
    {
      coords <- st_coordinates(st_geometry(.))
      .$longitude <- coords[, 'X']
      .$latitude <- coords[, 'Y']
      st_drop_geometry(.) %>%
        as.data.frame()
    } %>%
    {sp_df[rownames(sp_df) %in% rownames(.),]} # Reorder rows to match original data frame
  
  # Write the occurrence data to a CSV file if write_csv is set to TRUE
  if(exists(output)) write.csv(sp_df, output, row.names=F)
  
  # Return the filtered and intersected sf object
  return(sp_df)
}



# Snap to grid ------------------------------------------------------------


####
# Loads presence data, snaps presence points to the nearest grid point
process_presence_data <- function(grid, pres, crs_ref) {
  # Loads presence data, snaps presence points to the nearest grid point
  # 
  # Arguments:
  #   grid: A file path to a CSV file containing grid point data.
  #   pres: A file path to a CSV file containing presence data.
  #   crs_ref: The coordinate reference system (CRS) of the input data.
  # 
  # Returns:
  #   A data frame containing the processed presence data, with each point
  #   snapped to the nearest grid point.
  
  # Load grid points and convert to spatial points
  grid_coords <- read.csv(grid)
  grid_sf <- st_as_sf(grid_coords, coords=c("lon","lat"), crs=crs_ref)
  pres <- read.csv(pres)
  # Load and process presence data
  presences_all <- pres %>% # Load presence data from CSV file
    select(longitude, latitude, family, species, date, dataResourceName) %>% # Select relevant columns
    rename(lon = longitude, lat = latitude, source = dataResourceName) %>% # Rename columns
    st_as_sf(coords = c("lon","lat"), crs=crs_ref) %>% # Convert to spatial points
    {
      nearest_indices <- st_nearest_feature(., grid_sf) # Find nearest grid point for each presence point
      mutate(., lon = grid_coords[nearest_indices, "lon"], # Snap presence point to nearest grid point
             lat = grid_coords[nearest_indices, "lat"])
    } %>% # Add snapped coordinates to data frame
    st_drop_geometry() %>% # Remove spatial information
    select(lon, lat, family, species, date, source)# Select relevant columns
  
  return(presences_all) # Return processed presence data
}
####

# Create hex --------------------------------------------------------------

# Function to generate unique hexagon sizes
generate_unique_hex_sizes <- function(max_hex_size_km, min_hex_size_km, reduction_factor = 0.90) {
  max_cellsize <- max_hex_size_km  # Use kilometers directly
  min_cellsize <- min_hex_size_km  # Use kilometers directly
  
  hex_sizes <- numeric(0)
  
  i <- 0
  while (TRUE) {
    new_size <- max_cellsize * round((reduction_factor ^ i), 2)
    if (new_size %in% hex_sizes) {
      i <- i + 1
      next
    }
    if (new_size < min_cellsize) {
      if (min_cellsize %in% hex_sizes) {
        break
      } else {
        hex_sizes <- c(hex_sizes, min_cellsize)
        break
      }
    }
    hex_sizes <- c(hex_sizes, new_size)
    i <- i + 1
  }
  
  return(hex_sizes)
}

# Function to generate a hexagonal grid and intersect with the study area
generate_hex_grid <- function(area, current_cellsize_km) {
  cellsize_dd <- current_cellsize_km / 111  # Convert to decimal degrees once
  hex_grid_sf <- sf::st_make_grid(area, cellsize = cellsize_dd, square = FALSE)
  hex_grid_sf <- sf::st_as_sf(hex_grid_sf)
  hex_grid_sf <- sf::st_intersection(hex_grid_sf, area)
  hex_grid_sf$hex_id <- 1:nrow(hex_grid_sf)
  return(hex_grid_sf)
}


# Function to create variable hexagonal grids and intersect with the study area
create_variable_hex_grids <- function(shapefile_path, max_hex_km, min_hex_km, reduction_factor = 0.90, num_cores = 5) {
  area <- sf::read_sf(shapefile_path)
  
  hex_sizes_km <- generate_unique_hex_sizes(max_hex_km, min_hex_km, reduction_factor)
  
  if (is.null(num_cores)) {
    num_cores <- detectCores() - 1
  }
  
  hex_grids_list <- mclapply(hex_sizes_km, function(size) generate_hex_grid(area, size), mc.cores = num_cores)
  
  names(hex_grids_list) <- paste0(round(hex_sizes_km, 2), " km")
  
  return(hex_grids_list)
}

# Function to plot hexagonal grids
plot_hex_grids <- function(hex_grids_list) {
  
  # Determine the number of grids
  n_grids <- length(hex_grids_list)
  
  # Determine the layout for the plots
  n_cols <- ceiling(sqrt(n_grids))
  n_rows <- ceiling(n_grids / n_cols)
  
  # Set up the plotting layout
  par(mfrow = c(n_rows, n_cols), mar = c(2, 2, 2, 2))
  
  # Loop through each hex grid and plot it
  for (i in seq_along(hex_grids_list)) {
    hex_grid_sf <- hex_grids_list[[i]]
    terra::plot(hex_grid_sf$x, main = names(hex_grids_list)[i])
  }
  
  # Reset plotting layout
  par(mfrow = c(1, 1))
}


# Join species to hex  ----------------------------------------------------
# Function to select presence points for a species
species_presences <- function(pres_all, sp) {
  presences <- pres_all %>%
    filter(species == sp) %>%
    distinct(lon, lat, .keep_all = TRUE) %>%
    mutate(pa = 1) %>%
    as_tibble()
  return(presences)
}

potential_absences_from_sites <- function(pres_all, pres, min_sp) {
  # Generate potential absence data from presence data
  # 
  # Arguments:
  #   pres_all: A data frame containing presence data for multiple species.
  #   pres: A data frame containing presence data for the target species.
  #   min_sp: The minimum number of species required at a site to be considered a potential absence.
  # 
  # Returns:
  #   A data frame containing potential absence data for the target species, with each point
  #   snapped to the nearest grid point and a "pa" column added with value 0.
  
  # Generate initial possible absence list based on distinct species but not target
  absences <- pres_all %>% # select all presence observations
    filter(species != pres$species[1]) %>% # select only non-target species observations
    anti_join(presences, by = c("lon", "lat")) %>% # remove sites that have target species
    group_by(lon, lat) %>% # group by grid cell
    filter(n_distinct(species) >= min_sp) %>% # only sites with min_species or more different species
    ungroup() %>% # ungroup data frame
    distinct(lon, lat, .keep_all = TRUE) %>% # select a single potential absence per grid cell
    mutate(species = pres$species[1], pa = 0) # change species name to target, add 'pa' column with 0
  
  return(absences) # Return potential absence data for the target species
}
####
# Function to process species presences
process_species_presences <- function(presences_all, species, grid_sf) {
  presences <- species_presences(presences_all, species)
  
  pa_sf <- presences %>%
    select(lon, lat, species, pa) %>%
    st_as_sf(coords = c("lon", "lat"), crs = crs_ref) %>%
    st_transform(st_crs(grid_sf))
  
  hex_species <- pa_sf %>%
    {nearest_indices <- st_nearest_feature(., grid_sf)
    mutate(., hex_id = grid_sf$hex_id[nearest_indices])} %>%
    st_drop_geometry() %>%
    select(hex_id, species, pa) %>%
    distinct(hex_id, .keep_all = TRUE)
  
  return(hex_species)
}

# Function to process each hex grid and add species data
process_hex_grid <- function(grid_sf, presences_all, species_pool, species_cores) {
  grid_sf$hex_id <- 1:nrow(grid_sf)
  centroids <- st_centroid(grid_sf)
  grid_sf <- st_as_sf(centroids, coords = c("lon", "lat"), crs = crs_ref)
  
  hex_species_list <- mclapply(species_pool, function(species) {
    process_species_presences(presences_all, species, grid_sf)
  }, mc.cores = species_cores)
  
  hex_species_combined <- bind_rows(hex_species_list)
  
  species_mat <- hex_species_combined %>%
    pivot_wider(id_cols = c(hex_id),
                names_from = "species",
                values_from = "pa",
                values_fill = 0,
                values_fn = sum) %>%
    tibble::column_to_rownames("hex_id")
  
  return(species_mat)
}

parallel::detectCores()

# Main processing function
process_all_grids <- function(hex_grids_list, presence_all_path, total_cores = 6) {
  presences_all <- read.csv(presence_all_path)
  species_pool <- unique(presences_all$species)
  
  # Allocate cores
  hex_cores <- max(1, round(total_cores / 3))
  species_cores <- max(1, total_cores - hex_cores)
  
  hex_species_matrices <- mclapply(names(hex_grids_list), function(grid_name) {
    grid_sf <- hex_grids_list[[grid_name]]
    species_mat <- process_hex_grid(grid_sf, presences_all, species_pool, species_cores)
    return(species_mat)
  }, mc.cores = hex_cores)
  
  names(hex_species_matrices) <- names(hex_grids_list)
  return(hex_species_matrices)
}




# function_wrap -----------------------------------------------------------
calculate_pairwise_metrics <- function(x) {
  
  x <- as.matrix(x)
  
  shared <- x %*% t(x)
  not.shared <- abs(sweep(shared, 2, diag(shared)))
  sumSi <- sum(diag(shared))
  St <- sum(colSums(x) > 0)
  a <- sumSi - St
  sum.not.shared <- not.shared + t(not.shared)
  max.not.shared <- pmax(not.shared, t(not.shared))
  min.not.shared <- pmin(not.shared, t(not.shared))
  computations <- list(data = x, sumSi = sumSi, St = St, a = a, 
                       shared = shared, not.shared = not.shared, sum.not.shared = sum.not.shared, 
                       max.not.shared = max.not.shared, min.not.shared = min.not.shared)
  class(computations) <- "scalebeta"
  return(computations)
}


calculate_beta_diversity <- function(x, index_family = "sorensen", framework = "BAS") {
  index.family <- match.arg(index_family, c("jaccard", "sorensen", "wb"))
  framework <- match.arg(framework, c("BAS", "POD", "SET"))
  
  if (!inherits(x, "scalebeta")) {
    x <- calculate_pairwise_metrics(x)
  }
  
  result <- switch(index_family,
                   sorensen = {
                     beta_s = x$sum.not.shared / ((2 * x$shared) + x$sum.not.shared)
                     switch(framework,
                            BAS = {
                              beta_repl_bs = x$min.not.shared / (x$shared + x$min.not.shared)
                              nest_bs = (abs(x$not.shared - t(x$not.shared)) / ((2 * x$shared) + x$sum.not.shared)) * (x$shared / (x$shared + x$min.not.shared))
                              list(beta_s = as.dist(beta_s), beta_repl_bs = as.dist(beta_repl_bs), nest_bs = as.dist(nest_bs))
                            },
                            POD = {
                              beta_repl_s = (2 * x$min.not.shared) / ((2 * x$shared) + x$sum.not.shared)
                              rich_s = abs(x$not.shared - t(x$not.shared)) / ((2 * x$shared) + x$sum.not.shared)
                              list(beta_s = as.dist(beta_s), beta_repl_s = as.dist(beta_repl_s), rich_s = as.dist(rich_s))
                            },
                            SET = {
                              i_s = ifelse(x$shared > 0, abs(x$not.shared - t(x$not.shared)) / ((2 * x$shared) + x$sum.not.shared), 0)
                              rc_s = ifelse(x$shared > 0, (2 * x$min.not.shared) / ((2 * x$shared) + x$sum.not.shared), x$sum.not.shared / ((2 * x$shared) + x$sum.not.shared))
                              list(beta_s = as.dist(beta_s), i_s = as.dist(i_s), rc_s = as.dist(rc_s))
                            })
                   },
                   jaccard = {
                     beta_j = x$sum.not.shared / (x$shared + x$sum.not.shared)
                     switch(framework,
                            BAS = {
                              beta_repl_bj = (2 * x$min.not.shared) / (x$shared + (2 * x$min.not.shared))
                              nest_bj = (abs(x$not.shared - t(x$not.shared)) / (x$shared + x$sum.not.shared)) * (x$shared / (x$shared + (2 * x$min.not.shared)))
                              list(beta_j = as.dist(beta_j), beta_repl_bj = as.dist(beta_repl_bj), nest_bj = as.dist(nest_bj))
                            },
                            POD = {
                              beta_repl_j = (2 * x$min.not.shared) / (x$shared + x$sum.not.shared)
                              rich_j = abs(x$not.shared - t(x$not.shared)) / (x$shared + x$sum.not.shared)
                              list(beta_j = as.dist(beta_j), beta_repl_j = as.dist(beta_repl_j), rich_j = as.dist(rich_j))
                            },
                            SET = {
                              i_j = ifelse(x$shared > 0, abs(x$not.shared - t(x$not.shared)) / (x$shared + x$sum.not.shared), 0)
                              rc_j = ifelse(x$shared > 0, (2 * x$min.not.shared) / (x$shared + x$sum.not.shared), x$sum.not.shared / (x$shared + x$sum.not.shared))
                              list(beta_j = as.dist(beta_j), i_j = as.dist(i_j), rc_j = as.dist(rc_j))
                            })
                   },
                   wb = {
                     beta_wb = x$sum.not.shared
                     switch(framework,
                            POD = {
                              beta_repl_wb = 2 * x$min.not.shared
                              rich_wb = abs(x$not.shared - t(x$not.shared))
                              list(beta_wb = as.dist(beta_wb), beta_repl_wb = as.dist(beta_repl_wb), rich_wb = as.dist(rich_wb))
                            },
                            SET = {
                              i_wb = ifelse(x$shared > 0, abs(x$not.shared - t(x$not.shared)), 0)
                              rc_wb = ifelse(x$shared > 0, 2 * x$min.not.shared, x$sum.not.shared)
                              list(beta_wb = as.dist(beta_wb), i_wb = as.dist(i_wb), rc_wb = as.dist(rc_wb))
                            })
                   })
  
  return(result)
}



beta_across_scales <- function(hex_species_matrices, index_family = "sorensen", framework = "BAS", num_cores = (parallel::detectCores()-2)) {
  
  metrics_list <- mclapply(hex_species_matrices, function(species_mat) {
    calculate_beta_diversity(species_mat, index_family, framework)
  }, mc.cores = num_cores)
  
  names(metrics_list) <- names(hex_species_matrices)
  return(metrics_list)
}

plot_ridge <- function(df) {
  ggplot(df, aes(x = value, y = scale, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
    scale_fill_viridis_c(name = "Beta Diversity", option = "plasma", direction = -1) +
    scale_y_discrete(labels = function(x) paste0(format(as.numeric(gsub(" km", "", x)), big.mark = ","), " km²")) +
    labs(x = "Mean Beta Deviation", y = "Spatial Grain (km²)") +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(2, "cm"),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      strip.text = element_text(size = 14, face = "bold"),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    ) +
    facet_wrap(~measure, scales = "free_y", ncol = 1)
}
