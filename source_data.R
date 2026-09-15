# source_data.R — reads the latest PMC-processed CSV (written by
# eBird-projects/PMC/PMC.R) and writes the small derived files the
# GitHub Pages dashboard fetches: stats.json + checklists.geojson +
# grids.geojson + species.json + all_species.json.
#
# Run:  Rscript source_data.R
# (or call update_pmc_dashboard() from another script, e.g. an automation loop)

suppressMessages({
  library(tidyverse)
  library(glue)
  library(jsonlite)
  library(sf)
  library(geojsonsf)
})

PMC_DATA_DIR <- "E:/Abhinandan/BCI/eBird-projects/PMC/data"
OUT_DIR      <- "E:/Abhinandan/BCI/eBird-projects/PMC/PMC-dashboard"
GRID_SHP     <- file.path(OUT_DIR, "PMC-grids", "PMC_IN_2026.shp")
GRID_DISTRICT <- "Kachchh"   # which district's grid cells to publish (matches the map's bounds)

# ---- focal species to highlight on the map (common names, must match COMMON.NAME) ----
FOCAL_SPECIES <- c(
  "Blue-cheeked Bee-eater",
  "Common Cuckoo",
  "European Roller",
  "Greater Whitethroat",
  "Red-backed Shrike",
  "Red-tailed Shrike",
  "Rufous-tailed Scrub-Robin",
  "Spotted Flycatcher"
)

# slug helper: "Blue-cheeked Bee-eater" -> "blue-cheeked-bee-eater" (matches www/species/*.jpg)
slugify <- function(x) {
  x %>% str_to_lower() %>% str_remove_all("'") %>% str_replace_all("[^a-z0-9]+", "-") %>% str_remove_all("^-|-$")
}

update_pmc_dashboard <- function() {

  # ---- 1. find latest processed CSV ----
  csvs <- list.files(PMC_DATA_DIR, pattern = "^PMC-processed-.*\\.csv$", full.names = TRUE)
  if (length(csvs) == 0) stop(glue("No PMC-processed-*.csv found in {PMC_DATA_DIR}"))
  latest_csv <- csvs[which.max(file.info(csvs)$mtime)]
  message(glue("Latest data file: {latest_csv}"))

  data <- read_csv(latest_csv, show_col_types = FALSE, na = c("", "NA"))

  # ---- 2. top-line stats ----
  n_lists   <- n_distinct(data$GROUP.ID)
  n_birders <- n_distinct(data$OBSERVER.ID)
  n_species <- n_distinct(data$COMMON.NAME[data$CATEGORY %in% c("species", "issf")])

  stats <- list(
    unique_lists = n_lists,
    birders      = n_birders,
    species      = n_species,
    last_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
  write_json(stats, file.path(OUT_DIR, "stats.json"), auto_unbox = TRUE, pretty = TRUE)
  message(glue("stats.json written: {n_lists} lists, {n_birders} birders, {n_species} species."))

  # ---- 3. one point per checklist (GROUP.ID, so shared checklists count once) ----
  checklist_species <- data %>%
    filter(CATEGORY %in% c("species", "issf")) %>%
    group_by(GROUP.ID) %>%
    summarise(
      species_list = paste(sort(unique(COMMON.NAME)), collapse = "; "),
      n_species    = n_distinct(COMMON.NAME),
      focal_species_seen  = paste(sort(unique(COMMON.NAME[COMMON.NAME %in% FOCAL_SPECIES])), collapse = "; "),
      focal_species_slugs = paste(slugify(sort(unique(COMMON.NAME[COMMON.NAME %in% FOCAL_SPECIES]))), collapse = ";"),
      .groups = "drop"
    ) %>%
    mutate(has_focal = focal_species_seen != "")

  checklists <- data %>%
    distinct(GROUP.ID, .keep_all = TRUE) %>%
    select(GROUP.ID, LOCALITY, LATITUDE, LONGITUDE, OBSERVATION.DATE,
           OBSERVER.ID, DURATION.MINUTES, SAMPLING.EVENT.IDENTIFIER) %>%
    left_join(checklist_species, by = "GROUP.ID") %>%
    mutate(
      n_species           = replace_na(n_species, 0L),
      species_list        = replace_na(species_list, ""),
      focal_species_seen  = replace_na(focal_species_seen, ""),
      focal_species_slugs = replace_na(focal_species_slugs, ""),
      has_focal           = replace_na(has_focal, FALSE)
    ) %>%
    filter(!is.na(LATITUDE), !is.na(LONGITUDE)) %>%
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)

  # ---- 3b. assign each checklist to its survey grid cell ----
  grid <- st_read(GRID_SHP, quiet = TRUE) %>% st_transform(4326)

  checklists <- st_join(checklists, grid %>% select(GRID_CODE, DISTRICT, BLOCK), join = st_intersects, left = TRUE) %>%
    distinct(GROUP.ID, .keep_all = TRUE)   # guard against duplicate matches on shared grid edges

  message(glue("Grid cell assigned to {sum(!is.na(checklists$GRID_CODE))} of {nrow(checklists)} checklist(s)."))

  # one row per unique checklist (GROUP.ID) with its grid assignment
  checklist_grid_map <- checklists %>%
    st_drop_geometry() %>%
    select(GROUP.ID, SAMPLING.EVENT.IDENTIFIER, GRID_CODE)
  write_csv(checklist_grid_map, file.path(OUT_DIR, "checklist_grid_map.csv"))
  message(glue("checklist_grid_map.csv written: {nrow(checklist_grid_map)} checklist(s)."))

  geojson <- sf_geojson(checklists)
  if (length(geojson) != 1 || is.na(geojson) || !nzchar(geojson)) {
    stop("sf_geojson() produced no output for checklists.")
  }
  writeLines(geojson, file.path(OUT_DIR, "checklists.geojson"))
  message(glue("checklists.geojson written: {nrow(checklists)} checklist(s), {sum(checklists$has_focal)} with focal species."))

  # ---- 4. focal species gallery manifest (name, image, how many checklists) ----
  species_counts <- sapply(FOCAL_SPECIES, function(sp) sum(str_detect(checklists$focal_species_seen, fixed(sp))))
  species_manifest <- tibble(
    name       = FOCAL_SPECIES,
    slug       = slugify(FOCAL_SPECIES),
    image      = paste0("www/species/", slugify(FOCAL_SPECIES), ".jpg"),
    checklists = as.integer(species_counts[FOCAL_SPECIES])
  )
  write_json(species_manifest, file.path(OUT_DIR, "species.json"), auto_unbox = TRUE, pretty = TRUE)
  message(glue("species.json written: {nrow(species_manifest)} focal species."))

  # ---- 5. per-grid summary, for the hover-over-grid-cell view ----
  group_to_grid <- checklists %>% st_drop_geometry() %>% select(GROUP.ID, GRID_CODE)

  grid_summary <- data %>%
    left_join(group_to_grid, by = "GROUP.ID") %>%
    filter(!is.na(GRID_CODE)) %>%
    group_by(GRID_CODE) %>%
    summarise(
      n_lists     = n_distinct(GROUP.ID),
      n_birders   = n_distinct(OBSERVER.ID),
      n_species   = n_distinct(COMMON.NAME[CATEGORY %in% c("species", "issf")]),
      focal_lists = n_distinct(GROUP.ID[COMMON.NAME %in% FOCAL_SPECIES]),
      .groups = "drop"
    )

  grids_out <- grid %>%
    filter(DISTRICT == GRID_DISTRICT) %>%
    left_join(st_drop_geometry(grid_summary), by = "GRID_CODE") %>%
    mutate(across(c(n_lists, n_birders, n_species, focal_lists), ~ replace_na(., 0L)))

  grids_geojson <- sf_geojson(grids_out)
  if (length(grids_geojson) != 1 || is.na(grids_geojson) || !nzchar(grids_geojson)) {
    stop("sf_geojson() produced no output for grids.")
  }
  writeLines(grids_geojson, file.path(OUT_DIR, "grids.geojson"))
  message(glue("grids.geojson written: {nrow(grids_out)} {GRID_DISTRICT} grid cell(s), {sum(grids_out$n_lists > 0)} with checklists."))

  # ---- 6. complete species list (every category, incl. spuh/slash/hybrid), counted by distinct checklist ----
  all_species <- data %>%
    group_by(COMMON.NAME) %>%
    summarise(
      category   = first(CATEGORY),
      checklists = n_distinct(GROUP.ID),
      .groups = "drop"
    ) %>%
    arrange(desc(checklists))

  write_json(all_species, file.path(OUT_DIR, "all_species.json"), auto_unbox = TRUE, pretty = TRUE)
  message(glue("all_species.json written: {nrow(all_species)} taxa."))

  invisible(TRUE)
}

if (sys.nframe() == 0) {
  update_pmc_dashboard()
}
