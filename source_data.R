# source_data.R — reads the latest PMC-processed CSV (written by
# eBird-projects/PMC/PMC.R), splits it by count-year (M.YEAR), and writes
# the small derived files the GitHub Pages dashboard fetches:
#   years.json                        — manifest: which years exist, which is "current" (live)
#   grids_geometry.geojson             — static grid-cell polygons (Kachchh), no stats (shared by all years)
#   years/<year>/stats.json            — top-line counts
#   years/<year>/checklists.geojson    — one point per checklist
#   years/<year>/grid_stats.json       — per-grid-cell counts for that year (joined client-side to the geometry)
#   years/<year>/species.json          — focal-species gallery manifest
#   years/<year>/all_species.json      — every taxon (incl. spuh/slash/hybrid), by distinct checklist
#   years/<year>/checklist_grid_map.csv— GROUP.ID / SAMPLING.EVENT.IDENTIFIER / GRID_CODE
#   years/<year>/daily_cumulative.json — cumulative lists/species/birders by date, for year-over-year comparison
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

PMC_DATA_DIR  <- "E:/Abhinandan/BCI/eBird-projects/PMC/data"
OUT_DIR       <- "E:/Abhinandan/BCI/eBird-projects/PMC/PMC-dashboard"
YEARS_DIR     <- file.path(OUT_DIR, "years")
GRID_SHP      <- file.path(OUT_DIR, "PMC-grids", "PMC_IN_2026.shp")
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

# slug helper: "Blue-cheeked Bee-eater" -> "blue-cheeked-bee-eater" (matches www/species/*.svg)
slugify <- function(x) {
  x %>% str_to_lower() %>% str_remove_all("'") %>% str_replace_all("[^a-z0-9]+", "-") %>% str_remove_all("^-|-$")
}

empty_feature_collection <- function() '{"type":"FeatureCollection","features":[]}'

# ---- process a single count-year's worth of rows, write its output bundle ----
process_year <- function(data_year, grid, year) {
  year_dir <- file.path(YEARS_DIR, as.character(year))
  dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)

  n_lists   <- n_distinct(data_year$GROUP.ID)
  n_birders <- n_distinct(data_year$OBSERVER.ID)
  n_species <- n_distinct(data_year$COMMON.NAME[data_year$CATEGORY %in% c("species", "issf")])

  stats <- list(
    unique_lists = n_lists,
    birders      = n_birders,
    species      = n_species,
    last_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
  write_json(stats, file.path(year_dir, "stats.json"), auto_unbox = TRUE, pretty = TRUE)

  if (n_lists == 0) {
    # no data yet for this year (e.g. the current season hasn't started) — write valid empty outputs
    writeLines(empty_feature_collection(), file.path(year_dir, "checklists.geojson"))
    write_json(list(), file.path(year_dir, "grid_stats.json"), auto_unbox = TRUE)
    write_json(
      tibble(name = FOCAL_SPECIES, slug = slugify(FOCAL_SPECIES),
             image = paste0("www/species/", slugify(FOCAL_SPECIES), ".svg"), checklists = 0L),
      file.path(year_dir, "species.json"), auto_unbox = TRUE, pretty = TRUE
    )
    write_json(list(), file.path(year_dir, "all_species.json"), auto_unbox = TRUE)
    write_json(list(), file.path(year_dir, "daily_cumulative.json"), auto_unbox = TRUE)
    file.create(file.path(year_dir, "checklist_grid_map.csv"))
    message(glue("[{year}] no data yet — empty outputs written."))
    return(invisible(FALSE))
  }

  # ---- one point per checklist (GROUP.ID, so shared checklists count once) ----
  checklist_species <- data_year %>%
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

  checklists <- data_year %>%
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

  # ---- assign each checklist to its survey grid cell ----
  checklists <- st_join(checklists, grid %>% select(GRID_CODE, DISTRICT, BLOCK), join = st_intersects, left = TRUE) %>%
    distinct(GROUP.ID, .keep_all = TRUE)   # guard against duplicate matches on shared grid edges

  message(glue("[{year}] grid cell assigned to {sum(!is.na(checklists$GRID_CODE))} of {nrow(checklists)} checklist(s)."))

  checklist_grid_map <- checklists %>%
    st_drop_geometry() %>%
    select(GROUP.ID, SAMPLING.EVENT.IDENTIFIER, GRID_CODE)
  write_csv(checklist_grid_map, file.path(year_dir, "checklist_grid_map.csv"))

  geojson <- sf_geojson(checklists)
  if (length(geojson) != 1 || is.na(geojson) || !nzchar(geojson)) stop("sf_geojson() produced no output for checklists.")
  writeLines(geojson, file.path(year_dir, "checklists.geojson"))
  message(glue("[{year}] checklists.geojson written: {nrow(checklists)} checklist(s), {sum(checklists$has_focal)} with focal species."))

  # ---- focal species gallery manifest ----
  species_counts <- sapply(FOCAL_SPECIES, function(sp) sum(str_detect(checklists$focal_species_seen, fixed(sp))))
  species_manifest <- tibble(
    name       = FOCAL_SPECIES,
    slug       = slugify(FOCAL_SPECIES),
    image      = paste0("www/species/", slugify(FOCAL_SPECIES), ".svg"),
    checklists = as.integer(species_counts[FOCAL_SPECIES])
  )
  write_json(species_manifest, file.path(year_dir, "species.json"), auto_unbox = TRUE, pretty = TRUE)

  # ---- per-grid stats (geometry lives in the shared grids_geometry.geojson) ----
  group_to_grid <- checklists %>% st_drop_geometry() %>% select(GROUP.ID, GRID_CODE)

  grid_stats <- data_year %>%
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
  write_json(grid_stats, file.path(year_dir, "grid_stats.json"), auto_unbox = TRUE, pretty = TRUE)
  message(glue("[{year}] grid_stats.json written: {nrow(grid_stats)} {GRID_DISTRICT} cell(s) with data."))

  # ---- complete species list (every category, incl. spuh/slash/hybrid), by distinct checklist ----
  # OBSERVATION.COUNT is "X" (presence, no count) for some rows; those are excluded from the sum.
  all_species <- data_year %>%
    group_by(COMMON.NAME) %>%
    summarise(
      category    = first(CATEGORY),
      checklists  = n_distinct(GROUP.ID),
      total_count = sum(suppressWarnings(as.numeric(OBSERVATION.COUNT)), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(pct_checklists = round(100 * checklists / n_lists, 1)) %>%
    arrange(desc(checklists))
  write_json(all_species, file.path(year_dir, "all_species.json"), auto_unbox = TRUE, pretty = TRUE)

  # ---- daily cumulative totals, for "same date last year" comparisons ----
  dates <- sort(unique(data_year$OBSERVATION.DATE[!is.na(data_year$OBSERVATION.DATE)]))
  daily_cumulative <- map_dfr(dates, function(d) {
    upto <- data_year %>% filter(OBSERVATION.DATE <= d)
    tibble(
      date       = as.character(d),
      month_day  = format(d, "%m-%d"),
      cum_lists   = n_distinct(upto$GROUP.ID),
      cum_species = n_distinct(upto$COMMON.NAME[upto$CATEGORY %in% c("species", "issf")]),
      cum_birders = n_distinct(upto$OBSERVER.ID)
    )
  })
  write_json(daily_cumulative, file.path(year_dir, "daily_cumulative.json"), auto_unbox = TRUE, pretty = TRUE)
  message(glue("[{year}] daily_cumulative.json written: {nrow(daily_cumulative)} date(s)."))

  invisible(TRUE)
}

update_pmc_dashboard <- function() {

  # ---- 1. find latest processed CSV ----
  csvs <- list.files(PMC_DATA_DIR, pattern = "^PMC-processed-.*\\.csv$", full.names = TRUE)
  if (length(csvs) == 0) stop(glue("No PMC-processed-*.csv found in {PMC_DATA_DIR}"))
  latest_csv <- csvs[which.max(file.info(csvs)$mtime)]
  message(glue("Latest data file: {latest_csv}"))

  data <- read_csv(latest_csv, show_col_types = FALSE, na = c("", "NA"))

  # ---- 2. static grid geometry (shared across all years) ----
  grid <- st_read(GRID_SHP, quiet = TRUE) %>% st_transform(4326)
  grid_geometry <- grid %>% filter(DISTRICT == GRID_DISTRICT) %>% select(GRID_CODE, DISTRICT, BLOCK)
  grid_geojson <- sf_geojson(grid_geometry)
  if (length(grid_geojson) != 1 || is.na(grid_geojson) || !nzchar(grid_geojson)) stop("sf_geojson() produced no output for grid geometry.")
  writeLines(grid_geojson, file.path(OUT_DIR, "grids_geometry.geojson"))
  message(glue("grids_geometry.geojson written: {nrow(grid_geometry)} {GRID_DISTRICT} cell(s)."))

  # ---- 3. split by count-year (M.YEAR); always include the current calendar year, even if empty ----
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  data_years    <- sort(unique(data$M.YEAR))
  all_years     <- sort(union(data_years, current_year))

  dir.create(YEARS_DIR, recursive = TRUE, showWarnings = FALSE)
  for (yr in all_years) {
    process_year(data %>% filter(M.YEAR == yr), grid, yr)
  }

  write_json(
    list(years = all_years, current_year = current_year),
    file.path(OUT_DIR, "years.json"), auto_unbox = TRUE, pretty = TRUE
  )
  message(glue("years.json written: {paste(all_years, collapse=', ')} (current: {current_year})."))

  invisible(TRUE)
}

if (sys.nframe() == 0) {
  update_pmc_dashboard()
}
