# ══════════════════════════════════════════════════════════════════════════════
# Map each business record to a Census Division and build CD polygons.
# CD assignment is taken directly from charity_ident (no spatial join needed).
#
# Outputs:
#   updating_data/business_cd_lookup.csv   — Business Number + Year → CDUID
#   non-updating_data/cd_sf.rds            — CD polygons
# ══════════════════════════════════════════════════════════════════════════════

library(sf)
library(dplyr)

# ── 1. Build CD polygons from CD shapefile ───────────────────────────────────
# Shapefile only ships PRUID, so map it to the English PRNAME used elsewhere.
prname_by_pruid <- c(
  "10" = "Newfoundland and Labrador", "11" = "Prince Edward Island",
  "12" = "Nova Scotia",               "13" = "New Brunswick",
  "24" = "Quebec",                    "35" = "Ontario",
  "46" = "Manitoba",                  "47" = "Saskatchewan",
  "48" = "Alberta",                   "59" = "British Columbia",
  "60" = "Yukon",                     "61" = "Northwest Territories",
  "62" = "Nunavut"
)

cat("Loading CD shapefile...\n")
cd_sf <- st_read("non-updating_data/lcd_000a21a_e.shp", quiet = TRUE) %>%
  mutate(PRNAME = unname(prname_by_pruid[as.character(PRUID)])) %>%
  select(CDUID, CDNAME, PRUID, PRNAME, geometry)

cat("Census Divisions:", nrow(cd_sf), "\n")
saveRDS(cd_sf, "non-updating_data/cd_sf.rds")
cat("Saved non-updating_data/cd_sf.rds\n\n")

# ── 2. Load MASS business data ───────────────────────────────────────────────
# Reads charity_ident from the new normalized data. Matches the toggle in global.R.
cat("Loading MASS data...\n")
USE_REAL_DOLLARS <- FALSE
data_file <- if (USE_REAL_DOLLARS)
  "mass-culture-dashboard-data/df_flagged_value_na_real_dollars.rda" else
  "mass-culture-dashboard-data/df_flagged_value_na.rda"
loaded_obj <- load(data_file)
mass_data <- get(loaded_obj)

# ── 3. Build lookup directly from charity_ident's CD columns ─────────────────
# charity_ident already carries Census Division ID/Name per (Business Number, Year);
# attach PRNAME from the shapefile so the lookup matches the previous schema.
prname_lookup <- cd_sf %>% st_drop_geometry() %>% select(CDUID, PRNAME)

lookup <- mass_data$charity_ident %>%
  select(`Business Number`, Year,
         CDUID  = `Census Division ID`,
         CDNAME = `Census Division Name`) %>%
  filter(!is.na(CDUID)) %>%
  left_join(prname_lookup, by = "CDUID")

n_total   <- nrow(mass_data$charity_ident)
n_kept    <- nrow(lookup)
n_missing <- n_total - n_kept
if (n_missing > 0)
  cat("Note:", n_missing, "of", n_total,
      "charity_ident rows had no Census Division ID and were dropped.\n")

# ── 4. Save lookup ───────────────────────────────────────────────────────────
lookup <- lookup %>% select(`Business Number`, Year, CDUID, CDNAME, PRNAME)
write.csv(lookup, "updating_data/business_cd_lookup.csv", row.names = FALSE)

cat("\nSaved updating_data/business_cd_lookup.csv —", nrow(lookup), "rows\n")
cat("Unique CDs represented:", n_distinct(lookup$CDUID), "\n")
cat("Sample:\n")
print(head(lookup, 5))
