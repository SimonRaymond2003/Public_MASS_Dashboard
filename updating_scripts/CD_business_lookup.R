# ══════════════════════════════════════════════════════════════════════════════
# Map each business record to a Census Division via spatial join
# Uses Latitude/Longitude from the MASS data directly.
# Also dissolves the CSD shapefile into CD polygons and saves cd_sf.rds.
#
# Outputs:
#   updating_data/business_cd_lookup.csv   — Business Number + Year → CDUID
#   non-updating_data/cd_sf.rds            — CD polygons (dissolved from CSDs)
# ══════════════════════════════════════════════════════════════════════════════

library(sf)
library(dplyr)
library(readxl)

# ── 1. Build CD polygons from CSD shapefile ──────────────────────────────────
cat("Loading CSD shapefile...\n")
csd_sf <- st_read("non-updating_data/lcsd000a25a_e.shp", quiet = TRUE)

cat("Dissolving CSDs into Census Divisions...\n")
cd_sf <- csd_sf %>%
  group_by(CDUID, CDNAME, PRUID, PRNAME) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  # Strip bilingual province names to English only
  mutate(PRNAME = sub(" / .*$", "", PRNAME))

cat("Census Divisions:", nrow(cd_sf), "\n")

# Save for app use
saveRDS(cd_sf, "non-updating_data/cd_sf.rds")
cat("Saved non-updating_data/cd_sf.rds\n\n")

# ── 2. Load MASS business data ───────────────────────────────────────────────
cat("Loading MASS data...\n")
data_file <- list.files("MASS_data", pattern = "\\.xlsx$", full.names = TRUE)[1]
mass_raw <- read_excel(data_file)

# Keep only rows with valid coordinates
biz <- mass_raw %>%
  select(`Business Number`, Year, Latitude, Longitude) %>%
  filter(!is.na(Latitude), !is.na(Longitude))

cat("Business records with coordinates:", nrow(biz), "/", nrow(mass_raw), "\n")

# ── 3. Spatial join: business points → CD polygons ──────────────────────────
cat("Converting to spatial points (WGS84)...\n")
biz_sf <- st_as_sf(biz, coords = c("Longitude", "Latitude"), crs = 4326)

# Transform to match CSD shapefile projection
biz_sf <- st_transform(biz_sf, st_crs(cd_sf))

cat("Running spatial join...\n")
joined <- st_join(biz_sf, cd_sf[, c("CDUID", "CDNAME", "PRNAME")], join = st_within)

# ── 4. Handle unmatched records ──────────────────────────────────────────────
n_missing <- sum(is.na(joined$CDUID))
if (n_missing > 0) {
  cat("WARNING:", n_missing, "records did not fall within a CD polygon.",
      "Trying nearest feature fallback...\n")
  missing_idx <- which(is.na(joined$CDUID))
  nearest <- st_nearest_feature(biz_sf[missing_idx, ], cd_sf)
  joined$CDUID[missing_idx]  <- cd_sf$CDUID[nearest]
  joined$CDNAME[missing_idx] <- cd_sf$CDNAME[nearest]
  joined$PRNAME[missing_idx] <- cd_sf$PRNAME[nearest]
  cat("After fallback, unmatched:", sum(is.na(joined$CDUID)), "\n")
}

# ── 5. Save lookup ───────────────────────────────────────────────────────────
lookup <- joined %>%
  st_drop_geometry() %>%
  select(`Business Number`, Year, CDUID, CDNAME, PRNAME)

write.csv(lookup, "updating_data/business_cd_lookup.csv", row.names = FALSE)

cat("\nSaved updating_data/business_cd_lookup.csv —", nrow(lookup), "rows\n")
cat("Unique CDs represented:", n_distinct(lookup$CDUID), "\n")
cat("Sample:\n")
print(head(lookup, 5))
