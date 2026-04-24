# ══════════════════════════════════════════════════════════════════════════════
# Census Division employment shares (2021 Census)
# Uses cancensus to pull total employed labour force at CD and PR level,
# then computes each CD's share of its province's employment.
# Output: updating_data/cd_overall_shares.csv
# ══════════════════════════════════════════════════════════════════════════════

library(cancensus)
library(dplyr)

options(cancensus.api_key = "CensusMapper_23ab6804287fb370dc586dfc046ff859")

# v_CA21_6606 = All industries, total labour force 15+
EMP_VECTOR <- "v_CA21_6606"

# ── 1. CD-level employment ───────────────────────────────────────────────────
cd_raw <- get_census(
  dataset    = "CA21",
  regions    = list(C = "01"),   # all of Canada
  vectors    = EMP_VECTOR,
  level      = "CD",
  geo_format = NA
)

cd_emp <- cd_raw %>%
  select(
    CDUID  = GeoUID,
    CDNAME = `Region Name`,
    PRUID  = PR_UID,
    emp_cd = `v_CA21_6606: All industries`
  ) %>%
  mutate(emp_cd = as.numeric(emp_cd))

cat("CDs retrieved:", nrow(cd_emp), "\n")

# ── 2. Province-level employment ─────────────────────────────────────────────
pr_raw <- get_census(
  dataset    = "CA21",
  regions    = list(C = "01"),
  vectors    = EMP_VECTOR,
  level      = "PR",
  geo_format = NA
)

pr_emp <- pr_raw %>%
  select(
    PRUID    = GeoUID,
    province = `Region Name`,
    emp_prov = `v_CA21_6606: All industries`
  ) %>%
  mutate(emp_prov = as.numeric(emp_prov))

cat("Provinces retrieved:", nrow(pr_emp), "\n")

# ── 3. Join and compute shares ───────────────────────────────────────────────
cd_shares <- cd_emp %>%
  left_join(pr_emp, by = "PRUID") %>%
  mutate(cd_share = ifelse(emp_prov > 0, emp_cd / emp_prov, 0)) %>%
  arrange(desc(emp_cd)) %>%
  select(CDUID, CDNAME, province, emp_cd, emp_prov, cd_share)

cat("\nTop 15 CDs by employment:\n")
print(head(cd_shares, 15))

# ── 4. Save ──────────────────────────────────────────────────────────────────
write.csv(cd_shares, "updating_data/cd_overall_shares.csv", row.names = FALSE)
cat("\nSaved updating_data/cd_overall_shares.csv —", nrow(cd_shares), "rows\n")
