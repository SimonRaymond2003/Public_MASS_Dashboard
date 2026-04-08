# ══════════════════════════════════════════════════════════════════════════════
# City / Province employment ratios by industry (2024) — ALL CMAs
# ══════════════════════════════════════════════════════════════════════════════

library(cansim)
library(dplyr)
library(stringr)

# ── 1. CMA employment (14-10-0468-01) ────────────────────────────────────────
cma_raw <- get_cansim("14-10-0468-01")

# Industry rows only (not occupations, not sector totals, not class-of-worker)
INDUSTRY_ROWS <- c(
  "Agriculture",
  "Forestry, fishing, mining, quarrying, oil and gas",
  "Utilities",
  "Construction",
  "Manufacturing",
  "Wholesale and retail trade",
  "Transportation and warehousing",
  "Finance, insurance, real estate, rental and leasing",
  "Professional, scientific and technical services",
  "Business, building and other support services",
  "Educational services",
  "Health care and social assistance",
  "Information, culture and recreation",
  "Accommodation and food services",
  "Other services (except public administration)",
  "Public administration"
)

cma_emp <- cma_raw %>%
  filter(REF_DATE == max(REF_DATE),
         `Employment characteristics` %in% INDUSTRY_ROWS) %>%
  select(cma = GEO, industry = `Employment characteristics`, emp_city = VALUE) %>%
  mutate(emp_city = as.numeric(emp_city))

# ── 2. Extract province from CMA name ───────────────────────────────────────
# CMA format: "Toronto, Ontario" or "Ottawa-Gatineau, Ontario/Quebec"
# For cross-province CMAs (Ottawa-Gatineau), there are sub-entries too —
# we keep the combined one and assign to the first-listed province.

# Also skip the sub-parts ("Ontario part", "Quebec part") to avoid double-counting
cma_emp <- cma_emp %>%
  filter(!grepl("part,", cma)) %>%
  mutate(
    province = str_extract(cma, "(?<=, ).*$"),         # everything after last comma-space
    province = str_remove(province, "/.*$"),            # drop "/Quebec" from cross-prov CMAs
    province = trimws(province)
  )

cat("CMAs found:", length(unique(cma_emp$cma)), "\n")
cat("Provinces extracted:\n")
print(sort(unique(cma_emp$province)))


# ── 3. Provincial employment (14-10-0023-01) ────────────────────────────────
prov_raw <- get_cansim("14-10-0023-01")

prov_emp <- prov_raw %>%
  filter(REF_DATE == "2024",
         `Labour force characteristics` == "Employment",
         Gender == "Total - Gender",
         `Age group` == "15 years and over",
         `North American Industry Classification System (NAICS)` %in% INDUSTRY_ROWS) %>%
  select(province = GEO,
         industry = `North American Industry Classification System (NAICS)`,
         emp_prov = VALUE) %>%
  mutate(emp_prov = as.numeric(emp_prov))


# ── 4. Compute ratios ───────────────────────────────────────────────────────
city_ratios <- cma_emp %>%
  inner_join(prov_emp, by = c("province", "industry")) %>%
  mutate(city_share = ifelse(emp_prov > 0, emp_city / emp_prov, 0)) %>%
  arrange(cma, industry)

cat("\nTotal rows:", nrow(city_ratios), "\n")
cat("CMAs matched:", length(unique(city_ratios$cma)), "\n\n")


# ── 5. Print results ────────────────────────────────────────────────────────
cat("══ City / Province Employment Ratios (2024) ══\n\n")

for (city in sort(unique(city_ratios$cma))) {
  cat(sprintf("── %s ──\n", city))
  city_ratios %>%
    filter(cma == city) %>%
    mutate(display = sprintf("  %-55s %6.1f / %7.1f = %5.1f%%",
                             industry, emp_city, emp_prov, city_share * 100)) %>%
    pull(display) %>%
    cat(sep = "\n")
  cat("\n\n")
}

# Overall share per CMA
cat("══ Overall Shares ══\n")
city_ratios %>%
  group_by(cma, province) %>%
  summarise(total_city = sum(emp_city, na.rm = TRUE),
            total_prov = sum(emp_prov, na.rm = TRUE),
            overall = total_city / total_prov,
            .groups = "drop") %>%
  arrange(desc(overall)) %>%
  mutate(display = sprintf("  %-45s (%s): %5.1f%%", cma, province, overall * 100)) %>%
  pull(display) %>%
  cat(sep = "\n")


# ── 6. Check for unmatched CMAs ─────────────────────────────────────────────
all_cmas <- unique(cma_emp$cma)
matched  <- unique(city_ratios$cma)
unmatched <- setdiff(all_cmas, matched)
if (length(unmatched) > 0) {
  cat("\n\nWARNING — unmatched CMAs (province name mismatch?):\n")
  print(unmatched)
}



# ── 7. Save lookup tables ───────────────────────────────────────────────────

# Industry-specific shares
write.csv(
  city_ratios %>% select(cma, province, industry, emp_city, emp_prov, city_share),
  "updating_data/city_industry_shares.csv", row.names = FALSE
)

# Overall shares
city_overall <- city_ratios %>%
  group_by(cma, province) %>%
  summarise(total_city = sum(emp_city, na.rm = TRUE),
            total_prov = sum(emp_prov, na.rm = TRUE),
            overall_share = total_city / total_prov,
            .groups = "drop")

write.csv(city_overall, "updating_data/city_overall_shares.csv", row.names = FALSE)

