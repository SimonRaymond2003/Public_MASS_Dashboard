library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(cansim)
library(data.table)

# =============================================================================
# SUB-DOMAIN → CONSTITUENT IOIC INDUSTRIES
# (from StatCan CSA methodology Table 1)
# =============================================================================

subdomain_ioic <- tribble(
  ~csa_domain,                                             ~ioic_code,
  "Archives",                                              "BS519000",
  "Libraries",                                             "BS519000",
  "Culture heritage",                                      "BS71A000",
  "Natural heritage",                                      "BS71A000",
  "Performing arts",                                       "BS71A000",
  "Festivals and celebrations",                            "BS71A000",
  "Original visual art",                                   "BS453000",
  "Original visual art",                                   "BS71A000",
  "Art reproductions",                                     "BS442000",
  "Photography",                                           "BS519000",
  "Photography",                                           "BS541900",
  "Photography",                                           "BS812A00",
  "Crafts",                                                "BS323000",
  "Crafts",                                                "BS332A00",
  "Crafts",                                                "BS339900",
  "Crafts",                                                "BS414000",
  "Crafts",                                                "BS416000",
  "Crafts",                                                "BS418000",
  "Crafts",                                                "BS448000",
  "Crafts",                                                "BS453000",
  "Advertising",                                           "BS541800",
  "Architecture",                                          "BS541300",
  "Design",                                                "BS541400",
  "Design",                                                "BS541500",
  "Books",                                                 "BS5111A0",
  "Periodicals",                                           "BS5111A0",
  "Newspapers",                                            "BS511110",
  "Newspapers",                                            "BS519000",
  "Other published works",                                 "BS5111A0",
  "Multi sub-domain",                                      "BS323000",
  "Multi sub-domain",                                      "BS414000",
  "Multi sub-domain",                                      "BS419000",
  "Multi sub-domain",                                      "BS451000",
  "Multi sub-domain",                                      "BS5111A0",
  "Multi sub-domain",                                      "BS541900",
  "Film and video",                                        "BS334A00",
  "Film and video",                                        "BS414000",
  "Film and video",                                        "BS512130",
  "Film and video",                                        "BS5121A0",
  "Film and video",                                        "BS532A00",
  "Broadcasting",                                          "BS515100",
  "Broadcasting",                                          "BS515200",
  "Broadcasting",                                          "BS517000",
  "Interactive media",                                     "BS451000",
  "Interactive media",                                     "BS511200",
  "Music publishing",                                      "BS334A00",
  "Music publishing",                                      "BS451000",
  "Music publishing",                                      "BS512200",
  "Sound recording ==> Sound recording",                   "BS414000",
  "Sound recording ==> Sound recording",                   "BS451000",
  "Sound recording ==> Sound recording",                   "BS512200",
  "Collected information",                                 "BS519000",
  "Education and training (culture)",                      "BS610000",
  "Education and training (sport)",                        "BS610000",
  "Governance, funding and professional support (culture)", "BS52A000",
  "Governance, funding and professional support (culture)", "BS71A000",
  "Governance, funding and professional support (culture)", "BS813000",
  "Governance, funding and professional support (sport)",  "BS71A000",
  "Governance, funding and professional support (sport)",  "BS813000",
  "Multi domain",                                          "BS334A00",
  "Multi domain",                                          "BS519000",
  "Multi domain",                                          "BS533000",
  "Multi domain",                                          "BS561A00",
  "Organized sport",                                       "BS71A000",
  "Organized sport",                                       "BS813000",
  "Informal sport",                                        "BS713A00"
)

all_domains <- unique(subdomain_ioic$csa_domain)
cat("=== DOMAIN MAPPING ===\n")
cat("Sub-domains:", length(all_domains), "\n")
cat("Mapping rows:", nrow(subdomain_ioic), "\n")
cat("Unique IOIC codes:", length(unique(subdomain_ioic$ioic_code)), "\n")

# =============================================================================
# STEP 1: Load SUT output data (for industry size weights)
# =============================================================================

dat <- read_csv("SUT_data/SUT_C2021_D.csv", show_col_types = FALSE)
extract_code <- function(x) str_extract(x, "\\[([A-Z0-9_]+)\\]", group = 1)
is_producing <- function(x) grepl("\\[BS|\\[NP|\\[GS", x)

sut_output <- dat %>%
  filter(`Supply and use` == "Supply", Valuation == "Basic price",
         is_producing(Industry), extract_code(Product) != "TOTAL") %>%
  group_by(GEO, Industry) %>%
  summarise(output = sum(VALUE, na.rm = TRUE), .groups = "drop") %>%
  filter(output > 0) %>%
  mutate(code = extract_code(Industry)) %>%
  select(geo = GEO, code, output)

cat("\n=== SUT OUTPUT ===\n")
cat("Rows:", nrow(sut_output), "| Industries:", length(unique(sut_output$code)), "\n")

# =============================================================================
# STEP 2: Load multiplier tables (GDP + Jobs)
# =============================================================================

mult <- get_cansim("36-10-0594-01")
prov_mult <- get_cansim("36-10-0595-01")

extract_mult <- function(mult_df, var_filter) {
  mult_df %>%
    filter(Variable == var_filter) %>%
    select(year = REF_DATE, code = `Classification Code for Industry`,
           type = `Multiplier type`, VALUE) %>%
    filter(type %in% c("Direct multiplier", "Indirect multiplier",
                       "Induced multiplier", "Simple multiplier",
                       "Total multiplier")) %>%
    mutate(code = str_extract(code, "[A-Z0-9_]+"),
           year = as.character(year),
           type = case_when(
             type == "Direct multiplier"   ~ "direct",
             type == "Indirect multiplier" ~ "indirect",
             type == "Induced multiplier"  ~ "induced",
             type == "Simple multiplier"   ~ "simple",
             type == "Total multiplier"    ~ "total"
           )) %>%
    pivot_wider(names_from = type, values_from = VALUE)
}

extract_prov_mult <- function(mult_df, var_filter) {
  mult_df %>%
    filter(Variable == var_filter) %>%
    select(year = REF_DATE, geo = GEO, coverage = `Geographical coverage`,
           code = `Classification Code for Industry`,
           type = `Multiplier type`, VALUE) %>%
    filter(type %in% c("Direct multiplier", "Indirect multiplier",
                       "Induced multiplier", "Simple multiplier",
                       "Total multiplier")) %>%
    mutate(code = str_extract(code, "[A-Z0-9_]+"),
           geo = as.character(geo),
           year = as.character(year),
           type = case_when(
             type == "Direct multiplier"   ~ "direct",
             type == "Indirect multiplier" ~ "indirect",
             type == "Induced multiplier"  ~ "induced",
             type == "Simple multiplier"   ~ "simple",
             type == "Total multiplier"    ~ "total"
           )) %>%
    pivot_wider(names_from = type, values_from = VALUE)
}

nat_gdp  <- extract_mult(mult, "Gross domestic product (GDP) at basic prices")
nat_jobs <- extract_mult(mult, "Jobs")
prov_gdp  <- extract_prov_mult(prov_mult, "Gross domestic product (GDP) at basic prices")
prov_jobs <- extract_prov_mult(prov_mult, "Jobs")

cat("\n=== Multiplier tables ===\n")
cat("nat_gdp:", nrow(nat_gdp), "| nat_jobs:", nrow(nat_jobs), "\n")
cat("prov_gdp:", nrow(prov_gdp), "| prov_jobs:", nrow(prov_jobs), "\n")

# =============================================================================
# STEP 3: Compute SUT output-weighted baseline multipliers per sub-domain
#         (direct, indirect, induced — all from constituent IOIC industries)
#         Then note the induced/(direct+indirect) ratio
# =============================================================================

compute_baseline <- function(subdomain_ioic_df, output_df,
                             gdp_mult_df, jobs_mult_df,
                             geo_val = NULL, coverage_val = NULL,
                             year_val) {
  # SUT output for weighting
  if (is.null(geo_val)) {
    out <- output_df %>% filter(geo == "Canada")
  } else {
    out <- output_df %>% filter(geo == geo_val)
  }
  
  # Published multipliers
  if (is.null(geo_val)) {
    gm <- gdp_mult_df %>% filter(year == year_val)
    jm <- jobs_mult_df %>% filter(year == year_val)
  } else {
    gm <- gdp_mult_df %>% filter(year == year_val, geo == geo_val, coverage == coverage_val)
    jm <- jobs_mult_df %>% filter(year == year_val, geo == geo_val, coverage == coverage_val)
  }
  
  # Join: subdomain → constituent IOIC → SUT output + all multiplier components
  mapping <- subdomain_ioic_df %>%
    left_join(out %>% select(code, output), by = c("ioic_code" = "code")) %>%
    mutate(output = replace_na(output, 0)) %>%
    left_join(gm %>% select(code, gdp_dir = direct, gdp_ind = indirect, gdp_indu = induced),
              by = c("ioic_code" = "code")) %>%
    left_join(jm %>% select(code, jobs_dir = direct, jobs_ind = indirect, jobs_indu = induced),
              by = c("ioic_code" = "code"))
  
  # Output-weighted average of all 3 components per sub-domain
  result <- mapping %>%
    group_by(csa_domain) %>%
    summarise(
      n_ioic = n(),
      n_with_output = sum(output > 0),
      total_output = sum(output),
      # GDP baseline
      base_gdp_direct   = ifelse(sum(output) > 0, sum(output * replace_na(gdp_dir, 0)) / sum(output), NA_real_),
      base_gdp_indirect = ifelse(sum(output) > 0, sum(output * replace_na(gdp_ind, 0)) / sum(output), NA_real_),
      base_gdp_induced  = ifelse(sum(output) > 0, sum(output * replace_na(gdp_indu, 0)) / sum(output), NA_real_),
      # Jobs baseline
      base_jobs_direct   = ifelse(sum(output) > 0, sum(output * replace_na(jobs_dir, 0)) / sum(output), NA_real_),
      base_jobs_indirect = ifelse(sum(output) > 0, sum(output * replace_na(jobs_ind, 0)) / sum(output), NA_real_),
      base_jobs_induced  = ifelse(sum(output) > 0, sum(output * replace_na(jobs_indu, 0)) / sum(output), NA_real_),
      .groups = "drop"
    ) %>%
    # Step 2: note the induced/(direct+indirect) ratio
    mutate(
      gdp_induced_ratio  = ifelse((base_gdp_direct + base_gdp_indirect) > 0,
                                  base_gdp_induced / (base_gdp_direct + base_gdp_indirect), 0),
      jobs_induced_ratio = ifelse((base_jobs_direct + base_jobs_indirect) > 0,
                                  base_jobs_induced / (base_jobs_direct + base_jobs_indirect), 0)
    )
  
  result
}

# =============================================================================
# STEP 4: Load CSA data — national + provincial (GDP, Output, Jobs)
# =============================================================================

csa <- get_cansim("36-10-0652-01")
ptci <- get_cansim("36-10-0452-01")

load_csa_nat <- function(csa_df, domains) {
  csa_filt <- csa_df %>%
    filter(Domain %in% domains) %>%
    mutate(year = str_sub(as.character(REF_DATE), 1, 4),
           Domain = as.character(Domain))
  
  out <- csa_filt %>%
    filter(UOM == "Dollars", Indicators == "Output") %>%
    group_by(year, Domain) %>%
    summarise(output = sum(VALUE, na.rm = TRUE), .groups = "drop")
  
  gdp <- csa_filt %>%
    filter(UOM == "Dollars", Indicators == "Gross domestic product") %>%
    group_by(year, Domain) %>%
    summarise(gdp = sum(VALUE, na.rm = TRUE), .groups = "drop")
  
  jobs <- csa_filt %>%
    filter(Indicators == "Jobs") %>%
    group_by(year, Domain) %>%
    summarise(jobs = mean(VALUE, na.rm = TRUE), .groups = "drop")  # mean: jobs is a stock
  
  out %>%
    left_join(gdp, by = c("year", "Domain")) %>%
    left_join(jobs, by = c("year", "Domain")) %>%
    mutate(gdp_output_ratio = ifelse(output > 0, gdp / output, NA_real_),
           # output is in thousands; divide by 1e3 → millions; jobs per $1M
           jobs_output_ratio = ifelse(output > 0, jobs / (output / 1e3), NA_real_))
}

csa_nat <- load_csa_nat(csa, all_domains)

load_csa_prov <- function(ptci_df, domains) {
  ptci_filt <- ptci_df %>%
    filter(Domain %in% domains) %>%
    mutate(year = as.character(REF_DATE),
           geo = as.character(GEO),
           Domain = as.character(Domain))
  
  out <- ptci_filt %>%
    filter(Indicators == "Output") %>%
    group_by(year, geo, Domain) %>%
    summarise(output = sum(VALUE, na.rm = TRUE), .groups = "drop")
  
  gdp <- ptci_filt %>%
    filter(Indicators == "Gross domestic product") %>%
    group_by(year, geo, Domain) %>%
    summarise(gdp = sum(VALUE, na.rm = TRUE), .groups = "drop")
  
  jobs <- ptci_filt %>%
    filter(Indicators == "Jobs") %>%
    group_by(year, geo, Domain) %>%
    summarise(jobs = sum(VALUE, na.rm = TRUE), .groups = "drop")  # annual data, no mean needed
  
  out %>%
    left_join(gdp, by = c("year", "geo", "Domain")) %>%
    left_join(jobs, by = c("year", "geo", "Domain")) %>%
    mutate(gdp_output_ratio = ifelse(output > 0, gdp / output, NA_real_),
           # output is in thousands; divide by 1e3 → millions; jobs per $1M
           jobs_output_ratio = ifelse(output > 0, jobs / (output / 1e3), NA_real_))
}

csa_prov <- load_csa_prov(ptci, all_domains)

cat("\n=== CSA data ===\n")
cat("National:", nrow(csa_nat), "| Provincial:", nrow(csa_prov), "\n")

# =============================================================================
# STEP 5: Build sub-domain multipliers
#
#   1. SUT output-weighted baseline (direct, indirect, induced) from constituents
#   2. Note ratio: induced / (direct + indirect)
#   3. Replace direct with CSA GDP/output ratio (≈ direct multiplier)
#   4. Indirect stays from step 1
#   5. Induced = ratio from step 2 × new (direct + indirect)
# =============================================================================

build_splits <- function(csa_df, baseline) {
  # csa_df: Domain, output, gdp_output_ratio, jobs_output_ratio
  # baseline: from compute_baseline (all 3 components + ratios per domain)
  
  csa_df <- csa_df %>% filter(!is.na(gdp_output_ratio), output > 0)
  if (nrow(csa_df) == 0) return(NULL)
  
  csa_df <- csa_df %>%
    left_join(baseline, by = c("Domain" = "csa_domain")) %>%
    filter(!is.na(base_gdp_direct))
  
  if (nrow(csa_df) == 0) return(NULL)
  
  csa_df <- csa_df %>%
    mutate(
      # --- GDP ---
      # Step 3: CSA ratio replaces direct (it's GDP/output ≈ direct multiplier)
      gdp_direct  = gdp_output_ratio,
      # Step 4: indirect stays from baseline
      gdp_indirect = base_gdp_indirect,
      # Step 5: induced = ratio × new (direct + indirect)
      gdp_induced = gdp_induced_ratio * (gdp_direct + gdp_indirect),
      gdp_simple  = gdp_direct + gdp_indirect,
      gdp_total   = gdp_simple + gdp_induced,
      
      # --- Jobs ---
      # CSA jobs/output ratio is now in multiplier units (jobs per $1M)
      # mean quarterly jobs / (annual output in thousands / 1e3)
      # Same approach as GDP: CSA ratio replaces direct
      jobs_direct  = ifelse(!is.na(jobs_output_ratio) & jobs_output_ratio > 0,
                            jobs_output_ratio, base_jobs_direct),
      jobs_indirect = base_jobs_indirect,
      jobs_induced  = jobs_induced_ratio * (jobs_direct + jobs_indirect),
      jobs_simple   = jobs_direct + jobs_indirect,
      jobs_total    = jobs_simple + jobs_induced
    ) %>%
    select(Domain, output, gdp_output_ratio, jobs_output_ratio,
           gdp_direct, gdp_indirect, gdp_induced, gdp_simple, gdp_total,
           jobs_direct, jobs_indirect, jobs_induced, jobs_simple, jobs_total)
  
  csa_df
}

# =============================================================================
# STEP 6: NATIONAL SPLITS
# =============================================================================

nat_years <- intersect(unique(nat_gdp$year), unique(csa_nat$year))
cat("\n=== NATIONAL: years:", sort(nat_years), "===\n")

nat_splits <- lapply(nat_years, function(yr) {
  bl <- compute_baseline(subdomain_ioic, sut_output, nat_gdp, nat_jobs,
                         geo_val = NULL, coverage_val = NULL, year_val = yr)
  csa_sub <- csa_nat %>% filter(year == yr)
  result <- build_splits(csa_sub, bl)
  if (is.null(result)) return(NULL)
  result %>% mutate(year = yr, geo = "Canada", coverage = "National")
}) %>% bind_rows()

cat("National split rows:", nrow(nat_splits), "\n")

# =============================================================================
# STEP 7: PROVINCIAL SPLITS
# =============================================================================

prov_provs <- setdiff(unique(as.character(prov_gdp$geo)),
                      "Canadian territorial enclaves abroad")
ptci_provs <- setdiff(unique(csa_prov$geo), c("Canada", "Outside Canada"))
common_provs <- intersect(prov_provs, ptci_provs)
prov_years <- intersect(unique(prov_gdp$year), unique(csa_prov$year))

cat("\n=== PROVINCIAL: years:", sort(prov_years), "===\n")
cat("Provinces:", length(common_provs), "\n")

prov_splits <- lapply(prov_years, function(yr) {
  lapply(common_provs, function(pv) {
    lapply(c("Within province", "All provinces"), function(cov) {
      bl <- compute_baseline(subdomain_ioic, sut_output, prov_gdp, prov_jobs,
                             geo_val = pv, coverage_val = cov, year_val = yr)
      csa_sub <- csa_prov %>% filter(year == yr, geo == pv)
      result <- build_splits(csa_sub, bl)
      if (is.null(result)) return(NULL)
      result %>% mutate(year = yr, geo = pv, coverage = cov)
    }) %>% bind_rows()
  }) %>% bind_rows()
}) %>% bind_rows()

cat("Provincial split rows:", nrow(prov_splits), "\n")

# =============================================================================
# STEP 8: Combine
# =============================================================================

all_splits <- bind_rows(nat_splits, prov_splits) %>%
  arrange(year, geo, coverage, Domain)

cat("\n=== ALL SPLITS:", nrow(all_splits), "rows ===\n")

# =============================================================================
# STEP 9: Validation
# =============================================================================

cat("\n=== NATIONAL 2021 — SAMPLE SPLITS ===\n")
all_splits %>%
  filter(year == "2021", geo == "Canada") %>%
  select(Domain, gdp_direct, gdp_indirect, gdp_induced, gdp_total,
         jobs_direct, jobs_indirect, jobs_induced, jobs_total) %>%
  arrange(desc(gdp_total)) %>%
  print(n = 35, width = Inf)

cat("\n=== ONTARIO 2021 WITHIN PROVINCE ===\n")
all_splits %>%
  filter(year == "2021", geo == "Ontario", coverage == "Within province") %>%
  select(Domain, gdp_direct, gdp_indirect, gdp_induced, gdp_total,
         jobs_direct, jobs_indirect, jobs_induced, jobs_total) %>%
  arrange(desc(gdp_total)) %>%
  print(n = 35, width = Inf)

# Show baseline vs CSA-adjusted direct for comparison
cat("\n=== BASELINE vs CSA DIRECT — NATIONAL 2021 ===\n")
bl_nat_2021 <- compute_baseline(subdomain_ioic, sut_output, nat_gdp, nat_jobs,
                                geo_val = NULL, coverage_val = NULL, year_val = "2021")
comparison <- all_splits %>%
  filter(year == "2021", geo == "Canada") %>%
  select(Domain, csa_direct = gdp_direct) %>%
  left_join(bl_nat_2021 %>% select(Domain = csa_domain, sut_direct = base_gdp_direct,
                                   induced_ratio = gdp_induced_ratio),
            by = "Domain") %>%
  mutate(direct_change_pct = round((csa_direct / sut_direct - 1) * 100, 1)) %>%
  arrange(desc(abs(direct_change_pct)))

print(comparison, n = 35, width = Inf)

# =============================================================================
# STEP 10: Build combined df — published + CSA splits
# =============================================================================

pub_gdp <- prov_gdp %>%
  rename(gdp_direct = direct, gdp_indirect = indirect, gdp_induced = induced,
         gdp_simple = simple, gdp_total = total) %>%
  rename(Industry = code)

pub_jobs <- prov_jobs %>%
  rename(jobs_direct = direct, jobs_indirect = indirect, jobs_induced = induced,
         jobs_simple = simple, jobs_total = total) %>%
  rename(Industry = code)

prov_published <- pub_gdp %>%
  left_join(pub_jobs %>% select(year, geo, coverage, Industry,
                                jobs_direct, jobs_indirect, jobs_induced,
                                jobs_simple, jobs_total),
            by = c("year", "geo", "coverage", "Industry")) %>%
  mutate(source = "StatCan published",
         industry_code = Industry,
         domain_label = NA_character_) %>%
  select(year, geo, coverage, industry_code, domain_label,
         gdp_direct, gdp_indirect, gdp_induced, gdp_simple, gdp_total,
         jobs_direct, jobs_indirect, jobs_induced, jobs_simple, jobs_total,
         source)

prov_new <- all_splits %>%
  filter(geo != "Canada") %>%
  mutate(industry_code = paste0("csa_", gsub("[^A-Za-z]", "",
                                             substr(gsub(" ", "", Domain), 1, 10))),
         domain_label = Domain,
         source = "CSA split") %>%
  select(year, geo, coverage, industry_code, domain_label,
         gdp_direct, gdp_indirect, gdp_induced, gdp_simple, gdp_total,
         jobs_direct, jobs_indirect, jobs_induced, jobs_simple, jobs_total,
         source)

combined <- bind_rows(prov_published, prov_new) %>%
  arrange(year, geo, coverage, desc(gdp_total))

cat("\n========================================================\n")
cat("  COMBINED: PROVINCIAL PUBLISHED + CSA SPLITS (GDP + Jobs)\n")
cat("  >>> V4: SUT baseline + CSA direct + induced ratio <<<\n")
cat("========================================================\n")
cat("Total rows:", nrow(combined), "\n")
cat("Published:", sum(combined$source == "StatCan published"), "\n")
cat("CSA splits:", sum(combined$source == "CSA split"), "\n")

# =============================================================================
# STEP 11: Save
# =============================================================================

fwrite(combined, "updating_data/provincial_multipliers_with_csa_splits.csv.gz")
cat("\n=== SAVED ===\n")