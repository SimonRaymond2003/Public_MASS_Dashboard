library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(cansim)
library(data.table)

# =============================================================================
# DOMAIN-TO-PARENT MAPPING (unchanged — used for direct splits)
# =============================================================================

domain_map <- tribble(
  ~csa_domain,                                             ~indirect_parent_code,
  "Culture heritage",                                      "BS71A000",
  "Natural heritage",                                      "BS71A000",
  "Performing arts",                                       "BS71A000",
  "Festivals and celebrations",                            "BS71A000",
  "Original visual art",                                   "BS71A000",
  "Organized sport",                                       "BS71A000",
  "Multi domain",                                          "BS71A000",
  "Informal sport",                                        "BS713A00",
  "Film and video",                                        "BS5121A0",
  "Broadcasting",                                          "BS516100",
  "Interactive media",                                     "BS513200",
  "Music publishing",                                      "BS512200",
  "Sound recording ==> Sound recording",                   "BS512200",
  "Newspapers",                                            "BS513110",
  "Books",                                                 "BS5131A0",
  "Periodicals",                                           "BS5131A0",
  "Other published works",                                 "BS5131A0",
  "Multi sub-domain",                                      "BS5131A0",
  "Collected information",                                 "BS519000",
  "Archives",                                              "BS519000",
  "Libraries",                                             "BS519000",
  "Art reproductions",                                     "BS323000",
  "Photography",                                           "BS541400",
  "Design",                                                "BS541400",
  "Crafts",                                                "BS339900",
  "Advertising",                                           "BS541800",
  "Architecture",                                          "BS541300",
  "Education and training (culture)",                      "BS610000",
  "Education and training (sport)",                        "BS610000",
  "Governance, funding and professional support (culture)", "NP813A00",
  "Governance, funding and professional support (sport)",  "NP813A00"
)

# =============================================================================
# FULL SUB-DOMAIN → CONSTITUENT IOIC INDUSTRIES
# (from StatCan CSA methodology Table 1)
# Only BS-prefix industries — multipliers exist for these in the SUT
# NP/GS codes are excluded as they lack separate SUT multiplier rows
# =============================================================================

subdomain_ioic <- tribble(
  ~csa_domain,                                             ~ioic_code,
  # Heritage and libraries
  "Archives",                                              "BS519000",
  "Libraries",                                             "BS519000",
  "Culture heritage",                                      "BS71A000",
  "Natural heritage",                                      "BS71A000",
  # Live performance
  "Performing arts",                                       "BS71A000",
  "Festivals and celebrations",                            "BS71A000",
  # Visual and applied arts
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
  # Written and published works
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
  # Audio-visual and interactive media
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
  # Sound recording
  "Music publishing",                                      "BS334A00",
  "Music publishing",                                      "BS451000",
  "Music publishing",                                      "BS512200",
  "Sound recording ==> Sound recording",                   "BS414000",
  "Sound recording ==> Sound recording",                   "BS451000",
  "Sound recording ==> Sound recording",                   "BS512200",
  # Collected information (mapped same as Archives/Libraries)
  "Collected information",                                 "BS519000",
  # Education & training
  "Education and training (culture)",                      "BS610000",
  "Education and training (sport)",                        "BS610000",
  # Governance, funding and professional support
  "Governance, funding and professional support (culture)", "BS52A000",
  "Governance, funding and professional support (culture)", "BS71A000",
  "Governance, funding and professional support (culture)", "BS813000",
  "Governance, funding and professional support (sport)",  "BS71A000",
  "Governance, funding and professional support (sport)",  "BS813000",
  # Multi-Domain
  "Multi domain",                                          "BS334A00",
  "Multi domain",                                          "BS519000",
  "Multi domain",                                          "BS533000",
  "Multi domain",                                          "BS561A00",
  # Organized sport
  "Organized sport",                                       "BS71A000",
  "Organized sport",                                       "BS813000",
  # Informal sport — NP-only, no BS codes; fallback to parent
  "Informal sport",                                        "BS713A00"
)

cat("=== DOMAIN MAPPING ===\n")
cat("Sub-domains:", nrow(domain_map), "| Indirect parent industries:", length(unique(domain_map$indirect_parent_code)), "\n")
cat("Full IOIC mapping rows:", nrow(subdomain_ioic), "\n")
cat("Unique constituent IOIC codes:", length(unique(subdomain_ioic$ioic_code)), "\n")

# =============================================================================
# STEP 1: Load multiplier tables & build SUT output weights
# =============================================================================

# National multipliers
mult <- get_cansim("36-10-0594-01")

# Provincial multipliers
prov_mult <- get_cansim("36-10-0595-01")

# SUT output weights: use national GDP direct multipliers as a proxy for
# industry output weights when weighting indirect effects across sub-domains
extract_code <- function(x) str_extract(x, "\\[([A-Z0-9_]+)\\]", group = 1)

sut_industry_output <- mult %>%
  filter(REF_DATE == 2021,
         Variable == "Gross domestic product (GDP) at basic prices",
         `Multiplier type` == "Direct multiplier") %>%
  mutate(code = str_extract(`Classification Code for Industry`, "[A-Z0-9_]+"),
         output = VALUE) %>%
  select(geo = GEO, code, output)

# =============================================================================
# STEP 2: Load all multiplier tables (GDP + Jobs)
# =============================================================================

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

cat("\n=== Multiplier table sizes ===\n")
cat("nat_gdp:", nrow(nat_gdp), "| nat_jobs:", nrow(nat_jobs), "\n")
cat("prov_gdp:", nrow(prov_gdp), "| prov_jobs:", nrow(prov_jobs), "\n")

# =============================================================================
# STEP 2b: Build indirect multiplier lookup for ALL constituent industries
# =============================================================================

# National: indirect multipliers for every IOIC code (GDP + Jobs)
nat_indirect_gdp <- nat_gdp %>%
  select(year, code, indirect_gdp = indirect)

nat_indirect_jobs <- nat_jobs %>%
  select(year, code, indirect_jobs = indirect)

# Provincial: indirect multipliers for every IOIC code (GDP + Jobs)
prov_indirect_gdp <- prov_gdp %>%
  select(year, geo, coverage, code, indirect_gdp = indirect)

prov_indirect_jobs <- prov_jobs %>%
  select(year, geo, coverage, code, indirect_jobs = indirect)

cat("\n=== Indirect lookup sizes ===\n")
cat("nat_indirect_gdp:", nrow(nat_indirect_gdp), "| nat_indirect_jobs:", nrow(nat_indirect_jobs), "\n")
cat("prov_indirect_gdp:", nrow(prov_indirect_gdp), "| prov_indirect_jobs:", nrow(prov_indirect_jobs), "\n")

# =============================================================================
# STEP 2c: Compute output-weighted indirect multiplier per sub-domain
#
# For each sub-domain d, year y, (and geo g, coverage c for provincial):
#   indirect_d = Σ_i [ (output_i / Σ_i output_i) × indirect_i ]
# where i indexes the constituent IOIC industries of sub-domain d
#
# We use the SUT output data (national geo = "Canada") for national weights,
# and provincial SUT output for provincial weights.
# When SUT output is unavailable for a constituent industry, that industry
# gets zero weight (it contributes nothing to the sub-domain's output).
# =============================================================================

compute_weighted_indirect <- function(subdomain_ioic_df, output_df, indirect_gdp_df,
                                      indirect_jobs_df, geo_val = NULL,
                                      coverage_val = NULL, year_val) {
  # Get output for constituent industries
  if (is.null(geo_val)) {
    out <- output_df %>% filter(geo == "Canada")
  } else {
    out <- output_df %>% filter(geo == geo_val)
  }
  
  # Join constituent IOIC codes with their output
  mapping <- subdomain_ioic_df %>%
    left_join(out %>% select(code, output), by = c("ioic_code" = "code")) %>%
    mutate(output = replace_na(output, 0))
  
  # Join with indirect multipliers
  if (is.null(geo_val)) {
    gdp_ind <- indirect_gdp_df %>% filter(year == year_val)
    jobs_ind <- indirect_jobs_df %>% filter(year == year_val)
  } else {
    gdp_ind <- indirect_gdp_df %>%
      filter(year == year_val, geo == geo_val, coverage == coverage_val)
    jobs_ind <- indirect_jobs_df %>%
      filter(year == year_val, geo == geo_val, coverage == coverage_val)
  }
  
  mapping <- mapping %>%
    left_join(gdp_ind %>% select(code, indirect_gdp), by = c("ioic_code" = "code")) %>%
    left_join(jobs_ind %>% select(code, indirect_jobs), by = c("ioic_code" = "code"))
  
  # Compute weighted indirect per sub-domain
  result <- mapping %>%
    group_by(csa_domain) %>%
    summarise(
      n_ioic = n(),
      n_with_output = sum(output > 0),
      total_output = sum(output),
      wt_indirect_gdp = ifelse(sum(output) > 0,
                               sum(output * replace_na(indirect_gdp, 0)) / sum(output),
                               NA_real_),
      wt_indirect_jobs = ifelse(sum(output) > 0,
                                sum(output * replace_na(indirect_jobs, 0)) / sum(output),
                                NA_real_),
      .groups = "drop"
    )
  
  result
}

# =============================================================================
# STEP 3: Load CSA data — national + provincial (GDP, Output, Jobs)
# =============================================================================

csa <- get_cansim("36-10-0652-01")
ptci <- get_cansim("36-10-0452-01")

# --- National CSA (quarterly → annual) ---
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
    summarise(jobs = sum(VALUE, na.rm = TRUE), .groups = "drop")
  
  out %>%
    left_join(gdp, by = c("year", "Domain")) %>%
    left_join(jobs, by = c("year", "Domain")) %>%
    left_join(domain_map, by = c("Domain" = "csa_domain")) %>%
    mutate(gdp_output_ratio = ifelse(output > 0, gdp / output, NA_real_),
           jobs_output_ratio = ifelse(output > 0, jobs / output, NA_real_))
}

csa_nat_combined <- load_csa_nat(csa, domain_map$csa_domain)

# --- Provincial CSA (annual) ---
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
    summarise(jobs = sum(VALUE, na.rm = TRUE), .groups = "drop")
  
  out %>%
    left_join(gdp, by = c("year", "geo", "Domain")) %>%
    left_join(jobs, by = c("year", "geo", "Domain")) %>%
    left_join(domain_map, by = c("Domain" = "csa_domain")) %>%
    mutate(gdp_output_ratio = ifelse(output > 0, gdp / output, NA_real_),
           jobs_output_ratio = ifelse(output > 0, jobs / output, NA_real_))
}

ptci_combined <- load_csa_prov(ptci, domain_map$csa_domain)

cat("\n=== CSA data sizes ===\n")
cat("National:", nrow(csa_nat_combined), "| Provincial:", nrow(ptci_combined), "\n")

# =============================================================================
# HELPER: Split one group — with OUTPUT-WEIGHTED INDIRECT
# =============================================================================
# Changes from v1:
#   - gdp_indirect is now sub-domain-specific (weighted avg of constituent
#     IOIC industries' indirect multipliers, weighted by SUT output shares)
#   - jobs_indirect is similarly weighted
#   - gdp_induced and jobs_induced scale proportionally using the weighted
#     indirect (the induced effect reflects spending from indirect workers/GDP,
#     so if indirect differs, so does the induced component driven by it)
#
# v3: direct multiplier uses a SINGLE shared anchor (BS71A000) for all
#     sub-domains. The CSA GDP/output and Jobs/output ratios scale relative to
#     that one anchor, so directs are fully driven by CSA data rather than each
#     parent's own direct multiplier.
# Indirect/induced anchoring remains per-(indirect-)parent as before.
# =============================================================================

split_group <- function(csa_df, parent_gdp, parent_jobs,
                        wt_indirect_lookup,
                        anchor_direct_gdp, anchor_direct_jobs) {
  # csa_df has: Domain, output, gdp_output_ratio, jobs_output_ratio
  # wt_indirect_lookup has: csa_domain, wt_indirect_gdp, wt_indirect_jobs
  # anchor_direct_gdp / anchor_direct_jobs: BS71A000 direct multipliers (shared)
  csa_df <- csa_df %>% filter(!is.na(gdp_output_ratio), output > 0)
  if (nrow(csa_df) == 0) return(NULL)
  
  csa_df <- csa_df %>% mutate(output_wt = output / sum(output))
  
  # Join weighted indirect for each sub-domain
  csa_df <- csa_df %>%
    left_join(wt_indirect_lookup %>%
                select(csa_domain, wt_indirect_gdp, wt_indirect_jobs),
              by = c("Domain" = "csa_domain"))
  
  # --- GDP splits ---
  raw_gdp_avg <- sum(csa_df$output_wt * csa_df$gdp_output_ratio)
  if (raw_gdp_avg == 0 || is.na(raw_gdp_avg)) return(NULL)
  sf_gdp <- anchor_direct_gdp / raw_gdp_avg   # single shared anchor: BS71A000
  
  # Fallback: if weighted indirect is NA for a sub-domain, use parent indirect
  csa_df <- csa_df %>%
    mutate(
      gdp_indirect_raw = ifelse(!is.na(wt_indirect_gdp), wt_indirect_gdp, parent_gdp$indirect)
    )
  
  # Scale indirect so weighted average matches parent indirect exactly
  wt_avg_indirect_gdp <- sum(csa_df$output_wt * csa_df$gdp_indirect_raw)
  if (wt_avg_indirect_gdp > 0) {
    sf_indirect_gdp <- parent_gdp$indirect / wt_avg_indirect_gdp
  } else {
    sf_indirect_gdp <- 1
  }
  
  csa_df <- csa_df %>%
    mutate(
      gdp_direct   = gdp_output_ratio * sf_gdp,
      gdp_indirect = gdp_indirect_raw * sf_indirect_gdp,
      # Induced: proportional to (direct + indirect) relative to parent
      # This way sub-domains with higher direct+indirect get proportionally more induced
      gdp_di_sum   = gdp_direct + gdp_indirect
    )
  
  wt_avg_di_gdp <- sum(csa_df$output_wt * csa_df$gdp_di_sum)
  if (wt_avg_di_gdp > 0 && parent_gdp$induced > 0) {
    csa_df <- csa_df %>%
      mutate(
        gdp_induced = parent_gdp$induced * (gdp_di_sum / wt_avg_di_gdp),
        gdp_simple  = gdp_direct + gdp_indirect,
        gdp_total   = gdp_simple + gdp_induced
      )
  } else {
    csa_df <- csa_df %>%
      mutate(
        gdp_induced = 0,
        gdp_simple  = gdp_direct + gdp_indirect,
        gdp_total   = gdp_simple + gdp_induced
      )
  }
  
  # --- Jobs splits ---
  has_jobs <- !all(is.na(csa_df$jobs_output_ratio))
  
  if (has_jobs && !is.null(parent_jobs) && nrow(parent_jobs) > 0 && !is.na(parent_jobs$direct)) {
    csa_df <- csa_df %>%
      mutate(jobs_output_ratio = replace_na(jobs_output_ratio, 0))
    
    raw_jobs_avg <- sum(csa_df$output_wt * csa_df$jobs_output_ratio)
    
    if (raw_jobs_avg > 0) {
      sf_jobs <- anchor_direct_jobs / raw_jobs_avg   # single shared anchor: BS71A000
      
      # Weighted indirect for jobs
      csa_df <- csa_df %>%
        mutate(
          jobs_indirect_raw = ifelse(!is.na(wt_indirect_jobs), wt_indirect_jobs, parent_jobs$indirect)
        )
      
      wt_avg_indirect_jobs <- sum(csa_df$output_wt * csa_df$jobs_indirect_raw)
      if (wt_avg_indirect_jobs > 0) {
        sf_indirect_jobs <- parent_jobs$indirect / wt_avg_indirect_jobs
      } else {
        sf_indirect_jobs <- 1
      }
      
      csa_df <- csa_df %>%
        mutate(
          jobs_direct   = jobs_output_ratio * sf_jobs,
          jobs_indirect = jobs_indirect_raw * sf_indirect_jobs,
          jobs_di_sum   = jobs_direct + jobs_indirect
        )
      
      wt_avg_di_jobs <- sum(csa_df$output_wt * csa_df$jobs_di_sum)
      if (wt_avg_di_jobs > 0 && parent_jobs$induced > 0) {
        csa_df <- csa_df %>%
          mutate(
            jobs_induced = parent_jobs$induced * (jobs_di_sum / wt_avg_di_jobs),
            jobs_simple  = jobs_direct + jobs_indirect,
            jobs_total   = jobs_simple + jobs_induced
          )
      } else {
        csa_df <- csa_df %>%
          mutate(
            jobs_induced = 0,
            jobs_simple  = jobs_direct + jobs_indirect,
            jobs_total   = jobs_simple + jobs_induced
          )
      }
    } else {
      csa_df <- csa_df %>%
        mutate(jobs_direct = 0, jobs_indirect = parent_jobs$indirect,
               jobs_induced = 0, jobs_simple = parent_jobs$indirect,
               jobs_total = parent_jobs$indirect)
    }
  } else {
    csa_df <- csa_df %>%
      mutate(jobs_direct = NA_real_, jobs_indirect = NA_real_,
             jobs_induced = NA_real_, jobs_simple = NA_real_,
             jobs_total = NA_real_)
  }
  
  # Clean up temp cols
  csa_df %>% select(-any_of(c("gdp_indirect_raw", "jobs_indirect_raw",
                              "gdp_di_sum", "jobs_di_sum",
                              "wt_indirect_gdp", "wt_indirect_jobs")))
}

# =============================================================================
# STEP 4: NATIONAL SPLITS (GDP + Jobs) — with weighted indirect
# =============================================================================

parent_codes <- unique(domain_map$indirect_parent_code)
nat_years <- intersect(unique(nat_gdp$year), unique(csa_nat_combined$year))
cat("\n=== NATIONAL: overlapping years:", sort(nat_years), "===\n")

nat_splits <- lapply(nat_years, function(yr) {
  # Compute weighted indirect for this year (national)
  wt_ind <- compute_weighted_indirect(
    subdomain_ioic, sut_industry_output,
    nat_indirect_gdp, nat_indirect_jobs,
    geo_val = NULL, coverage_val = NULL, year_val = yr
  )

  # Single shared direct anchor: BS71A000
  anchor_gdp  <- nat_gdp  %>% filter(year == yr, code == "BS71A000")
  anchor_jobs <- nat_jobs %>% filter(year == yr, code == "BS71A000")
  anc_gdp_d  <- if (nrow(anchor_gdp)  > 0) anchor_gdp$direct[1]  else NA_real_
  anc_jobs_d <- if (nrow(anchor_jobs) > 0) anchor_jobs$direct[1] else NA_real_

  lapply(parent_codes, function(pc) {
    pg <- nat_gdp  %>% filter(year == yr, code == pc)
    pj <- nat_jobs %>% filter(year == yr, code == pc)
    if (nrow(pg) == 0) return(NULL)

    domains_in_group <- domain_map %>% filter(indirect_parent_code == pc) %>% pull(csa_domain)
    csa_sub <- csa_nat_combined %>% filter(year == yr, Domain %in% domains_in_group)

    result <- split_group(csa_sub, pg, pj, wt_ind, anc_gdp_d, anc_jobs_d)
    if (is.null(result)) return(NULL)
    result %>% mutate(year = yr, geo = "Canada", coverage = "National")
  }) %>% bind_rows()
}) %>% bind_rows()

cat("National split rows:", nrow(nat_splits), "\n")

# =============================================================================
# STEP 5: PROVINCIAL SPLITS (GDP + Jobs) — with weighted indirect
# =============================================================================

prov_provs <- setdiff(unique(as.character(prov_gdp$geo)),
                      "Canadian territorial enclaves abroad")
ptci_provs <- setdiff(unique(ptci_combined$geo), c("Canada", "Outside Canada"))
common_provs <- intersect(prov_provs, ptci_provs)
prov_years <- intersect(unique(prov_gdp$year), unique(ptci_combined$year))

cat("\n=== PROVINCIAL: years:", sort(prov_years), "===\n")
cat("Provinces:", length(common_provs), "\n")

prov_splits <- lapply(prov_years, function(yr) {
  lapply(common_provs, function(pv) {
    lapply(c("Within province", "All provinces"), function(cov) {
      # Compute weighted indirect for this province/year/coverage
      wt_ind <- compute_weighted_indirect(
        subdomain_ioic, sut_industry_output,
        prov_indirect_gdp, prov_indirect_jobs,
        geo_val = pv, coverage_val = cov, year_val = yr
      )

      # Single shared direct anchor: BS71A000
      anchor_gdp  <- prov_gdp  %>% filter(year == yr, geo == pv, coverage == cov, code == "BS71A000")
      anchor_jobs <- prov_jobs %>% filter(year == yr, geo == pv, coverage == cov, code == "BS71A000")
      anc_gdp_d  <- if (nrow(anchor_gdp)  > 0) anchor_gdp$direct[1]  else NA_real_
      anc_jobs_d <- if (nrow(anchor_jobs) > 0) anchor_jobs$direct[1] else NA_real_

      lapply(parent_codes, function(pc) {
        pg <- prov_gdp  %>% filter(year == yr, geo == pv, coverage == cov, code == pc)
        pj <- prov_jobs %>% filter(year == yr, geo == pv, coverage == cov, code == pc)
        if (nrow(pg) == 0) return(NULL)

        domains_in_group <- domain_map %>% filter(indirect_parent_code == pc) %>% pull(csa_domain)
        csa_sub <- ptci_combined %>% filter(year == yr, geo == pv, Domain %in% domains_in_group)

        result <- split_group(csa_sub, pg, pj, wt_ind, anc_gdp_d, anc_jobs_d)
        if (is.null(result)) return(NULL)
        result %>% mutate(year = yr, geo = pv, coverage = cov)
      }) %>% bind_rows()
    }) %>% bind_rows()
  }) %>% bind_rows()
}) %>% bind_rows()

cat("Provincial split rows:", nrow(prov_splits), "\n")

# =============================================================================
# STEP 6: Combine national + provincial splits
# =============================================================================

all_splits <- bind_rows(nat_splits, prov_splits) %>%
  select(year, geo, coverage, Domain, indirect_parent_code, output_wt,
         gdp_direct, gdp_indirect, gdp_induced, gdp_simple, gdp_total,
         jobs_direct, jobs_indirect, jobs_induced, jobs_simple, jobs_total) %>%
  arrange(year, geo, coverage, indirect_parent_code, Domain)

cat("\n=== ALL SPLITS:", nrow(all_splits), "rows ===\n")

# =============================================================================
# STEP 7: Validate weighted averages (GDP + Jobs, national 2021)
# =============================================================================

cat("\n=== WEIGHTED AVERAGE CHECK — NATIONAL 2021 ===\n")
check_nat <- all_splits %>%
  filter(year == "2021", geo == "Canada") %>%
  group_by(indirect_parent_code) %>%
  summarise(
    n = n(),
    wt_gdp_d  = round(sum(output_wt * gdp_direct, na.rm = TRUE), 4),
    wt_gdp_i  = round(sum(output_wt * gdp_indirect, na.rm = TRUE), 4),
    wt_gdp_t  = round(sum(output_wt * gdp_total, na.rm = TRUE), 4),
    wt_jobs_d = round(sum(output_wt * jobs_direct, na.rm = TRUE), 3),
    wt_jobs_i = round(sum(output_wt * jobs_indirect, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  left_join(nat_gdp %>% filter(year == "2021") %>%
              select(indirect_parent_code = code, p_gdp_d = direct,
                     p_gdp_i = indirect, p_gdp_t = total), by = "indirect_parent_code") %>%
  left_join(nat_jobs %>% filter(year == "2021") %>%
              select(indirect_parent_code = code, p_jobs_d = direct,
                     p_jobs_i = indirect), by = "indirect_parent_code") %>%
  mutate(d_gdp_d = round(wt_gdp_d - p_gdp_d, 4),
         d_gdp_i = round(wt_gdp_i - p_gdp_i, 4),
         d_gdp_t = round(wt_gdp_t - p_gdp_t, 4),
         d_jobs_d = round(wt_jobs_d - p_jobs_d, 3),
         d_jobs_i = round(wt_jobs_i - p_jobs_i, 3))

print(check_nat %>% select(indirect_parent_code, n,
                           wt_gdp_d, p_gdp_d, d_gdp_d,
                           wt_gdp_i, p_gdp_i, d_gdp_i,
                           wt_gdp_t, p_gdp_t, d_gdp_t,
                           wt_jobs_d, p_jobs_d, d_jobs_d,
                           wt_jobs_i, p_jobs_i, d_jobs_i),
      n = 20, width = Inf)

# Ontario 2021 within province
cat("\n=== WEIGHTED AVERAGE CHECK — ONTARIO 2021 WITHIN PROVINCE ===\n")
check_on <- all_splits %>%
  filter(year == "2021", geo == "Ontario", coverage == "Within province") %>%
  group_by(indirect_parent_code) %>%
  summarise(
    wt_gdp_d  = round(sum(output_wt * gdp_direct, na.rm = TRUE), 4),
    wt_gdp_i  = round(sum(output_wt * gdp_indirect, na.rm = TRUE), 4),
    wt_jobs_d = round(sum(output_wt * jobs_direct, na.rm = TRUE), 3),
    wt_jobs_i = round(sum(output_wt * jobs_indirect, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  left_join(prov_gdp %>% filter(year == "2021", geo == "Ontario",
                                coverage == "Within province") %>%
              select(indirect_parent_code = code, p_gdp_d = direct,
                     p_gdp_i = indirect), by = "indirect_parent_code") %>%
  left_join(prov_jobs %>% filter(year == "2021", geo == "Ontario",
                                 coverage == "Within province") %>%
              select(indirect_parent_code = code, p_jobs_d = direct,
                     p_jobs_i = indirect), by = "indirect_parent_code") %>%
  mutate(d_gdp_d = round(wt_gdp_d - p_gdp_d, 4),
         d_gdp_i = round(wt_gdp_i - p_gdp_i, 4),
         d_jobs_d = round(wt_jobs_d - p_jobs_d, 3),
         d_jobs_i = round(wt_jobs_i - p_jobs_i, 3))

print(check_on %>% select(indirect_parent_code,
                          wt_gdp_d, p_gdp_d, d_gdp_d,
                          wt_gdp_i, p_gdp_i, d_gdp_i,
                          wt_jobs_d, p_jobs_d, d_jobs_d,
                          wt_jobs_i, p_jobs_i, d_jobs_i),
      n = 20, width = Inf)

# =============================================================================
# STEP 8: Build combined df — provincial published + CSA splits
# =============================================================================

# Published provincial (GDP)
pub_gdp <- prov_gdp %>%
  rename(gdp_direct = direct, gdp_indirect = indirect, gdp_induced = induced,
         gdp_simple = simple, gdp_total = total) %>%
  rename(Industry = code)

# Published provincial (Jobs)
pub_jobs <- prov_jobs %>%
  rename(jobs_direct = direct, jobs_indirect = indirect, jobs_induced = induced,
         jobs_simple = simple, jobs_total = total) %>%
  rename(Industry = code)

# Merge GDP + Jobs for published
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

# CSA split rows (provincial only)
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

# Combined
combined <- bind_rows(prov_published, prov_new) %>%
  arrange(year, geo, coverage, desc(gdp_total))

cat("\n\n========================================================\n")
cat("  COMBINED: PROVINCIAL PUBLISHED + CSA SPLITS (GDP + Jobs)\n")
cat("  >>> V3: SINGLE-ANCHOR DIRECT + OUTPUT-WEIGHTED INDIRECT <<<\n")
cat("========================================================\n")
cat("Total rows:", nrow(combined), "\n")
cat("Published:", sum(combined$source == "StatCan published"), "\n")
cat("CSA splits:", sum(combined$source == "CSA split"), "\n")

cat("\n--- Sample: Ontario 2021 within province, top 20 ---\n")
combined %>%
  filter(year == "2021", geo == "Ontario", coverage == "Within province") %>%
  select(industry_code, domain_label, source, gdp_direct, gdp_indirect, gdp_total,
         jobs_direct, jobs_indirect, jobs_total) %>%
  head(20) %>%
  print(width = Inf)

# =============================================================================
# STEP 9: Output tables — show indirect variation
# =============================================================================

cat("\n\n=== ALL SUB-DOMAIN SPLITS — NATIONAL 2021 (v3: single-anchor direct + weighted indirect) ===\n")
all_splits %>%
  filter(year == "2021", geo == "Canada") %>%
  select(Domain, indirect_parent_code, output_wt,
         gdp_direct, gdp_indirect, gdp_induced, gdp_total,
         jobs_direct, jobs_indirect, jobs_induced, jobs_total) %>%
  arrange(desc(gdp_total)) %>%
  print(n = 35, width = Inf)

cat("\n=== ONTARIO 2021 WITHIN PROVINCE (v3: single-anchor direct + weighted indirect) ===\n")
all_splits %>%
  filter(year == "2021", geo == "Ontario", coverage == "Within province") %>%
  select(Domain, indirect_parent_code, output_wt,
         gdp_direct, gdp_indirect, gdp_induced, gdp_total,
         jobs_direct, jobs_indirect, jobs_induced, jobs_total) %>%
  arrange(desc(gdp_total)) %>%
  print(n = 35, width = Inf)

# =============================================================================
# STEP 10: Save to compressed CSV
# =============================================================================

fwrite(combined, "updating_data/provincial_multipliers_with_csa_splits.csv.gz")