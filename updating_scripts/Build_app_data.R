# ══════════════════════════════════════════════════════════════════════════════
# Pre-compute the joined org_data + impact-attached tables for fast app startup.
# Produces both nominal and real-dollar versions; global.R picks one at startup.
#
# Outputs:
#   updating_data/app_data_nominal.rds
#   updating_data/app_data_real.rds
#
# Each rds is a list with:
#   org_data, org_data_exp, org_data_rev,
#   org_data_single_exp, org_data_mixture_exp,
#   org_data_single_rev, org_data_mixture_rev,
#   combos_single, combos_mixture, combo_map_v2, mult_combined, MULT_COLS
# ══════════════════════════════════════════════════════════════════════════════

dir.create("updating_data", showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
})

build_one <- function(use_real_dollars) {
  label <- if (use_real_dollars) "REAL" else "NOMINAL"
  cat("\n──────────────────────────────────────────────\n")
  cat("Building app_data:", label, "\n")
  cat("──────────────────────────────────────────────\n")

  data_file <- if (use_real_dollars)
    "mass-culture-dashboard-data/df_flagged_value_na_real_dollars.rda" else
    "mass-culture-dashboard-data/df_flagged_value_na.rda"

  t0 <- Sys.time()

  loaded_obj <- load(data_file)
  mass_data <- get(loaded_obj)

  ident <- mass_data$charity_ident %>%
    select(`Business Number`, Year, `Legal Name`, City, Province,
           Latitude = LATITUDE, Longitude = LONGITUDE,
           adp_category, adp_sub_category) %>%
    rename(Category = adp_category, Discipline = adp_sub_category)
  rev_tbl <- mass_data$revenue_data %>%
    rename(`Total Government Revenue` = total_gov,
           `Government Funding Ratio` = gov_funding_ratio) %>%
    select(-`Fiscal Period End`)
  exp_tbl <- mass_data$expenditure_data
  staff_tbl <- mass_data$staff_counts_payroll %>% select(-`Fiscal Period End`)
  fp_tbl <- mass_data$fiscal_period %>% select(`Business Number`, Year, `Fiscal Period End`)

  org_data <- ident %>%
    left_join(rev_tbl,   by = c("Business Number", "Year")) %>%
    left_join(exp_tbl,   by = c("Business Number", "Year")) %>%
    left_join(staff_tbl, by = c("Business Number", "Year")) %>%
    left_join(fp_tbl,    by = c("Business Number", "Year"))

  cat(sprintf("  org_data: %d rows  (%.1fs)\n",
              nrow(org_data), as.numeric(Sys.time()-t0, units="secs")))

  # ── Multipliers ─────────────────────────────────────────────────────────────
  mult_dt <- fread("updating_data/provincial_multipliers_with_csa_splits.csv.gz")
  DISC_CODE <- fread("non-updating_data/discipline_multipliers.csv") %>%
               as.data.frame() %>% rename(disc_code = ind_primary)
  CAT_CODE  <- fread("non-updating_data/category_multipliers.csv") %>%
               as.data.frame() %>% rename(cat_code = ind_primary) %>%
               mutate(cat_code = na_if(trimws(as.character(cat_code)), ""))

  prov_lookup <- c(
    "Newfoundland and Labrador"="NL","Prince Edward Island"="PE",
    "Nova Scotia"="NS","New Brunswick"="NB","Quebec"="QC","Ontario"="ON",
    "Manitoba"="MB","Saskatchewan"="SK","Alberta"="AB","British Columbia"="BC",
    "Yukon"="YT","Northwest Territories"="NT","Nunavut"="NU")
  mult_dt[, Province := prov_lookup[geo]]
  mult_dt <- mult_dt[!is.na(Province)]

  all_prov <- mult_dt[coverage == "All provinces",
                      .(year, Province, industry_code,
                        gdp_direct, gdp_indirect, gdp_induced, gdp_total,
                        jobs_direct, jobs_indirect, jobs_induced, jobs_total,
                        source, domain_label)]
  wp <- mult_dt[coverage == "Within province",
                .(year, Province, industry_code,
                  gdp_wp_direct = gdp_direct, gdp_wp_indirect = gdp_indirect,
                  gdp_wp_induced = gdp_induced, gdp_wp_total = gdp_total,
                  jobs_wp_direct = jobs_direct, jobs_wp_indirect = jobs_indirect,
                  jobs_wp_induced = jobs_induced, jobs_wp_total = jobs_total)]
  mult_combined <- merge(all_prov, wp,
                         by = c("year","Province","industry_code"), all.x = TRUE)

  MULT_COLS <- c("gdp_direct","gdp_indirect","gdp_induced","gdp_total",
                 "gdp_wp_direct","gdp_wp_indirect","gdp_wp_induced","gdp_wp_total",
                 "jobs_direct","jobs_indirect","jobs_induced","jobs_total",
                 "jobs_wp_direct","jobs_wp_indirect","jobs_wp_induced","jobs_wp_total")
  for (col in MULT_COLS) set(mult_combined, which(is.na(mult_combined[[col]])), col, 0)
  mult_combined <- as.data.frame(mult_combined)

  # ── Combo map (Category × Discipline → industry codes) ──────────────────────
  combo_map_v2 <- expand.grid(Category   = CAT_CODE$Category,
                              Discipline = DISC_CODE$Discipline,
                              stringsAsFactors = FALSE) %>%
    left_join(CAT_CODE,  by = "Category") %>%
    left_join(DISC_CODE, by = "Discipline") %>%
    rowwise() %>%
    mutate(
      ind_primary = if (is.na(cat_code)) disc_code else cat_code,
      ind_all     = list(unique(na.omit(c(cat_code, disc_code))))
    ) %>% ungroup() %>%
    mutate(ind_all = ifelse(lengths(ind_all) == 0, list("BS71A000"), ind_all)) %>%
    select(Category, Discipline, ind_primary, ind_all)

  org_data <- org_data %>%
    left_join(combo_map_v2, by = c("Category","Discipline")) %>%
    mutate(ind_primary = replace_na(ind_primary, "BS71A000"),
           ind_all = ifelse(is.na(ind_all), list("BS71A000"), ind_all))

  # ── Multiplier-lookup helpers (mirror global.R verbatim) ────────────────────
  get_mult_row <- function(prov, yr, ind, mdf = mult_combined) {
    m <- mdf[mdf$Province == prov & mdf$year == yr & mdf$industry_code == ind, ]
    if (nrow(m) >= 1) return(m[1, ])
    m <- mdf[mdf$Province == prov & mdf$industry_code == ind & mdf$year <= yr, ]
    if (nrow(m) >= 1) return(m[which.max(m$year), ])
    m <- mdf[mdf$Province == prov & mdf$industry_code == ind, ]
    if (nrow(m) >= 1) return(m[which.max(m$year), ])
    m <- mdf[mdf$industry_code == ind, ]
    if (nrow(m) >= 1) {
      r <- as.data.frame(lapply(m[, MULT_COLS, drop=FALSE], mean, na.rm=TRUE))
      r$Province <- prov; r$year <- yr; r$industry_code <- ind
      r$source <- NA_character_; r$domain_label <- NA_character_; return(r)
    }
    m <- mdf[mdf$industry_code == "BS71A000", ]
    r <- as.data.frame(lapply(m[, MULT_COLS, drop=FALSE], mean, na.rm=TRUE))
    r$Province <- prov; r$year <- yr; r$industry_code <- "BS71A000"
    r$source <- NA_character_; r$domain_label <- NA_character_; r
  }
  get_mult_row_mixture <- function(prov, yr, ind_vec, mdf = mult_combined) {
    rows <- lapply(ind_vec, function(ind) get_mult_row(prov, yr, ind, mdf))
    df <- do.call(rbind, rows)
    r <- as.data.frame(lapply(df[, MULT_COLS, drop=FALSE], mean, na.rm=TRUE))
    r$Province <- prov; r$year <- yr; r$industry_code <- paste(ind_vec, collapse="+"); r
  }

  # ── Pre-compute combos_single / combos_mixture ──────────────────────────────
  t1 <- Sys.time()
  combos_scalar <- org_data %>% distinct(Province, Year, ind_primary)
  ind_all_lookup <- org_data %>% select(ind_primary, ind_all) %>%
    group_by(ind_primary) %>% slice(1) %>% ungroup()
  combos <- combos_scalar %>% left_join(ind_all_lookup, by = "ind_primary")

  combos_single <- combos %>% rowwise() %>%
    mutate(m = list(get_mult_row(Province, Year, ind_primary))) %>% ungroup()
  for (col in MULT_COLS)
    combos_single[[col]] <- sapply(combos_single$m,
      function(x) if (col %in% names(x)) x[[col]][1] else 0)
  combos_single$m <- NULL

  combos_mixture <- combos %>% rowwise() %>%
    mutate(m = list(get_mult_row_mixture(Province, Year, ind_all))) %>% ungroup()
  for (col in MULT_COLS)
    combos_mixture[[col]] <- sapply(combos_mixture$m,
      function(x) if (col %in% names(x)) x[[col]][1] else 0)
  combos_mixture$m <- NULL
  cat(sprintf("  combos pre-compute (%.1fs)\n",
              as.numeric(Sys.time()-t1, units="secs")))

  # ── Attach impacts to org rows ──────────────────────────────────────────────
  attach_impacts <- function(org_df, combos_df, base_col = "Total Expenditures") {
    lookup <- combos_df %>% select(-any_of("ind_all")) %>%
      distinct(Province, Year, ind_primary, .keep_all = TRUE)
    org_df %>%
      left_join(lookup, by = c("Province","Year","ind_primary")) %>%
      mutate(base_amt = .data[[base_col]],
             imp_gdp_direct = base_amt * gdp_direct,
             imp_gdp_indirect = base_amt * gdp_indirect,
             imp_gdp_induced = base_amt * gdp_induced,
             imp_gdp_total = base_amt * gdp_total,
             imp_gdp_wp_total = base_amt * gdp_wp_total,
             base_millions = base_amt / 1e6,
             imp_jobs_direct = base_millions * jobs_direct,
             imp_jobs_indirect = base_millions * jobs_indirect,
             imp_jobs_induced = base_millions * jobs_induced,
             imp_jobs_total = base_millions * jobs_total,
             imp_jobs_wp_total = base_millions * jobs_wp_total)
  }

  org_data_exp <- org_data %>% filter(!is.na(`Total Expenditures`), `Total Expenditures` > 0)
  org_data_rev <- org_data %>% filter(!is.na(`Total Revenue`), `Total Revenue` > 0)
  org_data_single_exp  <- attach_impacts(org_data_exp, combos_single, "Total Expenditures")
  org_data_mixture_exp <- attach_impacts(org_data_exp, combos_mixture, "Total Expenditures")
  org_data_single_rev  <- attach_impacts(org_data_rev, combos_single, "Total Revenue")
  org_data_mixture_rev <- attach_impacts(org_data_rev, combos_mixture, "Total Revenue")

  # ── Bundle and save ─────────────────────────────────────────────────────────
  bundle <- list(
    org_data = org_data,
    org_data_exp = org_data_exp,
    org_data_rev = org_data_rev,
    org_data_single_exp  = org_data_single_exp,
    org_data_mixture_exp = org_data_mixture_exp,
    org_data_single_rev  = org_data_single_rev,
    org_data_mixture_rev = org_data_mixture_rev,
    combos_single = combos_single,
    combos_mixture = combos_mixture,
    combo_map_v2 = combo_map_v2,
    mult_combined = mult_combined,
    MULT_COLS = MULT_COLS
  )
  out <- if (use_real_dollars)
    "updating_data/app_data_real.rds" else
    "updating_data/app_data_nominal.rds"
  saveRDS(bundle, out)
  cat(sprintf("  saved %s  (total %.1fs)\n",
              out, as.numeric(Sys.time()-t0, units="secs")))
}

build_one(use_real_dollars = FALSE)
build_one(use_real_dollars = TRUE)
cat("\nDone.\n")
