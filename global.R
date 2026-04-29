library(shiny)
library(bslib)
library(readxl)
library(data.table)
library(dplyr)
library(tidyr)
library(plotly)
library(scales)
library(stringr)
library(sf)
library(leaflet)

# ══════════════════════════════════════════════════════════════════════════════
# 1. LOAD DATA
# ══════════════════════════════════════════════════════════════════════════════

org_data <- read_excel("MASS_data/SIMON - 2025-10-24-MASS-Culture-Data-Summary-Real_Dollars.xlsx")
org_data$`Revenue Range` <- NULL
org_data$`Top Compensation Category` <- NULL
org_data$`Compensation Category Counts` <- NULL

mult_dt <- fread("updating_data/provincial_multipliers_with_csa_splits.csv.gz")
DISC_CODE <- fread("non-updating_data/discipline_multipliers.csv") %>%
             as.data.frame() %>% rename(disc_code = ind_primary)
CAT_CODE  <- fread("non-updating_data/category_multipliers.csv") %>%
             as.data.frame() %>% rename(cat_code = ind_primary) %>%
             mutate(cat_code = na_if(trimws(as.character(cat_code)), ""))
pop_raw  <- fread("updating_data/1710000501_databaseLoadingData.csv")
prov_sf <- st_read("non-updating_data/canada_provinces.geojson", quiet = TRUE)


# ══════════════════════════════════════════════════════════════════════════════
# 2. PREP MULTIPLIERS
# ══════════════════════════════════════════════════════════════════════════════

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

pop_data <- as.data.frame(pop_raw)
pop_data$Province <- prov_lookup[pop_data$GEO]
pop_data <- pop_data[!is.na(pop_data$Province),]
pop_data <- pop_data[, c("REF_DATE","Province","VALUE")]
names(pop_data) <- c("Year","Province","population")

# ══════════════════════════════════════════════════════════════════════════════
# 3. BUILD COMBO MAP FROM CATEGORY + DISCIPLINE CODES
#    Each Discipline has one code, each Category has one code (Arts Organization
#    has none — falls back to discipline). A combo's primary is the category
#    code (or discipline if cat is blank); mixture is the dedup union of both.
# ══════════════════════════════════════════════════════════════════════════════

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

# Full combo list — ALL possible Category x Discipline pairs
VALID_COMBOS_FULL <- combo_map_v2 %>% distinct(Category, Discipline)

# ══════════════════════════════════════════════════════════════════════════════
# 4. JOIN TO ORG DATA
# ══════════════════════════════════════════════════════════════════════════════

org_data <- org_data %>%
  left_join(combo_map_v2, by = c("Category","Discipline")) %>%
  mutate(ind_primary = replace_na(ind_primary, "BS71A000"),
         ind_all = ifelse(is.na(ind_all), list("BS71A000"), ind_all))

# ══════════════════════════════════════════════════════════════════════════════
# 5. MULTIPLIER LOOKUP HELPERS
# ══════════════════════════════════════════════════════════════════════════════

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

get_mult_row_weighted <- function(prov, yr, ind_vec, weights, mdf = mult_combined) {
  stopifnot(length(ind_vec) == length(weights))
  weights <- weights / sum(weights)
  rows <- lapply(ind_vec, function(ind) get_mult_row(prov, yr, ind, mdf))
  df <- do.call(rbind, rows)
  r <- as.data.frame(lapply(df[, MULT_COLS, drop = FALSE], function(col) sum(col * weights, na.rm = TRUE)))
  r$Province <- prov; r$year <- yr; r$industry_code <- paste(ind_vec, collapse = "+"); r
}

# ══════════════════════════════════════════════════════════════════════════════
# 6. PRE-COMPUTE MULTIPLIERS
# ══════════════════════════════════════════════════════════════════════════════

combos_scalar <- org_data %>% distinct(Province, Year, ind_primary)
ind_all_lookup <- org_data %>% select(ind_primary, ind_all) %>% group_by(ind_primary) %>% slice(1) %>% ungroup()
combos <- combos_scalar %>% left_join(ind_all_lookup, by = "ind_primary")

combos_single <- combos %>% rowwise() %>% mutate(m = list(get_mult_row(Province, Year, ind_primary))) %>% ungroup()
for (col in MULT_COLS) combos_single[[col]] <- sapply(combos_single$m, function(x) if(col %in% names(x)) x[[col]][1] else 0)
combos_single$m <- NULL

combos_mixture <- combos %>% rowwise() %>% mutate(m = list(get_mult_row_mixture(Province, Year, ind_all))) %>% ungroup()
for (col in MULT_COLS) combos_mixture[[col]] <- sapply(combos_mixture$m, function(x) if(col %in% names(x)) x[[col]][1] else 0)
combos_mixture$m <- NULL

attach_impacts <- function(org_df, combos_df, base_col = "Total Expenditures") {
  lookup <- combos_df %>% select(-any_of("ind_all")) %>% distinct(Province, Year, ind_primary, .keep_all = TRUE)
  org_df %>%
    left_join(lookup, by = c("Province","Year","ind_primary")) %>%
    mutate(base_amt = .data[[base_col]],
           imp_gdp_direct = base_amt * gdp_direct, imp_gdp_indirect = base_amt * gdp_indirect,
           imp_gdp_induced = base_amt * gdp_induced, imp_gdp_total = base_amt * gdp_total,
           imp_gdp_wp_total = base_amt * gdp_wp_total,
           base_millions = base_amt / 1e6,
           imp_jobs_direct = base_millions * jobs_direct, imp_jobs_indirect = base_millions * jobs_indirect,
           imp_jobs_induced = base_millions * jobs_induced, imp_jobs_total = base_millions * jobs_total,
           imp_jobs_wp_total = base_millions * jobs_wp_total)
}

org_data_exp <- org_data %>% filter(!is.na(`Total Expenditures`), `Total Expenditures` > 0)
org_data_rev <- org_data %>% filter(!is.na(`Total Revenue`), `Total Revenue` > 0)
org_data_single_exp  <- attach_impacts(org_data_exp, combos_single, "Total Expenditures")
org_data_mixture_exp <- attach_impacts(org_data_exp, combos_mixture, "Total Expenditures")
org_data_single_rev  <- attach_impacts(org_data_rev, combos_single, "Total Revenue")
org_data_mixture_rev <- attach_impacts(org_data_rev, combos_mixture, "Total Revenue")
org_data_single  <- org_data_single_exp
org_data_mixture <- org_data_mixture_exp

# ══════════════════════════════════════════════════════════════════════════════
# 7. MAP PREP
# ══════════════════════════════════════════════════════════════════════════════

geo_names <- names(prov_sf)
name_col  <- intersect(c("PRENAME","PRENAME_E","name","NAME","PREABBR"), geo_names)[1]
if (is.na(name_col)) { message("GeoJSON columns: ", paste(geo_names, collapse=", ")); name_col <- geo_names[1] }
prov_name_to_abbr <- c(
  "Newfoundland and Labrador"="NL","Newfoundland  & Labrador"="NL","Newfoundland & Labrador"="NL",
  "Prince Edward Island"="PE","Nova Scotia"="NS","New Brunswick"="NB",
  "Quebec"="QC","Québec"="QC","Ontario"="ON","Manitoba"="MB",
  "Saskatchewan"="SK","Alberta"="AB","British Columbia"="BC",
  "Yukon"="YT","Yukon Territory"="YT","Northwest Territories"="NT","Nunavut"="NU",
  "NL"="NL","PE"="PE","NS"="NS","NB"="NB","QC"="QC","ON"="ON","MB"="MB",
  "SK"="SK","AB"="AB","BC"="BC","YT"="YT","NT"="NT","NU"="NU")
prov_sf$prov_abbr <- prov_name_to_abbr[prov_sf[[name_col]]]

# ══════════════════════════════════════════════════════════════════════════════
# 8. CONSTANTS
# ══════════════════════════════════════════════════════════════════════════════

YEARS         <- sort(unique(org_data$Year))
MAX_YEAR      <- max(YEARS)
MAX_MULT_YEAR <- max(mult_combined$year, na.rm = TRUE)
PROVINCES     <- sort(unique(org_data$Province))

# For top filters / charts — only combos that actually exist in the data
CATEGORIES  <- sort(unique(org_data$Category))
DISCIPLINES <- sort(unique(org_data$Discipline))
VALID_COMBOS <- org_data %>% distinct(Category, Discipline)

# For calculator & forecaster — all combos from v4 mapping (even if 0 orgs in data)
CALC_CATEGORIES  <- sort(unique(VALID_COMBOS_FULL$Category))
CALC_DISCIPLINES <- sort(unique(VALID_COMBOS_FULL$Discipline))

# Years capped at MAX_MULT_YEAR for all selectors
YEARS_CAPPED <- YEARS[YEARS <= MAX_MULT_YEAR]
DEFAULT_YEAR    <- as.character(min(MAX_YEAR, MAX_MULT_YEAR))
MAX_YEAR_CAPPED <- min(MAX_YEAR, MAX_MULT_YEAR)

NAVY <- "#1B1464"; PINK <- "#FF3EC9"; GREEN <- "#2D6A4F"
CREAM <- "#FAF8F5"; WHITE <- "#FFFFFF"; GOLD <- "#D4A843"

# Build custom code list dynamically from data:
# CSA splits show their domain name; StatCan published codes show "CODE — Title".
# domain_label is populated upstream by updating_scripts/Multipliers.R for both
# row types; we fall back to the bare code if it's missing.
.code_label_lookup <- mult_combined %>%
  filter(!is.na(domain_label) & domain_label != "") %>%
  distinct(industry_code, domain_label)
.code_label_lookup <- setNames(.code_label_lookup$domain_label,
                               .code_label_lookup$industry_code)

.trunc_title <- function(s, n = 40) {
  ifelse(nchar(s) > n, paste0(substr(s, 1, n - 1), "…"), s)
}

.lookup_title <- function(code) {
  ttl <- unname(.code_label_lookup[code])
  if (length(ttl) == 0 || is.na(ttl) || ttl == "") NA_character_ else ttl
}

# Drop blank/NA codes (e.g. StatCan "Total industries" aggregate row).
# CSA splits use the "csa_" prefix; everything else is StatCan published.
.all_codes     <- unique(mult_combined$industry_code)
.all_codes     <- .all_codes[!is.na(.all_codes) & nzchar(.all_codes)]
.split_codes   <- sort(.all_codes[grepl("^csa_", .all_codes)])
.statcan_codes <- sort(.all_codes[!grepl("^csa_", .all_codes)])

.split_labels <- vapply(.split_codes, function(c) {
  ttl <- .lookup_title(c)
  if (is.na(ttl)) c else paste0("CSA: ", ttl)
}, character(1))

.statcan_labels <- vapply(.statcan_codes, function(c) {
  ttl <- .lookup_title(c)
  if (is.na(ttl)) c else paste0(c, " — ", .trunc_title(ttl))
}, character(1))

CUSTOM_IND_CODES <- setNames(
  c(.split_codes, .statcan_codes),
  c(.split_labels, .statcan_labels)
)

mult_toggle_ui <- function(id, label_single = "Primary Multiplier", label_mix = "Equal-Weight Mixture") {
  div(style = "display:flex; align-items:center; gap:8px; margin-bottom:10px;",
      tags$label(style = "font-size:0.75rem; font-weight:700; text-transform:uppercase; letter-spacing:0.4px; color:#777;", "Multiplier Method:"),
      tags$div(style = "display:flex; border-radius:20px; overflow:hidden; border:1px solid #ddd; font-size:0.75rem;",
               tags$button(id = paste0(id,"_single"), class = "mult-btn active-mult-btn",
                           style = sprintf("padding:5px 14px; border:none; cursor:pointer; font-weight:700; background:%s; color:%s; transition:all .2s;", NAVY, WHITE),
                           onclick = sprintf("Shiny.setInputValue('%s','single'); this.style.background='%s'; this.style.color='%s'; document.getElementById('%s_mix').style.background='#fff'; document.getElementById('%s_mix').style.color='#555';", id, NAVY, WHITE, id, id),
                           label_single),
               tags$button(id = paste0(id,"_mix"), class = "mult-btn",
                           style = "padding:5px 14px; border:none; cursor:pointer; font-weight:700; background:#fff; color:#555; transition:all .2s;",
                           onclick = sprintf("Shiny.setInputValue('%s','mixture'); this.style.background='%s'; this.style.color='%s'; document.getElementById('%s_single').style.background='#fff'; document.getElementById('%s_single').style.color='#555';", id, PINK, WHITE, id, id),
                           label_mix)))
}

# ══════════════════════════════════════════════════════════════════════════════
# 9. CENSUS DIVISION SPATIAL DATA
# ══════════════════════════════════════════════════════════════════════════════

cd_sf <- readRDS("non-updating_data/cd_sf.rds")
cd_sf <- st_transform(cd_sf, 4326)
# Attach 2021 Census population to each CD polygon for per-capita calculations
cd_pop <- fread("updating_data/cd_overall_shares.csv",
                colClasses = c(CDUID = "character"),
                select = c("CDUID", "pop_cd"))
cd_sf <- cd_sf %>% left_join(as.data.frame(cd_pop), by = "CDUID")

# Business → CD lookup (produced by updating_scripts/CD_business_lookup.R)
business_cd_lookup <- fread("updating_data/business_cd_lookup.csv",
                             colClasses = c(CDUID = "character"))

# Curated subset of Census Divisions shown in the city explorer tab.
# CDUIDs sourced from 2021 Census; display names match common usage.
# (Calgary = AB Div 6, Edmonton = AB Div 11, Winnipeg = MB Div 11,
#  Saskatoon = SK Div 11, Regina = SK Div 6)
CD_CHOICES <- c(
  "Toronto"         = "3520",
  "Montréal"        = "2466",
  "Vancouver"       = "5915",
  "Calgary"         = "4806",
  "Edmonton"        = "4811",
  "Ottawa"          = "3506",
  "Winnipeg"        = "4611",
  "Québec City"     = "2423",
  "Halifax"         = "1209",
  "Victoria"        = "5917"
)

CITY_COORDS <- list(
  "Toronto"     = list(lng = -79.38, lat = 43.70, zoom = 9),
  "Montréal"    = list(lng = -73.57, lat = 45.50, zoom = 9),
  "Vancouver"   = list(lng = -123.12, lat = 49.25, zoom = 9),
  "Calgary"     = list(lng = -114.07, lat = 51.05, zoom = 9),
  "Edmonton"    = list(lng = -113.49, lat = 53.55, zoom = 9),
  "Ottawa"      = list(lng = -75.70,  lat = 45.42, zoom = 9),
  "Winnipeg"    = list(lng = -97.14,  lat = 49.90, zoom = 9),
  "Québec City" = list(lng = -71.21,  lat = 46.81, zoom = 9),
  "Halifax"     = list(lng = -63.57,  lat = 44.65, zoom = 9),
  "Victoria"    = list(lng = -123.37, lat = 48.43, zoom = 9)
)

# ══════════════════════════════════════════════════════════════════════════════
# 10. CENSUS DIVISION EMPLOYMENT SHARES (2021 Census, for impact splitting)
# ══════════════════════════════════════════════════════════════════════════════

cd_ovr_shares <- fread("updating_data/cd_overall_shares.csv",
                        colClasses = c(CDUID = "character"))

get_city_share <- function(cduid, ind_code = NULL) {
  share <- cd_ovr_shares[CDUID == cduid, cd_share]
  if (length(share) > 0 && !is.na(share[1])) return(share[1])
  return(0)
}

# ── TERM DEFINITIONS (for hover tooltips) ────────────────────────────────────
TERMS <- list(
  "GDP" = "Gross Domestic Product — The total monetary value of all final goods and services.",
  "Direct GDP" = "GDP generated by the organization itself such as wages paid, goods and services purchased directly in its operations.",
  "Indirect GDP" = "GDP generated by the organization's suppliers and their suppliers, as spending ripples up the supply chain.",
  "Induced GDP" = "GDP generated when employees (of the organization and its suppliers) spend their wages on goods and services.",
  "Total GDP" = "Direct + Indirect + Induced GDP — the full economic footprint of the organization in the economy.",
  "Jobs" = "Headcount of jobs supported across all impact layers (direct, indirect, induced), regardless of full/part-time status.",
  "Direct Impact" = "Activity generated directly by the organization: wages, salaries, and operational spending.",
  "Indirect Impact" = "Activity generated in supplier industries as a result of the organization's purchasing.",
  "Induced Impact" = "Activity generated when employees spend their wages on other goods and services.",
  "Total Impact" = "Direct + Indirect + Induced economic impacts",
  "Multiplier" = "Ratio from the Input-Output model — multiply an organization's expenditures or revenues by this to estimate GDP or jobs generated.",
  "Within-Province" = "Impact retained inside the organization's home province, excluding economic activity that leaks to other provinces.",
  "Leakage" = "The share of impact that flows out to other provinces via out-of-province supply chains. Indirect and induced impacts alone drive leakage.",
  "I-O Model" = "Input-Output model, first developed by economist Wassily Leontief tracks how every industry buys from and sells to every other industry, then uses those links to estimate how much GDP and how many jobs a dollar of spending creates across the whole economy.",  
  "Census Division" = "A geographic region defined by Statistics Canada, roughly corresponding to counties or regional municipalities. Used here to assign organizations to local areas.",
  "Per Capita" = "Divided by population.",
  "Expenditures" = "Total spending by an organization, used as a base amount for impact calculations.",
  "Revenue" = "Total income received by an organization, used as a base for impact calculations.",
  "Employment Share" = "Proportion of a province's labour force located within the selected Census Division (2021 Census).",
  "Primary" = "Uses a single industry multiplier based on the organization's main activity code.",
  "Mixture" = "Weights multiple industry multipliers equally for organizations that span several sectors.",
  "Industry Code" = "StatCan I-O classification (e.g., BS71A000) identifying the industry whose multipliers apply.",
  "City Retention" = "Percentage of total GDP impact estimated to stay within the selected city's boundaries."
)

# Helper function to create a hoverable term with tooltip
term <- function(text, def_key = NULL) {
  key <- if (is.null(def_key)) text else def_key
  definition <- TERMS[[key]]
  if (is.null(definition)) {
    return(tags$span(text))
  }
  tags$span(class = "term-tip", `data-def` = definition, text)
}
