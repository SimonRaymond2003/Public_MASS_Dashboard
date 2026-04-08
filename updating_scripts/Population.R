# population_data.R
# Fetches provincial population estimates from Statistics Canada table 17-10-0005-01
# and saves the result to data/1710000501_databaseLoadingData.csv

library(cansim)
library(data.table)

# Provinces and territories used in the dashboard
PROVINCES <- c(
  "Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia",
  "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan",
  "Alberta", "British Columbia", "Yukon", "Northwest Territories", "Nunavut"
)

message("Fetching Statistics Canada table 17-10-0005-01 ...")
raw <- get_cansim("17-10-0005-01")

# Keep only the rows the dashboard needs:
#   - total population (all genders, all ages, national estimates)
pop <- raw[
  raw$GEO %in% PROVINCES &
  raw$Gender == "Total - gender" &
  raw$`Age group` == "All ages",
]

# Select and rename columns to match the original CSV format
keep_cols <- c(
  "REF_DATE", "GEO", "DGUID", "Gender", "Age group",
  "UOM", "UOM_ID", "SCALAR_FACTOR", "SCALAR_ID",
  "VECTOR", "COORDINATE", "VALUE", "STATUS", "SYMBOL",
  "TERMINATED", "DECIMALS"
)

# Some column names may differ slightly across cansim versions — use what's available
keep_cols <- keep_cols[keep_cols %in% names(pop)]
pop <- pop[, keep_cols]

# Sort to match original file order
pop <- pop[order(pop$GEO, pop$REF_DATE), ]

out_path <- file.path("updating_data/", "1710000501_databaseLoadingData.csv")
fwrite(pop, out_path)

message(
  "Done. Saved ", nrow(pop), " rows covering ",
  min(pop$REF_DATE), "\u2013", max(pop$REF_DATE),
  " to ", out_path
)
