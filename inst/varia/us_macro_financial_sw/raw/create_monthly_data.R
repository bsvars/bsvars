# Run from the package root; FRED is downloaded online.
# Rscript inst/varia/us_macro_financial_sw/raw/create_monthly_data.R
# Online revisions may differ from the bundled frozen input.
# Based on donotdespair/bsvar_hmsh, BPSSdata/create_monthly_data.R (9848512).
PATH <- "inst/varia/us_macro_financial_sw/"

library(tidyverse)
library(zoo)

# Function to convert date to first day of the month
to_first_day_of_month <- function(date_col) {
  as.Date(format(date_col, "%Y-%m-01"))
}

# Load packages
library(tidyverse)
library(fredr)

# Config
FRED_API_KEY <- Sys.getenv("FRED_API_KEY")
fredr_set_key(FRED_API_KEY)

start_date <- as.Date("1965-01-01")
end_date   <- Sys.Date()

series_vec <- c(
  "AAA", "BAA", "FEDFUNDS", "GS10", "INDPRO",
  "M1SL","M2SL", "MED3", "PCEPI", "TB3MS","BUSLOANS","CONSUMER","REALLN",
  "CP3M","CPF3M",
  "DTBTM", "DTCTHFM", "TOTALSL"
)

# Helper: fetch one series from FRED
fetch_series <- function(id) {
  fredr(
    series_id = id,
    observation_start = start_date,
    observation_end   = end_date,
    frequency = "m"
  ) |>
    transmute(date = as.Date(date), value, series = id)
}

# Download all & reshape to wide
raw_long <- purrr::map_dfr(series_vec, fetch_series)

raw_wide <- raw_long |>
  pivot_wider(names_from = series, values_from = value)

# Splice CP3M and CPF3M with additive adjustment over the overlap period.
# Anchor CPF3M and back-adjust CP3M so the two series align on average.
overlap_start <- as.Date("1997-01-01")
overlap_end <- as.Date("1997-08-01")
cp_splice_adj <- raw_wide |>
  filter(date >= overlap_start & date <= overlap_end) |>
  summarise(adj = mean(CPF3M - CP3M, na.rm = TRUE)) |>
  pull(adj)

# Create spread variables
raw_wide <- raw_wide |>
  mutate(
    GS10_TB3MS = GS10 - TB3MS,
    MED3_TB3MS = MED3 - TB3MS,
    GZ_SPR = BAA - AAA
  )

# Construct CP_Spliced funding series from FRED inputs
raw_wide <- raw_wide |>
  mutate(
    CP3M_adj = CP3M + cp_splice_adj,
    CP_Spliced = case_when(
      date >= as.Date("1971-04-01") & date <= as.Date("1997-08-01") ~ CP3M_adj,
      date >= as.Date("1997-09-01") ~ CPF3M,
      TRUE ~ NA_real_
    ),
    # CP spread over matched-maturity risk-free rate (3M T-bill)
    CP_Spliced_TB3MS = CP_Spliced - TB3MS
  )

# Corrections
corrections = c(
  30.9, # c&i loans
  5.9 + 21.5 + 1.2, # real estate loans
  322.3 + 41.3, # consumer loans
  9.7); # other misc. items
corrections[4] = sum(corrections[1:3]) + corrections[4]; # total loans correction

start_date_1 <- as.Date("2010-03-01")
end_date_1 <- as.Date("2016-05-01")

raw_wide[raw_wide$date >= start_date_1 & raw_wide$date <= end_date_1, c("BUSLOANS", "REALLN", "CONSUMER")] = 
  t(t(raw_wide[raw_wide$date >= start_date_1 & raw_wide$date <= end_date_1, c("BUSLOANS", "REALLN", "CONSUMER")]) - corrections[1:3])


## Corrections for October 2006 revision in data
start_date_2 <- as.Date("2006-10-01")
end_date_2 <- max(raw_wide$date)
corrections_2 = c(
  3.1, # c&i loans
  17.5 + 113.1+14, # real estate
  3.1, # consumer
  0); # other misc. items
corrections_2[4] = sum(corrections_2[1:3]) + corrections_2[4]; # total loans correction

raw_wide[raw_wide$date >= start_date_2 & raw_wide$date <= end_date_2, c("BUSLOANS", "REALLN", "CONSUMER")] = 
  t(t(raw_wide[raw_wide$date >= start_date_2 & raw_wide$date <= end_date_2, c("BUSLOANS", "REALLN", "CONSUMER")]) - corrections_2[1:3])


## Corrections for March 2007 conversion to thrift
start_date_3 <- as.Date("2007-03-01")
end_date_3 <- max(raw_wide$date)
corrections_3 = c(
  0, # c&i loans
  -(14.2 + 66.4), # real estate
  0, # consumer
  0); # other misc. items
corrections_3[4] = sum(corrections_3[1:3]) + corrections_3[4]; # total loans correction

raw_wide[raw_wide$date >= start_date_3 & raw_wide$date <= end_date_3, c("BUSLOANS", "REALLN", "CONSUMER")] = 
  t(t(raw_wide[raw_wide$date >= start_date_3 & raw_wide$date <= end_date_3, c("BUSLOANS", "REALLN", "CONSUMER")]) - corrections_3[1:3])


## Corrections for nonbank asset acquisition Oct 2008
start_date_4 <- as.Date("2008-10-01")
end_date_4 <- max(raw_wide$date)
corrections_4 = c(
  1.6, # c&i loans
  40 + 106 + 46.6, # real estate
  15.5, # consumer
  0); # other misc. items
corrections_4[4] = sum(corrections_4[1:3]) + corrections_4[4]; # total loans correction

raw_wide[raw_wide$date >= start_date_4 & raw_wide$date <= end_date_4, c("BUSLOANS", "REALLN", "CONSUMER")] = 
  t(t(raw_wide[raw_wide$date >= start_date_4 & raw_wide$date <= end_date_4, c("BUSLOANS", "REALLN", "CONSUMER")]) - corrections_4[1:3])


## Corrections for nonbank asset acquisition Nov 2009
start_date_5 <- as.Date("2009-11-01")
end_date_5 <- max(raw_wide$date)
corrections_5 = c(
  1.8, # c&i loans
  6.4 + 76.3 + 6.3, ## RE
  0, # consumer
  0); # other misc. items
corrections_5[4] = sum(corrections_5[1:3]) + corrections_5[4]; # total loans correction

raw_wide[raw_wide$date >= start_date_5 & raw_wide$date <= end_date_5, c("BUSLOANS", "REALLN", "CONSUMER")] = 
  t(t(raw_wide[raw_wide$date >= start_date_5 & raw_wide$date <= end_date_5, c("BUSLOANS", "REALLN", "CONSUMER")]) - corrections_5[1:3])

# Create HH variable
raw_wide <- raw_wide |>
  mutate(HH = REALLN + CONSUMER)

final_data <- raw_wide |>
  select(
    DATE = date,
    INDPRO,
    PCEPI,
    HH,
    BUSLOANS,
    M1SL,
    M2SL,
    FEDFUNDS,
    GS10_TB3MS,
    MED3_TB3MS,
    GZ_SPR,
    CP_Spliced,
    CP_Spliced_TB3MS
  )

# Read the _BCOMD month-end observations from the supplied GFD export.
commodity_lines <- readLines(paste0(PATH, "raw/CommPI.csv"))
bcomd_data <- read.csv(
  text = grep("^[0-9]{4}-[0-9]{2}-[0-9]{2},_BCOMD,", commodity_lines, value = TRUE),
  header = FALSE, col.names = c("Date", "Ticker", "BCOMD")
) |>
  transmute(Date = to_first_day_of_month(as.Date(Date)), BCOMD)

# Merge BCOMD
final_data <- left_join(final_data, bcomd_data, by = c("DATE" = "Date"))

final_data <- final_data |>
  mutate(
    FEDFUNDS = FEDFUNDS / 100,
    GS10_TB3MS = GS10_TB3MS / 100,
    MED3_TB3MS = MED3_TB3MS / 100,
    GZ_SPR = GZ_SPR / 100,
    CP_Spliced = CP_Spliced / 100,
    CP_Spliced_TB3MS = CP_Spliced_TB3MS / 100
  )

# Interpolate missing values
final_data <- final_data |>
  mutate(
    CP_Spliced = zoo::na.approx(CP_Spliced, na.rm = FALSE, rule = 1),
    MED3_TB3MS = zoo::na.approx(MED3_TB3MS, na.rm = FALSE, rule = 1),
    CP_Spliced_TB3MS = zoo::na.approx(CP_Spliced_TB3MS, na.rm = FALSE, rule = 1)
  )

# Ensure all desired variables are in final_data after all merges
final_data <- final_data[, c(
  "DATE",
  "INDPRO",
  "PCEPI",
  "HH",
  "BUSLOANS",
  "M1SL",
  "M2SL",
  "FEDFUNDS",
  "GS10_TB3MS",
  "MED3_TB3MS",
  "GZ_SPR",
  "BCOMD",
  "CP_Spliced",
  "CP_Spliced_TB3MS"
)]

# Save the new data
# Re-running this script updates bpss_monthly_10_vars.rda.
save(final_data, file = paste0(PATH, "bpss_monthly_10_vars.rda"))

# Print the data for April 2020 to confirm the fix
print("Data for April 2020 after interpolation:")
print(final_data[final_data$DATE == as.Date("2020-04-01"), ])
