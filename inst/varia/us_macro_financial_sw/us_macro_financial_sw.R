# Generate the frozen September 2025 SW dataset from the package root:
# Rscript inst/varia/us_macro_financial_sw/us_macro_financial_sw.R
# Author input: donotdespair/bsvar_hmsh, BPSSdata/data (0557165).
load("inst/varia/us_macro_financial_sw/bpss_monthly_10_vars.rda")
x <- final_data
levels <- data.frame(
  DATE = as.Date(x$DATE), ip = log(x$INDPRO), p = log(x$PCEPI),
  R = x$FEDFUNDS, hhc = log(x$HH), bc = log(x$BUSLOANS),
  GZ = x$GZ_SPR, ES = x$CP_Spliced_TB3MS, TS = x$GS10_TB3MS,
  m = log(x$M2SL), pcm = log(x$BCOMD)
)
levels <- levels[levels$DATE >= as.Date("1973-01-01") & complete.cases(levels), ]
values <- as.matrix(levels[-(1:12), -1])
for (v in c("ip", "p", "hhc", "bc", "m", "pcm")) {
  values[, v] <- diff(levels[[v]], lag = 12)
}
us_macro_financial_sw <- ts(
  values[levels$DATE[-(1:12)] <= as.Date("2025-09-01"), ],
  start = c(1974, 1), frequency = 12
)
save(us_macro_financial_sw, file = "data/us_macro_financial_sw.rda")
