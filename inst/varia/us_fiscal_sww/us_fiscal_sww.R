
# This file generates data set used by 
# Shang, F., Wang, L., and Woźniak, T. (2026) 
# A Design Concept of Forecasting Software for Normalized Vector Autoregressions with Fat Tails and Stochastic Volatilitys

# Notes by Fei Shang
############################################################
# This is a dataset from Mertens, Ravn (2014,JME) paper. 
# The sources are given in footnote 3 of the paper:
# Output is GDP in line 1 from Table 1.1.5; government spending is Federal Government Consumption Expenditures and Gross Investment in line 6 from Table 3.9.5; Total tax revenue is Federal Current Tax Receipts in line 2 of Table 3.2 and Contributions for Government Social Insurance in line 11 of Table 3.2 less corporate income taxes from Federal Reserve Banks (line 8 in Table 3.2). All series are deflated by the GDP deflator in line 1 from Table 1.1.9 and by the civilian population ages 16+ obtained from Francis and Ramey (2009). The NIPA data was last revised July 29, 2011.
#
# The transformations are given in the main body of the text above the footnote on page S3 of the paper.
# Differences with respect to the data from Mertens, Ravn (2014,JME):
# + Mertens, Ravn (2014,JME) sample period is 1950Q1 to 2006Q4. Ours is 1950Q1 to 2021Q4.
# + The population variable is not from Francis & Ramey (2009, JMCB) but from the FRED (with the same definition)
# + The orirginal population data is monthly. It's transformed to quarterly by taking monthly averages.

# Notes by Tomasz
############################################################
# data taken from https://apps.bea.gov/
############################################################
# Gross Domestic Product
# Output is GDP in line 1 from Table 1.1.5 CHECKED
########
# Table 1.1.5. Gross Domestic Product
# [Billions of dollars] Seasonally adjusted at annual rates
# Last Revised on: October 26, 2023 - Next Release Date November 29, 2023
# Suggested citation: U.S. Bureau of Economic Analysis, "Table 1.1.5. Gross Domestic Product" (accessed Tuesday, November 7, 2023). 
############################################################
# Government Spendings
# government spending is Federal Government Consumption Expenditures and Gross Investment in line 6 from Table 3.9.5 (UPDATE line 9) CHECKED
########
# Table 3.9.5. Government Consumption Expenditures and Gross Investment
# [Billions of dollars] Seasonally adjusted at annual rates
# Last Revised on: October 26, 2023 - Next Release Date November 29, 2023
# Suggested citation: U.S. Bureau of Economic Analysis, "Table 3.9.5. Government Consumption Expenditures and Gross Investment" (accessed Tuesday, November 7, 2023). 
############################################################
# Total Tax Revenue
# Total tax revenue is: 
#   Federal Current Tax Receipts in line 2 of Table 3.2 and  CHECKED
#   Contributions for Government Social Insurance in line 11 of Table 3.2 less CHECKED
#   corporate income taxes from Federal Reserve Banks (line 8 in Table 3.2) CHECKED
########
# Table 3.2. Federal Government Current Receipts and Expenditures
# [Billions of dollars] Seasonally adjusted at annual rates
# Last Revised on: October 26, 2023 - Next Release Date November 29, 2023
# Suggested citation: U.S. Bureau of Economic Analysis, "Table 3.2. Federal Government Current Receipts and Expenditures" (accessed Tuesday, November 7, 2023). 
############################################################
# GDP Deflator
########
# Table 1.1.9. Implicit Price Deflators for Gross Domestic Product CHECKED
# [Index numbers, 2017=100] Seasonally adjusted
# Last Revised on: October 26, 2023 - Next Release Date November 29, 2023
# Suggested citation: U.S. Bureau of Economic Analysis, "Table 1.1.9. Implicit Price Deflators for Gross Domestic Product" (accessed Tuesday, November 7, 2023). 
############################################################

PATH = "inst/varia/us_fiscal_sww/"

# population data
############################################################
log_mean    = function(x) {log(mean(x))}

pop_tmp     = fredr::fredr("CNP16OV")
pop         = xts::to.quarterly(xts::xts(pop_tmp$value, pop_tmp$date), OHLC = FALSE)
pop         = log(pop)

# GDP Deflator data
# GDP deflator in line 1 from Table 1.1.9
############################################################
pi_read     = read.csv(paste0(PATH, "Table1.1.9.csv"), header = FALSE)
pi_value    = log(as.numeric(t(pi_read[6, 3:ncol(pi_read)])))
pi_date     = zoo::as.yearqtr(paste(t(pi_read[4, 3:ncol(pi_read)]), t(pi_read[5, 3:ncol(pi_read)])))
pi          = xts::xts(pi_value, pi_date)

# GDP data
# Output is GDP in line 1 from Table 1.1.5
############################################################
gdp_read    = read.csv(paste0(PATH, "Table1.1.5.csv"), header = FALSE)
gdp_value   = log(as.numeric(t(gdp_read[6, 3:ncol(gdp_read)])))
gdp_date    = zoo::as.yearqtr(paste(t(gdp_read[4, 3:ncol(gdp_read)]), t(gdp_read[5, 3:ncol(gdp_read)])))
gdp         = xts::xts(gdp_value, gdp_date)
gdp         = gdp - pop - pi
gdp         = 100 * na.omit(diff(gdp))

# GS data
# government spending is Federal Government Consumption Expenditures and Gross Investment in line 6 from Table 3.9.5
############################################################
gs_read     = read.csv(paste0(PATH, "Table3.9.5.csv"), header = FALSE)
gs_value    = log(as.numeric(t(gs_read[14, 3:ncol(gs_read)])))
gs_date     = zoo::as.yearqtr(paste(t(gs_read[4, 3:ncol(gs_read)]), t(gs_read[5, 3:ncol(gs_read)])))
gs          = xts::xts(gs_value, gs_date)
gs          = gs - pop - pi
gs          = 100 * na.omit(diff(gs))

# TTR data
############################################################
# Federal Current Tax Receipts in line 2 of Table 3.2 
# and Contributions for Government Social Insurance in line 11 (update 10) of Table 3.2 
# less corporate income taxes from Federal Reserve Banks (line 8 in Table 3.2)
ttr_read     = read.csv(paste0(PATH, "Table3.2.csv"), header = FALSE)

ttr_ctr      = as.numeric(t(ttr_read[7, 3:ncol(ttr_read)]))
ttr_cgsi     = as.numeric(t(ttr_read[15, 3:ncol(ttr_read)]))
ttr_cit      = as.numeric(t(ttr_read[13, 3:ncol(ttr_read)]))

ttr_value    = log(ttr_ctr + ttr_cgsi - ttr_cit)
ttr_date     = zoo::as.yearqtr(paste(t(ttr_read[4, 3:ncol(ttr_read)]), t(ttr_read[5, 3:ncol(ttr_read)])))
ttr          = xts::xts(ttr_value, ttr_date)
ttr          = ttr - pop - pi
ttr          = 100 * na.omit(diff(ttr))

# data matrix
############################################################
us_fiscal_sml    = na.omit(
  cbind(
    ttr,
    gs,
    gdp
  )
)
colnames(us_fiscal_sml) = c("ttr", "gs", "gdp")


############################################################
# 10-variable system by Montford, Uhlig (2009, JAE)
############################################################
# private consumption
# Output is in line 2 from Table 1.1.5
############################################################
cons_read    = read.csv(paste0(PATH, "Table1.1.5.csv"), header = FALSE)
cons_value   = log(as.numeric(t(cons_read[7, 3:ncol(cons_read)])))
cons_date    = zoo::as.yearqtr(paste(t(cons_read[4, 3:ncol(cons_read)]), t(cons_read[5, 3:ncol(cons_read)])))
cons         = xts::xts(cons_value, cons_date)
cons         = cons - pop - pi
cons         = 100 * na.omit(diff(cons))
colnames(cons) = "cons"

# real wages
# "COMPRNFB"
############################################################
rw_tmp     = fredr::fredr("COMPRNFB")
rw         = xts::xts(log(rw_tmp$value), zoo::as.yearqtr(rw_tmp$date))
rw         = 100 * na.omit(diff(rw))
colnames(rw) = "rw"

# private non-residential investment
# Output is in line 7 from Table 1.1.5 (Gross private domestic investment) minus 
# line 13 Table 1.1.5 (Residential)
############################################################
pnri_read    = read.csv(paste0(PATH, "Table1.1.5.csv"), header = FALSE)
gpdi_value   = as.numeric(t(pnri_read[12, 3:ncol(pnri_read)]))
ri_value     = as.numeric(t(pnri_read[18, 3:ncol(pnri_read)]))
inv_date     = zoo::as.yearqtr(paste(t(pnri_read[4, 3:ncol(pnri_read)]), t(pnri_read[5, 3:ncol(pnri_read)])))
inv          = xts::xts(log(gpdi_value - ri_value), inv_date)
inv          = inv - pop - pi
inv          = 100 * na.omit(diff(inv))
colnames(inv) = "inv"

# Federal Funds Effective Rate
# "FEDFUNDS"
############################################################
FFR_tmp     = fredr::fredr("FEDFUNDS")
# FFR         = xts::apply.quarterly(xts::xts(FFR_tmp$value, FFR_tmp$date), \(x)(if (dim(x)[1==3) x[3,]))
FFR         = xts::to.quarterly(xts::xts(FFR_tmp$value, FFR_tmp$date), OHLC = FALSE)
FFR         = xts::xts(FFR, zoo::as.yearqtr(zoo::index(FFR)))
colnames(FFR) = "FFR"

# Monetary Base M2SL
# "M2SL"
############################################################
M_tmp     = fredr::fredr("M2SL")
m         = xts::to.quarterly(xts::xts(M_tmp$value, M_tmp$date), OHLC = FALSE)
m         = 100 * na.omit(diff(log(m)))
colnames(m) = "m2"


# Producer Price Index by Commodity: Industrial Commodities
# "PPIIDC"
############################################################
ppiic_tmp = fredr::fredr("PPIIDC")
ppiic     = xts::to.quarterly(xts::xts(ppiic_tmp$value, ppiic_tmp$date), OHLC = FALSE)
ppiic     = 100 * na.omit(diff(log(ppiic)))
colnames(ppiic) = "ppiic"

pi        = 100 * na.omit(diff(log(pi)))
colnames(pi) = "pi"

us_fiscal_sww = 
  ts(
    na.omit(cbind(
      us_fiscal_sml,
      FFR,
      cons,
      rw,
      inv,
      m,
      ppiic,
      pi
    )),
    start = c(1959, 2),
    frequency = 4
  )

save(
  us_fiscal_sww,
  file = paste0("data/us_fiscal_sww.rda")
)
