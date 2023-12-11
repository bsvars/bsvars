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

PATH = "inst/varia/"

# population data
############################################################
log_mean    = function(x) {log(mean(x))}

pop_tmp     = fredr::fredr("CNP16OV")
pop         = xts::apply.quarterly(xts::xts(pop_tmp$value, pop_tmp$date), log_mean)
pop         = xts::xts(pop, zoo::as.yearqtr(zoo::index(pop)))

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

# GS data
# government spending is Federal Government Consumption Expenditures and Gross Investment in line 6 from Table 3.9.5
############################################################
gs_read     = read.csv(paste0(PATH, "Table3.9.5.csv"), header = FALSE)
gs_value    = log(as.numeric(t(gs_read[14, 3:ncol(gs_read)])))
gs_date     = zoo::as.yearqtr(paste(t(gs_read[4, 3:ncol(gs_read)]), t(gs_read[5, 3:ncol(gs_read)])))
gs          = xts::xts(gs_value, gs_date)

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

# data matrix
############################################################
us_fiscal_lsuw_tmp    = na.omit(
                          cbind(
                            ttr - pop - pi,
                            gs - pop - pi,
                            gdp - pop - pi
                          )
                        )
colnames(us_fiscal_lsuw_tmp)  = c("ttr", "gs", "gdp")
us_fiscal_lsuw        = ts(as.matrix(us_fiscal_lsuw_tmp), start = c(1948, 1), frequency = 4)
save(us_fiscal_lsuw, file = "data/us_fiscal_lsuw.rda")


# Exogenous terms
############################################################
T             = nrow(us_fiscal_lsuw)
cubic_trend   = cbind(
                  1:T - mean(1:T), 
                  (1:T - mean(1:T))^2
                )
colnames(cubic_trend) = c("linear", "quadratic")
cubic_trend   = xts::xts(cubic_trend, zoo::as.yearqtr(zoo::index(us_fiscal_lsuw)))
dummy_1975Q2  = xts::xts(rep(0, T), zoo::as.yearqtr(zoo::index(us_fiscal_lsuw)))
dummy_1975Q2["1975-04/1975-06"] = 1
us_fiscal_ex_tmp    = cbind(cubic_trend, dummy_1975Q2)
us_fiscal_ex        = ts(as.matrix(us_fiscal_ex_tmp), start = c(1948, 1), frequency = 4)
save(us_fiscal_ex, file = "data/us_fiscal_ex.rda")
