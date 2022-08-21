
#' @title A 7-variable US monetary system for the period 1960 Q1 -- 2007 Q4
#'
#' @description A system used to identify the US monetary policy shock in various ways.
#'
#' @usage data(us_monetary_wd)
#' 
#' @format A matrix and a \code{ts} object with time series of 192 observations on 7 variables:
#' \describe{
#'   \item{gdp}{log of real gross domestic product, seasonally adjusted}
#'   \item{p}{log of gross domestic product: implicit price deflator, seasonally adjusted}
#'   \item{pcom}{log of nominal non-energy commodities price index: monthly data, value at the end of quarter}
#'   \item{FF}{effective federal funds rate}
#'   \item{nbr}{log of nonborrowed reserves of depository institutions, seasonally adjusted}
#'   \item{tr}{log of board of governors total reserves, adjusted for changes in reserve requirements, seasonally adjusted}
#'   \item{m1}{log of M1 money stock, seasonally adjusted}
#' }
#' Specific sources:
#' \tabular{rcrr}{
#'    Variable \tab Source \tab Index \cr
#'    \code{gdp} \tab BEA \tab GDPC1 \cr
#'    \code{p} \tab BEA \tab GDPDEF \cr
#'    \code{pcom} \tab BEA \tab INONFUEL \cr
#'    \code{FF} \tab FED \tab FEDFUNDS \cr
#'    \code{nbr} \tab FED \tab BOGNONBR \cr
#'    \code{tr} \tab FED \tab TRARR \cr
#'    \code{m1} \tab FED \tab M1SL \cr
#' }
#' Legend:
#' \describe{
#'   \item{BEA}{U.S. Department of Commerce: Bureau of Economic Analysis}
#'   \item{WB}{World Bank}
#'   \item{FED}{Board of Governors of the Federal Reserve System}
#' }
#' 
#' @references Woźniak, T., and Droumaguet, M. (2015) Assessing Monetary Policy Models: Bayesian Inference for Heteroskedastic Structural VARs, URL: \url{http://fbe.unimelb.edu.au/__data/assets/pdf_file/0010/1724932/2017TWozniakhoneyhoney.pdf}.
#' 
#' @source FRED Economic Database, Federal Reserve Bank of St. Louis, \url{https://fred.stlouisfed.org/}
"us_monetary_wd"