#' @title A 10-variable US macro-financial system for the period 1974-01 -- 2025-09
#'
#' @description A system used to identify the US monetary policy shock
#' by Shang and Woźniak (2026).
#' Last data update was implemented on 2026-03-02.
#'
#' @usage data(us_macro_financial_sw)
#'
#' @format A matrix and a \code{ts} object with over six hundred monthly observations on
#' 10 variables:
#' \describe{
#'   \item{ip}{industrial production (INDPRO) expressed in 12-month log differences}
#'   \item{p}{PCE price index (PCEPI) expressed in 12-month log differences}
#'   \item{R}{Federal Funds Effective Rate}
#'   \item{hhc}{household credit (real estate and consumer loans) expressed in 12-month log differences}
#'   \item{bc}{business credit expressed in 12-month log differences}
#'   \item{GZ}{BAA minus AAA corporate bond yield spread}
#'   \item{ES}{spliced commercial paper rate minus 3-month Treasury bill rate}
#'   \item{TS}{10-year Treasury yield minus 3-month Treasury bill rate}
#'   \item{m}{M2 monetary aggregate (M2SL) expressed in 12-month log differences}
#'   \item{pcm}{GFD Bloomberg Commodity Index with extension expressed in 12-month log differences of month-end values}
#' }
#'
#' @references
#' Shang, F., and Woźniak, T. (2026) Identification Verification for Structural
#' Vector Autoregressions with Heterogeneous Markov Switching
#' Heteroskedasticity. Manuscript.
#'
#' @source
#' FRED Economic Database, Federal Reserve Bank of St. Louis,
#' \url{https://fred.stlouisfed.org/}.
#' Global Financial Data,
#' \url{https://globalfinancialdata.com/}.
#'
#' @examples
#' data(us_macro_financial_sw)   # upload the data
#' plot(us_macro_financial_sw)   # plot the data
"us_macro_financial_sw"
