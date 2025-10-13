
#' @title A matrix to be used in a conditional forecasting example including
#' the projected values of total tax revenue that are projected to increase at an 
#' average quarterly sample growth rate. The other two columns are filled with 
#' \code{NA} values, which implies that the future values of the corresponding 
#' endogenous variables, namely government spending and GDP, will be forecasted
#' given the provided projected values of total tax revenue. The matrix includes 
#' future values for the forecast horizon of two years for the US fiscal model 
#' for the period 2024 Q3 -- 2026 Q2.
#'
#' @description Conditional projections variables to be used in conditional 
#' forecasting of government spending and GDP given the provided projected 
#' values of total tax revenue. Last data update was implemented on 2024-10-22.
#'
#' @usage data(us_fiscal_cond_forecasts)
#' 
#' @format A matrix and a \code{ts} object with time series of eight values on 
#' 3 variables:
#' \describe{
#'   \item{ttr}{the values are provided. This variable will not 
#'   be forecasted.}
#'   \item{gs}{not provided. This variable will be forecasted conditionally on 
#'   the provided values for ttr.}
#'   \item{gdp}{not provided. This variable will be forecasted conditionally on 
#'   the provided values for ttr}
#' }
#' 
#' The series are as described by Mertens & Ravn (2014). The data was used by 
#' Lütkepohl, Shang, Uzeda, Woźniak (2024).
#' 
#' @references 
#' Lütkepohl, H., Shang, F., Uzeda, L., and Woźniak, T. (2024) Partial Identification of Heteroskedastic Structural VARs: Theory and Bayesian Inference. \emph{University of Melbourne Working Paper}, 1--57, \doi{10.48550/arXiv.2404.11057}.
#' 
#' Mertens, K., and Ravn, M.O. (2014) A Reconciliation of SVAR and Narrative Estimates of Tax Multipliers, \emph{Journal of Monetary Economics}, 68(S), S1–S19. DOI: \doi{10.1016/j.jmoneco.2013.04.004}.
#' 
#' @examples 
#' data(us_fiscal_cond_forecasts)   # upload the data
#' 
"us_fiscal_cond_forecasts"