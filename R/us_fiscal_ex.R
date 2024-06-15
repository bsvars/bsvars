
#' @title A 3-variable system of exogenous variables for the US fiscal model for 
#' the period 1948 Q1 -- 2024 Q1
#'
#' @description Exogenous variables used to identify the US fiscal policy shocks.
#' Last data update was implemented on 2024-06-15.
#'
#' @usage data(us_fiscal_ex)
#' 
#' @format A matrix and a \code{ts} object with time series of over three hundred observations on 3 variables:
#' \describe{
#'   \item{t}{a time trend}
#'   \item{t^2}{a quadratic trend}
#'   \item{1975Q2}{a dummy variable taking the value of 1 for quarter 2 1975 and zero elsewhere}
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
#' data(us_fiscal_ex)   # upload the data
#' plot(us_fiscal_ex)   # plot the data
"us_fiscal_ex"