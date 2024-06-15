#  #####################################################################################
#  R package bsvars by Tomasz Woźniak Copyright (C) 2022
#
#  This file is part of the R package bsvars: Bayesian Estimation
#  of Structural Vector Autoregressive Models
#
#  The R package bsvars is free software: you can redistribute it
#  and/or modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation, either version 3 or
#  any later version of the License.
#
#  The R package bsvars is distributed in the hope that it will be
#  useful, but WITHOUT ANY WARRANTY; without even the implied warranty
#  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#  General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with the R package bsvars. If that is not the case, please
#  refer to <http://www.gnu.org/licenses/>.
#  #####################################################################################
#
#' @title Bayesian Estimation of Structural Vector Autoregressive Models
#'
#' @description Provides fast and efficient procedures for Bayesian analysis of 
#' Structural Vector Autoregressions. This package estimates a wide range of 
#' models, including homo-, heteroskedastic and non-normal specifications. 
#' Structural models can be identified by adjustable exclusion restrictions, 
#' time-varying volatility, or non-normality.
#' They all include a flexible three-level equation-specific local-global 
#' hierarchical prior distribution for the estimated level of shrinkage for 
#' autoregressive and structural parameters. Additionally, the package facilitates 
#' predictive and structural analyses such as impulse responses, forecast error 
#' variance and historical decompositions, forecasting, verification of 
#' heteroskedasticity and hypotheses on autoregressive parameters, and analyses 
#' of structural shocks, volatilities, and fitted values. Beautiful plots, 
#' informative summary functions, and extensive documentation complement all this. 
#' The implemented techniques align closely with those presented in 
#' Lütkepohl, Shang, Uzeda, & Woźniak (2024) <doi:10.48550/arXiv.2404.11057>, 
#' Lütkepohl & Woźniak (2020) <doi:10.1016/j.jedc.2020.103862>, 
#' Song & Woźniak (2021) <doi:10.1093/acrefore/9780190625979.013.174>, and 
#' Woźniak & Droumaguet (2015) <doi:10.13140/RG.2.2.19492.55687>.
#' 
#' @details 
#' 
#' \strong{Models.} All the SVAR models in this package are specified by two equations, including 
#' the reduced form equation:
#' \deqn{Y = AX + E}
#' where \eqn{Y} is an \code{NxT} matrix of dependent variables, 
#' \eqn{X} is a \code{KxT} matrix of explanatory variables, 
#' \eqn{E} is an \code{NxT} matrix of reduced form error terms, 
#' and \eqn{A} is an \code{NxK} matrix of autoregressive slope coefficients and 
#' parameters on deterministic terms in \eqn{X}.
#' 
#' The structural equation is given by:
#' \deqn{BE = U}
#' where \eqn{U} is an \code{NxT} matrix of structural form error terms, and
#' \eqn{B} is an \code{NxN} matrix of contemporaneous relationships.
#' 
#' Finally, all of the models share the following assumptions regarding the structural
#' shocks \code{U}, namely, joint conditional normality given the past observations collected
#' in matrix \code{X}, and temporal and contemporaneous independence. The latter implies 
#' zero correlations and autocorrelations. 
#' 
#' The various SVAR models estimated differ by the specification of structural shocks
#' variances. The different models include:
#' \itemize{
#'   \item homoskedastic model with unit variances
#'   \item heteroskedastic model with stationary Markov switching in the variances
#'   \item heteroskedastic model with non-centred Stochastic Volatility process for variances
#'   \item heteroskedastic model with centred Stochastic Volatility process for variances
#'   \item non-normal model with a finite mixture of normal components and component-specific variances
#'   \item heteroskedastic model with sparse Markov switching in the variances where the number of heteroskedastic components is estimated
#'   \item non-normal model with a sparse mixture of normal components and component-specific variances where the number of heteroskedastic components is estimated
#' }
#' 
#' \strong{Prior distributions.} All the models feature a Minnesota prior for autoregressive 
#' parameters in matrix \eqn{A} and a generalised-normal distribution for the structural 
#' matrix \eqn{B}. Both of these distributions feature a 3-level equation-specific
#' local-global hierarchical prior that make the shrinkage estimation flexible improving
#' the model fit and its forecasting performance.
#' 
#' \strong{Estimation algorithm.} The models are estimated using frontier numerical methods
#' making the Gibbs sampler fast and efficient. The sampler of the structural matrix 
#' follows Waggoner & Zha (2003), whereas that 
#' for autoregressive parameters follows Chan, Koop, Yu (2022). 
#' The specification of Markov switching heteroskedasticity is inspired by 
#' Song & Woźniak (2021), and that of 
#' Stochastic Volatility model by Kastner & Frühwirth-Schnatter (2014).
#' The estimation algorithms for particular models are scrutinised in 
#' Lütkepohl, Shang, Uzeda, & Woźniak (2024) and Woźniak & Droumaguet (2024)
#' and some other inferential and identification problems are considered in 
#' Lütkepohl & Woźniak (2020).
#' 
#' @name bsvars-package
#' @aliases bsvars-package bsvars
#' @docType package
#' @useDynLib bsvars, .registration = TRUE
#' @importFrom GIGrvg rgig
#' @importFrom R6 R6Class
#' @importFrom Rcpp sourceCpp
#' @import RcppProgress
#' @importFrom RcppTN rtn
#' @importFrom stochvol svsample_fast_cpp
#' @importFrom stats quantile sd
#' @importFrom graphics polygon abline par mtext axis
#' @note This package is currently in active development. Your comments,
#' suggestions and requests are warmly welcome!
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' @references
#' 
#' Chan, J.C.C., Koop, G, and Yu, X. (2024) Large Order-Invariant Bayesian VARs with Stochastic Volatility. \emph{Journal of Business & Economic Statistics}, \bold{42}, \doi{10.1080/07350015.2023.2252039}.
#' 
#' Kastner, G. and Frühwirth-Schnatter, S. (2014) Ancillarity-Sufficiency Interweaving Strategy (ASIS) for Boosting MCMC 
#' Estimation of Stochastic Volatility Models. \emph{Computational Statistics & Data Analysis}, \bold{76}, 408--423, 
#' \doi{10.1016/j.csda.2013.01.002}.
#' 
#' Lütkepohl, H., Shang, F., Uzeda, L., and Woźniak, T. (2024) Partial Identification of Heteroskedastic Structural VARs: Theory and Bayesian Inference. \emph{University of Melbourne Working Paper}, 1--57, \doi{10.48550/arXiv.2404.11057}.
#' 
#' Lütkepohl, H., and Woźniak, T., (2020) Bayesian Inference for Structural Vector Autoregressions Identified by Markov-Switching Heteroskedasticity. \emph{Journal of Economic Dynamics and Control} \bold{113}, 103862, \doi{10.1016/j.jedc.2020.103862}.
#' 
#' Song, Y., and Woźniak, T. (2021) Markov Switching Heteroskedasticity in Time Series Analysis. In: \emph{Oxford Research Encyclopedia of Economics and Finance}. Oxford University Press, \doi{10.1093/acrefore/9780190625979.013.174}.
#' 
#' Waggoner, D.F., and Zha, T., (2003) A Gibbs sampler for structural vector autoregressions. \emph{Journal of Economic Dynamics and Control}, \bold{28}, 349--366, \doi{10.1016/S0165-1889(02)00168-9}.
#' 
#' Woźniak, T., and Droumaguet, M., (2024) Bayesian Assessment of Identifying Restrictions for Heteroskedastic Structural VARs.
#' 
#' @keywords package models ts
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)    # upload dependent variables
#' data(us_fiscal_ex)      # upload exogenous variables
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar_sv$new(us_fiscal_lsuw, p = 4, exogenous = us_fiscal_ex)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' 
#' # compute impulse responses 2 years ahead
#' irf           = compute_impulse_responses(posterior, horizon = 8)
#' 
#' # compute forecast error variance decomposition 2 years ahead
#' fevd           = compute_variance_decompositions(posterior, horizon = 8)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new(p = 4, exogenous = us_fiscal_ex) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_variance_decompositions(horizon = 8) -> fevds
#' 
"_PACKAGE"
