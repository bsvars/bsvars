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
#' @description Efficient and fast algorithms for Bayesian estimation of 
#' Structural Vector Autoregressive (SVAR) models via Markov chain Monte Carlo methods. 
#' A wide range of SVAR models is considered, including homo- and heteroskedastic specifications 
#' and those with non-normal structural shocks. The heteroskedastic SVAR model setup is similar as in 
#' Woźniak & Droumaguet (2015) <doi:10.13140/RG.2.2.19492.55687> and Lütkepohl & Woźniak (2020) <doi:10.1016/j.jedc.2020.103862>.
#' The sampler of the structural matrix follows Waggoner & Zha (2003) ,doi:10.1016/S0165-1889(02)00168-9>, 
#' whereas that for autoregressive parameters follows Chan, Koop, Yu (2022) <https://www.joshuachan.org/papers/OISV.pdf>. 
#' The specification of Markov switching heteroskedasticity is inspired by Song & Woźniak (2021) <doi:10.1093/acrefore/9780190625979.013.174>,
#' and that of Stochastic Volatility model by Kastner & Frühwirth-Schnatter (2014) <doi:10.1016/j.csda.2013.01.002>.
#' 
#' @details 
#' All the SVAR models in this package are specified by two equations, including 
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
#' @note This package is currently in active development. Your comments,
#' suggestions and requests are warmly welcome!
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' @references
#' Woźniak, T., and Droumaguet, M., (2022) Bayesian Assessment of Identifying Restrictions for Heteroskedastic Structural VARs.
#' @keywords package models ts
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)    # upload dependent variables
#' data(us_fiscal_ex)      # upload exogenous variables
#' 
#' # specify the model and set seed
#' specification  = specify_bsvar_sv$new(us_fiscal_lsuw, p = 4, exogenous = us_fiscal_ex)
#' set.seed(123)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 10, thin = 2)
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
NULL
