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
#' @description This package provides an efficient algorithm for fully Bayesian 
#' estimation of Structural Vector Autoregressive (SVAR) models via
#' Markov chain Monte Carlo (MCMC) methods. 
#' 
#' @details 
#' All the SVAR models in this package are specified by two equations, including 
#' the reduced form equation:
#' \Sexpr[results=rd, stage=build]{katex::math_to_rd("Y = AX + E")}
#' where \code{Y} is an \code{NxT} matrix of dependent variables, 
#' \code{X} is a \code{KxT} matrix of explanatory variables, 
#' \code{E} is an \code{NxT} matrix of reduced form error terms, 
#' and \code{A} is an \code{NxK} matrix of autoregressive slope coefficients and 
#' parameters on deterministic terms in \code{X}.
#' 
#' The structural equation is given by:
#' \Sexpr[results=rd, stage=build]{katex::math_to_rd("BE=U")}
#' where \code{U} is an \code{NxT} matrix of structural form error terms, and
#' \code{B} is an \code{NxN} matrix of contemporaneous relationships between 
#' structural shocks in the columns of matrix \code{U}.
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
#'   \item heteroskedastic model with Markov switching in the variances
#'   \item heteroskedastic model with Stochastic Volatility process for variances
#'   \item heteroskedastic model with the Infinite Hidden Markov Model (IHMM) for variances
#'   \item non-normal model with a finite mixture of normal components and component-specific variances
#'   \item non-normal model with an infinite mixture of normal components and component-specific variances
#' }
#' @name bsvars-package
#' @aliases bsvars-package bsvars
#' @docType package
#' @useDynLib bsvars, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @note This package is currently in active development. Your comments,
#' suggestions and requests are warmly welcome!
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' @references
#' Woźniak, Tomasz, and Matthieu Droumaguet. (2015) Assessing Monetary Policy Models: Bayesian Inference for Heteroskedastic Structural VARs, \url{http://fbe.unimelb.edu.au/__data/assets/pdf_file/0010/1724932/2017TWozniakhoneyhoney.pdf}.
#' @keywords package models ts
NULL
