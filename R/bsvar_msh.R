
#' @title Bayesian estimation of a Structural Vector Autoregression with 
#' Markov-switching heteroskedasticity via Gibbs sampler
#'
#' @description Estimates the SVAR with Markov-switching heteroskedasticity with \code{M} regimes (MS(M)) proposed by Woźniak & Droumaguet (2022).
#' Implements the Gibbs sampler proposed by Waggoner & Zha (2003)
#' for the structural matrix \eqn{B} and the equation-by-equation sampler by Chan, Koop, & Yu (2021)
#' for the autoregressive slope parameters \eqn{A}. Additionally, the parameter matrices \eqn{A} and \eqn{B}
#' follow a Minnesota prior and generalised-normal prior distributions respectively with the matrix-specific
#' overall shrinkage parameters estimated thanks to a 3-level hierarchical prior distribution. The MS
#' model is estimated using the prior distributions and algorithms proposed by Woźniak & Droumaguet (2022).
#' See section \bold{Details} for the model equations.
#' 
#' @details 
#' The heteroskedastic SVAR model is given by the reduced form equation:
#' \deqn{Y = AX + E}
#' where \eqn{Y} is an \code{NxT} matrix of dependent variables, \eqn{X} is a \code{KxT} matrix of explanatory variables, 
#' \eqn{E} is an \code{NxT} matrix of reduced form error terms, and \eqn{A} is an \code{NxK} matrix of autoregressive slope coefficients and parameters on deterministic terms in \code{X}.
#' 
#' The structural equation is given by
#' \deqn{BE = U}
#' where \eqn{U} is an \code{NxT} matrix of structural form error terms, and
#' \eqn{B} is an \code{NxN} matrix of contemporaneous relationships between structural shocks in the columns of matrix \eqn{U}.
#' 
#' Finally, the structural shocks, \eqn{U}, are temporally and contemporaneously independent and jointly normally distributed with zero mean.
#' The conditional variance of the \code{n}th shock at time \code{t} is given by:
#' \deqn{Var_{t-1}[u_{n.t}] = s^2_{n.s_t}}
#' where \eqn{s_t} is a stationary, irreducible, aperiodic Markov process driving the time-variability of 
#' the regime-specific conditional variances of structural shocks \eqn{s^2_{n.s_t}}. 
#' In this model, the variances of each of the structural shocks sum to \code{M}.
#' 
#' NOTE: The estimation of the Markov process for this model requires at least 2 occurrences of each of the regimes at each MCMC iteration.
#' This restriction might limit the number of states applicable to some time series.
#' 
#' @param S a positive integer, the number of posterior draws to be generated
#' @param Y an \code{NxT} matrix, the matrix containing \code{T} observations on \code{N} dependent time series variables
#' @param X a \code{KxT} matrix, the matrix containing \code{T} observations on \code{K = N*p+d} regressors including \code{p} lags of dependent variables and \code{d} deterministic terms
#' @param prior a list containing the following elements
#' \describe{
#'  \item{A}{an \code{NxK} matrix, the mean of the normal prior distribution for the parameter matrix \eqn{A}}
#'  \item{A_V_inv}{a \code{KxK} precision matrix of the normal prior distribution for each of the row of the parameter matrix \eqn{A}. This precision matrix is equation invariant.}
#'  \item{B_V_inv}{an \code{NxN} precision matrix of the generalised-normal prior distribution for the structural matrix \eqn{B}. This precision matrix is equation invariant.}
#'  \item{B_nu}{a positive integer greater of equal than \code{N}, a shape parameter of the generalised-normal prior distribution for the structural matrix \code{B}}
#'  \item{hyper_nu}{a positive scalar, the shape parameter of the inverted-gamma 2 prior distribution for the two overall shrinkage parameters for matrices \code{B} and \code{A}}
#'  \item{hyper_a}{a positive scalar, the shape parameter of the gamma prior for the two overall shrinkage parameters}
#'  \item{hyper_V}{a positive scalar,  the shape parameter of the inverted-gamma 2 for the level 3 hierarchy of shrinkage parameters}
#'  \item{hyper_S}{a positive scalar,  the scale parameter of the inverted-gamma 2 for the level 3 hierarchy of shrinkage parameters}
#'  \item{sigma_nu}{a positive scalar, the common shape parameter of the IG2-based Dirichlet prior for conditional variances}
#'  \item{sigma_s}{a positive scalar, the common scale parameter of the IG2-based Dirichlet prior for conditional variances}
#'  \item{PR_TR}{an \code{MxM} matrix of the parameters of Dirichlet prior for transition probability matrix. The rows of \code{PR_TR} correspond to the rows of the transition matrix. }
#' }
#' @param VB a list of \code{N} matrices determining the unrestricted elements of matrix \eqn{B}
#' @param starting_values a list containing the following elements:
#' \describe{
#'  \item{A}{an \code{NxK} matrix of starting values for the parameter \eqn{A}}
#'  \item{B}{an \code{NxN} matrix of starting values for the parameter \eqn{B}}
#'  \item{hyper}{a \code{5}-vector of starting values for the shrinkage hyper-parameters of the hierarchical prior distribution}
#'  \item{sigma2}{an \code{NxM} matrix of staring values for the structural shocks conditional variances}
#'  \item{PR_TR}{an \code{MxM} matrix  of staring values for the transition matrix. These have to be non-negative values summing to 1 by rows.}
#'  \item{xi}{an \code{MxT} matrix of starting values for the regime allocation matrix. Its elements are zeros and ones and sum to 1 by columns.}
#'  \item{pi_0}{an \code{M}-vector of starting values for the ergodic probabilities}
#' }
#' 
#' @return A list containing two elements:
#'  \code{posterior} a list with a collection of \code{S} draws from the posterior distribution generated via Gibbs sampler containing:
#'  \describe{
#'  \item{A}{an \code{NxKxS} array with the posterior draws for matrix \eqn{A}}
#'  \item{B}{an \code{NxNxS} array with the posterior draws for matrix \eqn{B}}
#'  \item{hyper}{a \code{5xS} matrix with the posterior draws for the hyper-parameters of the hierarchical prior distribution}
#'  \item{sigma2}{an \code{NxMxS} array with the posterior draws for the structural shocks conditional variances}
#'  \item{PR_TR}{an \code{MxMxS} array with the posterior draws for the transition matrix.}
#'  \item{xi}{an \code{MxTxS} array with the posterior draws for the regime allocation matrix.}
#'  \item{pi_0}{an \code{MxS} matrix with the posterior draws for the ergodic probabilities}
#' }
#' 
#' \code{last_draw} a list with the last draw of the simulation (to be provided as \code{starting_values} to the follow-up run of \code{bsvar}) containing the following objects:
#' \describe{
#'  \item{A}{an \code{NxK} matrix with the last MCMC draw of the parameter matrix \eqn{A}}
#'  \item{B}{an \code{NxN} matrix with the last MCMC draw of the parameter matrix \eqn{B}}
#'  \item{hyper}{a \code{5}-vector with the last MCMC draw of the hyper-parameter of the hierarchical prior distribution}
#'  \item{sigma2}{an \code{NxM} matrix with the last MCMC draw of the structural shocks conditional variances}
#'  \item{PR_TR}{an \code{MxM} matrix with the last MCMC draw of the transition matrix.}
#'  \item{xi}{an \code{MxT} matrix with the last MCMC draw of the regime allocation matrix.}
#'  \item{pi_0}{an \code{M}-vector with the last MCMC draw of the ergodic probabilities}
#'  }
#'
#' @seealso \code{\link{normalisation_wz2003}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @references The model, prior distributions, and estimation algorithms were proposed by
#' 
#' Woźniak, T., and Droumaguet, M., (2022) Bayesian Assessment of Identifying Restrictions for Heteroskedastic Structural VARs
#' 
#' Some more analysis on heteroskedastic SVAR models was proposed by:
#' 
#' Lütkepohl, H., and Woźniak, T., (2020) Bayesian Inference for Structural Vector Autoregressions Identified by Markov-Switching Heteroskedasticity. \emph{Journal of Economic Dynamics and Control} \bold{113}, 103862, \doi{https://doi.org/10.1016/j.jedc.2020.103862}.
#' 
#' Sampling from the generalised-normal full conditional posterior distribution of matrix \eqn{B} is implemented using the Gibbs sampler by:
#' 
#' Waggoner, D.F., and Zha, T., (2003) A Gibbs sampler for structural vector autoregressions. \emph{Journal of Economic Dynamics and Control}, \bold{28}, 349--366, \doi{https://doi.org/10.1016/S0165-1889(02)00168-9}.
#'
#' Sampling from the multivariate normal full conditional posterior distribution of each of the \eqn{A} matrix row is implemented using the sampler by:
#' 
#' Chan, J.C.C., Koop, G, and Yu, X. (2021) Large Order-Invariant Bayesian VARs with Stochastic Volatility.
#' 
#' The estimation of the Markov-switching heteroskedasticity closely follows procedures described by:
#' 
#' Song, Y., and Woźniak, T., (2021) Markov Switching. \emph{Oxford Research Encyclopedia of Economics and Finance}, Oxford University Press, \doi{https://doi.org/10.1093/acrefore/9780190625979.013.174}.
#' 
#' and
#' 
#' Frühwirth-Schnatter, S., (2006) Finite Mixture and Markov Switching Models. Springer Series in Statistics. New York: Springer, \doi{https://doi.org/10.1007/978-0-387-35768-3}.
#' 
#' The forward-filtering backward-sampling is implemented following the proposal by:
#' 
#' Chib, S. (1996) Calculating posterior distributions and modal estimates in Markov mixture models. \emph{Journal of Econometrics}, \bold{75}(1), 79–97, \doi{https://doi.org/10.1016/0304-4076(95)01770-4}.
#' 
#' @export
bsvar_msh <- function(S, Y, X, prior, VB, starting_values) {
  model = "MSH"
  .Call(`_bsvars_bsvar_msh_cpp`, S, Y, X, prior, VB, starting_values, 100, TRUE, TRUE, model)
}
