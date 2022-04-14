
#' @title Bayesian estimation of a Structural Vector Autoregression with 
#' Stochastic Volatility heteroskedasticity via Gibbs sampler
#'
#' @description Estimates the SVAR with Stochastic Volatility (SV) heteroskedasticity proposed by Lütkepohl, Shang, Uzeda, and Woźniak (2022).
#' Implements the Gibbs sampler proposed by Waggoner & Zha (2003)
#' for the structural matrix \code{B} and the equation-by-equation sampler by Chan, Koop, & Yu (2021)
#' for the autoregressive slope parameters \code{A}. Additionally, the parameter matrices \code{A} and \code{B}
#' follow a Minnesota prior and generalised-normal prior distributions respectively with the matrix-specific
#' overall shrinkage parameters estimated thanks to a 3-level hierarchical prior distribution. The SV model 
#' is estimated in a non-centred parameterisation using a range of techniques including: 
#' simulation smoother, auxiliary mixture, ancillarity-sufficiency interweaving strategy, 
#' and generalised inverse Gaussian distribution summarised by Kastner & Frühwirth-Schnatter (2014). 
#' See section \bold{Details} for the model equations.
#' 
#' @details 
#' The heteroskedastic SVAR model is given by the reduced form equation:
#' \Sexpr[results=rd, stage=build]{katex::math_to_rd("Y = AX + E")}
#' where \code{Y} is an \code{NxT} matrix of dependent variables, \code{X} is a \code{KxT} matrix of explanatory variables, 
#' \code{E} is an \code{NxT} matrix of reduced form error terms, and \code{A} is an \code{NxK} matrix of autoregressive slope coefficients and parameters on deterministic terms in \code{X}.
#' 
#' The structural equation is given by
#' \Sexpr[results=rd, stage=build]{katex::math_to_rd("BE=U")}
#' where \code{U} is an \code{NxT} matrix of structural form error terms, and
#' \code{B} is an \code{NxN} matrix of contemporaneous relationships between structural shocks in the columns of matrix \code{U}.
#' 
#' Finally, the structural shocks, \code{U}, are temporally and contemporaneously independent and jointly normally distributed with zero mean.
#' The conditional variance of the \code{n}th shock at time \code{t} is given by:
#' \Sexpr[results=rd, stage=build]{katex::math_to_rd("Var_{t-1}[u_{n.t}] = exp(w_n h_{n.t})")}
#' where \code{w_n} is the estimated conditional standard deviation of the log-conditional variance
#' and the log-volatility process \code{h_{n.t}} follows an autoregressive process:
#' \Sexpr[results=rd, stage=build]{katex::math_to_rd("h_{n.t} = g_n h_{n.t-1} + v_{n.t}")}
#' where \code{h_{n.0}=0}, \code{g_n} is an autoregressive parameter and \code{v_{n.t}} is a standard normal error term.
#' 
#' @param S a positive integer, the number of posterior draws to be generated
#' @param Y an \code{NxT} matrix, the matrix containing \code{T} observations on \code{N} dependent time series variables
#' @param X a \code{KxT} matrix, the matrix containing \code{T} observations on \code{K = N*p+d} regressors including \code{p} lags of dependent variables and \code{d} deterministic terms
#' @param prior a list containing the following elements
#' \describe{
#'  \item{A}{an \code{NxK} matrix, the mean of the normal prior distribution for the parameter matrix \code{A}}
#'  \item{A_V_inv}{a \code{KxK} precision matrix of the normal prior distribution for each of the row of the parameter matrix \code{A}. This precision matrix is equation invariant.}
#'  \item{B_V_inv}{an \code{NxN} precision matrix of the generalised-normal prior distribution for the structural matrix \code{B}. This precision matrix is equation invariant.}
#'  \item{B_nu}{a positive integer greater of equal than \code{N}, a shape parameter of the generalised-normal prior distribution for the structural matrix \code{B}}
#'  \item{hyper_nu}{a positive scalar, the shape parameter of the inverted-gamma 2 prior distribution for the two overall shrinkage parameters for matrices \code{B} and \code{A}}
#'  \item{hyper_a}{a positive scalar, the shape parameter of the gamma prior for the two overall shrinkage parameters}
#'  \item{hyper_V}{a positive scalar,  the shape parameter of the inverted-gamma 2 for the level 3 hierarchy of shrinkage parameters}
#'  \item{hyper_S}{a positive scalar,  the scale parameter of the inverted-gamma 2 for the level 3 hierarchy of shrinkage parameters}
#'  \item{sv_a_}{a positive scalar, the shape parameter of the gamma prior in the hierarchial prior for \code{sigma2_omega}}
#'  \item{sv_s_}{a positive scalar, the scale parameter of the gamma prior in the hierarchial prior for \code{sigma2_omega}}
#' }
#' @param VB a list of \code{N} matrices determining the unrestricted elements of matrix \code{B}
#' @param starting_values a list containing the following elements:
#' \describe{
#'  \item{A}{an \code{NxK} matrix of starting values for the parameter \code{A}}
#'  \item{B}{an \code{NxN} matrix of starting values for the parameter \code{B}}
#'  \item{hyper}{a \code{5}-vector of starting values for the shrinkage hyper-parameters of the hierarchical prior distribution}
#'  \item{h}{an \code{NxT} matrix with the starting values of the log-volatility processes}
#'  \item{rho}{an \code{N}-vector with values of SV autoregressive parameters}
#'  \item{omega}{an \code{N}-vector with values of SV process conditional standard deviations}
#'  \item{S}{an \code{NxT} integer matrix with the auxiliary mixture component indicators}
#'  \item{sigma2_omega}{an \code{N}-vector with variances of the zero-mean normal prior for \code{omega}}
#'  \item{s_}{a positive scalar with the scale of the gamma prior of the hierarchical prior for \code{sigma2_omega}}
#' }
#' @param sample_s_ a logical value. If \code{TRUE} the hyper-parameter \code{s_} is estimated
#' 
#' @return A list containing two elements:
#' 
#'  \code{posterior} a list with a collection of \code{S} draws from the posterior distribution generated via Gibbs sampler containing:
#'  \describe{
#'  \item{A}{an \code{NxKxS} array with the posterior draws for matrix \code{A}}
#'  \item{B}{an \code{NxNxS} array with the posterior draws for matrix \code{B}}
#'  \item{hyper}{a \code{5xS} matrix with the posterior draws for the hyper-parameters of the hierarchical prior distribution}
#'  \item{h}{an \code{NxTxS} array with the posterior draws of the log-volatility processes}
#'  \item{rho}{an \code{NxS} matrix with the posterior draws of SV autoregressive parameters}
#'  \item{omega}{an \code{NxS} matrix with the posterior draws of SV process conditional standard deviations}
#'  \item{S}{an \code{NxTxS} array with the posterior draws of the auxiliary mixture component indicators}
#'  \item{sigma2_omega}{an \code{NxS} matrix with the posterior draws of the variances of the zero-mean normal prior for \code{omega}}
#'  \item{s_}{an \code{S}-vector with the posterior draws of the scale of the gamma prior of the hierarchical prior for \code{sigma2_omega}}
#' }
#' 
#' \code{last_draw} a list with the last draw of the simulation (to be provided as \code{starting_values} to the follow-up run of \code{bsvar}) containing the following objects:
#' \describe{
#'  \item{A}{an \code{NxK} matrix with the last MCMC draw of the parameter matrix \code{A}}
#'  \item{B}{an \code{NxN} matrix with the last MCMC draw of the parameter matrix \code{B}}
#'  \item{hyper}{a \code{5}-vector with the last MCMC draw of the hyper-parameter of the hierarchical prior distribution}
#'  \item{h}{an \code{NxT} matrix with last MCMC draw of the log-volatility processes}
#'  \item{rho}{an \code{N}-vector with last MCMC draw of SV autoregressive parameters}
#'  \item{omega}{an \code{N}-vector with last MCMC draw of SV process conditional standard deviations}
#'  \item{S}{an \code{NxT} matrix with last MCMC draw of the auxiliary mixture component indicators}
#'  \item{sigma2_omega}{an \code{N}-vector with last MCMC draw of the variances of the zero-mean normal prior for \code{omega}}
#'  \item{s_}{a scalar with the last MCMC draw of the scale of the gamma prior of the hierarchical prior for \code{sigma2_omega}}
#'  }
#'
#' @seealso \code{\link{normalisation_wz2003}}, \code{\link{logSDDR_homoskedasticity}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @references The model, prior distributions, and estimation algorithms were proposed by
#' 
#' Lütkepohl, H., Shang, F., Uzeda, L., and Woźniak, T. (2022) Partial Identification of Heteroskedastic Structural VARs: Theory and Bayesian Inference.
#' 
#' Sampling from the generalised-normal full conditional posterior distribution of matrix \code{B} is implemented using the Gibbs sampler by:
#' 
#' Waggoner, D.F., and Zha, T., (2003) A Gibbs sampler for structural vector autoregressions. \emph{Journal of Economic Dynamics and Control}, \bold{28}, 349--366, \doi{https://doi.org/10.1016/S0165-1889(02)00168-9}.
#'
#' Sampling from the multivariate normal full conditional posterior distribution of each of the \code{A} matrix row is implemented using the sampler by:
#' 
#' Chan, J.C.C., Koop, G, and Yu, X. (2021) Large Order-Invariant Bayesian VARs with Stochastic Volatility.
#' 
#' Many of the techniques employed for the estimation of the Stochastic Volatility model 
#' are summarised by:
#' 
#' Kastner, G. and Frühwirth-Schnatter, S. (2014) Ancillarity-Sufficiency Interweaving Strategy (ASIS) for Boosting MCMC 
#' Estimation of Stochastic Volatility Models. \emph{Computational Statistics & Data Analysis}, \bold{76}, 408--423, 
#' \doi{10.1016/j.csda.2013.01.002}.
#' 
#' @export
bsvar_sv <- function(S, Y, X, prior, VB, starting_values, sample_s_ = TRUE) {
  bsvar_sv_cpp(S, Y, X, prior, VB, starting_values, sample_s_)
}
