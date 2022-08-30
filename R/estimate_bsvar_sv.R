
#' @title Bayesian estimation of a Structural Vector Autoregression with 
#' Stochastic Volatility heteroskedasticity via Gibbs sampler
#'
#' @description Estimates the SVAR with Stochastic Volatility (SV) heteroskedasticity proposed by Lütkepohl, Shang, Uzeda, and Woźniak (2022).
#' Implements the Gibbs sampler proposed by Waggoner & Zha (2003)
#' for the structural matrix \eqn{B} and the equation-by-equation sampler by Chan, Koop, & Yu (2021)
#' for the autoregressive slope parameters \eqn{A}. Additionally, the parameter matrices \eqn{A} and \eqn{B}
#' follow a Minnesota prior and generalised-normal prior distributions respectively with the matrix-specific
#' overall shrinkage parameters estimated thanks to a 3-level hierarchical prior distribution. The SV model 
#' is estimated in a non-centred parameterisation using a range of techniques including: 
#' simulation smoother, auxiliary mixture, ancillarity-sufficiency interweaving strategy, 
#' and generalised inverse Gaussian distribution summarised by Kastner & Frühwirth-Schnatter (2014). 
#' See section \bold{Details} for the model equations.
#' 
#' @details 
#' The heteroskedastic SVAR model is given by the reduced form equation:
#' \deqn{Y = AX + E}
#' where \eqn{Y} is an \code{NxT} matrix of dependent variables, \eqn{X} is a \code{KxT} matrix of explanatory variables, 
#' \eqn{E} is an \code{NxT} matrix of reduced form error terms, and \eqn{A} is an \code{NxK} matrix of autoregressive slope coefficients and parameters on deterministic terms in \eqn{X}.
#' 
#' The structural equation is given by
#' \deqn{BE = U}
#' where \eqn{U} is an \code{NxT} matrix of structural form error terms, and
#' \eqn{B} is an \code{NxN} matrix of contemporaneous relationships.
#' 
#' Finally, the structural shocks, \eqn{U}, are temporally and contemporaneously independent and jointly normally distributed with zero mean.
#' The conditional variance of the \code{n}th shock at time \code{t} is given by:
#' \deqn{Var_{t-1}[u_{n.t}] = exp(w_n h_{n.t})}
#' where \eqn{w_n} is the estimated conditional standard deviation of the log-conditional variance
#' and the log-volatility process \eqn{h_{n.t}} follows an autoregressive process:
#' \deqn{h_{n.t} = g_n h_{n.t-1} + v_{n.t}}
#' where \eqn{h_{n.0}=0}, \eqn{g_n} is an autoregressive parameter and \eqn{v_{n.t}} is a standard normal error term.
#' 
#' @param S a positive integer, the number of posterior draws to be generated
#' @param specification an object of class BSVAR-SV generated using the \code{specify_bsvar_sv$new()} function.
#' @param thin a positive integer, specifying the frequency of MCMC output thinning
#' @param show_progress a logical value, if \code{TRUE} the estimation progress bar is visible
#' 
#' @return An object of class PosteriorBSVAR-SV containing the Bayesian estimation output and containing two elements:
#' 
#'  \code{posterior} a list with a collection of \code{S} draws from the posterior distribution generated via Gibbs sampler containing:
#'  \describe{
#'  \item{A}{an \code{NxKxS} array with the posterior draws for matrix \eqn{A}}
#'  \item{B}{an \code{NxNxS} array with the posterior draws for matrix \eqn{B}}
#'  \item{hyper}{a \code{5xS} matrix with the posterior draws for the hyper-parameters of the hierarchical prior distribution}
#'  \item{h}{an \code{NxTxS} array with the posterior draws of the log-volatility processes}
#'  \item{rho}{an \code{NxS} matrix with the posterior draws of SV autoregressive parameters}
#'  \item{omega}{an \code{NxS} matrix with the posterior draws of SV process conditional standard deviations}
#'  \item{S}{an \code{NxTxS} array with the posterior draws of the auxiliary mixture component indicators}
#'  \item{sigma2_omega}{an \code{NxS} matrix with the posterior draws of the variances of the zero-mean normal prior for \code{omega}}
#'  \item{s_}{an \code{S}-vector with the posterior draws of the scale of the gamma prior of the hierarchical prior for \code{sigma2_omega}}
#' }
#' 
#' \code{last_draw} an object of class BSVAR-SV with the last draw of the current MCMC run as the starting value to be passed to the continuation of the MCMC estimation using \code{bsvar_sv()}. 
#'
#' @seealso \code{\link{specify_bsvar_sv}}, \code{\link{specify_posterior_bsvar_sv}}, \code{\link{normalise_posterior}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @references The model, prior distributions, and estimation algorithms were proposed by
#' 
#' Lütkepohl, H., Shang, F., Uzeda, L., and Woźniak, T. (2022) Partial Identification of Heteroskedastic Structural VARs: Theory and Bayesian Inference.
#' 
#' Sampling from the generalised-normal full conditional posterior distribution of matrix \eqn{B} is implemented using the Gibbs sampler by:
#' 
#' Waggoner, D.F., and Zha, T., (2003) A Gibbs sampler for structural vector autoregressions. \emph{Journal of Economic Dynamics and Control}, \bold{28}, 349--366, \doi{https://doi.org/10.1016/S0165-1889(02)00168-9}.
#'
#' Sampling from the multivariate normal full conditional posterior distribution of each of the \eqn{A} matrix row is implemented using the sampler by:
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
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' specification  = specify_bsvar_sv$new(us_fiscal_lsuw, p = 4)
#' set.seed(123)
#' 
#' # run the burn-in
#' burn_in        = estimate_bsvar_sv(10, specification)
#' 
#' # estimate the model
#' posterior      = estimate_bsvar_sv(50, burn_in$get_last_draw())
#' 
#' @export
estimate_bsvar_sv <- function(S, specification, thin = 10, show_progress = TRUE) {
  
  stopifnot("Argument S must be a positive integer number." = S > 1 & S %% 1 == 0)
  stopifnot("Argument specification must be of class BSVAR-SV generated using the specify_bsvar_sv$new() function." = any(class(specification) == "BSVAR-SV"))
  stopifnot("Argument thin must be a positive integer number." = thin > 0 & thin %% 1 == 0)
  
  prior               = specification$prior$get_prior()
  starting_values     = specification$starting_values$get_starting_values()
  VB                  = specification$identification$get_identification()
  data_matrices       = specification$data_matrices$get_data_matrices()
  
  qqq                 = .Call(`_bsvars_bsvar_sv_cpp`, S, data_matrices$Y, data_matrices$X, prior, VB, starting_values, thin, TRUE, show_progress)
  
  specification$starting_values$set_starting_values(qqq$last_draw)
  output              = specify_posterior_bsvar_sv$new(specification, qqq$posterior)
  
  return(output)
}
