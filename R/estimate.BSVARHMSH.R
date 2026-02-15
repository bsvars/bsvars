
#' @title Bayesian estimation of a Structural Vector Autoregression with 
#' Heterogeneous Markov-switching heteroskedasticity via Gibbs sampler
#'
#' @description Estimates the SVAR with Heterogeneous Markov-switching 
#' heteroskedasticity with \code{M} regimes (HMS(M)) proposed by Shang & Woźniak (2025).
#' Implements the Gibbs sampler proposed by Waggoner & Zha (2003)
#' for the structural matrix \eqn{B} and the equation-by-equation sampler by Chan, Koop, & Yu (2024)
#' for the autoregressive slope parameters \eqn{A}. Additionally, the parameter 
#' matrices \eqn{A} and \eqn{B} follow a Minnesota prior and generalised-normal 
#' prior distributions respectively with the matrix-specific
#' overall shrinkage parameters estimated thanks to a hierarchical prior 
#' distribution. The MS model is estimated using the prior distributions and 
#' algorithms proposed Shang & Woźniak (2025), Lütkepohl & Woźniak (2020), and 
#' Song & Woźniak (2021). See section \bold{Details} for the model equations.
#' 
#' @details 
#' The heteroskedastic SVAR model is given by the reduced form equation:
#' \deqn{Y = AX + E}
#' where \eqn{Y} is an \code{NxT} matrix of dependent variables, \eqn{X} is a 
#' \code{KxT} matrix of explanatory variables, 
#' \eqn{E} is an \code{NxT} matrix of reduced form error terms, and \eqn{A} is 
#' an \code{NxK} matrix of autoregressive slope coefficients and parameters on 
#' deterministic terms in \code{X}.
#' 
#' The structural equation is given by
#' \deqn{BE = U}
#' where \eqn{U} is an \code{NxT} matrix of structural form error terms, and
#' \eqn{B} is an \code{NxN} matrix of contemporaneous relationships.
#' 
#' Finally, the structural shocks, \eqn{U}, are temporally and contemporaneously 
#' independent and jointly distributed with zero mean.
#' The structural shocks can be either normally or Student-t distributed, where in 
#' the latter case the shock-specific degrees of freedom parameters are estimated.
#' The conditional variance of the \code{n}th shock at time \code{t} is given by:
#' \deqn{Var_{t-1}[u_{n.t}] = s^2_{n.s_{n.t}}}
#' where \eqn{s_{n.t}} is an equation-specific Markov process driving the time-variability of 
#' the regime-specific conditional variances of the nth structural shock \eqn{s^2_{n.s_{n.t}}}. 
#' In this model, the variances of each of the structural shocks sum to \code{M}.
#' 
#' Each of the Markov processes \eqn{s_{n.t}} is either:
#' \itemize{
#'   \item stationary, irreducible, and aperiodic which requires all regimes to have 
#'   a positive number occurrences over the sample period, or
#'   \item sparse with potentially many regimes with zero occurrences over the sample period
#'   and in which the number of regimes is estimated.
#' }
#' 
#' @param specification an object of class BSVARHMSH generated using 
#' the \code{specify_bsvar_hmsh$new()} function.
#' @param S a positive integer, the number of posterior draws to be generated
#' @param thin a positive integer, specifying the frequency of MCMC output thinning
#' @param show_progress a logical value, if \code{TRUE} the estimation progress bar is visible
#' 
#' @return An object of class PosteriorBSVARHMSH containing the Bayesian estimation output and containing two elements:
#' 
#'  \code{posterior} a list with a collection of \code{S} draws from the posterior distribution generated via Gibbs sampler containing:
#'  \describe{
#'  \item{A}{an \code{NxKxS} array with the posterior draws for matrix \eqn{A}}
#'  \item{B}{an \code{NxNxS} array with the posterior draws for matrix \eqn{B}}
#'  \item{hyper}{a \code{5xS} matrix with the posterior draws for the hyper-parameters of the hierarchical prior distribution}
#'  \item{sigma2}{an \code{NxMxS} array with the posterior draws for the structural shocks conditional variances}
#'  \item{PR_TR}{an \code{MxMxNxS} array with the posterior draws for the transition matrix.}
#'  \item{xi}{an \code{MxTxNxS} array with the posterior draws for the regime allocation matrix.}
#'  \item{pi_0}{an \code{MxNxS} matrix with the posterior draws for the initial state probabilities}
#'  \item{sigma}{an \code{NxTxS} array with the posterior draws for the structural shocks conditional standard deviations' series over the sample period}
#' }
#' 
#' \code{last_draw} an object of class BSVARHMSH with the last draw of the current 
#' MCMC run as the starting value to be passed to the continuation of the MCMC 
#' estimation using \code{estimate()}. 
#' 
#' @seealso \code{\link{specify_bsvar_hmsh}}, \code{\link{specify_posterior_bsvar_hmsh}}, \code{\link{normalise}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @references 
#' 
#' Shang, F., and Woźniak, T. (2025) Identification Verification using Structural Vector Autoregressions with Heterogeneous Markov Switching Heteroskedasticity.
#' 
#' Chan, J.C.C., Koop, G, and Yu, X. (2024) Large Order-Invariant Bayesian VARs with Stochastic Volatility. \emph{Journal of Business & Economic Statistics}, \bold{42}, \doi{10.1080/07350015.2023.2252039}.
#' 
#' Lütkepohl, H., and Woźniak, T., (2020) Bayesian Inference for Structural Vector Autoregressions Identified by Markov-Switching Heteroskedasticity. \emph{Journal of Economic Dynamics and Control} \bold{113}, 103862, \doi{10.1016/j.jedc.2020.103862}.
#' 
#' Song, Y., and Woźniak, T., (2021) Markov Switching. \emph{Oxford Research Encyclopedia of Economics and Finance}, Oxford University Press, \doi{10.1093/acrefore/9780190625979.013.174}.
#' 
#' Waggoner, D.F., and Zha, T., (2003) A Gibbs sampler for structural vector autoregressions. \emph{Journal of Economic Dynamics and Control}, \bold{28}, 349--366, \doi{10.1016/S0165-1889(02)00168-9}.
#' 
#' Woźniak, T., and Droumaguet, M., (2024) Bayesian Assessment of Identifying Restrictions for Heteroskedastic Structural VARs
#' 
#' @method estimate BSVARHMSH
#' 
#' @examples
#' # simple workflow
#' ############################################################
#' specification  = specify_bsvar_hmsh$new(us_fiscal_lsuw, M = 2)
#' burn_in        = estimate(specification, 5)
#' posterior      = estimate(burn_in, 5)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar_hmsh$new(M = 2) |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) -> posterior
#' 
#' @export
estimate.BSVARHMSH <- function(specification, S, thin = 1, show_progress = TRUE) {
  
  # get the inputs to estimation
  prior               = specification$prior$get_prior()
  starting_values     = specification$starting_values$get_starting_values()
  VB                  = specification$identification$VB
  VA                  = specification$identification$VA
  data_matrices       = specification$data_matrices$get_data_matrices()
  finiteM             = specification$finiteM
  if (finiteM) {
    model             = "stationaryHMSH"
  } else {
    model             = "sparseHMSH"
  }
  normal              = specification$get_normal()
  
  # estimation
  qqq                 = .Call(`_bsvars_bsvar_hmsh_cpp`, S, data_matrices$Y, data_matrices$X, prior, VB, VA, starting_values, normal, thin, finiteM, TRUE, model, show_progress)
  
  specification$starting_values$set_starting_values(qqq$last_draw)
  output              = specify_posterior_bsvar_hmsh$new(specification, qqq$posterior)
  
  # reshape some of the outputs
  PR_TR               = array(NA, c(dim(starting_values$PR_TR), S))
  xi                  = array(NA, c(dim(starting_values$xi), S))
  SS                  = dim(qqq$posterior$PR_TR)[1]
  for (s in 1:SS) {
    PR_TR[,,,s]       = qqq$posterior$PR_TR[s,1][[1]]
    xi[,,,s]          = qqq$posterior$xi[s,1][[1]]
  }
  output$posterior$PR_TR = PR_TR
  output$posterior$xi    = xi
  
  # normalise output
  output              = normalise(output)
  
  return(output)
}





#' @inherit estimate.BSVARHMSH
#' 
#' @method estimate PosteriorBSVARHMSH
#' 
#' @param specification an object of class PosteriorBSVARHMSH generated using 
#' the \code{estimate()} function. This setup facilitates the continuation of 
#' the MCMC sampling starting from the last draw of the previous run.
#' 
#' @examples
#' # simple workflow
#' ############################################################
#' specification  = specify_bsvar_hmsh$new(us_fiscal_lsuw, M = 2)
#' burn_in        = estimate(specification, 5)
#' posterior      = estimate(burn_in, 5)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar_hmsh$new(M = 2) |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) -> posterior
#' 
#' @export
estimate.PosteriorBSVARHMSH <- function(specification, S, thin = 1, show_progress = TRUE) {
  
  # get the inputs to estimation
  prior               = specification$last_draw$prior$get_prior()
  starting_values     = specification$last_draw$starting_values$get_starting_values()
  VB                  = specification$last_draw$identification$VB
  VA                  = specification$last_draw$identification$VA
  data_matrices       = specification$last_draw$data_matrices$get_data_matrices()
  finiteM             = specification$last_draw$finiteM
  if (finiteM) {
    model             = "stationaryHMSH"
  } else {
    model             = "sparseHMSH"
  }
  normal              = specification$last_draw$get_normal()
  
  # estimation
  qqq                 = .Call(`_bsvars_bsvar_hmsh_cpp`, S, data_matrices$Y, data_matrices$X, prior, VB, VA, starting_values, normal, thin, finiteM, TRUE, model, show_progress)
  
  specification$last_draw$starting_values$set_starting_values(qqq$last_draw)
  output              = specify_posterior_bsvar_hmsh$new(specification$last_draw, qqq$posterior)
  
  # reshape some of the outputs
  PR_TR               = array(NA, c(dim(starting_values$PR_TR), S))
  xi                  = array(NA, c(dim(starting_values$xi), S))
  for (s in 1:S) {
    PR_TR[,,,s]       = qqq$posterior$PR_TR[s,1][[1]]
    xi[,,,s]          = qqq$posterior$xi[s,1][[1]]
  }
  output$posterior$PR_TR = PR_TR
  output$posterior$xi    = xi
  
  # normalise output
  output              = normalise(output)
  
  return(output)
}
