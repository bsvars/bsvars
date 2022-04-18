

#' @title Waggoner & Zha (2003) row signs normalisation of the posterior draws for matrix \eqn{B}
#'
#' @description Normalises the sign of rows of matrix \eqn{B} MCMC draws, 
#'  provided as the first argument \code{posterior_B}, relative to matrix
#'  \code{B_hat}, provided as the second argument of the function. The implemented
#'  procedure proposed by Waggoner, Zha (2003) normalises the MCMC output in an
#'  optimal way leading to the unimodal posterior. Only normalised MCMC output is 
#'  suitable for the computations of the posterior characteristics of the \eqn{B}
#'  matrix elements and their functions such as the impulse response functions and other 
#'  economically interpretable values. 
#' 
#' @param posterior_B an \code{NxNxS} array containing \code{S} draws from the posterior 
#' distribution of the \code{NxN} matrix of contemporaneous relationships \eqn{B}. 
#' These draws are to be normalised with respect to:
#' @param B_hat an \code{NxN} matrix specified by the user to have the desired row signs
#' 
#' @return Nothing. The normalised elements overwrite the corresponding elements of 
#' the first argument \code{posterior_B} by reference.
#' 
#' @seealso \code{\link{bsvar}}, \code{\link{bsvar_msh}}, \code{\link{bsvar_sv}}, \code{\link{bsvar_ihmm}}
#' \code{\link{bsvar_mix}}, \code{\link{bsvar_dph}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @references 
#' Waggoner, D.F., and Zha, T., (2003) Likelihood Preserving Normalization in Multiple Equation Models. 
#' \emph{Journal of Econometrics}, \bold{114}(2), 329--47, \doi{https://doi.org/10.1016/S0304-4076(03)00087-3}.
#'
#' @export
normalisation_wz2003 <- function(posterior_B, B_hat) {
  invisible(.Call(`_bsvars_normalisation_wz2003`, posterior_B, B_hat))
}
