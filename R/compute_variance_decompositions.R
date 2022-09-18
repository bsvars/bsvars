
#' @title Computes posterior draws of the forecast error variance decomposition
#'
#' @description Each of the draws from the posterior estimation of a model
#' is transformed into a draw from the posterior distribution of the forecast error variance decomposition. 
#' 
#' @param posterior posterior estimation outcome - an object of one of the classes: 
#' PosteriorBSVAR, PosteriorBSVAR-MSH, PosteriorBSVAR-MIX, or PosteriorBSVAR-SV
#' obtained by running one of the \code{estimate_bsvar_*} functions - these draws must be normalised
#' using function \code{normalise_posterior()} for the forecast error variance decomposition to be interpretable.
#' @param horizon a positive integer number denoting the forecast horizon for the impulse responses computations.
#' 
#' @return An object of class PosteriorFEVD, that is, an \code{NxNx(horizon+1)xS} array with attribute PosteriorFEVD 
#' containing \code{S} draws of the forecast error variance decomposition.
#'
#' @seealso \code{\link{compute_impulse_responses}}, \code{\link{estimate_bsvar}}, \code{\link{estimate_bsvar_msh}}, \code{\link{estimate_bsvar_sv}}, \code{\link{estimate_bsvar_mix}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @references 
#' Kilian, L., & Lütkepohl, H. (2017). Structural VAR Tools, Chapter 4, In: Structural vector autoregressive analysis. Cambridge University Press.
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar$new(us_fiscal_lsuw, p = 4)
#' 
#' # run the burn-in
#' burn_in        = estimate_bsvar(10, specification)
#' 
#' # estimate the model
#' posterior      = estimate_bsvar(50, burn_in$get_last_draw())
#' 
#' # normalise the posterior
#' BB             = posterior$last_draw$starting_values$B      # get the last draw of B
#' B_hat          = diag(sign(diag(BB))) %*% BB                # set positive diagonal elements
#' normalise_posterior(posterior, B_hat)              # draws in posterior are normalised
#' 
#' # compute forecast error variance decomposition 5 years ahead
#' fevd           = compute_variance_decompositions(posterior, horizon = 20)
#' 
#' @export
compute_variance_decompositions <- function(posterior, horizon) {
  
  stopifnot("Argument x must contain estimation output from one of the estimate_bsvar* functions." = any(class(posterior)[1] == c("PosteriorBSVAR", "PosteriorBSVAR-MSH", "PosteriorBSVAR-MIX", "PosteriorBSVAR-SV")))
  stopifnot("The posterior output must be normalised for the impulse responses to be interpretable." = posterior$is_normalised())
  stopifnot("Argument horizon must be a positive integer number." = horizon > 0 & horizon %% 1 == 0)

  posterior_B     = posterior$posterior$B
  posterior_A     = posterior$posterior$A
  N               = dim(posterior_A)[1]
  p               = (dim(posterior_A)[2] - 1) / N
  S               = dim(posterior_A)[3]
  
  posterior_irf   = .Call(`_bsvars_bsvars_ir`, posterior_B, posterior_A, horizon, p)
  qqq             = .Call(`_bsvars_bsvars_fevd`, posterior_irf)
  
  fevd            = array(NA, c(N, N, horizon + 1, S))
  for (s in 1:S) fevd[,,,s] = qqq[s][[1]]
  class(fevd)     = "PosteriorFEVD"
  
  return(fevd)
}
