
#' @title Computes posterior draws of structural shocks
#'
#' @description Each of the draws from the posterior estimation of a model is transformed into
#' a draw from the posterior distribution of the structural shocks. 
#' 
#' @param posterior posterior estimation outcome - an object of either of the classes: 
#' PosteriorBSVAR, PosteriorBSVAR-MSH, PosteriorBSVAR-MIX, or PosteriorBSVAR-SV
#' obtained by running one of the \code{estimate_bsvar_*} functions. The draws must be normalised
#' using function \code{normalise_posterior()} for the structural shocks to be interpretable.
#' 
#' @return An object of class PosteriorShocks, that is, an \code{NxTxS} array with attribute PosteriorShocks 
#' containing \code{S} draws of the structural shocks.
#'
#' @seealso \code{\link{estimate_bsvar}}, \code{\link{estimate_bsvar_msh}}, \code{\link{estimate_bsvar_sv}}, \code{\link{estimate_bsvar_mix}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
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
#' burn_in        = estimate_bsvar(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate_bsvar(burn_in$get_last_draw(), 50)
#' 
#' # compute impulse responses 5 years ahead
#' shocks         = compute_structural_shocks(posterior)
#' 
#' @export
compute_structural_shocks <- function(posterior) {
  
  stopifnot("Argument posterior must contain estimation output from one of the estimate_bsvar* functions." = any(class(posterior)[1] == c("PosteriorBSVAR", "PosteriorBSVAR-MSH", "PosteriorBSVAR-MIX", "PosteriorBSVAR-SV")))
  stopifnot("The posterior output must be normalised for the structural shocks to be interpretable." = posterior$is_normalised())

  posterior_B     = posterior$posterior$B
  posterior_A     = posterior$posterior$A
  Y               = posterior$last_draw$data_matrices$Y
  X               = posterior$last_draw$data_matrices$X
  
  ss              = .Call(`_bsvars_bsvars_structural_shocks`, posterior_B, posterior_A, Y, X)
  class(ss)       = "PosteriorShocks"
  
  return(ss)
}
