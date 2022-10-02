
#' @title Computes posterior draws of historical decompositions
#'
#' @description Each of the draws from the posterior estimation of a model is transformed into
#' a draw from the posterior distribution of the historical decompositions. 
#' 
#' @param posterior posterior estimation outcome - an object of either of the classes: 
#' PosteriorBSVAR, PosteriorBSVAR-MSH, PosteriorBSVAR-MIX, or PosteriorBSVAR-SV
#' obtained by running one of the \code{estimate_bsvar_*} functions. The draws must be normalised
#' using function \code{normalise_posterior()} for the historical decompositions to be interpretable.
#' 
#' @return An object of class PosteriorHD, that is, an \code{NxNxTxS} array with attribute PosteriorHD 
#' containing \code{S} draws of the historical decompositions.
#'
#' @seealso \code{\link{estimate_bsvar}}, \code{\link{estimate_bsvar_msh}}, \code{\link{estimate_bsvar_sv}}, \code{\link{estimate_bsvar_mix}}
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
#' burn_in        = estimate_bsvar(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate_bsvar(burn_in$get_last_draw(), 50)
#' 
#' # compute historical decompositions
#' hd            = compute_historical_decompositions(posterior)
#' 
#' @export
compute_historical_decompositions <- function(posterior) {
  
  stopifnot("Argument posterior must contain estimation output from one of the estimate_bsvar* functions." = any(class(posterior)[1] == c("PosteriorBSVAR", "PosteriorBSVAR-MSH", "PosteriorBSVAR-MIX", "PosteriorBSVAR-SV")))
  stopifnot("The posterior output must be normalised for the historical decompositions to be interpretable." = posterior$is_normalised())
  
  posterior_B     = posterior$posterior$B
  posterior_A     = posterior$posterior$A
  
  Y               = posterior$last_draw$data_matrices$Y
  X               = posterior$last_draw$data_matrices$X
  
  N               = nrow(Y)
  T               = ncol(Y)
  p               = (dim(posterior_A)[2] - 1) / N
  S               = dim(posterior_A)[3]
  
  ss              = .Call(`_bsvars_bsvars_structural_shocks`, posterior_B, posterior_A, Y, X)
  ir              = .Call(`_bsvars_bsvars_ir`, posterior_B, posterior_A, T, p)
  
  qqq             = .Call(`_bsvars_bsvars_hd`, ir, ss)

  hd              = array(NA, c(N, N, T, S))
  for (s in 1:S) hd[,,,s] = qqq[s][[1]]
  class(hd)       = "PosteriorHD"

  return(hd)
}
