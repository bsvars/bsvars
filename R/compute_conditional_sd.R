
#' @title Computes posterior draws of structural shock conditional standard deviations
#'
#' @description Each of the draws from the posterior estimation of a model is transformed into
#' a draw from the posterior distribution of the structural shock conditional standard deviations. 
#' 
#' @param posterior posterior estimation outcome - an object of either of the classes: 
#' PosteriorBSVAR, PosteriorBSVAR-MSH, PosteriorBSVAR-MIX, or PosteriorBSVAR-SV
#' obtained by running one of the \code{estimate_bsvar_*} functions.
#' 
#' @return An object of class PosteriorSigma, that is, an \code{NxTxS} array with attribute PosteriorSigma 
#' containing \code{S} draws of the structural shock conditional standard deviations.
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
#' # compute structural shocks' conditional standard deviations
#' sigma          = compute_conditional_sd(posterior)
#' 
#' @export
compute_conditional_sd <- function(posterior) {
  
  stopifnot("Argument posterior must contain estimation output from one of the estimate_bsvar* functions for heteroskedastic model." = any(class(posterior)[1] == c("PosteriorBSVAR", "PosteriorBSVAR-MSH", "PosteriorBSVAR-MIX", "PosteriorBSVAR-SV")))
  
  Y     = posterior$last_draw$data_matrices$Y
  N     = nrow(Y)
  T     = ncol(Y)
  
  if (class(posterior)[1] == "PosteriorBSVAR") {
    posterior_sigma       = matrix(1, N, T)
    message("The model is homoskedastic. Returning an NxT matrix of conditional sd all equal to 1.")
  } else {
    posterior_sigma       = posterior$posterior$sigma
  }
  class(posterior_sigma)  = "PosteriorSigma"
  
  return(posterior_sigma)
}

