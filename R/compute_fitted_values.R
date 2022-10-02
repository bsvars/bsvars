
#' @title Computes posterior draws of dependent variables' fitted values
#'
#' @description Each of the draws from the posterior estimation of a model is transformed into
#' a draw from the posterior distribution of the fitted values. 
#' 
#' @param posterior posterior estimation outcome - an object of either of the classes: 
#' PosteriorBSVAR, PosteriorBSVAR-MSH, PosteriorBSVAR-MIX, or PosteriorBSVAR-SV
#' obtained by running one of the \code{estimate_bsvar_*} functions.
#' 
#' @return An object of class PosteriorFitted, that is, an \code{NxTxS} array with attribute PosteriorFitted 
#' containing \code{S} draws of the fitted values.
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
#' # compute dependent variables' fitted values
#' fitted         = compute_fitted_values(posterior)
#' 
#' @export
compute_fitted_values <- function(posterior) {
  
  stopifnot("Argument posterior must contain estimation output from one of the estimate_bsvar* functions." = any(class(posterior)[1] == c("PosteriorBSVAR", "PosteriorBSVAR-MSH", "PosteriorBSVAR-MIX", "PosteriorBSVAR-SV")))
  
  posterior_A     = posterior$posterior$A
  X               = posterior$last_draw$data_matrices$X
  
  fv              = .Call(`_bsvars_bsvars_fitted_values`, posterior_A, X)
  class(fv)       = "PosteriorFitted"
  
  return(fv)
}

