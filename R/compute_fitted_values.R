
#' @title Computes posterior draws of dependent variables' fitted values
#'
#' @description Each of the draws from the posterior estimation of a model is transformed into
#' a draw from the posterior distribution of the fitted values. 
#' 
#' @param posterior posterior estimation outcome - an object of either of the classes: 
#' PosteriorBSVAR, PosteriorBSVARMSH, PosteriorBSVARMIX, or PosteriorBSVARSV
#' obtained by running the \code{estimate} function.
#' 
#' @return An object of class PosteriorFitted, that is, an \code{NxTxS} array with attribute PosteriorFitted 
#' containing \code{S} draws of the fitted values.
#'
#' @seealso \code{\link{estimate}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar$new(us_fiscal_lsuw, p = 1)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 50)
#' 
#' # compute dependent variables' fitted values
#' fitted         = compute_fitted_values(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 50) |> 
#'   estimate(S = 100) |> 
#'   compute_fitted_values() -> fitted
#' 
#' @export
compute_fitted_values <- function(posterior) {
  
  stopifnot("Argument posterior must contain estimation output from the estimate function." = any(class(posterior)[1] == c("PosteriorBSVAR", "PosteriorBSVARMSH", "PosteriorBSVARMIX", "PosteriorBSVARSV")))
  
  posterior_A     = posterior$posterior$A
  X               = posterior$last_draw$data_matrices$X
  
  fv              = .Call(`_bsvars_bsvars_fitted_values`, posterior_A, X)
  class(fv)       = "PosteriorFitted"
  
  return(fv)
}

