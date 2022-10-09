
#' @title Computes posterior draws of structural shocks
#'
#' @description Each of the draws from the posterior estimation of a model is transformed into
#' a draw from the posterior distribution of the structural shocks. 
#' 
#' @param posterior posterior estimation outcome - an object of either of the classes: 
#' PosteriorBSVAR, PosteriorBSVARMSH, PosteriorBSVARMIX, or PosteriorBSVARSV
#' obtained by running the \code{estimate} function. The interpretation depends on the normalisation of the shocks
#' using function \code{normalise_posterior()}. Verify if the default settings are appropriate.
#' 
#' @return An object of class PosteriorShocks, that is, an \code{NxTxS} array with attribute PosteriorShocks 
#' containing \code{S} draws of the structural shocks.
#'
#' @seealso \code{\link{estimate}}, \code{\link{normalise_posterior}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar$new(us_fiscal_lsuw, p = 2)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in$get_last_draw(), 50)
#' 
#' # compute structural shocks
#' shocks         = compute_structural_shocks(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 50) |> 
#'   compute_structural_shocks() -> ss
#' 
#' @export
compute_structural_shocks <- function(posterior) {
  
  stopifnot("Argument posterior must contain estimation output from the estimate function." = any(class(posterior)[1] == c("PosteriorBSVAR", "PosteriorBSVARMSH", "PosteriorBSVARMIX", "PosteriorBSVARSV")))
  stopifnot("The posterior output must be normalised for the structural shocks to be interpretable." = posterior$is_normalised())

  posterior_B     = posterior$posterior$B
  posterior_A     = posterior$posterior$A
  Y               = posterior$last_draw$data_matrices$Y
  X               = posterior$last_draw$data_matrices$X
  
  ss              = .Call(`_bsvars_bsvars_structural_shocks`, posterior_B, posterior_A, Y, X)
  class(ss)       = "PosteriorShocks"
  
  return(ss)
}

