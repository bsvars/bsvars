
#' @title Computes posterior draws of structural shock conditional standard deviations
#'
#' @description Each of the draws from the posterior estimation of a model is transformed into
#' a draw from the posterior distribution of the structural shock conditional standard deviations. 
#' 
#' @param posterior posterior estimation outcome - an object of either of the classes: 
#' PosteriorBSVAR, PosteriorBSVARMSH, PosteriorBSVARMIX, or PosteriorBSVARSV
#' obtained by running the \code{estimate} function. The interpretation depends on the normalisation of the shocks
#' using function \code{normalise_posterior()}. Verify if the default settings are appropriate.
#' 
#' @return An object of class PosteriorSigma, that is, an \code{NxTxS} array with attribute PosteriorSigma 
#' containing \code{S} draws of the structural shock conditional standard deviations.
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
#' specification  = specify_bsvar$new(us_fiscal_lsuw, p = 1)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in$get_last_draw(), 50)
#' 
#' # compute structural shocks' conditional standard deviations
#' sigma          = compute_conditional_sd(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 50) |> 
#'   estimate(S = 100) |> 
#'   compute_conditional_sd() -> csd
#' 
#' @export
compute_conditional_sd <- function(posterior) {
  
  stopifnot("Argument posterior must contain estimation output from the estimate function for heteroskedastic model." = any(class(posterior)[1] == c("PosteriorBSVAR", "PosteriorBSVARMSH", "PosteriorBSVARMIX", "PosteriorBSVARSV")))
  
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

