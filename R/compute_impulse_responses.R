
#' @title Computes posterior draws of impulse responses 
#'
#' @description Each of the draws from the posterior estimation of a model is transformed into
#' a draw from the posterior distribution of the impulse responses. 
#' 
#' @param posterior posterior estimation outcome - an object of either of the classes: 
#' PosteriorBSVAR, PosteriorBSVARMSH, PosteriorBSVARMIX, or PosteriorBSVARSV
#' obtained by running the \code{estimate} function. The interpretation depends on the normalisation of the shocks
#' using function \code{normalise_posterior()}. Verify if the default settings are appropriate.
#' @param horizon a positive integer number denoting the forecast horizon for the impulse responses computations.
#' @param standardise a logical value. If \code{TRUE}, the impulse responses are standardised 
#' so that the variables' own shocks at horizon 0 are equal to 1. Otherwise, the parameter estimates 
#' determine this magnitude.
#' 
#' @return An object of class PosteriorIR, that is, an \code{NxNx(horizon+1)xS} array with attribute PosteriorIR 
#' containing \code{S} draws of the impulse responses.
#'
#' @seealso \code{\link{estimate}}, \code{\link{normalise_posterior}}
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
#' specification  = specify_bsvar$new(us_fiscal_lsuw, p = 1)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 50)
#' 
#' # compute impulse responses 2 years ahead
#' irf           = compute_impulse_responses(posterior, horizon = 8)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 50) |> 
#'   compute_impulse_responses(horizon = 8) -> ir
#' 
#' @export
compute_impulse_responses <- function(posterior, horizon, standardise = FALSE) {
  
  stopifnot("Argument posterior must contain estimation output from the estimate function." = any(class(posterior)[1] == c("PosteriorBSVAR", "PosteriorBSVARMSH", "PosteriorBSVARMIX", "PosteriorBSVARSV")))
  stopifnot("The posterior output must be normalised for the impulse responses to be interpretable." = posterior$is_normalised())
  stopifnot("Argument horizon must be a positive integer number." = horizon > 0 & horizon %% 1 == 0)
  
  posterior_B     = posterior$posterior$B
  posterior_A     = posterior$posterior$A
  N               = dim(posterior_A)[1]
  p               = (dim(posterior_A)[2] - 1) / N
  S               = dim(posterior_A)[3]
  
  qqq             = .Call(`_bsvars_bsvars_ir`, posterior_B, posterior_A, horizon, p, standardise)
  
  irfs            = array(NA, c(N, N, horizon + 1, S))
  for (s in 1:S) irfs[,,,s] = qqq[s][[1]]
  class(irfs)     = "PosteriorIR"
  
  return(irfs)
}

