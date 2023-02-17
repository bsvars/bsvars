
#' @title Computes posterior draws of historical decompositions
#'
#' @description Each of the draws from the posterior estimation of a model is transformed into
#' a draw from the posterior distribution of the historical decompositions. 
#' 
#' @param posterior posterior estimation outcome - an object of either of the classes: 
#' PosteriorBSVAR, PosteriorBSVARMSH, PosteriorBSVARMIX, or PosteriorBSVARSV
#' obtained by running the \code{estimate} function. The interpretation depends on the normalisation of the shocks
#' using function \code{normalise_posterior()}. Verify if the default settings are appropriate.
#' 
#' @return An object of class PosteriorHD, that is, an \code{NxNxTxS} array with attribute PosteriorHD 
#' containing \code{S} draws of the historical decompositions.
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
#' # compute historical decompositions
#' hd            = compute_historical_decompositions(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 50) |> 
#'   estimate(S = 100) |> 
#'   compute_historical_decompositions() -> hd
#' 
#' @export
compute_historical_decompositions <- function(posterior) {
  
  stopifnot("Argument posterior must contain estimation output from the estimate function." = any(class(posterior)[1] == c("PosteriorBSVAR", "PosteriorBSVARMSH", "PosteriorBSVARMIX", "PosteriorBSVARSV")))
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
  ir              = .Call(`_bsvars_bsvars_ir`, posterior_B, posterior_A, T, p, TRUE)
  
  qqq             = .Call(`_bsvars_bsvars_hd`, ir, ss)

  hd              = array(NA, c(N, N, T, S))
  for (s in 1:S) hd[,,,s] = qqq[s][[1]]
  class(hd)       = "PosteriorHD"

  return(hd)
}
