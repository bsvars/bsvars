
#' @title Computes posterior draws of the forecast error variance decomposition
#'
#' @description Each of the draws from the posterior estimation of models from
#' packages \pkg{bsvars} or \pkg{bsvarSIGNs}
#' is transformed into a draw from the posterior distribution of the forecast error variance decomposition. 
#' 
#' @param posterior posterior estimation outcome obtained by running the \code{estimate} function. 
#' The interpretation depends on the normalisation of the shocks
#' using function \code{normalise_posterior()}. Verify if the default settings are appropriate.
#' @param horizon a positive integer number denoting the forecast horizon for the impulse responses computations.
#' 
#' @return An object of class PosteriorFEVD, that is, an \code{NxNx(horizon+1)xS} array with attribute PosteriorFEVD 
#' containing \code{S} draws of the forecast error variance decomposition.
#'
#' @seealso \code{\link{compute_impulse_responses}}, \code{\link{estimate}}, \code{\link{normalise_posterior}}, \code{\link{summary}}
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
#' specification  = specify_bsvar$new(us_fiscal_lsuw, p = 2)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 50)
#' 
#' # compute forecast error variance decomposition 2 years ahead
#' fevd           = compute_variance_decompositions(posterior, horizon = 8)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new(p = 2) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 50) |> 
#'   compute_variance_decompositions(horizon = 8) -> fevd
#' 
#' @export
compute_variance_decompositions <- function(posterior, horizon) {
  stopifnot("Argument horizon must be a positive integer number." = horizon > 0 & horizon %% 1 == 0)
  UseMethod("compute_variance_decompositions", posterior)
}




#' @inherit compute_variance_decompositions
#' @method compute_variance_decompositions PosteriorBSVAR
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVAR} obtained by running the \code{estimate} function.
#' 
#' @export
compute_variance_decompositions.PosteriorBSVAR <- function(posterior, horizon) {

  posterior_B     = posterior$posterior$B
  posterior_A     = posterior$posterior$A
  N               = dim(posterior_A)[1]
  p               = posterior$last_draw$p
  S               = dim(posterior_A)[3]

  posterior_irf   = .Call(`_bsvars_bsvars_ir`, posterior_B, posterior_A, horizon, p, TRUE)
  qqq             = .Call(`_bsvars_bsvars_fevd`, posterior_irf)

  fevd            = array(NA, c(N, N, horizon + 1, S))
  for (s in 1:S) fevd[,,,s] = qqq[s][[1]]
  class(fevd)     = "PosteriorFEVD"

  return(fevd)
}





#' @inherit compute_variance_decompositions
#' @method compute_variance_decompositions PosteriorBSVARMSH
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARMSH} obtained by running the \code{estimate} function.
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar_msh$new(us_fiscal_lsuw, p = 2, M = 2)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 50)
#' 
#' # compute forecast error variance decomposition 2 years ahead
#' fevd           = compute_variance_decompositions(posterior, horizon = 8)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_msh$new(p = 2, M = 2) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 50) |> 
#'   compute_variance_decompositions(horizon = 8) -> fevd
#' 
#' @export
compute_variance_decompositions.PosteriorBSVARMSH <- function(posterior, horizon) {
  
  posterior_B     = posterior$posterior$B
  posterior_A     = posterior$posterior$A
  N               = dim(posterior_A)[1]
  p               = posterior$last_draw$p
  S               = dim(posterior_A)[3]
  
  posterior_irf   = .Call(`_bsvars_bsvars_ir`, posterior_B, posterior_A, horizon, p, TRUE)
  qqq             = .Call(`_bsvars_bsvars_fevd`, posterior_irf)
  
  fevd            = array(NA, c(N, N, horizon + 1, S))
  for (s in 1:S) fevd[,,,s] = qqq[s][[1]]
  class(fevd)     = "PosteriorFEVD"
  
  return(fevd)
}









#' @inherit compute_variance_decompositions
#' @method compute_variance_decompositions PosteriorBSVARMIX
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARMIX} obtained by running the \code{estimate} function.
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar_mix$new(us_fiscal_lsuw, p = 2, M = 2)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 50)
#' 
#' # compute forecast error variance decomposition 2 years ahead
#' fevd           = compute_variance_decompositions(posterior, horizon = 8)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_mix$new(p = 2, M = 2) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 50) |> 
#'   compute_variance_decompositions(horizon = 8) -> fevd
#' 
#' @export
compute_variance_decompositions.PosteriorBSVARMIX <- function(posterior, horizon) {
  
  posterior_B     = posterior$posterior$B
  posterior_A     = posterior$posterior$A
  N               = dim(posterior_A)[1]
  p               = posterior$last_draw$p
  S               = dim(posterior_A)[3]
  
  posterior_irf   = .Call(`_bsvars_bsvars_ir`, posterior_B, posterior_A, horizon, p, TRUE)
  qqq             = .Call(`_bsvars_bsvars_fevd`, posterior_irf)
  
  fevd            = array(NA, c(N, N, horizon + 1, S))
  for (s in 1:S) fevd[,,,s] = qqq[s][[1]]
  class(fevd)     = "PosteriorFEVD"
  
  return(fevd)
}



#' @inherit compute_variance_decompositions
#' @method compute_variance_decompositions PosteriorBSVARSV
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARSV} obtained by running the \code{estimate} function.
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar_sv$new(us_fiscal_lsuw, p = 2)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 50)
#' 
#' # compute forecast error variance decomposition 2 years ahead
#' fevd           = compute_variance_decompositions(posterior, horizon = 8)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new(p = 2) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 50) |> 
#'   compute_variance_decompositions(horizon = 8) -> fevd
#' 
#' @export
compute_variance_decompositions.PosteriorBSVARSV <- function(posterior, horizon) {
  
  posterior_B     = posterior$posterior$B
  posterior_A     = posterior$posterior$A
  N               = dim(posterior_A)[1]
  p               = posterior$last_draw$p
  S               = dim(posterior_A)[3]
  
  posterior_irf   = .Call(`_bsvars_bsvars_ir`, posterior_B, posterior_A, horizon, p, TRUE)
  qqq             = .Call(`_bsvars_bsvars_fevd`, posterior_irf)
  
  fevd            = array(NA, c(N, N, horizon + 1, S))
  for (s in 1:S) fevd[,,,s] = qqq[s][[1]]
  class(fevd)     = "PosteriorFEVD"
  
  return(fevd)
}
