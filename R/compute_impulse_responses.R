
#' @title Computes posterior draws of impulse responses 
#'
#' @description Each of the draws from the posterior estimation of models from 
#' packages \pkg{bsvars} or \pkg{bsvarSIGNs} is transformed into
#' a draw from the posterior distribution of the impulse responses. 
#' 
#' @param posterior posterior estimation outcome obtained by running the \code{estimate} function. 
#' The interpretation depends on the normalisation of the shocks
#' using function \code{normalise_posterior()}. Verify if the default settings are appropriate.
#' @param horizon a positive integer number denoting the forecast horizon for the impulse responses computations.
#' @param standardise a logical value. If \code{TRUE}, the impulse responses are standardised 
#' so that the variables' own shocks at horizon 0 are equal to 1. Otherwise, the parameter estimates 
#' determine this magnitude.
#' 
#' @return An object of class PosteriorIR, that is, an \code{NxNx(horizon+1)xS} array with attribute PosteriorIR 
#' containing \code{S} draws of the impulse responses.
#'
#' @seealso \code{\link{estimate}}, \code{\link{normalise_posterior}}, \code{\link{summary}}
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
#' posterior      = estimate(burn_in, 20)
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
#'   estimate(S = 20) |> 
#'   compute_impulse_responses(horizon = 8) -> ir
#' 
#' @export
compute_impulse_responses <- function(posterior, horizon, standardise = FALSE) {
  stopifnot("Argument horizon must be a positive integer number." = horizon > 0 & horizon %% 1 == 0)
  stopifnot("Argument standardise must be a logical value." = is.logical(standardise) & !is.na(standardise))
  UseMethod("compute_impulse_responses", posterior)
}





#' @inherit compute_impulse_responses
#' @method compute_impulse_responses PosteriorBSVAR
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVAR} obtained by running the \code{estimate} function.
#' 
#' @export
compute_impulse_responses.PosteriorBSVAR <- function(posterior, horizon, standardise = FALSE) {

  Y               = posterior$last_draw$data_matrices$Y
  posterior_B     = posterior$posterior$B
  posterior_A     = posterior$posterior$A
  N               = dim(posterior_A)[1]
  p               = posterior$last_draw$p
  S               = dim(posterior_A)[3]

  qqq             = .Call(`_bsvars_bsvars_ir`, posterior_B, posterior_A, horizon, p, standardise)

  irfs            = array(NA, c(N, N, horizon + 1, S), dimnames = list(rownames(Y), rownames(Y), 0:horizon, 1:S))
  for (s in 1:S) irfs[,,,s] = qqq[s][[1]]
  class(irfs)     = "PosteriorIR"

  return(irfs)
}







#' @inherit compute_impulse_responses
#' @method compute_impulse_responses PosteriorBSVARMSH
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARMSH} obtained by running the \code{estimate} function.
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar_msh$new(us_fiscal_lsuw, p = 1, M = 2)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' 
#' # compute impulse responses
#' irfs            = compute_impulse_responses(posterior, 4)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_msh$new(p = 1, M = 2) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_impulse_responses(horizon = 4) -> irfs
#'   
#' @export
compute_impulse_responses.PosteriorBSVARMSH <- function(posterior, horizon, standardise = FALSE) {

  Y               = posterior$last_draw$data_matrices$Y
  posterior_B     = posterior$posterior$B
  posterior_A     = posterior$posterior$A
  N               = dim(posterior_A)[1]
  p               = posterior$last_draw$p
  S               = dim(posterior_A)[3]

  qqq             = .Call(`_bsvars_bsvars_ir`, posterior_B, posterior_A, horizon, p, standardise)

  irfs            = array(NA, c(N, N, horizon + 1, S), dimnames = list(rownames(Y), rownames(Y), 0:horizon, 1:S))
  for (s in 1:S) irfs[,,,s] = qqq[s][[1]]
  class(irfs)     = "PosteriorIR"

  return(irfs)
}








#' @inherit compute_impulse_responses
#' @method compute_impulse_responses PosteriorBSVARMIX
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARMIX} obtained by running the \code{estimate} function.
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar_mix$new(us_fiscal_lsuw, p = 1, M = 2)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' 
#' # compute impulse responses
#' irfs            = compute_impulse_responses(posterior, 4)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_mix$new(p = 1, M = 2) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_impulse_responses(horizon = 4) -> irfs
#'   
#' @export
compute_impulse_responses.PosteriorBSVARMIX <- function(posterior, horizon, standardise = FALSE) {
  
  Y               = posterior$last_draw$data_matrices$Y
  posterior_B     = posterior$posterior$B
  posterior_A     = posterior$posterior$A
  N               = dim(posterior_A)[1]
  p               = posterior$last_draw$p
  S               = dim(posterior_A)[3]
  
  qqq             = .Call(`_bsvars_bsvars_ir`, posterior_B, posterior_A, horizon, p, standardise)
  
  irfs            = array(NA, c(N, N, horizon + 1, S), dimnames = list(rownames(Y), rownames(Y), 0:horizon, 1:S))
  for (s in 1:S) irfs[,,,s] = qqq[s][[1]]
  class(irfs)     = "PosteriorIR"
  
  return(irfs)
}








#' @inherit compute_impulse_responses
#' @method compute_impulse_responses PosteriorBSVARSV
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARSV} obtained by running the \code{estimate} function.
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar_sv$new(us_fiscal_lsuw, p = 1)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' 
#' # compute impulse responses
#' irfs            = compute_impulse_responses(posterior, 4)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_impulse_responses(horizon = 4) -> irfs
#'   
#' @export
compute_impulse_responses.PosteriorBSVARSV <- function(posterior, horizon, standardise = FALSE) {
  
  Y               = posterior$last_draw$data_matrices$Y
  posterior_B     = posterior$posterior$B
  posterior_A     = posterior$posterior$A
  N               = dim(posterior_A)[1]
  p               = posterior$last_draw$p
  S               = dim(posterior_A)[3]
  
  qqq             = .Call(`_bsvars_bsvars_ir`, posterior_B, posterior_A, horizon, p, standardise)
  
  irfs            = array(NA, c(N, N, horizon + 1, S), dimnames = list(rownames(Y), rownames(Y), 0:horizon, 1:S))
  for (s in 1:S) irfs[,,,s] = qqq[s][[1]]
  class(irfs)     = "PosteriorIR"
  
  return(irfs)
}





#' @inherit compute_impulse_responses
#' @method compute_impulse_responses PosteriorBSVART
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVART} obtained by running the \code{estimate} function.
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar_t$new(us_fiscal_lsuw, p = 1)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' 
#' # compute impulse responses
#' irfs            = compute_impulse_responses(posterior, 4)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_t$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_impulse_responses(horizon = 4) -> irfs
#'   
#' @export
compute_impulse_responses.PosteriorBSVART <- function(posterior, horizon, standardise = FALSE) {
  
  Y               = posterior$last_draw$data_matrices$Y
  posterior_B     = posterior$posterior$B
  posterior_A     = posterior$posterior$A
  N               = dim(posterior_A)[1]
  p               = posterior$last_draw$p
  S               = dim(posterior_A)[3]
  
  qqq             = .Call(`_bsvars_bsvars_ir`, posterior_B, posterior_A, horizon, p, standardise)
  
  irfs            = array(NA, c(N, N, horizon + 1, S), dimnames = list(rownames(Y), rownames(Y), 0:horizon, 1:S))
  for (s in 1:S) irfs[,,,s] = qqq[s][[1]]
  class(irfs)     = "PosteriorIR"
  
  return(irfs)
}

