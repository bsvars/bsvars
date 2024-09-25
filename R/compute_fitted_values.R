
#' @title Computes posterior draws from data predictive density
#'
#' @description Each of the draws from the posterior estimation of models from 
#' packages \pkg{bsvars} or \pkg{bsvarSIGNs} is transformed into
#' a draw from the data predictive density. 
#' 
#' @param posterior posterior estimation outcome
#' obtained by running the \code{estimate} function.
#' 
#' @return An object of class \code{PosteriorFitted}, that is, an \code{NxTxS} 
#' array with attribute \code{PosteriorFitted} containing \code{S} draws from 
#' the data predictive density.
#'
#' @seealso \code{\link{estimate}}, \code{\link{summary}}
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
#' posterior      = estimate(burn_in, 20)
#' 
#' # compute draws from in-sample predictive density
#' fitted         = compute_fitted_values(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_fitted_values() -> fitted
#' 
#' @export
compute_fitted_values <- function(posterior) {
  UseMethod("compute_fitted_values", posterior)
}




#' @method compute_fitted_values PosteriorBSVAR
#' 
#' @title Computes posterior draws from data predictive density
#'
#' @description Each of the draws from the posterior estimation of models from 
#' packages \pkg{bsvars} or \pkg{bsvarSIGNs} is transformed into
#' a draw from the data predictive density. 
#' 
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVAR} obtained by running the \code{estimate} function.
#' 
#' @return An object of class \code{PosteriorFitted}, that is, an \code{NxTxS} 
#' array with attribute \code{PosteriorFitted} containing \code{S} draws from 
#' the data predictive density.
#'
#' @seealso \code{\link{estimate}}, \code{\link{summary}}
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
#' posterior      = estimate(burn_in, 20)
#' 
#' # compute draws from in-sample predictive density
#' fitted         = compute_fitted_values(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_fitted_values() -> fitted
#' 
#' @export
compute_fitted_values.PosteriorBSVAR <- function(posterior) {

  Y               = posterior$last_draw$data_matrices$Y
  posterior_A     = posterior$posterior$A
  posterior_B     = posterior$posterior$B

  N               = dim(posterior_A)[1]
  T               = dim(posterior$last_draw$data_matrices$X)[2]
  S               = dim(posterior_A)[3]
  posterior_sigma = array(1, c(N, T, S))
  X               = posterior$last_draw$data_matrices$X

  fv              = .Call(`_bsvars_bsvars_fitted_values`, posterior_A, posterior_B, posterior_sigma, X)
  class(fv)       = "PosteriorFitted"
  S               = dim(posterior_A)[3]      
  dimnames(fv)    = list(rownames(Y), colnames(Y), 1:S)

  return(fv)
}




#' @method compute_fitted_values PosteriorBSVARMSH
#' 
#' @title Computes posterior draws from data predictive density
#'
#' @description Each of the draws from the posterior estimation of models from 
#' packages \pkg{bsvars} or \pkg{bsvarSIGNs} is transformed into
#' a draw from the data predictive density. 
#' 
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARMSH} obtained by running the \code{estimate} function.
#' 
#' @return An object of class \code{PosteriorFitted}, that is, an \code{NxTxS} 
#' array with attribute \code{PosteriorFitted} containing \code{S} draws from 
#' the data predictive density.
#'
#' @seealso \code{\link{estimate}}, \code{\link{summary}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
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
#' # compute draws from in-sample predictive density
#' csd     = compute_fitted_values(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_msh$new(p = 1, M = 2) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_fitted_values() -> csd
#'   
#' @export
compute_fitted_values.PosteriorBSVARMSH <- function(posterior) {
  
  Y               = posterior$last_draw$data_matrices$Y
  posterior_A     = posterior$posterior$A
  posterior_B     = posterior$posterior$B
  posterior_sigma = posterior$posterior$sigma
  X               = posterior$last_draw$data_matrices$X

  fv              = .Call(`_bsvars_bsvars_fitted_values`, posterior_A, posterior_B, posterior_sigma, X)
  class(fv)       = "PosteriorFitted"
  S               = dim(posterior_A)[3]      
  dimnames(fv)    = list(rownames(Y), colnames(Y), 1:S)

  return(fv)
}










#' @method compute_fitted_values PosteriorBSVARMIX
#' 
#' @title Computes posterior draws from data predictive density
#'
#' @description Each of the draws from the posterior estimation of models from 
#' packages \pkg{bsvars} or \pkg{bsvarSIGNs} is transformed into
#' a draw from the data predictive density. 
#' 
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARMIX} obtained by running the \code{estimate} function.
#' 
#' @return An object of class \code{PosteriorFitted}, that is, an \code{NxTxS} 
#' array with attribute \code{PosteriorFitted} containing \code{S} draws from 
#' the data predictive density.
#'
#' @seealso \code{\link{estimate}}, \code{\link{summary}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
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
#' # compute draws from in-sample predictive density
#' csd     = compute_fitted_values(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_mix$new(p = 1, M = 2) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_fitted_values() -> csd
#'   
#' @export
compute_fitted_values.PosteriorBSVARMIX <- function(posterior) {
  
  Y               = posterior$last_draw$data_matrices$Y
  posterior_A     = posterior$posterior$A
  posterior_B     = posterior$posterior$B
  posterior_sigma = posterior$posterior$sigma
  X               = posterior$last_draw$data_matrices$X
  
  fv              = .Call(`_bsvars_bsvars_fitted_values`, posterior_A, posterior_B, posterior_sigma, X)
  class(fv)       = "PosteriorFitted"
  S               = dim(posterior_A)[3]      
  dimnames(fv)    = list(rownames(Y), colnames(Y), 1:S)
  
  return(fv)
}








#' @method compute_fitted_values PosteriorBSVARSV
#' 
#' @title Computes posterior draws from data predictive density
#'
#' @description Each of the draws from the posterior estimation of models from 
#' packages \pkg{bsvars} or \pkg{bsvarSIGNs} is transformed into
#' a draw from the data predictive density. 
#' 
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARSV} obtained by running the \code{estimate} function.
#' 
#' @return An object of class \code{PosteriorFitted}, that is, an \code{NxTxS} 
#' array with attribute \code{PosteriorFitted} containing \code{S} draws from 
#' the data predictive density.
#'
#' @seealso \code{\link{estimate}}, \code{\link{summary}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
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
#' # compute draws from in-sample predictive density
#' csd     = compute_fitted_values(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_fitted_values() -> csd
#'   
#' @export
compute_fitted_values.PosteriorBSVARSV <- function(posterior) {
  
  Y               = posterior$last_draw$data_matrices$Y
  posterior_A     = posterior$posterior$A
  posterior_B     = posterior$posterior$B
  posterior_sigma = posterior$posterior$sigma
  X               = posterior$last_draw$data_matrices$X
  
  fv              = .Call(`_bsvars_bsvars_fitted_values`, posterior_A, posterior_B, posterior_sigma, X)
  class(fv)       = "PosteriorFitted"
  S               = dim(posterior_A)[3]      
  dimnames(fv)    = list(rownames(Y), colnames(Y), 1:S)
  
  return(fv)
}





#' @method compute_fitted_values PosteriorBSVART
#' 
#' @title Computes posterior draws from data predictive density
#'
#' @description Each of the draws from the posterior estimation of the model is 
#' transformed into a draw from the data predictive density. 
#' 
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVART} obtained by running the \code{estimate} function.
#' 
#' @return An object of class \code{PosteriorFitted}, that is, an \code{NxTxS} 
#' array with attribute \code{PosteriorFitted} containing \code{S} draws from 
#' the data predictive density.
#'
#' @seealso \code{\link{estimate}}, \code{\link{summary}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
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
#' # compute draws from in-sample predictive density
#' fitted         = compute_fitted_values(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_t$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_fitted_values() -> fitted
#' 
#' @export
compute_fitted_values.PosteriorBSVART <- function(posterior) {
  
  Y               = posterior$last_draw$data_matrices$Y
  posterior_A     = posterior$posterior$A
  posterior_B     = posterior$posterior$B
  posterior_sigma = compute_conditional_sd(posterior)
  X               = posterior$last_draw$data_matrices$X
  
  fv              = .Call(`_bsvars_bsvars_fitted_values`, posterior_A, posterior_B, posterior_sigma, X)
  class(fv)       = "PosteriorFitted"
  S               = dim(posterior_A)[3]      
  dimnames(fv)    = list(rownames(Y), colnames(Y), 1:S)
  
  return(fv)
}