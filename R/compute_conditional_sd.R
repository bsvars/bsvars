
#' @title Computes posterior draws of structural shock conditional standard deviations
#'
#' @description Each of the draws from the posterior estimation of models is 
#' transformed into a draw from the posterior distribution of the structural 
#' shock conditional standard deviations. 
#' 
#' @param posterior posterior estimation outcome obtained by running the 
#' \code{estimate} function. The interpretation depends on the normalisation of 
#' the shocks using function \code{normalise_posterior()}. Verify if the default 
#' settings are appropriate.
#' 
#' @return An object of class \code{PosteriorSigma}, that is, an \code{NxTxS} 
#' array with attribute \code{PosteriorSigma} containing \code{S} draws of the 
#' structural shock conditional standard deviations.
#'
#' @seealso \code{\link{estimate}}, \code{\link{normalise_posterior}}, \code{\link{summary}}
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
#' # compute structural shocks' conditional standard deviations
#' sigma          = compute_conditional_sd(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_conditional_sd() -> csd
#' 
#' @export
compute_conditional_sd <- function(posterior) {
  UseMethod("compute_conditional_sd", posterior)
}




#' @method compute_conditional_sd PosteriorBSVAR
#' 
#' @title Computes posterior draws of structural shock conditional standard deviations
#'
#' @description Each of the draws from the posterior estimation of models is 
#' transformed into a draw from the posterior distribution of the structural 
#' shock conditional standard deviations. 
#' 
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVAR} obtained by running the \code{estimate} function.
#' 
#' @return An object of class \code{PosteriorSigma}, that is, an \code{NxTxS} 
#' array with attribute \code{PosteriorSigma} containing \code{S} draws of the 
#' structural shock conditional standard deviations.
#'
#' @seealso \code{\link{estimate}}, \code{\link{normalise_posterior}}, \code{\link{summary}}
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
#' # compute structural shocks' conditional standard deviations
#' sigma          = compute_conditional_sd(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_conditional_sd() -> csd
#' 
#' @export
compute_conditional_sd.PosteriorBSVAR <- function(posterior) {

  Y     = posterior$last_draw$data_matrices$Y
  N     = nrow(Y)
  T     = ncol(Y)
  S     = dim(posterior$posterior$A)[3]

  posterior_sigma       = array(1, c(N, T, S), dimnames = list(rownames(Y), colnames(Y), 1:S))
  message("The model is homoskedastic. Returning an NxTxS matrix of conditional sd all equal to 1.")
  class(posterior_sigma)  = "PosteriorSigma"

  return(posterior_sigma)
}





#' @method compute_conditional_sd PosteriorBSVARMSH
#' 
#' @title Computes posterior draws of structural shock conditional standard deviations
#'
#' @description Each of the draws from the posterior estimation of models is 
#' transformed into a draw from the posterior distribution of the structural 
#' shock conditional standard deviations. 
#' 
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARMSH} obtained by running the \code{estimate} function.
#' 
#' @return An object of class \code{PosteriorSigma}, that is, an \code{NxTxS} 
#' array with attribute \code{PosteriorSigma} containing \code{S} draws of the 
#' structural shock conditional standard deviations.
#'
#' @seealso \code{\link{estimate}}, \code{\link{normalise_posterior}}, \code{\link{summary}}
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
#' # compute structural shocks' conditional standard deviations
#' csd     = compute_conditional_sd(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_msh$new(p = 1, M = 2) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_conditional_sd() -> csd
#'   
#' @export
compute_conditional_sd.PosteriorBSVARMSH <- function(posterior) {

  Y                         = posterior$last_draw$data_matrices$Y
  posterior_sigma           = posterior$posterior$sigma
  S                         = dim(posterior_sigma)[3]
  class(posterior_sigma)    = "PosteriorSigma"
  dimnames(posterior_sigma) = list(rownames(Y), colnames(Y), 1:S)

  return(posterior_sigma)
}






#' @method compute_conditional_sd PosteriorBSVARMIX
#' 
#' @title Computes posterior draws of structural shock conditional standard deviations
#'
#' @description Each of the draws from the posterior estimation of models is 
#' transformed into a draw from the posterior distribution of the structural 
#' shock conditional standard deviations. 
#' 
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARMIX} obtained by running the \code{estimate} function.
#' 
#' @return An object of class \code{PosteriorSigma}, that is, an \code{NxTxS} 
#' array with attribute \code{PosteriorSigma} containing \code{S} draws of the 
#' structural shock conditional standard deviations.
#'
#' @seealso \code{\link{estimate}}, \code{\link{normalise_posterior}}, \code{\link{summary}}
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
#' # compute structural shocks' conditional standard deviations
#' csd     = compute_conditional_sd(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_mix$new(p = 1, M = 2) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_conditional_sd() -> csd
#'   
#' @export
compute_conditional_sd.PosteriorBSVARMIX <- function(posterior) {
  
  Y                         = posterior$last_draw$data_matrices$Y
  posterior_sigma           = posterior$posterior$sigma
  S                         = dim(posterior_sigma)[3]
  class(posterior_sigma)    = "PosteriorSigma"
  dimnames(posterior_sigma) = list(rownames(Y), colnames(Y), 1:S)
  
  return(posterior_sigma)
}




#' @method compute_conditional_sd PosteriorBSVARSV
#' 
#' @title Computes posterior draws of structural shock conditional standard deviations
#'
#' @description Each of the draws from the posterior estimation of models is 
#' transformed into a draw from the posterior distribution of the structural 
#' shock conditional standard deviations. 
#' 
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARSV} obtained by running the \code{estimate} function.
#' 
#' @return An object of class \code{PosteriorSigma}, that is, an \code{NxTxS} 
#' array with attribute \code{PosteriorSigma} containing \code{S} draws of the 
#' structural shock conditional standard deviations.
#'
#' @seealso \code{\link{estimate}}, \code{\link{normalise_posterior}}, \code{\link{summary}}
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
#' # compute structural shocks' conditional standard deviations
#' csd     = compute_conditional_sd(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_conditional_sd() -> csd
#'   
#' @export
compute_conditional_sd.PosteriorBSVARSV <- function(posterior) {
  
  Y                         = posterior$last_draw$data_matrices$Y
  posterior_sigma           = posterior$posterior$sigma
  S                         = dim(posterior_sigma)[3]
  class(posterior_sigma)    = "PosteriorSigma"
  dimnames(posterior_sigma) = list(rownames(Y), colnames(Y), 1:S)
  
  return(posterior_sigma)
}



#' @method compute_conditional_sd PosteriorBSVART
#' 
#' @title Computes posterior draws of structural shock conditional standard deviations
#'
#' @description Each of the draws from the posterior estimation of models is 
#' transformed into a draw from the posterior distribution of the structural 
#' shock conditional standard deviations. 
#' 
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVART} obtained by running the \code{estimate} function.
#' 
#' @return An object of class \code{PosteriorSigma}, that is, an \code{NxTxS} 
#' array with attribute \code{PosteriorSigma} containing \code{S} draws of the 
#' structural shock conditional standard deviations.
#'
#' @seealso \code{\link{estimate}}, \code{\link{normalise_posterior}}, \code{\link{summary}}
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
#' # compute structural shocks' conditional standard deviations
#' sigma          = compute_conditional_sd(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_t$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_conditional_sd() -> csd
#' 
#' @export
compute_conditional_sd.PosteriorBSVART <- function(posterior) {
  
  Y                       = posterior$last_draw$data_matrices$Y
  N                       = dim(posterior$posterior$B)[1]
  T                       = dim(posterior$posterior$lambda)[1]
  S                       = dim(posterior$posterior$lambda)[2]
  posterior_sigma         = array(NA, c(N,T,S), dimnames = list(rownames(Y), colnames(Y), 1:S))
  for (n in 1:N) {
    posterior_sigma[n,,]  = sqrt(posterior$posterior$lambda)
  }
  class(posterior_sigma)  = "PosteriorSigma"
  
  return(posterior_sigma)
}