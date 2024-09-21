
#' @title Computes posterior draws of historical decompositions
#'
#' @description Each of the draws from the posterior estimation of models from
#' packages \pkg{bsvars} or \pkg{bsvarSIGNs} is transformed into
#' a draw from the posterior distribution of the historical decompositions. 
#' IMPORTANT! The historical decompositions are interpreted correctly for 
#' covariance stationary data. Application to unit-root non-stationary data might
#' result in non-interpretable outcomes.
#' 
#' @param posterior posterior estimation outcome obtained by running the \code{estimate} 
#' function. The interpretation depends on the normalisation of the shocks
#' using function \code{normalise_posterior()}. Verify if the default settings 
#' are appropriate.
#' @param show_progress a logical value, if \code{TRUE} the estimation progress bar is visible
#' 
#' @return An object of class \code{PosteriorHD}, that is, an \code{NxNxTxS} array 
#' with attribute \code{PosteriorHD} containing \code{S} draws of the historical 
#' decompositions.
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
#' specification  = specify_bsvar$new(diff(us_fiscal_lsuw), p = 1)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' 
#' # compute historical decompositions
#' hd            = compute_historical_decompositions(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' diff(us_fiscal_lsuw) |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_historical_decompositions() -> hd
#' 
#' @export
compute_historical_decompositions <- function(posterior, show_progress = TRUE) {
  stopifnot("Argument show_progress must be a logical value." = is.logical(show_progress))
  UseMethod("compute_historical_decompositions", posterior)
}







#' @method compute_historical_decompositions PosteriorBSVAR
#' 
#' @title Computes posterior draws of historical decompositions
#'
#' @description Each of the draws from the posterior estimation of models from
#' packages \pkg{bsvars} or \pkg{bsvarSIGNs} is transformed into
#' a draw from the posterior distribution of the historical decompositions. 
#' IMPORTANT! The historical decompositions are interpreted correctly for 
#' covariance stationary data. Application to unit-root non-stationary data might
#' result in non-interpretable outcomes.
#' 
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVAR} obtained by running the \code{estimate} function.
#' @param show_progress a logical value, if \code{TRUE} the estimation progress bar is visible
#' 
#' @return An object of class \code{PosteriorHD}, that is, an \code{NxNxTxS} array 
#' with attribute \code{PosteriorHD} containing \code{S} draws of the historical 
#' decompositions.
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
#' specification  = specify_bsvar$new(diff(us_fiscal_lsuw), p = 1)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' 
#' # compute historical decompositions
#' hd            = compute_historical_decompositions(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' diff(us_fiscal_lsuw) |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_historical_decompositions() -> hd
#'   
#' @export
compute_historical_decompositions.PosteriorBSVAR <- function(posterior, show_progress = TRUE) {
  
  posterior_B     = posterior$posterior$B
  posterior_A     = posterior$posterior$A
  
  Y               = posterior$last_draw$data_matrices$Y
  X               = posterior$last_draw$data_matrices$X
  
  N               = nrow(Y)
  T               = ncol(Y)
  p               = posterior$last_draw$p
  S               = dim(posterior_A)[3]
  
  ss              = .Call(`_bsvars_bsvars_structural_shocks`, posterior_B, posterior_A, Y, X)
  ir              = .Call(`_bsvars_bsvars_ir`, posterior_B, posterior_A, T, p, TRUE)
  qqq             = .Call(`_bsvars_bsvars_hd`, ir, ss, show_progress)
  
  hd              = array(NA, c(N, N, T, S), dimnames = list(rownames(Y), rownames(Y), colnames(Y), 1:S))
  for (s in 1:S) hd[,,,s] = qqq[s][[1]]
  class(hd)       = "PosteriorHD"
  
  return(hd)
}







#' @method compute_historical_decompositions PosteriorBSVARMSH
#' 
#' @title Computes posterior draws of historical decompositions
#'
#' @description Each of the draws from the posterior estimation of models from
#' packages \pkg{bsvars} or \pkg{bsvarSIGNs} is transformed into
#' a draw from the posterior distribution of the historical decompositions. 
#' IMPORTANT! The historical decompositions are interpreted correctly for 
#' covariance stationary data. Application to unit-root non-stationary data might
#' result in non-interpretable outcomes.
#' 
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARMSH} obtained by running the \code{estimate} function.
#' @param show_progress a logical value, if \code{TRUE} the estimation progress bar is visible
#' 
#' @return An object of class \code{PosteriorHD}, that is, an \code{NxNxTxS} array 
#' with attribute \code{PosteriorHD} containing \code{S} draws of the historical 
#' decompositions.
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
#' specification  = specify_bsvar_msh$new(us_fiscal_lsuw, p = 1, M = 2)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' 
#' # compute historical decompositions
#' hd             = compute_historical_decompositions(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_msh$new(p = 1, M = 2) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_historical_decompositions() -> hds
#'   
#' @export
compute_historical_decompositions.PosteriorBSVARMSH <- function(posterior, show_progress = TRUE) {

  posterior_B     = posterior$posterior$B
  posterior_A     = posterior$posterior$A

  Y               = posterior$last_draw$data_matrices$Y
  X               = posterior$last_draw$data_matrices$X

  N               = nrow(Y)
  T               = ncol(Y)
  p               = posterior$last_draw$p
  S               = dim(posterior_A)[3]

  ss              = .Call(`_bsvars_bsvars_structural_shocks`, posterior_B, posterior_A, Y, X)
  ir              = .Call(`_bsvars_bsvars_ir`, posterior_B, posterior_A, T, p, TRUE)
  qqq             = .Call(`_bsvars_bsvars_hd`, ir, ss, show_progress)

  hd              = array(NA, c(N, N, T, S), dimnames = list(rownames(Y), rownames(Y), colnames(Y), 1:S))
  for (s in 1:S) hd[,,,s] = qqq[s][[1]]
  class(hd)       = "PosteriorHD"

  return(hd)
}




#' @method compute_historical_decompositions PosteriorBSVARMIX
#' 
#' @title Computes posterior draws of historical decompositions
#'
#' @description Each of the draws from the posterior estimation of models from
#' packages \pkg{bsvars} or \pkg{bsvarSIGNs} is transformed into
#' a draw from the posterior distribution of the historical decompositions. 
#' IMPORTANT! The historical decompositions are interpreted correctly for 
#' covariance stationary data. Application to unit-root non-stationary data might
#' result in non-interpretable outcomes.
#' 
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARMIX} obtained by running the \code{estimate} function.
#' @param show_progress a logical value, if \code{TRUE} the estimation progress bar is visible
#' 
#' @return An object of class \code{PosteriorHD}, that is, an \code{NxNxTxS} array 
#' with attribute \code{PosteriorHD} containing \code{S} draws of the historical 
#' decompositions.
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
#' specification  = specify_bsvar_mix$new(us_fiscal_lsuw, p = 1, M = 2)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' 
#' # compute historical decompositions
#' hd             = compute_historical_decompositions(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_mix$new(p = 1, M = 2) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_historical_decompositions() -> hds
#'   
#' @export
compute_historical_decompositions.PosteriorBSVARMIX <- function(posterior, show_progress = TRUE) {
  
  posterior_B     = posterior$posterior$B
  posterior_A     = posterior$posterior$A
  
  Y               = posterior$last_draw$data_matrices$Y
  X               = posterior$last_draw$data_matrices$X
  
  N               = nrow(Y)
  T               = ncol(Y)
  p               = posterior$last_draw$p
  S               = dim(posterior_A)[3]
  
  ss              = .Call(`_bsvars_bsvars_structural_shocks`, posterior_B, posterior_A, Y, X)
  ir              = .Call(`_bsvars_bsvars_ir`, posterior_B, posterior_A, T, p, TRUE)
  qqq             = .Call(`_bsvars_bsvars_hd`, ir, ss, show_progress)
  
  hd              = array(NA, c(N, N, T, S), dimnames = list(rownames(Y), rownames(Y), colnames(Y), 1:S))
  for (s in 1:S) hd[,,,s] = qqq[s][[1]]
  class(hd)       = "PosteriorHD"
  
  return(hd)
}





#' @method compute_historical_decompositions PosteriorBSVARSV
#' 
#' @title Computes posterior draws of historical decompositions
#'
#' @description Each of the draws from the posterior estimation of models from
#' packages \pkg{bsvars} or \pkg{bsvarSIGNs} is transformed into
#' a draw from the posterior distribution of the historical decompositions. 
#' IMPORTANT! The historical decompositions are interpreted correctly for 
#' covariance stationary data. Application to unit-root non-stationary data might
#' result in non-interpretable outcomes.
#' 
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARSV} obtained by running the \code{estimate} function.
#' @param show_progress a logical value, if \code{TRUE} the estimation progress bar is visible
#' 
#' @return An object of class \code{PosteriorHD}, that is, an \code{NxNxTxS} array 
#' with attribute \code{PosteriorHD} containing \code{S} draws of the historical 
#' decompositions.
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
#' specification  = specify_bsvar_sv$new(us_fiscal_lsuw, p = 1)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 5)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 5)
#' 
#' # compute historical decompositions
#' hd             = compute_historical_decompositions(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new(p = 1) |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
#'   compute_historical_decompositions() -> hds
#'   
#' @export
compute_historical_decompositions.PosteriorBSVARSV <- function(posterior, show_progress = TRUE) {
  
  posterior_B     = posterior$posterior$B
  posterior_A     = posterior$posterior$A
  
  Y               = posterior$last_draw$data_matrices$Y
  X               = posterior$last_draw$data_matrices$X
  
  N               = nrow(Y)
  T               = ncol(Y)
  p               = posterior$last_draw$p
  S               = dim(posterior_A)[3]
  
  ss              = .Call(`_bsvars_bsvars_structural_shocks`, posterior_B, posterior_A, Y, X)
  ir              = .Call(`_bsvars_bsvars_ir`, posterior_B, posterior_A, T, p, TRUE)
  qqq             = .Call(`_bsvars_bsvars_hd`, ir, ss, show_progress)
  
  hd              = array(NA, c(N, N, T, S), dimnames = list(rownames(Y), rownames(Y), colnames(Y), 1:S))
  for (s in 1:S) hd[,,,s] = qqq[s][[1]]
  class(hd)       = "PosteriorHD"
  
  return(hd)
}







#' @method compute_historical_decompositions PosteriorBSVART
#' 
#' @title Computes posterior draws of historical decompositions
#'
#' @description Each of the draws from the posterior estimation of models from
#' packages \pkg{bsvars} or \pkg{bsvarSIGNs} is transformed into
#' a draw from the posterior distribution of the historical decompositions. 
#' IMPORTANT! The historical decompositions are interpreted correctly for 
#' covariance stationary data. Application to unit-root non-stationary data might
#' result in non-interpretable outcomes.
#' 
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVART} obtained by running the \code{estimate} function.
#' @param show_progress a logical value, if \code{TRUE} the estimation progress bar is visible
#' 
#' @return An object of class \code{PosteriorHD}, that is, an \code{NxNxTxS} array 
#' with attribute \code{PosteriorHD} containing \code{S} draws of the historical 
#' decompositions.
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
#' specification  = specify_bsvar_t$new(diff(us_fiscal_lsuw), p = 1)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 10)
#' 
#' # compute historical decompositions
#' hd            = compute_historical_decompositions(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' diff(us_fiscal_lsuw) |>
#'   specify_bsvar_t$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 10) |> 
#'   compute_historical_decompositions() -> hd
#'   
#' @export
compute_historical_decompositions.PosteriorBSVART <- function(posterior, show_progress = TRUE) {
  
  posterior_B     = posterior$posterior$B
  posterior_A     = posterior$posterior$A
  
  Y               = posterior$last_draw$data_matrices$Y
  X               = posterior$last_draw$data_matrices$X
  
  N               = nrow(Y)
  T               = ncol(Y)
  p               = posterior$last_draw$p
  S               = dim(posterior_A)[3]
  
  ss              = .Call(`_bsvars_bsvars_structural_shocks`, posterior_B, posterior_A, Y, X)
  ir              = .Call(`_bsvars_bsvars_ir`, posterior_B, posterior_A, T, p, TRUE)
  qqq             = .Call(`_bsvars_bsvars_hd`, ir, ss, show_progress)
  
  hd              = array(NA, c(N, N, T, S), dimnames = list(rownames(Y), rownames(Y), colnames(Y), 1:S))
  for (s in 1:S) hd[,,,s] = qqq[s][[1]]
  class(hd)       = "PosteriorHD"
  
  return(hd)
}


