#' @title Computes posterior draws of conditional covariance matrices
#'
#' @description Computes in-sample posterior draws of the covariance matrices
#' of the reduced-form innovations.
#'
#' @param posterior posterior estimation outcome obtained using \code{estimate()}.
#'
#' @details For the structural equation \eqn{B\epsilon_t = u_t},
#' \deqn{\Sigma_t = B^{-1}\mathrm{diag}(\sigma_t^2)(B^{-1})',}
#' where \eqn{\sigma_t} contains the structural shock standard deviations
#' returned by \code{compute_conditional_sd()}.
#'
#' Each matrix is conditional on the draw's parameters and in-sample
#' volatility or regime. For Student-t shocks, the auxiliary mixing scales
#' \code{lambda} are integrated out.
#'
#' @return An object of class \code{PosteriorCovariance}: an
#' \code{NxNxTxS} array for \code{N} observed variables, \code{T} estimation
#' observations (after lags), and \code{S} saved posterior draws. The axes
#' are labelled with variable names, variable names, estimation times,
#' and draw indices, respectively.
#'
#' @seealso \code{\link{compute_conditional_sd}}, \code{\link{estimate}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me},
#' Fei Shang \email{sfmax2010@gmail.com}
#'
#' @examples
#' specification = specify_bsvar$new(us_fiscal_lsuw, p = 1)
#' burn_in       = estimate(specification, 5)
#' posterior     = estimate(burn_in, 5)
#' covariance    = compute_conditional_covariance(posterior)
#'
#' # covariance matrix at the first observation and draw
#' covariance[, , 1, 1]
#'
#' @export
compute_conditional_covariance <- function(posterior) {
  UseMethod("compute_conditional_covariance", posterior)
}

#' @inherit compute_conditional_covariance
#' @method compute_conditional_covariance PosteriorBSVAR
#' @param posterior posterior estimation outcome - an object of class
#' \code{PosteriorBSVAR} obtained using \code{estimate()}.
#' @export
compute_conditional_covariance.PosteriorBSVAR <- function(posterior) {
  posterior_B     = posterior$posterior$B
  Y               = posterior$last_draw$data_matrices$Y
  N               = nrow(Y)
  T               = ncol(Y)
  S               = dim(posterior_B)[3]
  posterior_sigma = array(1, c(N, T, S))

  qqq = .Call(`_bsvars_bsvars_conditional_covariance`, posterior_B, posterior_sigma)
  covariance = array(NA, c(N, N, T, S),
                     dimnames = list(rownames(Y), rownames(Y), colnames(Y), 1:S))
  for (s in 1:S) covariance[,,,s] = qqq[s][[1]]
  class(covariance) = "PosteriorCovariance"
  return(covariance)
}


#' @inherit compute_conditional_covariance
#' @method compute_conditional_covariance PosteriorBSVAREXH
#' @param posterior posterior estimation outcome - an object of class
#' \code{PosteriorBSVAREXH} obtained using \code{estimate()}.
#' @export
compute_conditional_covariance.PosteriorBSVAREXH <- function(posterior) {
  posterior_B     = posterior$posterior$B
  Y               = posterior$last_draw$data_matrices$Y
  N               = nrow(Y)
  T               = ncol(Y)
  S               = dim(posterior_B)[3]
  posterior_sigma = posterior$posterior$sigma

  qqq = .Call(`_bsvars_bsvars_conditional_covariance`, posterior_B, posterior_sigma)
  covariance = array(NA, c(N, N, T, S),
                     dimnames = list(rownames(Y), rownames(Y), colnames(Y), 1:S))
  for (s in 1:S) covariance[,,,s] = qqq[s][[1]]
  class(covariance) = "PosteriorCovariance"
  return(covariance)
}


#' @inherit compute_conditional_covariance
#' @method compute_conditional_covariance PosteriorBSVARMSH
#' @param posterior posterior estimation outcome - an object of class
#' \code{PosteriorBSVARMSH} obtained using \code{estimate()}.
#' @export
compute_conditional_covariance.PosteriorBSVARMSH <- function(posterior) {
  posterior_B     = posterior$posterior$B
  Y               = posterior$last_draw$data_matrices$Y
  N               = nrow(Y)
  T               = ncol(Y)
  S               = dim(posterior_B)[3]
  posterior_sigma = posterior$posterior$sigma

  qqq = .Call(`_bsvars_bsvars_conditional_covariance`, posterior_B, posterior_sigma)
  covariance = array(NA, c(N, N, T, S),
                     dimnames = list(rownames(Y), rownames(Y), colnames(Y), 1:S))
  for (s in 1:S) covariance[,,,s] = qqq[s][[1]]
  class(covariance) = "PosteriorCovariance"
  return(covariance)
}


#' @inherit compute_conditional_covariance
#' @method compute_conditional_covariance PosteriorBSVARHMSH
#' @param posterior posterior estimation outcome - an object of class
#' \code{PosteriorBSVARHMSH} obtained using \code{estimate()}.
#' @export
compute_conditional_covariance.PosteriorBSVARHMSH <- function(posterior) {
  posterior_B     = posterior$posterior$B
  Y               = posterior$last_draw$data_matrices$Y
  N               = nrow(Y)
  T               = ncol(Y)
  S               = dim(posterior_B)[3]
  posterior_sigma = posterior$posterior$sigma

  qqq = .Call(`_bsvars_bsvars_conditional_covariance`, posterior_B, posterior_sigma)
  covariance = array(NA, c(N, N, T, S),
                     dimnames = list(rownames(Y), rownames(Y), colnames(Y), 1:S))
  for (s in 1:S) covariance[,,,s] = qqq[s][[1]]
  class(covariance) = "PosteriorCovariance"
  return(covariance)
}


#' @inherit compute_conditional_covariance
#' @method compute_conditional_covariance PosteriorBSVARMIX
#' @param posterior posterior estimation outcome - an object of class
#' \code{PosteriorBSVARMIX} obtained using \code{estimate()}.
#' @export
compute_conditional_covariance.PosteriorBSVARMIX <- function(posterior) {
  posterior_B     = posterior$posterior$B
  Y               = posterior$last_draw$data_matrices$Y
  N               = nrow(Y)
  T               = ncol(Y)
  S               = dim(posterior_B)[3]
  posterior_sigma = posterior$posterior$sigma

  qqq = .Call(`_bsvars_bsvars_conditional_covariance`, posterior_B, posterior_sigma)
  covariance = array(NA, c(N, N, T, S),
                     dimnames = list(rownames(Y), rownames(Y), colnames(Y), 1:S))
  for (s in 1:S) covariance[,,,s] = qqq[s][[1]]
  class(covariance) = "PosteriorCovariance"
  return(covariance)
}


#' @inherit compute_conditional_covariance
#' @method compute_conditional_covariance PosteriorBSVARSV
#' @param posterior posterior estimation outcome - an object of class
#' \code{PosteriorBSVARSV} obtained using \code{estimate()}.
#' @export
compute_conditional_covariance.PosteriorBSVARSV <- function(posterior) {
  posterior_B     = posterior$posterior$B
  Y               = posterior$last_draw$data_matrices$Y
  N               = nrow(Y)
  T               = ncol(Y)
  S               = dim(posterior_B)[3]
  posterior_sigma = posterior$posterior$sigma

  qqq = .Call(`_bsvars_bsvars_conditional_covariance`, posterior_B, posterior_sigma)
  covariance = array(NA, c(N, N, T, S),
                     dimnames = list(rownames(Y), rownames(Y), colnames(Y), 1:S))
  for (s in 1:S) covariance[,,,s] = qqq[s][[1]]
  class(covariance) = "PosteriorCovariance"
  return(covariance)
}


#' @inherit compute_conditional_covariance
#' @method compute_conditional_covariance PosteriorBSVART
#' @param posterior posterior estimation outcome - an object of class
#' \code{PosteriorBSVART} obtained using \code{estimate()}.
#' @export
compute_conditional_covariance.PosteriorBSVART <- function(posterior) {
  posterior_B     = posterior$posterior$B
  Y               = posterior$last_draw$data_matrices$Y
  N               = nrow(Y)
  T               = ncol(Y)
  S               = dim(posterior_B)[3]
  posterior_sigma = array(1, c(N, T, S))

  qqq = .Call(`_bsvars_bsvars_conditional_covariance`, posterior_B, posterior_sigma)
  covariance = array(NA, c(N, N, T, S),
                     dimnames = list(rownames(Y), rownames(Y), colnames(Y), 1:S))
  for (s in 1:S) covariance[,,,s] = qqq[s][[1]]
  class(covariance) = "PosteriorCovariance"
  return(covariance)
}

#' @export
print.PosteriorCovariance <- function(x, ...) {
  cat("Posterior draws of conditional covariance matrices\n")
  cat("An array of dimensions:", paste(dim(x), collapse = " x "), "\n")
  invisible(x)
}
