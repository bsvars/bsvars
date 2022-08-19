
#' @title The log of Bayes factor for the homoskedasticity hypothesis for each structural shock
#'
#' @description Computes the logarithm of Bayes factor for the homoskedasticity hypothesis 
#' for each of the structural shocks via Savage-Dickey Density Ration (SDDR).
#' The hypothesis is represented by restriction:
#' \deqn{w_n = 0}
#' The logarithm of Bayes factor for this hypothesis can be computed using the SDDR 
#' as the difference of logarithms of the marginal posterior distribution ordinate at the restriction 
#' less the marginal prior distribution ordinate at the same point:
#' \deqn{log p(w_n = 0 | data) - log p(w_n = 0)}
#' Therefore, a negative value of the difference is the evidence against 
#' homoskedasticity of the structural shock. The estimation of both elements of the difference requires 
#' numerical integration.
#' 
#' @param posterior the \code{posterior} element of the list from the estimation outcome obtained using function \code{bsvar_sv}
#' @param prior A list specifying the prior distribution. See argument \code{prior} of function \code{bsvar_sv}
#' @param Y an \code{NxT} matrix, the matrix containing \code{T} observations on \code{N} dependent time series variables
#' @param X a \code{KxT} matrix, the matrix containing \code{T} observations on \code{K = N*p+d} regressors including \code{p} lags of dependent variables and \code{d} deterministic terms
#' @param sample_s_ a logical value set to the same value as the corresponding argument of function \code{bsvar_sv}
#' 
#' @return A list of three components:
#' 
#' \code{logSDDR} an \code{N}-vector with values of the logarithm of the Bayes factors for 
#' the homoskedasticity hypothesis for each of the shocks
#' 
#' \code{log_SDDR_se} an \code{N}-vector with estimation standard errors of the logarithm of 
#' the Bayes factors reported in output element \code{logSDDR} that are computed based on 30 random 
#' sub-samples of the log-ordinates of the marginal posterior and prior distributions.
#' 
#' \code{components} a list of three components for the computation of the Bayes factor
#' \describe{
#'   \item{log_denominator}{an \code{N}-vector with values of the logarithm of the Bayes factor denominators}
#'   \item{log_numerator}{an \code{N}-vector with values of the logarithm of the Bayes factor numerators}
#'   \item{log_numerator_s}{an \code{NxS} matrix of the log-full conditional posterior density ordinates computed to estimate the numerator}
#'   \item{se_components}{an \code{Nx30} matrix containing the log-Bayes factors on the basis of which the standard errors are computed}
#' }
#' 
#' @seealso \code{\link{bsvar_sv}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @references 
#' Lütkepohl, H., Shang, F., Uzeda, L., and Woźniak, T. (2022) Partial Identification of Heteroskedastic Structural VARs: Theory and Bayesian Inference.
#'
#' @export
logSDDR_homoskedasticity <- function(posterior, prior, Y, X, sample_s_ = TRUE) {
  .Call(`_bsvars_logSDDR_homoskedasticity`, posterior, prior, Y, X, sample_s_)
}
