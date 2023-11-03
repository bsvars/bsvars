
#' @title Verifies heteroskedasticity of structural shocks equation by equation 
#'
#' @description Computes the logarithm of Bayes factor for the homoskedasticity hypothesis 
#' for each of the structural shocks via Savage-Dickey Density Ration (SDDR).
#' The hypothesis of homoskedasticity, \eqn{H_0}, is represented by model-specific restrictions.
#' Consult help files for individual classes of models for details.
#' The logarithm of Bayes factor for this hypothesis can be computed using the SDDR 
#' as the difference of logarithms of the marginal posterior distribution ordinate at the restriction 
#' less the marginal prior distribution ordinate at the same point:
#' \deqn{log p(H_0 | data) - log p(H_0)}
#' Therefore, a negative value of the difference is the evidence against 
#' homoskedasticity of the structural shock. The estimation of both elements of the difference requires 
#' numerical integration.
#' 
#' @param posterior the \code{posterior} element of the list from the estimation outcome
#' 
#' @return An object of class SDDR that is a list of three components:
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
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @references 
#' Lütkepohl, H., and Woźniak, T., (2020) Bayesian Inference for Structural Vector Autoregressions Identified by Markov-Switching Heteroskedasticity. \emph{Journal of Economic Dynamics and Control} \bold{113}, 103862, \doi{https://doi.org/10.1016/j.jedc.2020.103862}.
#' 
#' Lütkepohl, H., Shang, F., Uzeda, L., and Woźniak, T., (2023) Partial Identification of Heteroskedastic Structural VARs: Theory and Bayesian Inference.
#' 
#' @examples
#' # simple workflow
#' ############################################################
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' specification  = specify_bsvar_sv$new(us_fiscal_lsuw, p = 1)
#' set.seed(123)
#' 
#' # run the burn-in
#' posterior      = estimate(specification, 60, thin = 1)
#' 
#' # verify heteroskedasticity
#' sddr           = verify_volatility(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new(p = 1) |>
#'   estimate(S = 60, thin = 1) |> 
#'   verify_volatility() -> sddr
#' 
#' @export
verify_volatility <- function(posterior) {
  
  # call method
  UseMethod("verify_volatility", posterior)
}





#' @inherit verify_volatility
#' @method verify_volatility PosteriorBSVARSV
#' @inheritParams verify_volatility
#'
#' @description Computes the logarithm of Bayes factor for the homoskedasticity hypothesis 
#' for each of the structural shocks via Savage-Dickey Density Ration (SDDR).
#' The hypothesis of homoskedasticity is represented by restriction:
#' \deqn{H_0: \omega_n = 0}
#' The logarithm of Bayes factor for this hypothesis can be computed using the SDDR 
#' as the difference of logarithms of the marginal posterior distribution ordinate at the restriction 
#' less the marginal prior distribution ordinate at the same point:
#' \deqn{log p(\omega_n = 0 | data) - log p(\omega_n = 0)}
#' Therefore, a negative value of the difference is the evidence against 
#' homoskedasticity of the structural shock. The estimation of both elements of the difference requires 
#' numerical integration.
#' 
#' @seealso \code{\link{specify_bsvar_sv}}, \code{\link{estimate}}
#'
#' @examples
#' # simple workflow
#' ############################################################
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' specification  = specify_bsvar_sv$new(us_fiscal_lsuw, p = 1)
#' set.seed(123)
#' 
#' # run the burn-in
#' posterior      = estimate(specification, 60, thin = 1)
#' 
#' # verify heteroskedasticity
#' sddr           = verify_volatility(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new(p = 1) |>
#'   estimate(S = 60, thin = 1) |> 
#'   verify_volatility() -> sddr
#'   
#' @export
verify_volatility.PosteriorBSVARSV <- function(posterior) {
  
  centred_sv          = posterior$last_draw$centred_sv
  stopifnot("There is not a known method of verifying heteroskedasticity using SDDR for a centered SV model.", !centred_sv)
  
  # get the inputs to estimation
  just_posterior  = posterior$posterior
  prior           = posterior$last_draw$prior$get_prior()
  Y               = posterior$last_draw$data_matrices$Y
  X               = posterior$last_draw$data_matrices$X
  
  # estimate the SDDR
  sddr            = .Call(`_bsvars_verify_volatility_cpp`, just_posterior, prior, Y, X, TRUE)
  
  class(sddr)     = "SDDR"
  return(sddr)
}



#' @inherit verify_volatility
#' @method verify_volatility PosteriorBSVAR
#' @inheritParams verify_volatility
#'
#' @description Displays information that the model is homoskedastic.
#' 
#' @return Nothing. Just displays a message: The model is homoskedastic.
#'
#' @examples
#' # simple workflow
#' ############################################################
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' specification  = specify_bsvar$new(us_fiscal_lsuw, p = 1)
#' set.seed(123)
#' 
#' # run the burn-in
#' posterior      = estimate(specification, 5, thin = 1)
#' 
#' # verify heteroskedasticity
#' sddr           = verify_volatility(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 5, thin = 1) |> 
#'   verify_volatility() -> sddr
#'   
#' @export
verify_volatility.PosteriorBSVAR <- function(posterior) {
  message("The model is homoskedastic.")
}