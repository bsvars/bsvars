
#' @title Verifies heteroskedasticity of structural shocks equation by equation 
#'
#' @description This function will be deprecated starting from version 4.0. 
#' It is replaced by \code{\link{verify_identification}} function.
#' 
#' Computes the logarithm of Bayes factor for the homoskedasticity hypothesis 
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
#' @return An object of class \code{SDDRvolatility} that is a list of three components:
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
#' Lütkepohl, H., and Woźniak, T., (2020) Bayesian Inference for Structural Vector Autoregressions Identified by Markov-Switching Heteroskedasticity. \emph{Journal of Economic Dynamics and Control} \bold{113}, 103862, \doi{10.1016/j.jedc.2020.103862}.
#' 
#' Lütkepohl, H., Shang, F., Uzeda, L., and Woźniak, T. (2024) Partial Identification of Heteroskedastic Structural VARs: Theory and Bayesian Inference. \emph{University of Melbourne Working Paper}, 1--57, \doi{10.48550/arXiv.2404.11057}.
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
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify heteroskedasticity
#' sddr           = verify_volatility(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   verify_volatility() -> sddr
#' 
#' @export
verify_volatility <- function(posterior) {
  
  # call method
  UseMethod("verify_volatility", posterior)
}





#' @inherit verify_volatility
#' @method verify_volatility PosteriorBSVAR
#' @inheritParams verify_volatility
#'
#' @description This function will be deprecated starting from version 4.0. 
#' It is replaced by \code{\link{verify_identification}} function.
#' 
#' Displays information that the model is homoskedastic.
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
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify heteroskedasticity
#' sddr           = verify_volatility(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   verify_volatility() -> sddr
#'   
#' @export
verify_volatility.PosteriorBSVAR <- function(posterior) {
  message("The model is homoskedastic.")
}






#' @inherit verify_volatility
#' @method verify_volatility PosteriorBSVARSV
#' @inheritParams verify_volatility
#'
#' @description This function will be deprecated starting from version 4.0. 
#' It is replaced by \code{\link{verify_identification}} function.
#' 
#' Computes the logarithm of Bayes factor for the homoskedasticity hypothesis 
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
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify heteroskedasticity
#' sddr           = verify_volatility(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   verify_volatility() -> sddr
#'   
#' @export
verify_volatility.PosteriorBSVARSV <- function(posterior) {
  
  centred_sv          = posterior$last_draw$centred_sv
  if ( centred_sv ) {
    message("There is not a known method of verifying heteroskedasticity using SDDR for a centered SV model.")
  } else {
    # get the inputs to estimation
    just_posterior  = posterior$posterior
    prior           = posterior$last_draw$prior$get_prior()
    Y               = posterior$last_draw$data_matrices$Y
    X               = posterior$last_draw$data_matrices$X
    
    # estimate the SDDR
    sddr            = .Call(`_bsvars_verify_volatility_sv_cpp`, just_posterior, prior, Y, X, TRUE)
    
    class(sddr)     = "SDDRvolatility"
    return(sddr)
  }
}




#' @inherit verify_volatility
#' @method verify_volatility PosteriorBSVARMIX
#' @inheritParams verify_volatility
#'
#' @description This function will be deprecated starting from version 4.0. 
#' It is replaced by \code{\link{verify_identification}} function.
#' 
#' Computes the logarithm of Bayes factor for the homoskedasticity hypothesis 
#' for each of the structural shocks via Savage-Dickey Density Ration (SDDR).
#' The hypothesis of homoskedasticity is represented by restriction:
#' \deqn{H_0: \sigma^2_{n.1} = ... = \sigma^2_{n.M} = 1}
#' The logarithm of Bayes factor for this hypothesis can be computed using the SDDR 
#' as the difference of logarithms of the marginal posterior distribution ordinate at the restriction 
#' less the marginal prior distribution ordinate at the same point:
#' \deqn{log p(\omega_n = 0 | data) - log p(\omega_n = 0)}
#' Therefore, a negative value of the difference is the evidence against 
#' homoskedasticity of the structural shock. The estimation of both elements of the difference requires 
#' numerical integration.
#' 
#' @seealso \code{\link{specify_bsvar_mix}}, \code{\link{estimate}}
#'
#' @examples
#' # simple workflow
#' ############################################################
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' specification  = specify_bsvar_mix$new(us_fiscal_lsuw, p = 1, M = 2)
#' set.seed(123)
#' 
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify heteroskedasticity
#' sddr           = verify_volatility(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_mix$new(p = 1, M = 2) |>
#'   estimate(S = 10) |> 
#'   verify_volatility() -> sddr
#'   
#' @export
verify_volatility.PosteriorBSVARMIX <- function(posterior) {

  # get the inputs to estimation
  just_posterior  = posterior$posterior
  prior           = posterior$last_draw$prior$get_prior()
  Y               = posterior$last_draw$data_matrices$Y
  X               = posterior$last_draw$data_matrices$X
  
  # estimate the SDDR
  sddr            = .Call(`_bsvars_verify_volatility_msh_cpp`, just_posterior, prior, Y, X)
  
  class(sddr)     = "SDDRvolatility"
  return(sddr)
}





#' @inherit verify_volatility
#' @method verify_volatility PosteriorBSVARMSH
#' @inheritParams verify_volatility
#'
#' @description This function will be deprecated starting from version 4.0. 
#' It is replaced by \code{\link{verify_identification}} function.
#' 
#' Computes the logarithm of Bayes factor for the homoskedasticity hypothesis 
#' for each of the structural shocks via Savage-Dickey Density Ration (SDDR).
#' The hypothesis of homoskedasticity is represented by restriction:
#' \deqn{H_0: \sigma^2_{n.1} = ... = \sigma^2_{n.M} = 1}
#' The logarithm of Bayes factor for this hypothesis can be computed using the SDDR 
#' as the difference of logarithms of the marginal posterior distribution ordinate at the restriction 
#' less the marginal prior distribution ordinate at the same point:
#' \deqn{log p(\omega_n = 0 | data) - log p(\omega_n = 0)}
#' Therefore, a negative value of the difference is the evidence against 
#' homoskedasticity of the structural shock. The estimation of both elements of the difference requires 
#' numerical integration.
#' 
#' @seealso \code{\link{specify_bsvar_msh}}, \code{\link{estimate}}
#'
#' @examples
#' # simple workflow
#' ############################################################
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' specification  = specify_bsvar_msh$new(us_fiscal_lsuw, p = 1, M = 2)
#' set.seed(123)
#' 
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify heteroskedasticity
#' sddr           = verify_volatility(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_msh$new(p = 1, M = 2) |>
#'   estimate(S = 10) |> 
#'   verify_volatility() -> sddr
#'   
#' @export
verify_volatility.PosteriorBSVARMSH <- function(posterior) {
  
  # get the inputs to estimation
  just_posterior  = posterior$posterior
  prior           = posterior$last_draw$prior$get_prior()
  Y               = posterior$last_draw$data_matrices$Y
  X               = posterior$last_draw$data_matrices$X
  
  # estimate the SDDR
  sddr            = .Call(`_bsvars_verify_volatility_msh_cpp`, just_posterior, prior, Y, X)
  
  class(sddr)     = "SDDRvolatility"
  return(sddr)
}








#' @title Verifies hypotheses involving autoregressive parameters
#'
#' @description Computes the logarithm of Bayes factor for the joint hypothesis, 
#' \eqn{H_0}, possibly for many autoregressive parameters represented by argument 
#' \code{hypothesis} via Savage-Dickey Density Ration (SDDR).
#' The logarithm of Bayes factor for this hypothesis can be computed using the SDDR 
#' as the difference of logarithms of the marginal posterior distribution ordinate at the restriction 
#' less the marginal prior distribution ordinate at the same point:
#' \deqn{log p(H_0 | data) - log p(H_0)}
#' Therefore, a negative value of the difference is the evidence against 
#' hypothesis. The estimation of both elements of the difference requires 
#' numerical integration.
#' 
#' @param posterior the \code{posterior} element of the list from the estimation outcome
#' @param hypothesis an \code{NxK} matrix of the same dimension as the autoregressive 
#' matrix \eqn{A} with numeric values for the parameters to be verified,
#' in which case the values represent the joint hypothesis, and missing value \code{NA} 
#' for these parameters that are not tested
#' 
#' @return An object of class \code{SDDRautoregression} that is a list of three components:
#' 
#' \code{logSDDR} a scalar with values of the logarithm of the Bayes factors for 
#' the autoregressive hypothesis for each of the shocks
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
#'   \item{log_denominator_s}{an \code{NxS} matrix of the log-full conditional posterior density ordinates computed to estimate the denominator}
#'   \item{se_components}{a \code{30}-vector containing the log-Bayes factors on the basis of which the standard errors are computed}
#' }
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @references 
#' Woźniak, T., and Droumaguet, M., (2024) Bayesian Assessment of Identifying Restrictions for Heteroskedastic Structural VARs
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
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify autoregression
#' H0             = matrix(NA, ncol(us_fiscal_lsuw), ncol(us_fiscal_lsuw) + 1)
#' H0[1,3]        = 0        # a hypothesis of no Granger causality from gdp to ttr
#' sddr           = verify_autoregression(posterior, H0)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   verify_autoregression(hypothesis = H0) -> sddr
#' 
#' @export
verify_autoregression <- function(posterior, hypothesis) {
  
  stopifnot("Argument hypothesis must be a numeric matrix." = is.matrix(hypothesis) & is.numeric(hypothesis))
  
  # call method
  UseMethod("verify_autoregression", posterior)
}




#' @inherit verify_autoregression
#' @method verify_autoregression PosteriorBSVAR
#' @inheritParams verify_autoregression
#' 
#' @export
verify_autoregression.PosteriorBSVAR <- function(posterior, hypothesis) {
  
  # get the inputs to estimation
  just_posterior  = posterior$posterior
  prior           = posterior$last_draw$prior$get_prior()
  Y               = posterior$last_draw$data_matrices$Y
  X               = posterior$last_draw$data_matrices$X
  
  hypothesis_cpp  = hypothesis
  hypothesis_cpp[is.na(hypothesis_cpp)] = 999
  
  # estimate the SDDR
  sddr            = .Call(`_bsvars_verify_autoregressive_homosk_cpp`, hypothesis_cpp, just_posterior, prior, Y, X)
  
  class(sddr)     = "SDDRautoregression"
  return(sddr)
}


#' @inherit verify_autoregression
#' @method verify_autoregression PosteriorBSVARSV
#' @inheritParams verify_autoregression
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
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify autoregression
#' H0             = matrix(NA, ncol(us_fiscal_lsuw), ncol(us_fiscal_lsuw) + 1)
#' H0[1,3]        = 0        # a hypothesis of no Granger causality from gdp to ttr
#' sddr           = verify_autoregression(posterior, H0)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   verify_autoregression(hypothesis = H0) -> sddr
#' 
#' @export
verify_autoregression.PosteriorBSVARSV <- function(posterior, hypothesis) {
  
  # get the inputs to estimation
  just_posterior  = posterior$posterior
  prior           = posterior$last_draw$prior$get_prior()
  Y               = posterior$last_draw$data_matrices$Y
  X               = posterior$last_draw$data_matrices$X
  
  hypothesis_cpp  = hypothesis
  hypothesis_cpp[is.na(hypothesis_cpp)] = 999
  
  # estimate the SDDR
  sddr            = .Call(`_bsvars_verify_autoregressive_heterosk_cpp`, hypothesis_cpp, just_posterior, prior, Y, X)
  
  class(sddr)     = "SDDRautoregression"
  return(sddr)
}


#' @inherit verify_autoregression
#' @method verify_autoregression PosteriorBSVARMIX
#' @inheritParams verify_autoregression
#' 
#' @examples
#' # simple workflow
#' ############################################################
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' specification  = specify_bsvar_mix$new(us_fiscal_lsuw, p = 1, M = 2)
#' set.seed(123)
#' 
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify autoregression
#' H0             = matrix(NA, ncol(us_fiscal_lsuw), ncol(us_fiscal_lsuw) + 1)
#' H0[1,3]        = 0        # a hypothesis of no Granger causality from gdp to ttr
#' sddr           = verify_autoregression(posterior, H0)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_mix$new(p = 1, M = 2) |>
#'   estimate(S = 10) |> 
#'   verify_autoregression(hypothesis = H0) -> sddr
#' 
#' @export
verify_autoregression.PosteriorBSVARMIX <- function(posterior, hypothesis) {
  
  # get the inputs to estimation
  just_posterior  = posterior$posterior
  prior           = posterior$last_draw$prior$get_prior()
  Y               = posterior$last_draw$data_matrices$Y
  X               = posterior$last_draw$data_matrices$X
  
  hypothesis_cpp  = hypothesis
  hypothesis_cpp[is.na(hypothesis_cpp)] = 999
  
  # estimate the SDDR
  sddr            = .Call(`_bsvars_verify_autoregressive_heterosk_cpp`, hypothesis_cpp, just_posterior, prior, Y, X)
  
  class(sddr)     = "SDDRautoregression"
  return(sddr)
}


#' @inherit verify_autoregression
#' @method verify_autoregression PosteriorBSVARMSH
#' @inheritParams verify_autoregression
#' 
#' @examples
#' # simple workflow
#' ############################################################
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' specification  = specify_bsvar_msh$new(us_fiscal_lsuw, p = 1, M = 2)
#' set.seed(123)
#' 
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify autoregression
#' H0             = matrix(NA, ncol(us_fiscal_lsuw), ncol(us_fiscal_lsuw) + 1)
#' H0[1,3]        = 0        # a hypothesis of no Granger causality from gdp to ttr
#' sddr           = verify_autoregression(posterior, H0)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_msh$new(p = 1, M = 2) |>
#'   estimate(S = 10) |> 
#'   verify_autoregression(hypothesis = H0) -> sddr
#' 
#' @export
verify_autoregression.PosteriorBSVARMSH <- function(posterior, hypothesis) {
  
  # get the inputs to estimation
  just_posterior  = posterior$posterior
  prior           = posterior$last_draw$prior$get_prior()
  Y               = posterior$last_draw$data_matrices$Y
  X               = posterior$last_draw$data_matrices$X
  
  hypothesis_cpp  = hypothesis
  hypothesis_cpp[is.na(hypothesis_cpp)] = 999
  
  # estimate the SDDR
  sddr            = .Call(`_bsvars_verify_autoregressive_heterosk_cpp`, hypothesis_cpp, just_posterior, prior, Y, X)
  
  class(sddr)     = "SDDRautoregression"
  return(sddr)
}



#' @inherit verify_autoregression
#' @method verify_autoregression PosteriorBSVART
#' @inheritParams verify_autoregression
#' 
#' @examples
#' # simple workflow
#' ############################################################
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' specification  = specify_bsvar_t$new(us_fiscal_lsuw)
#' set.seed(123)
#' 
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify autoregression
#' H0             = matrix(NA, ncol(us_fiscal_lsuw), ncol(us_fiscal_lsuw) + 1)
#' H0[1,3]        = 0        # a hypothesis of no Granger causality from gdp to ttr
#' sddr           = verify_autoregression(posterior, H0)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_t$new() |>
#'   estimate(S = 10) |> 
#'   verify_autoregression(hypothesis = H0) -> sddr
#' 
#' @export
verify_autoregression.PosteriorBSVART <- function(posterior, hypothesis) {
  
  # get the inputs to estimation
  just_posterior  = posterior$posterior
  prior           = posterior$last_draw$prior$get_prior()
  Y               = posterior$last_draw$data_matrices$Y
  X               = posterior$last_draw$data_matrices$X

  posterior_sigma = array(NA, c(dim(Y), dim(just_posterior$B)[3]))
  for (n in 1:dim(Y)[1]) {
    posterior_sigma[n,,] = sqrt(just_posterior$lambda)
  }
  just_posterior$sigma = posterior_sigma
  
  hypothesis_cpp  = hypothesis
  hypothesis_cpp[is.na(hypothesis_cpp)] = 999
  
  # estimate the SDDR
  sddr            = .Call(`_bsvars_verify_autoregressive_heterosk_cpp`, hypothesis_cpp, just_posterior, prior, Y, X)
  
  class(sddr)     = "SDDRautoregression"
  return(sddr)
}









#' @title Verifies identification through heteroskedasticity or non-normality of 
#' of structural shocks
#'
#' @description Computes the logarithm of Bayes factor(s) for the hypothesis 
#' in which the model is not identified through heteroskedasticity of non-normality
#' using Savage-Dickey Density Ration (SDDR).
#' The hypothesis of no such identification, \eqn{H_0}, is represented by 
#' model-specific restrictions.Consult help files for individual classes of models 
#' for details.
#' The logarithm of Bayes factor for this hypothesis can be computed using the SDDR 
#' as the difference of the logarithm of the marginal posterior distribution 
#' ordinate at the restriction less the log-marginal prior distribution ordinate 
#' at the same point:
#' \deqn{log p(H_0 | data) - log p(H_0)}
#' Therefore, a negative value of the difference is the evidence against 
#' the lack of identification of the structural shock through heteroskedasticity 
#' or non-normality.
#' 
#' @param posterior the estimation outcome obtained using \code{\link{estimate}} function
#' 
#' @return An object of class \code{SDDRid*} that is a list with components:
#' 
#' \code{logSDDR} a vector with values of the logarithm of the Bayes factors
#' 
#' \code{log_SDDR_se} a vector with numerical standard errors of the logarithm of 
#' the Bayes factors reported in output element \code{logSDDR} that are computed 
#' based on 30 random sub-samples of the log-ordinates of the marginal posterior 
#' and prior distributions.
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @seealso \code{\link{verify_identification.PosteriorBSVAR}}, \code{\link{verify_identification.PosteriorBSVARSV}},
#' \code{\link{verify_identification.PosteriorBSVARMIX}}, \code{\link{verify_identification.PosteriorBSVARMSH}},
#' \code{\link{verify_identification.PosteriorBSVART}}
#' 
#' @references 
#' Lütkepohl, H., and Woźniak, T., (2020) Bayesian Inference for Structural Vector Autoregressions Identified by Markov-Switching Heteroskedasticity. \emph{Journal of Economic Dynamics and Control} \bold{113}, 103862, \doi{10.1016/j.jedc.2020.103862}.
#' 
#' Lütkepohl, H., Shang, F., Uzeda, L., and Woźniak, T. (2024) Partial Identification of Heteroskedastic Structural VARs: Theory and Bayesian Inference. \emph{University of Melbourne Working Paper}, 1--57, \doi{10.48550/arXiv.2404.11057}.
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
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify heteroskedasticity
#' sddr           = verify_identification(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   verify_identification() -> sddr
#' 
#' @export
verify_identification <- function(posterior) {
  
  # call method
  UseMethod("verify_identification", posterior)
}





#' @inherit verify_identification
#' @method verify_identification PosteriorBSVAR
#' @inheritParams verify_identification
#'
#' @description Displays information that the model is homoskedastic and with normal shocks.
#' 
#' @return Nothing. Just displays a message.
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
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify heteroskedasticity
#' sddr           = verify_identification(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   verify_identification() -> sddr
#'   
#' @export
verify_identification.PosteriorBSVAR <- function(posterior) {
  message("The model is homoskedastic with normal shocks.")
}






#' @inherit verify_identification
#' @method verify_identification PosteriorBSVARSV
#' @inheritParams verify_identification
#'
#' @description Computes the logarithm of Bayes factor for the homoskedasticity 
#' hypothesis for each of the structural shocks via Savage-Dickey Density Ratio 
#' (SDDR). The hypothesis of homoskedasticity for the structural shock \code{n} 
#' is represented by restriction:
#' \deqn{H_0: \omega_n = 0}
#' The logarithm of Bayes factor for this hypothesis can be computed using the SDDR 
#' as the difference of the logarithm of the marginal posterior distribution ordinate at the restriction 
#' less the log-marginal prior distribution ordinate at the same point:
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
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify heteroskedasticity
#' sddr           = verify_identification(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   verify_identification() -> sddr
#'   
#' @export
verify_identification.PosteriorBSVARSV <- function(posterior) {
  
  centred_sv          = posterior$last_draw$centred_sv
  if ( centred_sv ) {
    message("There is not a known method of verifying heteroskedasticity using SDDR for a centered SV model.")
  } else {
    # get the inputs to estimation
    just_posterior  = posterior$posterior
    prior           = posterior$last_draw$prior$get_prior()
    Y               = posterior$last_draw$data_matrices$Y
    X               = posterior$last_draw$data_matrices$X
    
    # estimate the SDDR
    sddr            = .Call(`_bsvars_verify_volatility_sv_cpp`, just_posterior, prior, Y, X, TRUE)
    
    out             = list()
    out$logSDDR     = sddr$logSDDR
    out$logSDDR_se  = sddr$logSDDR_se
    class(out)     = "SDDRidSV"
    return(out)
  }
}




#' @inherit verify_identification
#' @method verify_identification PosteriorBSVARMIX
#' @inheritParams verify_identification
#'
#' @description Computes the logarithm of Bayes factor for the hypothesis of normality
#' for each of the structural shocks via Savage-Dickey Density Ration (SDDR).
#' The hypothesis of normality in this mixture of normals model is represented 
#' by restriction:
#' \deqn{H_0: \sigma^2_{n.1} = ... = \sigma^2_{n.M} = 1}
#' The logarithm of Bayes factor for this hypothesis can be computed using the SDDR 
#' as the difference of logarithms of the marginal posterior distribution ordinate at the restriction 
#' less the marginal prior distribution ordinate at the same point:
#' \deqn{log p(H_0 | data) - log p(H_0)}
#' Therefore, a negative value of the difference is the evidence against 
#' homoskedasticity of the structural shock. The estimation of both elements of the difference requires 
#' numerical integration.
#' 
#' @seealso \code{\link{specify_bsvar_mix}}, \code{\link{estimate}}
#'
#' @examples
#' # simple workflow
#' ############################################################
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' specification  = specify_bsvar_mix$new(us_fiscal_lsuw, p = 1, M = 2)
#' set.seed(123)
#' 
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify heteroskedasticity
#' sddr           = verify_identification(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_mix$new(p = 1, M = 2) |>
#'   estimate(S = 10) |> 
#'   verify_identification() -> sddr
#'   
#' @export
verify_identification.PosteriorBSVARMIX <- function(posterior) {
  
  # get the inputs to estimation
  just_posterior  = posterior$posterior
  prior           = posterior$last_draw$prior$get_prior()
  Y               = posterior$last_draw$data_matrices$Y
  X               = posterior$last_draw$data_matrices$X
  
  # estimate the SDDR
  sddr            = .Call(`_bsvars_verify_volatility_msh_cpp`, just_posterior, prior, Y, X)
  
  out             = list()
  out$logSDDR     = sddr$logSDDR
  out$logSDDR_se  = sddr$logSDDR_se
  class(out)     = "SDDRidMIX"
  return(out)
}





#' @inherit verify_identification
#' @method verify_identification PosteriorBSVARMSH
#' @inheritParams verify_identification
#'
#' @description Computes the logarithm of Bayes factor for the homoskedasticity hypothesis 
#' for each of the structural shocks via Savage-Dickey Density Ration (SDDR).
#' The hypothesis of homoskedasticity is represented by restriction:
#' \deqn{H_0: \sigma^2_{n.1} = ... = \sigma^2_{n.M} = 1}
#' The logarithm of Bayes factor for this hypothesis can be computed using the SDDR 
#' as the difference of logarithms of the marginal posterior distribution ordinate 
#' at the restriction less the marginal prior distribution ordinate at the same point:
#' \deqn{log p(H_0 | data) - log p(H_0)}
#' Therefore, a negative value of the difference is the evidence against 
#' homoskedasticity of the structural shock. The estimation of both elements of 
#' the difference requires numerical integration.
#' 
#' @seealso \code{\link{specify_bsvar_msh}}, \code{\link{estimate}}
#'
#' @examples
#' # simple workflow
#' ############################################################
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' specification  = specify_bsvar_msh$new(us_fiscal_lsuw, p = 1, M = 2)
#' set.seed(123)
#' 
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify heteroskedasticity
#' sddr           = verify_identification(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_msh$new(p = 1, M = 2) |>
#'   estimate(S = 10) |> 
#'   verify_identification() -> sddr
#'   
#' @export
verify_identification.PosteriorBSVARMSH <- function(posterior) {
  
  # get the inputs to estimation
  just_posterior  = posterior$posterior
  prior           = posterior$last_draw$prior$get_prior()
  Y               = posterior$last_draw$data_matrices$Y
  X               = posterior$last_draw$data_matrices$X
  
  # estimate the SDDR
  sddr            = .Call(`_bsvars_verify_volatility_msh_cpp`, just_posterior, prior, Y, X)
  
  out             = list()
  out$logSDDR     = sddr$logSDDR
  out$logSDDR_se  = sddr$logSDDR_se
  class(out)     = "SDDRidMSH"
  return(out)
}






#' @inherit verify_identification
#' @method verify_identification PosteriorBSVART
#' @inheritParams verify_identification
#'
#' @description Computes the logarithm of Bayes factor for the hypothesis of normality
#' of the joint conditional distribution of the structural shocks via 
#' Savage-Dickey Density Ration (SDDR).
#' The hypothesis of normality in this t-distributed shocks model is represented 
#' by restriction setting the degrees-of-freedom parameter \eqn{\nu} to infinity:
#' \deqn{H_0: \nu = \infty}
#' The logarithm of Bayes factor for this hypothesis can be computed using the SDDR 
#' as the difference of logarithms of the marginal posterior distribution ordinate at the restriction 
#' less the marginal prior distribution ordinate at the same point:
#' \deqn{log p(H_0 | data) - log p(H_0)}
#' Therefore, a negative value of the difference is the evidence against 
#' homoskedasticity of the structural shock. The estimation of the marginal posterior
#' ordinate is done using truncated Gaussian kernel smoothing.
#' 
#' @return An object of class \code{SDDRidT} that is a list with components:
#' 
#' \code{logSDDR} the value of the logarithm of the Bayes factor
#' 
#' \code{SDDR} the value of the Bayes factor
#' 
#' @seealso \code{\link{specify_bsvar_t}}, \code{\link{estimate}}
#'
#' @examples
#' # simple workflow
#' ############################################################
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' specification  = specify_bsvar_t$new(us_fiscal_lsuw)
#' set.seed(123)
#' 
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify heteroskedasticity
#' sddr           = verify_identification(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_t$new() |>
#'   estimate(S = 10) |> 
#'   verify_identification() -> sddr
#'   
#' @export
verify_identification.PosteriorBSVART <- function(posterior) {
  
  # get the inputs to estimation
  posterior_df    = posterior$posterior$df
  eta             = posterior_df / (1 + posterior_df)
  
  # estimate the SDDR
  sddr_numerator  = tail(stats::density(eta, to = 1, n = 1100)$y, 1)
  
  out             = list()
  out$logSDDR     = log(sddr_numerator)
  out$SDDR        = sddr_numerator
  class(out)      = "SDDRidT"
  return(out)
}

