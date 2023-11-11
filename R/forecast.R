
#' @title Forecasting using Structural Vector Autoregression
#'
#' @description Samples from the joint predictive density of all of the dependent variables at forecast horizons 
#' from 1 to \code{horizon} specified as an argument of the function.
#' 
#' @param posterior posterior estimation outcome - an object of either of the classes: 
#' PosteriorBSVAR, PosteriorBSVARMSH, PosteriorBSVARMIX, or PosteriorBSVARSV
#' obtained by running the \code{estimate} function.
#' @param horizon a positive integer, specifying the forecasting horizon.
#' @param exogenous_forecast a matrix of dimension \code{horizon x d} containing 
#' forecasted values of the exogenous variables.
#' 
#' @return A list of class \code{Forecasts} containing the
#' draws from the predictive density and for heteroskedastic models the draws from the predictive density of 
#' structural shocks conditional standard deviations. The output elements include:
#' 
#' \describe{
#'  \item{forecasts}{an \code{NxTxS} array with the draws from predictive density}
#'  \item{forecasts_sigma}{provided only for heteroskedastic models, an \code{NxTxS} array with the draws 
#'  from the predictive density of structural shocks conditional standard deviations}
#' }
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
#' # sample from predictive density 1 year ahead
#' predictive     = forecast(posterior, 4)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   forecast(horizon = 4) -> predictive
#' 
#' @export
forecast <- function(posterior, horizon, exogenous_forecast) {
  stopifnot("Argument horizon must be a positive integer number." = horizon > 0 & horizon %% 1 == 0)
  UseMethod("forecast", posterior)
}

#' @inherit forecast
#' @method forecast PosteriorBSVAR
#' @inheritParams forecast
#' @param posterior posterior estimation outcome - an object of class 
#' PosteriorBSVAR obtained by running the \code{estimate} function.
#' 
#' @return A list of class \code{Forecasts} containing the
#' draws from the predictive density. The output list includes element:
#' 
#' \describe{
#'  \item{forecasts}{an \code{NxTxS} array with the draws from predictive density}
#' }
#' 
#' @export
forecast.PosteriorBSVAR = function(posterior, horizon, exogenous_forecast = NULL) {
  
  
  posterior_B     = posterior$posterior$B
  posterior_A     = posterior$posterior$A
  T               = ncol(posterior$last_draw$data_matrices$X)
  X_T             = posterior$last_draw$data_matrices$X[,T]
  
  N               = nrow(posterior_B)
  K               = length(X_T)
  d               = K - N * posterior$last_draw$p - 1
  if (d == 0 ) {
    exogenous_forecast = matrix(NA, horizon, 0)
  } else {
    stopifnot("Forecasted values of exogenous variables are missing." = (d > 0) & !is.null(exogenous_forecast))
    stopifnot("The matrix of exogenous_forecast does not have a correct number of columns." = ncol(exogenous_forecast) == d)
    stopifnot("Provide exogenous_forecast for all forecast periods specified by argument horizon." = nrow(exogenous_forecast) == horizon)
    stopifnot("Argument exogenous has to be a matrix." = is.matrix(exogenous_forecast) & is.numeric(exogenous_forecast))
    stopifnot("Argument exogenous cannot include missing values." = sum(is.na(exogenous_forecast)) == 0 )
  }
  
  fore            = .Call(`_bsvars_forecast_bsvar`, posterior_B, posterior_A, X_T, exogenous_forecast, horizon)
  class(fore)     = "Forecasts"
  
  return(fore)
}



#' @inherit forecast
#' @method forecast PosteriorBSVARMSH
#' @inheritParams forecast
#' @param posterior posterior estimation outcome - an object of class 
#' PosteriorBSVARMSH obtained by running the \code{estimate} function.
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
#' # sample from predictive density 1 year ahead
#' predictive     = forecast(posterior, 4)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_msh$new(p = 1, M = 2) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   forecast(horizon = 4) -> predictive
#'   
#' @export
forecast.PosteriorBSVARMSH = function(posterior, horizon, exogenous_forecast = NULL) {
  
  posterior_B       = posterior$posterior$B
  posterior_A       = posterior$posterior$A
  posterior_sigma2  = posterior$posterior$sigma2
  posterior_PR_TR   = posterior$posterior$PR_TR
  T                 = ncol(posterior$last_draw$data_matrices$X)
  X_T               = posterior$last_draw$data_matrices$X[,T]
  S_T               = posterior$posterior$xi[,T,]
  
  N               = nrow(posterior_B)
  K               = length(X_T)
  d               = K - N * posterior$last_draw$p - 1
  if (d == 0 ) {
    exogenous_forecast = matrix(NA, horizon, 0)
  } else {
    stopifnot("Forecasted values of exogenous variables are missing." = (d > 0) & !is.null(exogenous_forecast))
    stopifnot("The matrix of exogenous_forecast does not have a correct number of columns." = ncol(exogenous_forecast) == d)
    stopifnot("Provide exogenous_forecast for all forecast periods specified by argument horizon." = nrow(exogenous_forecast) == horizon)
    stopifnot("Argument exogenous has to be a matrix." = is.matrix(exogenous_forecast) & is.numeric(exogenous_forecast))
    stopifnot("Argument exogenous cannot include missing values." = sum(is.na(exogenous_forecast)) == 0 )
  }
  
  fore            = .Call(`_bsvars_forecast_bsvar_msh`, posterior_B, posterior_A, posterior_sigma2, posterior_PR_TR, X_T, S_T, exogenous_forecast, horizon)
  class(fore)     = "Forecasts"
  
  return(fore)
}



#' @inherit forecast
#' @method forecast PosteriorBSVARMIX
#' @inheritParams forecast
#' @param posterior posterior estimation outcome - an object of class 
#' PosteriorBSVARMIX obtained by running the \code{estimate} function.
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
#' # sample from predictive density 1 year ahead
#' predictive     = forecast(posterior, 4)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_mix$new(p = 1, M = 2) |>
#'   estimate(S = 10) |>
#'   estimate(S = 20) |>  
#'   forecast(horizon = 4) -> predictive
#'   
#' @export
forecast.PosteriorBSVARMIX = function(posterior, horizon, exogenous_forecast = NULL) {
  
  posterior_B       = posterior$posterior$B
  posterior_A       = posterior$posterior$A
  posterior_sigma2  = posterior$posterior$sigma2
  posterior_PR_TR   = posterior$posterior$PR_TR
  T                 = ncol(posterior$last_draw$data_matrices$X)
  X_T               = posterior$last_draw$data_matrices$X[,T]
  S_T               = posterior$posterior$xi[,T,]
  
  N               = nrow(posterior_B)
  K               = length(X_T)
  d               = K - N * posterior$last_draw$p - 1
  if (d == 0 ) {
    exogenous_forecast = matrix(NA, horizon, 0)
  } else {
    stopifnot("Forecasted values of exogenous variables are missing." = (d > 0) & !is.null(exogenous_forecast))
    stopifnot("The matrix of exogenous_forecast does not have a correct number of columns." = ncol(exogenous_forecast) == d)
    stopifnot("Provide exogenous_forecast for all forecast periods specified by argument horizon." = nrow(exogenous_forecast) == horizon)
    stopifnot("Argument exogenous has to be a matrix." = is.matrix(exogenous_forecast) & is.numeric(exogenous_forecast))
    stopifnot("Argument exogenous cannot include missing values." = sum(is.na(exogenous_forecast)) == 0 )
  }
  
  fore            = .Call(`_bsvars_forecast_bsvar_msh`, posterior_B, posterior_A, posterior_sigma2, posterior_PR_TR, X_T, S_T, exogenous_forecast, horizon)
  class(fore)     = "Forecasts"
  
  return(fore)
}



#' @inherit forecast
#' @method forecast PosteriorBSVARSV
#' @inheritParams forecast
#' @param posterior posterior estimation outcome - an object of class 
#' PosteriorBSVARSV obtained by running the \code{estimate} function.
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
#' posterior      = estimate(burn_in, 10, thin = 2)
#' 
#' # sample from predictive density 1 year ahead
#' predictive     = forecast(posterior, 4)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new(p = 1) |>
#'   estimate(S = 10) |>
#'   estimate(S = 10, thin = 2) |>  
#'   forecast(horizon = 4) -> predictive
#'   
#' @export
forecast.PosteriorBSVARSV = function(posterior, horizon, exogenous_forecast = NULL) {
  
  posterior_B       = posterior$posterior$B
  posterior_A       = posterior$posterior$A
  posterior_rho     = posterior$posterior$rho
  posterior_omega   = posterior$posterior$omega
  
  T                 = ncol(posterior$last_draw$data_matrices$X)
  X_T               = posterior$last_draw$data_matrices$X[,T]
  posterior_h_T     = posterior$posterior$h[,T,]
  centred_sv        = posterior$last_draw$centred_sv
  
  N               = nrow(posterior_B)
  K               = length(X_T)
  d               = K - N * posterior$last_draw$p - 1
  if (d == 0 ) {
    exogenous_forecast = matrix(NA, horizon, 0)
  } else {
    stopifnot("Forecasted values of exogenous variables are missing." = (d > 0) & !is.null(exogenous_forecast))
    stopifnot("The matrix of exogenous_forecast does not have a correct number of columns." = ncol(exogenous_forecast) == d)
    stopifnot("Provide exogenous_forecast for all forecast periods specified by argument horizon." = nrow(exogenous_forecast) == horizon)
    stopifnot("Argument exogenous has to be a matrix." = is.matrix(exogenous_forecast) & is.numeric(exogenous_forecast))
    stopifnot("Argument exogenous cannot include missing values." = sum(is.na(exogenous_forecast)) == 0 )
  }

  fore            = .Call(`_bsvars_forecast_bsvar_sv`, posterior_B, posterior_A, posterior_h_T, posterior_rho, posterior_omega, X_T, exogenous_forecast, horizon, centred_sv)
  class(fore)     = "Forecasts"
  
  return(fore)
}
