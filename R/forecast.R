
#' @export
generics::forecast


#' @title Forecasting using Bayesian Structural Vector Autoregression
#'
#' @description Samples from the joint predictive density of all of the dependent 
#' variables for models at forecast horizons from 1 to \code{horizon} specified as 
#' an argument of the function.
#' 
#' @method forecast PosteriorBSVAR
#' @param object posterior estimation outcome - an object of class 
#' \code{PosteriorBSVAR} obtained by running the \code{estimate} function.
#' @param horizon a positive integer, specifying the forecasting horizon.
#' @param exogenous_forecast a matrix of dimension \code{horizon x d} containing 
#' forecasted values of the exogenous variables. 
#' @param conditional_forecast a \code{horizon x N} matrix with forecasted values 
#' for selected variables. It should only contain \code{numeric} or \code{NA} 
#' values. The entries with \code{NA} values correspond to the values that are 
#' forecasted conditionally on the realisations provided as \code{numeric} values.
#' @param ... not used
#' 
#' @return A list of class \code{Forecasts} containing the
#' draws from the predictive density and for heteroskedastic models the draws 
#' from the predictive density of structural shocks conditional standard 
#' deviations and data. The output elements include:
#' 
#' \describe{
#'  \item{forecasts}{an \code{NxTxS} array with the draws from predictive density}
#'  \item{Y}{an \eqn{NxT} matrix with the data on dependent variables}
#'  \item{forecast_mean}{an \code{NxTxS} array with the mean of the predictive density}
#'  \item{forecast_covariance}{an \code{NxTxS} array with the covariance of the predictive density}
#' }
#' 
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' specification  = specify_bsvar$new(us_fiscal_lsuw)
#' burn_in        = estimate(specification, 5)
#' posterior      = estimate(burn_in, 5)
#' predictive     = forecast(posterior, 4)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar$new() |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
#'   forecast(horizon = 4) -> predictive
#' 
#' # conditional forecasting using a model with exogenous variables
#' ############################################################
#' specification  = specify_bsvar$new(us_fiscal_lsuw, exogenous = us_fiscal_ex)
#' burn_in        = estimate(specification, 5)
#' posterior      = estimate(burn_in, 5)
#' 
#' # forecast 2 years ahead
#' predictive     = forecast(
#'                     posterior, 
#'                     horizon = 8,
#'                     exogenous_forecast = us_fiscal_ex_forecasts,
#'                     conditional_forecast = us_fiscal_cond_forecasts
#'                   )
#' summary(predictive)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar$new( exogenous = us_fiscal_ex) |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
#'   forecast(
#'     horizon = 8,
#'     exogenous_forecast = us_fiscal_ex_forecasts,
#'     conditional_forecast = us_fiscal_cond_forecasts
#'   ) |> plot()
#' 
#' @export
forecast.PosteriorBSVAR = function(
    object, 
    horizon = 1, 
    exogenous_forecast = NULL,
    conditional_forecast = NULL,
    ...
) {
  
  stopifnot("Argument horizon must be a positive integer number." = horizon > 0 & horizon %% 1 == 0)
  
  posterior_B     = object$posterior$B
  posterior_A     = object$posterior$A
  T               = ncol(object$last_draw$data_matrices$X)
  X_T             = object$last_draw$data_matrices$X[,T]
  Y               = object$last_draw$data_matrices$Y
  posterior_df    = object$posterior$df
  normal          = object$last_draw$get_normal()
  
  N               = nrow(posterior_B)
  K               = length(X_T)
  d               = K - N * object$last_draw$p - 1
  S               = dim(posterior_B)[3]
  
  # prepare forecasting with exogenous variables
  if (d == 0 ) {
    exogenous_forecast = matrix(NA, horizon, 1)
  } else {
    stopifnot("Forecasted values of exogenous variables are missing." = (d > 0) & !is.null(exogenous_forecast))
    stopifnot("The matrix of exogenous_forecast does not have a correct number of columns." = ncol(exogenous_forecast) == d)
    stopifnot("Provide exogenous_forecast for all forecast periods specified by argument horizon." = nrow(exogenous_forecast) == horizon)
    stopifnot("Argument exogenous has to be a matrix." = is.matrix(exogenous_forecast) & is.numeric(exogenous_forecast))
    stopifnot("Argument exogenous cannot include missing values." = sum(is.na(exogenous_forecast)) == 0 )
  }
  
  # prepare forecasting with conditional forecasts
  if ( is.null(conditional_forecast) ) {
    # this will not be used for forecasting, but needs to be provided
    conditional_forecast = matrix(NA, horizon, N)
  } else {
    stopifnot("Argument conditional_forecast must be a matrix with numeric values."
              = is.matrix(conditional_forecast) & is.numeric(conditional_forecast)
    )
    stopifnot("Argument conditional_forecast must have the number of rows equal to 
              the value of argument horizon."
              = nrow(conditional_forecast) == horizon
    )
    stopifnot("Argument conditional_forecast must have the number of columns 
              equal to the number of columns in the used data."
              = ncol(conditional_forecast) == N
    )
  }
  
  # forecast volatility
  if (normal) {
    forecast_sigma2   = array(1, c(N, horizon, S))
  } else {
    forecast_sigma2 = .Call(`_bsvars_forecast_lambda_t`, 
                            posterior_df,
                            horizon
    ) # END .Call
  }
  
  # perform forecasting
  output      = .Call(`_bsvars_forecast_bsvars`, 
                      posterior_B,
                      posterior_A,
                      forecast_sigma2,    # (N, horizon, S)
                      X_T,
                      exogenous_forecast,
                      conditional_forecast,
                      horizon
                ) # END .Call
  
  forecast_covariance         = array(NA, c(N, N, horizon, S))
  for (s in 1:S) forecast_covariance[,,,s] = output$forecast_cov[s,][[1]]
  output$forecast_covariance  = forecast_covariance
  
  # output$forecasts_sigma = forecast_sigma2
  output$Y          = Y
  class(output)     = "Forecasts"
  
  return(output)
} # END forecast.PosteriorBSVAR








#' @inherit forecast.PosteriorBSVAR
#' @method forecast PosteriorBSVAREXH
#' @param object posterior estimation outcome - an object of class 
#' \code{PosteriorBSVAREXH} obtained by running the \code{estimate} function.
#' @param horizon a positive integer, specifying the forecasting horizon.
#' @param exogenous_forecast a matrix of dimension \code{horizon x d} containing 
#' forecasted values of the exogenous variables.
#' @param conditional_forecast a \code{horizon x N} matrix with forecasted values 
#' for selected variables. It should only contain \code{numeric} or \code{NA} 
#' values. The entries with \code{NA} values correspond to the values that are 
#' forecasted conditionally on the realisations provided as \code{numeric} values.
#' 
#' @examples
#' specification  = specify_bsvar_exh$new(us_fiscal_lsuw)
#' burn_in        = estimate(specification, 5)
#' posterior      = estimate(burn_in, 5)
#' predictive     = forecast(posterior, 4)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar_exh$new() |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
#'   forecast(horizon = 4) -> predictive
#'   
#' # conditional forecasting using a model with exogenous variables
#' ############################################################
#' specification  = specify_bsvar_exh$new(us_fiscal_lsuw, exogenous = us_fiscal_ex)
#' burn_in        = estimate(specification, 5)
#' posterior      = estimate(burn_in, 5)
#' 
#' # forecast 2 years ahead
#' predictive     = forecast(
#'                     posterior, 
#'                     horizon = 8,
#'                     exogenous_forecast = us_fiscal_ex_forecasts,
#'                     conditional_forecast = us_fiscal_cond_forecasts
#'                   )
#' summary(predictive)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar_exh$new(exogenous = us_fiscal_ex) |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
#'   forecast(
#'     horizon = 8,
#'     exogenous_forecast = us_fiscal_ex_forecasts,
#'     conditional_forecast = us_fiscal_cond_forecasts
#'   ) |> plot()
#'   
#' @export
forecast.PosteriorBSVAREXH = function(
    object, 
    horizon = 1, 
    exogenous_forecast = NULL,
    conditional_forecast = NULL,
    ...
) {
  
  stopifnot("Argument horizon must be a positive integer number." = horizon > 0 & horizon %% 1 == 0)
  
  posterior_B       = object$posterior$B
  posterior_A       = object$posterior$A
  posterior_sigma2  = object$posterior$sigma2
  T                 = ncol(object$last_draw$data_matrices$X)
  X_T               = object$last_draw$data_matrices$X[,T]
  Y                 = object$last_draw$data_matrices$Y
  sigma2_T        = object$posterior$sigma[,T,]^2
  posterior_df    = object$posterior$df
  normal          = object$last_draw$get_normal()
  
  N               = nrow(posterior_B)
  K               = length(X_T)
  d               = K - N * object$last_draw$p - 1
  S               = dim(posterior_B)[3]
  
  # prepare forecasting with exogenous variables
  if (d == 0 ) {
    exogenous_forecast = matrix(NA, horizon, 1)
  } else {
    stopifnot("Forecasted values of exogenous variables are missing." = (d > 0) & !is.null(exogenous_forecast))
    stopifnot("The matrix of exogenous_forecast does not have a correct number of columns." = ncol(exogenous_forecast) == d)
    stopifnot("Provide exogenous_forecast for all forecast periods specified by argument horizon." = nrow(exogenous_forecast) == horizon)
    stopifnot("Argument exogenous has to be a matrix." = is.matrix(exogenous_forecast) & is.numeric(exogenous_forecast))
    stopifnot("Argument exogenous cannot include missing values." = sum(is.na(exogenous_forecast)) == 0 )
  }
  
  # prepare forecasting with conditional forecasts
  if ( is.null(conditional_forecast) ) {
    # this will not be used for forecasting, but needs to be provided
    conditional_forecast = matrix(NA, horizon, N)
  } else {
    stopifnot("Argument conditional_forecast must be a matrix with numeric values."
              = is.matrix(conditional_forecast) & is.numeric(conditional_forecast)
    )
    stopifnot("Argument conditional_forecast must have the number of rows equal to 
              the value of argument horizon."
              = nrow(conditional_forecast) == horizon
    )
    stopifnot("Argument conditional_forecast must have the number of columns 
              equal to the number of columns in the used data."
              = ncol(conditional_forecast) == N
    )
  }
  
  # forecast volatility
  forecast_sigma2   = array(NA, c(N, horizon, S))
  for (h in 1:horizon) {
    forecast_sigma2[,h,]   = sigma2_T
  }
  
  # for Student-t shocks
  if (!normal) {
    forecast_lambda = .Call(`_bsvars_forecast_lambda_t`, 
                            posterior_df,
                            horizon
    ) # END .Call
    forecast_sigma2 = forecast_sigma2 * forecast_lambda
  }
  
  # perform forecasting
  output       = .Call(`_bsvars_forecast_bsvars`, 
                       posterior_B,
                       posterior_A,
                       forecast_sigma2,    # (N, horizon, S)
                       X_T,
                       exogenous_forecast,
                       conditional_forecast,
                       horizon
  ) # END .Call
  
  forecast_covariance         = array(NA, c(N, N, horizon, S))
  for (s in 1:S) forecast_covariance[,,,s] = output$forecast_cov[s,][[1]]
  output$forecast_covariance  = forecast_covariance
  
  output$Y          = Y
  class(output)     = "Forecasts"
  
  return(output)
} # END forecast.PosteriorBSVAREXH







#' @inherit forecast.PosteriorBSVAR
#' @method forecast PosteriorBSVARHMSH
#' @param object posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARHMSH} obtained by running the \code{estimate} function.
#' @param horizon a positive integer, specifying the forecasting horizon.
#' @param exogenous_forecast a matrix of dimension \code{horizon x d} containing 
#' forecasted values of the exogenous variables.
#' @param conditional_forecast a \code{horizon x N} matrix with forecasted values 
#' for selected variables. It should only contain \code{numeric} or \code{NA} 
#' values. The entries with \code{NA} values correspond to the values that are 
#' forecasted conditionally on the realisations provided as \code{numeric} values.
#' 
#' @examples
#' specification  = specify_bsvar_hmsh$new(us_fiscal_lsuw, M = 2)
#' burn_in        = estimate(specification, 5)
#' posterior      = estimate(burn_in, 5)
#' predictive     = forecast(posterior, 4)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar_hmsh$new(M = 2) |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
#'   forecast(horizon = 4) -> predictive
#'   
#' # forecasting using a model with exogenous variables
#' ############################################################
#' specification  = specify_bsvar_hmsh$new(us_fiscal_lsuw, M = 2, exogenous = us_fiscal_ex)
#' burn_in        = estimate(specification, 5)
#' posterior      = estimate(burn_in, 5)
#' 
#' # forecast 2 years ahead
#' predictive     = forecast(
#'                     posterior, 
#'                     horizon = 8,
#'                     exogenous_forecast = us_fiscal_ex_forecasts
#'                   )
#' summary(predictive)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar_hmsh$new(M = 2, exogenous = us_fiscal_ex) |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
#'   forecast(
#'     horizon = 8,
#'     exogenous_forecast = us_fiscal_ex_forecasts
#'   ) |> plot()
#'   
#' @export
forecast.PosteriorBSVARHMSH = function(
    object, 
    horizon = 1, 
    exogenous_forecast = NULL,
    conditional_forecast = NULL,
    ...
) {
  
  stopifnot("Argument horizon must be a positive integer number." = horizon > 0 & horizon %% 1 == 0)
  
  posterior_B       = object$posterior$B
  posterior_A       = object$posterior$A
  posterior_sigma2  = object$posterior$sigma2
  posterior_PR_TR   = object$posterior$PR_TR_cpp
  T                 = ncol(object$last_draw$data_matrices$X)
  X_T               = object$last_draw$data_matrices$X[,T]
  Y                 = object$last_draw$data_matrices$Y
  posterior_df    = object$posterior$df
  normal          = object$last_draw$get_normal()
  
  M               = ncol(posterior_sigma2)
  N               = nrow(posterior_B)
  K               = length(X_T)
  d               = K - N * object$last_draw$p - 1
  S               = dim(posterior_B)[3]
  
  S_T             = array(NA, c(M,N,S))
  for (s in 1:S) {
    S_T[,,s]      = object$posterior$xi_cpp[S,1][[1]][,T,]
  }
    
  # prepare forecasting with exogenous variables
  if (d == 0 ) {
    exogenous_forecast = matrix(NA, horizon, 1)
  } else {
    stopifnot("Forecasted values of exogenous variables are missing." = (d > 0) & !is.null(exogenous_forecast))
    stopifnot("The matrix of exogenous_forecast does not have a correct number of columns." = ncol(exogenous_forecast) == d)
    stopifnot("Provide exogenous_forecast for all forecast periods specified by argument horizon." = nrow(exogenous_forecast) == horizon)
    stopifnot("Argument exogenous has to be a matrix." = is.matrix(exogenous_forecast) & is.numeric(exogenous_forecast))
    stopifnot("Argument exogenous cannot include missing values." = sum(is.na(exogenous_forecast)) == 0 )
  }
  
  # prepare forecasting with conditional forecasts
  if ( is.null(conditional_forecast) ) {
    # this will not be used for forecasting, but needs to be provided
    conditional_forecast = matrix(NA, horizon, N)
  } else {
    stopifnot("Argument conditional_forecast must be a matrix with numeric values."
              = is.matrix(conditional_forecast) & is.numeric(conditional_forecast)
    )
    stopifnot("Argument conditional_forecast must have the number of rows equal to 
              the value of argument horizon."
              = nrow(conditional_forecast) == horizon
    )
    stopifnot("Argument conditional_forecast must have the number of columns 
              equal to the number of columns in the used data."
              = ncol(conditional_forecast) == N
    )
  }
  
  # forecast volatility
  forecast_sigma2   = .Call(`_bsvars_forecast_sigma2_hmsh`, 
                            posterior_sigma2,
                            posterior_PR_TR,
                            S_T,
                            horizon
  )  # END .Call

  # for Student-t shocks
  if (!normal) {
    forecast_lambda = .Call(`_bsvars_forecast_lambda_t`, 
                            posterior_df,
                            horizon
    ) # END .Call
    forecast_sigma2 = forecast_sigma2 * forecast_lambda
  }
  
  # perform forecasting
  output       = .Call(`_bsvars_forecast_bsvars`, 
                      posterior_B,
                      posterior_A,
                      forecast_sigma2,    # (N, horizon, S)
                      X_T,
                      exogenous_forecast,
                      conditional_forecast,
                      horizon
  ) # END .Call
  
  forecast_covariance         = array(NA, c(N, N, horizon, S))
  for (s in 1:S) forecast_covariance[,,,s] = output$forecast_cov[s,][[1]]
  output$forecast_covariance  = forecast_covariance
  
  output$Y          = Y
  class(output)     = "Forecasts"
  
  return(output)
} # END forecast.PosteriorBSVARHMSH










#' @inherit forecast.PosteriorBSVAR
#' @method forecast PosteriorBSVARMSH
#' @param object posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARMSH} obtained by running the \code{estimate} function.
#' @param horizon a positive integer, specifying the forecasting horizon.
#' @param exogenous_forecast a matrix of dimension \code{horizon x d} containing 
#' forecasted values of the exogenous variables.
#' @param conditional_forecast a \code{horizon x N} matrix with forecasted values 
#' for selected variables. It should only contain \code{numeric} or \code{NA} 
#' values. The entries with \code{NA} values correspond to the values that are 
#' forecasted conditionally on the realisations provided as \code{numeric} values.
#' 
#' @examples
#' specification  = specify_bsvar_msh$new(us_fiscal_lsuw, M = 2)
#' burn_in        = estimate(specification, 5)
#' posterior      = estimate(burn_in, 5)
#' predictive     = forecast(posterior, 4)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar_msh$new(M = 2) |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
#'   forecast(horizon = 4) -> predictive
#'   
#' # conditional forecasting using a model with exogenous variables
#' ############################################################
#' specification  = specify_bsvar_msh$new(us_fiscal_lsuw, M = 2, exogenous = us_fiscal_ex)
#' burn_in        = estimate(specification, 5)
#' posterior      = estimate(burn_in, 5)
#' 
#' # forecast 2 years ahead
#' predictive     = forecast(
#'                     posterior, 
#'                     horizon = 8,
#'                     exogenous_forecast = us_fiscal_ex_forecasts,
#'                     conditional_forecast = us_fiscal_cond_forecasts
#'                   )
#' summary(predictive)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar_msh$new(M = 2, exogenous = us_fiscal_ex) |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
#'   forecast(
#'     horizon = 8,
#'     exogenous_forecast = us_fiscal_ex_forecasts,
#'     conditional_forecast = us_fiscal_cond_forecasts
#'   ) |> plot()
#'   
#' @export
forecast.PosteriorBSVARMSH = function(
    object, 
    horizon = 1, 
    exogenous_forecast = NULL,
    conditional_forecast = NULL,
    ...
) {
  
  stopifnot("Argument horizon must be a positive integer number." = horizon > 0 & horizon %% 1 == 0)
  
  posterior_B       = object$posterior$B
  posterior_A       = object$posterior$A
  posterior_sigma2  = object$posterior$sigma2
  posterior_PR_TR   = object$posterior$PR_TR
  T                 = ncol(object$last_draw$data_matrices$X)
  X_T               = object$last_draw$data_matrices$X[,T]
  Y                 = object$last_draw$data_matrices$Y
  S_T               = object$posterior$xi[,T,]
  posterior_df    = object$posterior$df
  normal          = object$last_draw$get_normal()
  
  N               = nrow(posterior_B)
  K               = length(X_T)
  d               = K - N * object$last_draw$p - 1
  S               = dim(posterior_B)[3]
  
  # prepare forecasting with exogenous variables
  if (d == 0 ) {
    exogenous_forecast = matrix(NA, horizon, 1)
  } else {
    stopifnot("Forecasted values of exogenous variables are missing." = (d > 0) & !is.null(exogenous_forecast))
    stopifnot("The matrix of exogenous_forecast does not have a correct number of columns." = ncol(exogenous_forecast) == d)
    stopifnot("Provide exogenous_forecast for all forecast periods specified by argument horizon." = nrow(exogenous_forecast) == horizon)
    stopifnot("Argument exogenous has to be a matrix." = is.matrix(exogenous_forecast) & is.numeric(exogenous_forecast))
    stopifnot("Argument exogenous cannot include missing values." = sum(is.na(exogenous_forecast)) == 0 )
  }
  
  # prepare forecasting with conditional forecasts
  if ( is.null(conditional_forecast) ) {
    # this will not be used for forecasting, but needs to be provided
    conditional_forecast = matrix(NA, horizon, N)
  } else {
    stopifnot("Argument conditional_forecast must be a matrix with numeric values."
              = is.matrix(conditional_forecast) & is.numeric(conditional_forecast)
    )
    stopifnot("Argument conditional_forecast must have the number of rows equal to 
              the value of argument horizon."
              = nrow(conditional_forecast) == horizon
    )
    stopifnot("Argument conditional_forecast must have the number of columns 
              equal to the number of columns in the used data."
              = ncol(conditional_forecast) == N
    )
  }
  
  # forecast volatility
  forecast_sigma2   = .Call(`_bsvars_forecast_sigma2_msh`, 
                            posterior_sigma2,
                            posterior_PR_TR,
                            S_T,
                            horizon
                      )  # END .Call
  
  # for Student-t shocks
  if (!normal) {
    forecast_lambda = .Call(`_bsvars_forecast_lambda_t`, 
                            posterior_df,
                            horizon
    ) # END .Call
    forecast_sigma2 = forecast_sigma2 * forecast_lambda
  }
  
  # perform forecasting
  output       = .Call(`_bsvars_forecast_bsvars`, 
                      posterior_B,
                      posterior_A,
                      forecast_sigma2,    # (N, horizon, S)
                      X_T,
                      exogenous_forecast,
                      conditional_forecast,
                      horizon
                  ) # END .Call
  
  forecast_covariance         = array(NA, c(N, N, horizon, S))
  for (s in 1:S) forecast_covariance[,,,s] = output$forecast_cov[s,][[1]]
  output$forecast_covariance  = forecast_covariance
  
  output$Y          = Y
  class(output)     = "Forecasts"
  
  return(output)
} # END forecast.PosteriorBSVARMSH



#' @inherit forecast.PosteriorBSVAR
#' @method forecast PosteriorBSVARMIX
#' @param object posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARMIX} obtained by running the \code{estimate} function.
#' @param horizon a positive integer, specifying the forecasting horizon.
#' @param exogenous_forecast a matrix of dimension \code{horizon x d} containing 
#' forecasted values of the exogenous variables.
#' @param conditional_forecast a \code{horizon x N} matrix with forecasted values 
#' for selected variables. It should only contain \code{numeric} or \code{NA} 
#' values. The entries with \code{NA} values correspond to the values that are 
#' forecasted conditionally on the realisations provided as \code{numeric} values.
#' 
#' @examples
#' specification  = specify_bsvar_mix$new(us_fiscal_lsuw, M = 2)
#' burn_in        = estimate(specification, 5)
#' posterior      = estimate(burn_in, 5)
#' predictive     = forecast(posterior, 4)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar_mix$new(M = 2) |>
#'   estimate(S = 5) |>
#'   estimate(S = 5) |>  
#'   forecast(horizon = 4) -> predictive
#'   
#' # conditional forecasting using a model with exogenous variables
#' ############################################################
#' specification  = specify_bsvar_mix$new(us_fiscal_lsuw, M = 2, exogenous = us_fiscal_ex)
#' burn_in        = estimate(specification, 5)
#' posterior      = estimate(burn_in, 5)
#' 
#' # forecast 2 years ahead
#' predictive     = forecast(
#'                     posterior, 
#'                     horizon = 8,
#'                     exogenous_forecast = us_fiscal_ex_forecasts,
#'                     conditional_forecast = us_fiscal_cond_forecasts
#'                   )
#' summary(predictive)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar_mix$new(M = 2, exogenous = us_fiscal_ex) |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
#'   forecast(
#'     horizon = 8,
#'     exogenous_forecast = us_fiscal_ex_forecasts,
#'     conditional_forecast = us_fiscal_cond_forecasts
#'   ) |> plot()
#'  
#' @export
forecast.PosteriorBSVARMIX = function(
    object, 
    horizon = 1, 
    exogenous_forecast = NULL,
    conditional_forecast = NULL,
    ...
) {
  
  stopifnot("Argument horizon must be a positive integer number." = horizon > 0 & horizon %% 1 == 0)
  
  posterior_B       = object$posterior$B
  posterior_A       = object$posterior$A
  posterior_sigma2  = object$posterior$sigma2
  posterior_PR_TR   = object$posterior$PR_TR
  T                 = ncol(object$last_draw$data_matrices$X)
  X_T               = object$last_draw$data_matrices$X[,T]
  Y                 = object$last_draw$data_matrices$Y
  S_T               = object$posterior$xi[,T,]
  posterior_df    = object$posterior$df
  normal          = object$last_draw$get_normal()
  
  N               = nrow(posterior_B)
  K               = length(X_T)
  d               = K - N * object$last_draw$p - 1
  S               = dim(posterior_B)[3]
  
  # prepare forecasting with exogenous variables
  if (d == 0 ) {
    exogenous_forecast = matrix(NA, horizon, 1)
  } else {
    stopifnot("Forecasted values of exogenous variables are missing." = (d > 0) & !is.null(exogenous_forecast))
    stopifnot("The matrix of exogenous_forecast does not have a correct number of columns." = ncol(exogenous_forecast) == d)
    stopifnot("Provide exogenous_forecast for all forecast periods specified by argument horizon." = nrow(exogenous_forecast) == horizon)
    stopifnot("Argument exogenous has to be a matrix." = is.matrix(exogenous_forecast) & is.numeric(exogenous_forecast))
    stopifnot("Argument exogenous cannot include missing values." = sum(is.na(exogenous_forecast)) == 0 )
  }
  
  # prepare forecasting with conditional forecasts
  if ( is.null(conditional_forecast) ) {
    # this will not be used for forecasting, but needs to be provided
    conditional_forecast = matrix(NA, horizon, N)
  } else {
    stopifnot("Argument conditional_forecast must be a matrix with numeric values."
              = is.matrix(conditional_forecast) & is.numeric(conditional_forecast)
    )
    stopifnot("Argument conditional_forecast must have the number of rows equal to 
              the value of argument horizon."
              = nrow(conditional_forecast) == horizon
    )
    stopifnot("Argument conditional_forecast must have the number of columns 
              equal to the number of columns in the used data."
              = ncol(conditional_forecast) == N
    )
  }
  
  # forecast volatility
  forecast_sigma2   = .Call(`_bsvars_forecast_sigma2_msh`, 
                            posterior_sigma2,
                            posterior_PR_TR,
                            S_T,
                            horizon
  ) # END .Call
  
  # for Student-t shocks
  if (!normal) {
    forecast_lambda = .Call(`_bsvars_forecast_lambda_t`, 
                            posterior_df,
                            horizon
    ) # END .Call
    forecast_sigma2 = forecast_sigma2 * forecast_lambda
  }
  
  # perform forecasting
  output       = .Call(`_bsvars_forecast_bsvars`, 
                      posterior_B,
                      posterior_A,
                      forecast_sigma2,    # (N, horizon, S)
                      X_T,
                      exogenous_forecast,
                      conditional_forecast,
                      horizon
  ) # END .Call
  
  forecast_covariance         = array(NA, c(N, N, horizon, S))
  for (s in 1:S) forecast_covariance[,,,s] = output$forecast_cov[s,][[1]]
  output$forecast_covariance  = forecast_covariance
  
  output$Y          = Y
  class(output)     = "Forecasts"
  
  return(output)
} # END forecast.PosteriorBSVARMIX



#' @inherit forecast.PosteriorBSVAR
#' @method forecast PosteriorBSVARSV
#' @param object posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARSV} obtained by running the \code{estimate} function.
#' @param horizon a positive integer, specifying the forecasting horizon.
#' @param exogenous_forecast a matrix of dimension \code{horizon x d} containing 
#' forecasted values of the exogenous variables.
#' @param conditional_forecast a \code{horizon x N} matrix with forecasted values 
#' for selected variables. It should only contain \code{numeric} or \code{NA} 
#' values. The entries with \code{NA} values correspond to the values that are 
#' forecasted conditionally on the realisations provided as \code{numeric} values.
#' 
#' @examples
#' specification  = specify_bsvar_sv$new(us_fiscal_lsuw)
#' burn_in        = estimate(specification, 5)
#' posterior      = estimate(burn_in, 5)
#' predictive     = forecast(posterior, 2)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new() |>
#'   estimate(S = 5) |>
#'   estimate(S = 5) |>  
#'   forecast(horizon = 2) -> predictive
#'   
#' # conditional forecasting using a model with exogenous variables
#' ############################################################
#' specification  = specify_bsvar_sv$new(us_fiscal_lsuw, exogenous = us_fiscal_ex)
#' burn_in        = estimate(specification, 5)
#' posterior      = estimate(burn_in, 5)
#' 
#' # forecast 2 years ahead
#' predictive     = forecast(
#'                     posterior, 
#'                     horizon = 8,
#'                     exogenous_forecast = us_fiscal_ex_forecasts,
#'                     conditional_forecast = us_fiscal_cond_forecasts
#'                   )
#' summary(predictive)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new(exogenous = us_fiscal_ex) |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
#'   forecast(
#'     horizon = 8,
#'     exogenous_forecast = us_fiscal_ex_forecasts,
#'     conditional_forecast = us_fiscal_cond_forecasts
#'   ) |> plot()
#'
#' @export
forecast.PosteriorBSVARSV = function(
    object, 
    horizon = 1, 
    exogenous_forecast = NULL,
    conditional_forecast = NULL,
    ...
) {
  
  stopifnot("Argument horizon must be a positive integer number." = horizon > 0 & horizon %% 1 == 0)
  
  posterior_B       = object$posterior$B
  posterior_A       = object$posterior$A
  posterior_rho     = object$posterior$rho
  posterior_omega   = object$posterior$omega
  
  T                 = ncol(object$last_draw$data_matrices$X)
  X_T               = object$last_draw$data_matrices$X[,T]
  Y                 = object$last_draw$data_matrices$Y
  posterior_h_T     = object$posterior$h[,T,]
  centred_sv        = object$last_draw$centred_sv
  posterior_df    = object$posterior$df
  normal          = object$last_draw$get_normal()
  
  N               = nrow(posterior_B)
  K               = length(X_T)
  d               = K - N * object$last_draw$p - 1
  S               = dim(posterior_B)[3]
  
  # prepare forecasting with exogenous variables
  if (d == 0 ) {
    exogenous_forecast = matrix(NA, horizon, 1)
  } else {
    stopifnot("Forecasted values of exogenous variables are missing." = (d > 0) & !is.null(exogenous_forecast))
    stopifnot("The matrix of exogenous_forecast does not have a correct number of columns." = ncol(exogenous_forecast) == d)
    stopifnot("Provide exogenous_forecast for all forecast periods specified by argument horizon." = nrow(exogenous_forecast) == horizon)
    stopifnot("Argument exogenous has to be a matrix." = is.matrix(exogenous_forecast) & is.numeric(exogenous_forecast))
    stopifnot("Argument exogenous cannot include missing values." = sum(is.na(exogenous_forecast)) == 0 )
  }
  
  # prepare forecasting with conditional forecasts
  if ( is.null(conditional_forecast) ) {
    # this will not be used for forecasting, but needs to be provided
    conditional_forecast = matrix(NA, horizon, N)
  } else {
    stopifnot("Argument conditional_forecast must be a matrix with numeric values."
              = is.matrix(conditional_forecast) & is.numeric(conditional_forecast)
    )
    stopifnot("Argument conditional_forecast must have the number of rows equal to 
              the value of argument horizon."
              = nrow(conditional_forecast) == horizon
    )
    stopifnot("Argument conditional_forecast must have the number of columns 
              equal to the number of columns in the used data."
              = ncol(conditional_forecast) == N
    )
  }
  
  # forecast volatility
  forecast_sigma2   = .Call(`_bsvars_forecast_sigma2_sv`, 
                            posterior_h_T,
                            posterior_rho,
                            posterior_omega,
                            horizon,
                            centred_sv
                      ) # END .Call
                            
  # for Student-t shocks
  if (!normal) {
    forecast_lambda = .Call(`_bsvars_forecast_lambda_t`, 
                            posterior_df,
                            horizon
    ) # END .Call
    forecast_sigma2 = forecast_sigma2 * forecast_lambda
  }
  
  # perform forecasting
  output       = .Call(`_bsvars_forecast_bsvars`, 
                      posterior_B,
                      posterior_A,
                      forecast_sigma2,    # (N, horizon, S)
                      X_T,
                      exogenous_forecast,
                      conditional_forecast,
                      horizon
                ) # END .Call
  
  forecast_covariance         = array(NA, c(N, N, horizon, S))
  for (s in 1:S) forecast_covariance[,,,s] = output$forecast_cov[s,][[1]]
  output$forecast_covariance  = forecast_covariance
  
  output$Y          = Y
  class(output)     = "Forecasts"
  
  return(output)
} # END forecast.PosteriorBSVARSV





#' @inherit forecast.PosteriorBSVAR
#' @method forecast PosteriorBSVART
#' @param object posterior estimation outcome - an object of class 
#' \code{PosteriorBSVART} obtained by running the \code{estimate} function.
#' @param horizon a positive integer, specifying the forecasting horizon.
#' @param exogenous_forecast a matrix of dimension \code{horizon x d} containing 
#' forecasted values of the exogenous variables. 
#' @param conditional_forecast a \code{horizon x N} matrix with forecasted values 
#' for selected variables. It should only contain \code{numeric} or \code{NA} 
#' values. The entries with \code{NA} values correspond to the values that are 
#' forecasted conditionally on the realisations provided as \code{numeric} values.
#' 
#' @examples
#' specification  = specify_bsvar_t$new(us_fiscal_lsuw)
#' burn_in        = estimate(specification, 5)
#' posterior      = estimate(burn_in, 5)
#' predictive     = forecast(posterior, 4)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar_t$new() |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
#'   forecast(horizon = 4) -> predictive
#' 
#' # conditional forecasting using a model with exogenous variables
#' ############################################################
#' specification  = specify_bsvar_t$new(us_fiscal_lsuw, exogenous = us_fiscal_ex)
#' burn_in        = estimate(specification, 5)
#' posterior      = estimate(burn_in, 5)
#' 
#' # forecast 2 years ahead
#' predictive     = forecast(
#'                     posterior, 
#'                     horizon = 8,
#'                     exogenous_forecast = us_fiscal_ex_forecasts,
#'                     conditional_forecast = us_fiscal_cond_forecasts
#'                   )
#' summary(predictive)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar_t$new(exogenous = us_fiscal_ex) |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
#'   forecast(
#'     horizon = 8,
#'     exogenous_forecast = us_fiscal_ex_forecasts,
#'     conditional_forecast = us_fiscal_cond_forecasts
#'   ) |> plot()
#'   
#' @export
forecast.PosteriorBSVART = function(
    object, 
    horizon = 1, 
    exogenous_forecast = NULL,
    conditional_forecast = NULL,
    ...
) {
  
  stopifnot("Argument horizon must be a positive integer number." = horizon > 0 & horizon %% 1 == 0)
  
  posterior_B     = object$posterior$B
  posterior_A     = object$posterior$A
  posterior_df    = object$posterior$df
  T               = ncol(object$last_draw$data_matrices$X)
  X_T             = object$last_draw$data_matrices$X[,T]
  Y               = object$last_draw$data_matrices$Y
  
  N               = nrow(posterior_B)
  K               = length(X_T)
  d               = K - N * object$last_draw$p - 1
  S               = dim(posterior_B)[3]
  
  # prepare forecasting with exogenous variables
  if (d == 0 ) {
    exogenous_forecast = matrix(NA, horizon, 1)
  } else {
    stopifnot("Forecasted values of exogenous variables are missing." = (d > 0) & !is.null(exogenous_forecast))
    stopifnot("The matrix of exogenous_forecast does not have a correct number of columns." = ncol(exogenous_forecast) == d)
    stopifnot("Provide exogenous_forecast for all forecast periods specified by argument horizon." = nrow(exogenous_forecast) == horizon)
    stopifnot("Argument exogenous has to be a matrix." = is.matrix(exogenous_forecast) & is.numeric(exogenous_forecast))
    stopifnot("Argument exogenous cannot include missing values." = sum(is.na(exogenous_forecast)) == 0 )
  }
  
  # prepare forecasting with conditional forecasts
  if ( is.null(conditional_forecast) ) {
    # this will not be used for forecasting, but needs to be provided
    conditional_forecast = matrix(NA, horizon, N)
  } else {
    stopifnot("Argument conditional_forecast must be a matrix with numeric values."
              = is.matrix(conditional_forecast) & is.numeric(conditional_forecast)
    )
    stopifnot("Argument conditional_forecast must have the number of rows equal to 
              the value of argument horizon."
              = nrow(conditional_forecast) == horizon
    )
    stopifnot("Argument conditional_forecast must have the number of columns 
              equal to the number of columns in the used data."
              = ncol(conditional_forecast) == N
    )
  }
  
  # forecast volatility
  forecast_sigma2 = .Call(`_bsvars_forecast_lambda_t`, 
                              posterior_df,
                              horizon
                        ) # END .Call
  
  # perform forecasting
  output       = .Call(`_bsvars_forecast_bsvars`, 
                      posterior_B,
                      posterior_A,
                      forecast_sigma2,    # (N, horizon, S)
                      X_T,
                      exogenous_forecast,
                      conditional_forecast,
                      horizon
                ) # END .Call
  
  forecast_covariance         = array(NA, c(N, N, horizon, S))
  for (s in 1:S) forecast_covariance[,,,s] = output$forecast_cov[s,][[1]]
  output$forecast_covariance  = forecast_covariance
  
  output$Y          = Y
  class(output)     = "Forecasts"
  
  return(output)
} # END forecast.PosteriorBSVART
