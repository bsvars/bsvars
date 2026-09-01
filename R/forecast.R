
#' @export
generics::forecast



#' @title R6 Class Representing Forecasts
#'
#' @description
#' R6 class representing draws from the predictive density of a Bayesian
#' Structural Vector Autoregression model.
#'
#' @details
#' The class contains the following objects:
#'
#' \describe{
#'   \item{\code{forecasts}}{An \code{N x horizon x S} array containing draws 
#'   from the predictive density.}
#'   \item{\code{forecast_mean}}{An \code{N x horizon x S} array containing the 
#'   conditional means of the predictive density.}
#'   \item{\code{forecast_covariance}}{An \code{N x N x horizon x S} array 
#'   containing the conditional covariance matrices of the predictive density.}
#'   \item{\code{Y}}{An \code{N x T} matrix containing the data on the dependent 
#'   variables used for estimation.}
#' }
#'
#' The method \code{as_list()} returns the contents of the \code{Forecasts}
#' object as a list.
#'
#' @param output A list containing the forecasting output, including
#' \code{forecasts}, \code{forecast_mean}, and \code{forecast_cov}.
#' @param Y An \code{N x T} matrix containing the data on the dependent variables.
#'
#' @return An object of class \code{Forecasts}.
#'
#' @examples
#' spec = specify_bsvar$new(us_fiscal_lsuw)
#' burn = estimate(spec, 5)
#' post = estimate(burn, 5)
#' fore = forecast(post, 4)
#' apply(fore$forecasts, 1:2, mean) # compute mean forecasts 
#'
#' @export
specify_forecasts = R6::R6Class(
  classname = "Forecasts",
  
  public = list(
    
    #' @field forecasts
    #' An \code{N x horizon x S} numeric array containing draws from the
    #' predictive density.
    forecasts = array(),
    
    #' @field forecast_mean
    #' An \code{N x horizon x S} numeric array containing the conditional
    #' means of the predictive density.
    forecast_mean = array(),
    
    #' @field forecast_covariance
    #' An \code{N x N x horizon x S} numeric array containing the conditional
    #' covariance matrices of the predictive density.
    forecast_covariance = array(),
    
    #' @field Y
    #' An \code{N x T} numeric matrix containing the data on the dependent
    #' variables used for estimation.
    Y = matrix(),
    
    #' @description
    #' Creates a new \code{Forecasts} object from the output of the forecasting
    #' procedure.
    #'
    #' @param output A list containing the forecasting output, including
    #' \code{forecasts}, \code{forecast_mean}, and \code{forecast_cov}.
    #' @param Y An \code{N x T} matrix containing the data on the dependent variables.
    #'
    #' @return An object of class \code{Forecasts}.
    initialize = function(output, Y) {
      
      N       = dim(output$forecasts)[1]
      horizon = dim(output$forecasts)[2]
      S       = dim(output$forecasts)[3]
      
      forecast_covariance = array(
        NA,
        c(N, N, horizon, S)
      )
      
      for (s in seq_len(S)) {
        forecast_covariance[, , , s] = output$forecast_cov[s, ][[1]]
      }
      
      self$forecasts           = output$forecasts
      self$forecast_mean       = output$forecast_mean
      self$forecast_covariance = forecast_covariance
      self$Y                   = Y
      
      invisible(self)
    },
    
    #' @description
    #' Converts the \code{Forecasts} object to a list.
    #'
    #' @return A list containing \code{forecasts}, \code{forecast_mean},
    #' \code{forecast_covariance}, and \code{Y}.
    get_forecasts = function() {
      
      list(
        forecasts           = self$forecasts,
        forecast_mean       = self$forecast_mean,
        forecast_covariance = self$forecast_covariance,
        Y                   = self$Y
      )
    }
  )
)



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
  X               = object$last_draw$data_matrices$X
  Y               = object$last_draw$data_matrices$Y
  T               = ncol(X)
  N               = nrow(posterior_B)
  lag_entries     = N * object$last_draw$p
  X_T             = c(
    Y[,T],
    X[seq_len(lag_entries - N),T],
    tail(X[,T], nrow(X) - lag_entries)
  )
  posterior_df    = object$posterior$df
  normal          = object$last_draw$get_normal()
  
  K               = length(X_T)
  d               = K - N * object$last_draw$p - 1
  S               = dim(posterior_B)[3]
  
  # prepare forecasting with exogenous variables
  if (d == 0 ) {
    exogenous_forecast = matrix(NA, horizon, 1)
  } else {
    stopifnot("Forecasted values of exogenous variables are missing." = (d > 0) & !is.null(exogenous_forecast))
    stopifnot("The matrix of exogenous_forecast does not have a correct number of columns." = ncol(exogenous_forecast) == d)
    stopifnot("Argument exogenous has to be a matrix." = is.matrix(exogenous_forecast) & is.numeric(exogenous_forecast))
    stopifnot("Argument exogenous cannot include missing values." = sum(is.na(exogenous_forecast)) == 0 )
    
    if ( is.null(conditional_forecast) ) {
      horizon = nrow(exogenous_forecast)
      message("The value of argument horizon is set to the number of rows in exogenous_forecast.")
    }
  }
  
  # prepare forecasting with conditional forecasts
  if ( is.null(conditional_forecast) ) {
    # this will not be used for forecasting, but needs to be provided
    conditional_forecast = matrix(NA, horizon, N)
  } else {
    stopifnot("Argument conditional_forecast must be a matrix with numeric values."
              = is.matrix(conditional_forecast) & is.numeric(conditional_forecast)
    )
    stopifnot("Argument conditional_forecast must have the number of columns 
              equal to the number of columns in the used data."
              = ncol(conditional_forecast) == N
    )
    
    if (d == 0) {
      horizon = nrow(conditional_forecast)
      message("The value of argument horizon is set to the number of rows in conditional_forecast.")
    }
  }
  
  # prepare forecasting with conditional forecasts
  if ( !is.null(conditional_forecast) && d != 0) {
    stopifnot("Argument conditional_forecast must have the same number of rows 
              as argument exogenous_forecast."
              = nrow(conditional_forecast) == nrow(exogenous_forecast)
    )  
    horizon = nrow(conditional_forecast)
    message("The value of argument horizon has been aligned with the dimensions 
            of arguments conditional_forecast and exogenous_forecast.")
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
  
  output = specify_forecasts$new(output, Y)
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
  X                 = object$last_draw$data_matrices$X
  Y                 = object$last_draw$data_matrices$Y
  T                 = ncol(X)
  N                 = nrow(posterior_B)
  lag_entries       = N * object$last_draw$p
  X_T               = c(
    Y[,T],
    X[seq_len(lag_entries - N),T],
    tail(X[,T], nrow(X) - lag_entries)
  )
  sigma2_T        = object$posterior$sigma[,T,]^2
  posterior_df    = object$posterior$df
  normal          = object$last_draw$get_normal()
  
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
  
  output = specify_forecasts$new(output, Y)
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
  X                 = object$last_draw$data_matrices$X
  Y                 = object$last_draw$data_matrices$Y
  T                 = ncol(X)
  N                 = nrow(posterior_B)
  lag_entries       = N * object$last_draw$p
  X_T               = c(
    Y[,T],
    X[seq_len(lag_entries - N),T],
    tail(X[,T], nrow(X) - lag_entries)
  )
  posterior_df    = object$posterior$df
  normal          = object$last_draw$get_normal()
  
  M               = ncol(posterior_sigma2)
  K               = length(X_T)
  d               = K - N * object$last_draw$p - 1
  S               = dim(posterior_B)[3]
  
  S_T             = array(NA, c(1,N,S))
  for (s in 1:S) {
    S_T[,,s]      = object$posterior$xi_cpp[s,1][[1]][1,T,]
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
  
  output = specify_forecasts$new(output, Y)
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
  X                 = object$last_draw$data_matrices$X
  Y                 = object$last_draw$data_matrices$Y
  T                 = ncol(X)
  N                 = nrow(posterior_B)
  lag_entries       = N * object$last_draw$p
  X_T               = c(
    Y[,T],
    X[seq_len(lag_entries - N),T],
    tail(X[,T], nrow(X) - lag_entries)
  )
  posterior_df    = object$posterior$df
  normal          = object$last_draw$get_normal()
  
  K               = length(X_T)
  d               = K - N * object$last_draw$p - 1
  S               = dim(posterior_B)[3]
  S_T             = matrix(object$posterior$xi[1,T,], 1, S)
  
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
  
  output = specify_forecasts$new(output, Y)
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
  X                 = object$last_draw$data_matrices$X
  Y                 = object$last_draw$data_matrices$Y
  T                 = ncol(X)
  N                 = nrow(posterior_B)
  lag_entries       = N * object$last_draw$p
  X_T               = c(
    Y[,T],
    X[seq_len(lag_entries - N),T],
    tail(X[,T], nrow(X) - lag_entries)
  )
  posterior_df    = object$posterior$df
  normal          = object$last_draw$get_normal()
  
  K               = length(X_T)
  d               = K - N * object$last_draw$p - 1
  S               = dim(posterior_B)[3]
  S_T             = matrix(object$posterior$xi[1,T,], 1, S)
  
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
  
  output = specify_forecasts$new(output, Y)
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
  
  X                 = object$last_draw$data_matrices$X
  Y                 = object$last_draw$data_matrices$Y
  T                 = ncol(X)
  N                 = nrow(posterior_B)
  lag_entries       = N * object$last_draw$p
  X_T               = c(
    Y[,T],
    X[seq_len(lag_entries - N),T],
    tail(X[,T], nrow(X) - lag_entries)
  )
  posterior_h_T     = object$posterior$h[,T,]
  centred_sv        = object$last_draw$centred_sv
  posterior_df    = object$posterior$df
  normal          = object$last_draw$get_normal()
  
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
  
  output = specify_forecasts$new(output, Y)
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
  X               = object$last_draw$data_matrices$X
  Y               = object$last_draw$data_matrices$Y
  T               = ncol(X)
  N               = nrow(posterior_B)
  lag_entries     = N * object$last_draw$p
  X_T             = c(
    Y[,T],
    X[seq_len(lag_entries - N),T],
    tail(X[,T], nrow(X) - lag_entries)
  )

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
  
  output = specify_forecasts$new(output, Y)
  return(output)
} # END forecast.PosteriorBSVART
