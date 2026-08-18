# Forecasting using Bayesian Structural Vector Autoregression

Samples from the joint predictive density of all of the dependent
variables for models at forecast horizons from 1 to `horizon` specified
as an argument of the function.

## Usage

``` r
# S3 method for class 'PosteriorBSVARMSH'
forecast(
  object,
  horizon = 1,
  exogenous_forecast = NULL,
  conditional_forecast = NULL,
  ...
)
```

## Arguments

- object:

  posterior estimation outcome - an object of class `PosteriorBSVARMSH`
  obtained by running the `estimate` function.

- horizon:

  a positive integer, specifying the forecasting horizon.

- exogenous_forecast:

  a matrix of dimension `horizon x d` containing forecasted values of
  the exogenous variables.

- conditional_forecast:

  a `horizon x N` matrix with forecasted values for selected variables.
  It should only contain `numeric` or `NA` values. The entries with `NA`
  values correspond to the values that are forecasted conditionally on
  the realisations provided as `numeric` values.

- ...:

  not used

## Value

A list of class `Forecasts` containing the draws from the predictive
density and for heteroskedastic models the draws from the predictive
density of structural shocks conditional standard deviations and data.
The output elements include:

- forecasts:

  an `NxTxS` array with the draws from predictive density

- Y:

  an \\NxT\\ matrix with the data on dependent variables

- forecast_mean:

  an `NxTxS` array with the mean of the predictive density

- forecast_covariance:

  an `NxTxS` array with the covariance of the predictive density

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
specification  = specify_bsvar_msh$new(us_fiscal_lsuw, M = 2)
#> The identification is set to the default option of lower-triangular structural matrix.
burn_in        = estimate(specification, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
posterior      = estimate(burn_in, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
predictive     = forecast(posterior, 4)

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_msh$new(M = 2) |>
  estimate(S = 5) |> 
  estimate(S = 5) |> 
  forecast(horizon = 4) -> predictive
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
  
# conditional forecasting using a model with exogenous variables
############################################################
specification  = specify_bsvar_msh$new(us_fiscal_lsuw, M = 2, exogenous = us_fiscal_ex)
#> The identification is set to the default option of lower-triangular structural matrix.
burn_in        = estimate(specification, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
posterior      = estimate(burn_in, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# forecast 2 years ahead
predictive     = forecast(
                    posterior, 
                    horizon = 8,
                    exogenous_forecast = us_fiscal_ex_forecasts,
                    conditional_forecast = us_fiscal_cond_forecasts
                  )
summary(predictive)
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Posterior summary of forecasts                  |
#>  **************************************************|
#> $variable1
#>        mean sd 5% quantile 95% quantile
#> 1 -8.860009  0   -8.860009    -8.860009
#> 2 -8.854638  0   -8.854638    -8.854638
#> 3 -8.849268  0   -8.849268    -8.849268
#> 4 -8.843897  0   -8.843897    -8.843897
#> 5 -8.838526  0   -8.838526    -8.838526
#> 6 -8.833155  0   -8.833155    -8.833155
#> 7 -8.827784  0   -8.827784    -8.827784
#> 8 -8.822413  0   -8.822413    -8.822413
#> 
#> $variable2
#>        mean         sd 5% quantile 95% quantile
#> 1 -9.825536 0.01153656   -9.837172    -9.812386
#> 2 -9.814667 0.02178741   -9.839802    -9.792942
#> 3 -9.822189 0.01711226   -9.840748    -9.801235
#> 4 -9.827701 0.02450261   -9.860673    -9.808416
#> 5 -9.827782 0.02475397   -9.854605    -9.801566
#> 6 -9.821665 0.01785561   -9.836564    -9.797973
#> 7 -9.819354 0.03665408   -9.859609    -9.779005
#> 8 -9.803624 0.06876404   -9.882239    -9.752342
#> 
#> $variable3
#>        mean          sd 5% quantile 95% quantile
#> 1 -7.030206 0.004843474   -7.035728    -7.025026
#> 2 -7.021064 0.016847977   -7.033799    -6.998168
#> 3 -7.023092 0.015663531   -7.036274    -7.002408
#> 4 -7.020420 0.015788276   -7.034857    -7.000161
#> 5 -7.014342 0.014838960   -7.030251    -6.995958
#> 6 -7.026176 0.025902074   -7.053337    -6.993829
#> 7 -7.030610 0.025683121   -7.054880    -6.998942
#> 8 -7.025047 0.029456943   -7.056424    -6.991074
#> 

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_msh$new(M = 2, exogenous = us_fiscal_ex) |>
  estimate(S = 5) |> 
  estimate(S = 5) |> 
  forecast(
    horizon = 8,
    exogenous_forecast = us_fiscal_ex_forecasts,
    conditional_forecast = us_fiscal_cond_forecasts
  ) |> plot()
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

  
```
