# Forecasting using Bayesian Structural Vector Autoregression

Samples from the joint predictive density of all of the dependent
variables for models at forecast horizons from 1 to `horizon` specified
as an argument of the function.

## Usage

``` r
# S3 method for class 'PosteriorBSVARSV'
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

  posterior estimation outcome - an object of class `PosteriorBSVARSV`
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
specification  = specify_bsvar_sv$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.
burn_in        = estimate(specification, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
posterior      = estimate(burn_in, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
predictive     = forecast(posterior, 2)

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_sv$new() |>
  estimate(S = 5) |>
  estimate(S = 5) |>  
  forecast(horizon = 2) -> predictive
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
  
# conditional forecasting using a model with exogenous variables
############################################################
specification  = specify_bsvar_sv$new(us_fiscal_lsuw, exogenous = us_fiscal_ex)
#> The identification is set to the default option of lower-triangular structural matrix.
burn_in        = estimate(specification, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
posterior      = estimate(burn_in, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
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
#> 1 -9.798559 0.01544841   -9.815259    -9.781591
#> 2 -9.783625 0.02040422   -9.806705    -9.762591
#> 3 -9.787544 0.03107488   -9.825056    -9.752760
#> 4 -9.774295 0.02679498   -9.801654    -9.742354
#> 5 -9.769706 0.01359904   -9.780783    -9.751246
#> 6 -9.754655 0.03697444   -9.802229    -9.725575
#> 7 -9.750166 0.04026345   -9.799543    -9.709357
#> 8 -9.737412 0.04275414   -9.772152    -9.681128
#> 
#> $variable3
#>        mean         sd 5% quantile 95% quantile
#> 1 -7.039674 0.01038748   -7.050607    -7.027879
#> 2 -7.045944 0.01529031   -7.060589    -7.026142
#> 3 -7.045067 0.01983702   -7.064610    -7.020049
#> 4 -7.034862 0.02184815   -7.060312    -7.014931
#> 5 -7.039351 0.03011361   -7.073671    -7.009980
#> 6 -7.025386 0.02252648   -7.054045    -7.007778
#> 7 -7.004243 0.01272646   -7.020197    -6.992909
#> 8 -6.976387 0.02103069   -7.000193    -6.951295
#> 

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_sv$new(exogenous = us_fiscal_ex) |>
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
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

```
