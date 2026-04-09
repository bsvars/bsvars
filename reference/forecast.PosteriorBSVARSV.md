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
#> 1 -8.914237  0   -8.914237    -8.914237
#> 2 -8.908921  0   -8.908921    -8.908921
#> 3 -8.903604  0   -8.903604    -8.903604
#> 4 -8.898288  0   -8.898288    -8.898288
#> 5 -8.892971  0   -8.892971    -8.892971
#> 6 -8.887655  0   -8.887655    -8.887655
#> 7 -8.882338  0   -8.882338    -8.882338
#> 8 -8.877022  0   -8.877022    -8.877022
#> 
#> $variable2
#>        mean         sd 5% quantile 95% quantile
#> 1 -9.809777 0.04969446   -9.868595    -9.756102
#> 2 -9.803115 0.05632582   -9.861726    -9.732043
#> 3 -9.773732 0.04712315   -9.828022    -9.723062
#> 4 -9.795742 0.05989611   -9.877750    -9.755447
#> 5 -9.763225 0.04104293   -9.817978    -9.726896
#> 6 -9.746374 0.05278569   -9.798828    -9.683664
#> 7 -9.705944 0.06397597   -9.760669    -9.620536
#> 8 -9.689232 0.11224475   -9.795809    -9.545358
#> 
#> $variable3
#>        mean         sd 5% quantile 95% quantile
#> 1 -7.052566 0.01309860   -7.068145    -7.038070
#> 2 -7.053622 0.02408131   -7.080873    -7.027591
#> 3 -7.051874 0.01862310   -7.074294    -7.036460
#> 4 -7.039972 0.03001512   -7.077817    -7.017122
#> 5 -7.041016 0.03047342   -7.080136    -7.012962
#> 6 -7.037664 0.03444494   -7.084085    -7.010544
#> 7 -7.032062 0.04039314   -7.086005    -6.997441
#> 8 -7.040403 0.05098027   -7.109204    -7.001249
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
