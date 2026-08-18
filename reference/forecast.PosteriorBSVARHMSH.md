# Forecasting using Bayesian Structural Vector Autoregression

Samples from the joint predictive density of all of the dependent
variables for models at forecast horizons from 1 to `horizon` specified
as an argument of the function.

## Usage

``` r
# S3 method for class 'PosteriorBSVARHMSH'
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

  posterior estimation outcome - an object of class `PosteriorBSVARHMSH`
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
specification  = specify_bsvar_hmsh$new(us_fiscal_lsuw, M = 2)
#> The identification is set to the default option of lower-triangular structural matrix.
burn_in        = estimate(specification, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryHMSH model
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
posterior      = estimate(burn_in, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryHMSH model
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
predictive     = forecast(posterior, 4)

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_hmsh$new(M = 2) |>
  estimate(S = 5) |> 
  estimate(S = 5) |> 
  forecast(horizon = 4) -> predictive
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryHMSH model
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryHMSH model
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
  
# forecasting using a model with exogenous variables
############################################################
specification  = specify_bsvar_hmsh$new(us_fiscal_lsuw, M = 2, exogenous = us_fiscal_ex)
#> The identification is set to the default option of lower-triangular structural matrix.
burn_in        = estimate(specification, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryHMSH model
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
posterior      = estimate(burn_in, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryHMSH model
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# forecast 2 years ahead
predictive     = forecast(
                    posterior, 
                    horizon = 8,
                    exogenous_forecast = us_fiscal_ex_forecasts
                  )
summary(predictive)
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Posterior summary of forecasts                  |
#>  **************************************************|
#> $variable1
#>          mean        sd 5% quantile 95% quantile
#> 1  4.02029493 11.819406 -11.9522610    13.291611
#> 2 -0.02511119  6.893017  -9.4033238     4.671419
#> 3  1.43902967 10.202666  -9.7486538    13.667916
#> 4  3.20271550 10.829758  -8.7476411    15.488197
#> 5 13.40722960 13.085006  -3.6048308    24.840854
#> 6 15.00824756 13.507690  -0.9019486    30.231069
#> 7 18.37840011 14.682434   4.0721581    34.510864
#> 8 23.87886003 12.651681  12.6167716    38.455939
#> 
#> $variable2
#>         mean        sd 5% quantile 95% quantile
#> 1  3.0439321 11.789037  -12.887897    12.299021
#> 2 -0.5155204  7.363368  -10.460872     4.678284
#> 3  1.1932901 10.266047  -10.628126    13.114938
#> 4  3.1834504 10.650347   -7.858533    15.283851
#> 5 13.6281674 13.529247   -4.389773    24.784187
#> 6 15.8394493 14.003594   -1.387652    30.942218
#> 7 19.9054482 14.406930    6.086739    35.510718
#> 8 26.1411319 12.347083   13.571415    40.065589
#> 
#> $variable3
#>        mean        sd 5% quantile 95% quantile
#> 1 -6.390979 0.5860747   -7.181394    -5.927254
#> 2 -6.584162 0.3500443   -7.050245    -6.290792
#> 3 -6.500480 0.4077210   -7.007636    -6.074518
#> 4 -6.430466 0.3863483   -6.879389    -6.092114
#> 5 -5.946212 0.5989017   -6.692826    -5.310131
#> 6 -5.873223 0.5854067   -6.601898    -5.345863
#> 7 -5.710194 0.7622578   -6.421908    -4.708996
#> 8 -5.445104 0.7194860   -5.948493    -4.459186
#> 

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_hmsh$new(M = 2, exogenous = us_fiscal_ex) |>
  estimate(S = 5) |> 
  estimate(S = 5) |> 
  forecast(
    horizon = 8,
    exogenous_forecast = us_fiscal_ex_forecasts
  ) |> plot()
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryHMSH model
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryHMSH model
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

  
```
