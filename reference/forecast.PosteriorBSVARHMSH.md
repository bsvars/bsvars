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
#>        mean       sd 5% quantile 95% quantile
#> 1 -5.174149 1.549280   -7.188004    -3.717264
#> 2 -3.370071 3.822738   -6.725845     1.682964
#> 3 -5.082113 1.351215   -6.588447    -3.583973
#> 4 -7.489879 2.305675   -9.830389    -5.008223
#> 5 -5.111498 3.689861   -9.308937    -1.564024
#> 6 -4.974956 3.813887   -9.916131    -1.352593
#> 7 -4.119903 4.605811   -9.088298     1.513660
#> 8 -5.290825 1.145303   -5.994198    -3.705099
#> 
#> $variable2
#>         mean        sd 5% quantile 95% quantile
#> 1  0.4279152  4.295576  -5.1561602     4.483563
#> 2  6.3594254 10.581249  -3.5629070    19.973236
#> 3  2.6714529  3.482986  -1.8684669     5.799332
#> 4 -3.2823177  5.068560  -8.8832218     1.775055
#> 5  3.5247333  9.823339  -7.4725782    11.342243
#> 6  4.6295567 11.776773 -11.0587161    15.226573
#> 7  8.1285709 13.667070  -8.8344928    22.261355
#> 8  6.1134596  6.125618  -0.5426752    11.599008
#> 
#> $variable3
#>        mean        sd 5% quantile 95% quantile
#> 1 -5.896335 0.4786882   -6.518021    -5.444637
#> 2 -5.302247 1.1853700   -6.352715    -3.732888
#> 3 -5.795442 0.4392980   -6.153908    -5.220780
#> 4 -6.534615 0.7651092   -7.348209    -5.634539
#> 5 -5.784938 1.1337373   -7.114639    -4.608921
#> 6 -5.721630 1.0643848   -7.116425    -4.708676
#> 7 -5.473492 1.2845867   -6.936708    -3.977081
#> 8 -5.841924 0.2520826   -6.152444    -5.588379
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
