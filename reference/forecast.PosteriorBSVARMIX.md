# Forecasting using Bayesian Structural Vector Autoregression

Samples from the joint predictive density of all of the dependent
variables for models at forecast horizons from 1 to `horizon` specified
as an argument of the function.

## Usage

``` r
# S3 method for class 'PosteriorBSVARMIX'
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

  posterior estimation outcome - an object of class `PosteriorBSVARMIX`
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
specification  = specify_bsvar_mix$new(us_fiscal_lsuw, M = 2)
#> The identification is set to the default option of lower-triangular structural matrix.
burn_in        = estimate(specification, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
posterior      = estimate(burn_in, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
predictive     = forecast(posterior, 4)

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_mix$new(M = 2) |>
  estimate(S = 5) |>
  estimate(S = 5) |>  
  forecast(horizon = 4) -> predictive
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
  
# conditional forecasting using a model with exogenous variables
############################################################
specification  = specify_bsvar_mix$new(us_fiscal_lsuw, M = 2, exogenous = us_fiscal_ex)
#> The identification is set to the default option of lower-triangular structural matrix.
burn_in        = estimate(specification, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
posterior      = estimate(burn_in, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
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
#> 1 -9.797446 0.02340770   -9.827578    -9.774744
#> 2 -9.781846 0.02073773   -9.804087    -9.756406
#> 3 -9.758421 0.03808435   -9.803073    -9.715912
#> 4 -9.751454 0.06393057   -9.812837    -9.670271
#> 5 -9.740608 0.10154362   -9.844548    -9.613493
#> 6 -9.718969 0.10735098   -9.818606    -9.582429
#> 7 -9.707652 0.10561311   -9.814870    -9.575304
#> 8 -9.692946 0.10673223   -9.802661    -9.566181
#> 
#> $variable3
#>        mean         sd 5% quantile 95% quantile
#> 1 -7.045820 0.01458515   -7.062730    -7.029511
#> 2 -7.043749 0.01953863   -7.066014    -7.020527
#> 3 -7.043029 0.01698096   -7.061084    -7.024373
#> 4 -7.039933 0.02544988   -7.073293    -7.018183
#> 5 -7.042942 0.02920145   -7.071423    -7.007629
#> 6 -7.036264 0.03583706   -7.071459    -6.991578
#> 7 -7.027494 0.03247945   -7.066743    -6.991926
#> 8 -7.031976 0.02316292   -7.057327    -7.003911
#> 

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_mix$new(M = 2, exogenous = us_fiscal_ex) |>
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
#>  Gibbs sampler for the SVAR-finiteMIX model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

 
```
