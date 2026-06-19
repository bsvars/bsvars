# Provides posterior summary of variables' fitted values

Provides posterior summary of the fitted values including their mean,
standard deviations, as well as 5 and 95 percentiles.

## Usage

``` r
# S3 method for class 'PosteriorFitted'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorFitted obtained using the
  [`compute_fitted_values()`](https://bsvars.org/bsvars/reference/compute_fitted_values.md)
  function containing draws the predictive density of the sample data.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the fitted values for each of the shocks and
periods.

## See also

[`compute_fitted_values`](https://bsvars.org/bsvars/reference/compute_fitted_values.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
set.seed(123)
specification  = specify_bsvar$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.

# run the burn-in
burn_in        = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#>  s: 0
#>  s: 1
#>  s: 2
#>  s: 3
#>  s: 4
#>  s: 5
#>  s: 6
#>  s: 7
#>  s: 8
#>  s: 9

# estimate the model
posterior      = estimate(burn_in, 20)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 20 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#>  s: 0
#>  s: 1
#>  s: 2
#>  s: 3
#>  s: 4
#>  s: 5
#>  s: 6
#>  s: 7
#>  s: 8
#>  s: 9
#>  s: 10
#>  s: 11
#>  s: 12
#>  s: 13
#>  s: 14
#>  s: 15
#>  s: 16
#>  s: 17
#>  s: 18
#>  s: 19

# compute fitted values
fitted         = compute_fitted_values(posterior)
fitted_summary = summary(fitted)
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Posterior summary of fitted values              |
#>  **************************************************|

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar$new() |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  compute_fitted_values() |>
  summary() -> fitted_summary
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#>  s: 0
#>  s: 1
#>  s: 2
#>  s: 3
#>  s: 4
#>  s: 5
#>  s: 6
#>  s: 7
#>  s: 8
#>  s: 9
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 20 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#>  s: 0
#>  s: 1
#>  s: 2
#>  s: 3
#>  s: 4
#>  s: 5
#>  s: 6
#>  s: 7
#>  s: 8
#>  s: 9
#>  s: 10
#>  s: 11
#>  s: 12
#>  s: 13
#>  s: 14
#>  s: 15
#>  s: 16
#>  s: 17
#>  s: 18
#>  s: 19
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Posterior summary of fitted values              |
#>  **************************************************|
```
