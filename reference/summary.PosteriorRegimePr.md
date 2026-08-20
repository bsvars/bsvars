# Provides posterior summary of regime probabilities

Provides posterior summary of regime probabilities including their mean,
standard deviations, as well as 5 and 95 percentiles.

## Usage

``` r
# S3 method for class 'PosteriorRegimePr'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorRegimePr obtained using the
  [`compute_regime_probabilities()`](https://bsvars.org/bsvars/reference/compute_regime_probabilities.md)
  function containing posterior draws of regime allocations.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean and standard deviations of the
regime probabilities.

## See also

[`compute_regime_probabilities`](https://bsvars.org/bsvars/reference/compute_regime_probabilities.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
specification  = specify_bsvar_msh$new(us_fiscal_lsuw)
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

# compute regime probabilities
rp             = compute_regime_probabilities(posterior)
rp_summary     = summary(rp)
head(rp_summary$MarkovProcess1$regime1) # browse the results
#>   mean        sd
#> 1  0.0 0.0000000
#> 2  0.0 0.0000000
#> 3  0.2 0.4472136
#> 4  0.0 0.0000000
#> 5  0.0 0.0000000
#> 6  0.0 0.0000000

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_msh$new() |>
  estimate(S = 5) |> 
  estimate(S = 5) |> 
  compute_regime_probabilities() |>
  summary() -> rp_summary
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
head(rp_summary$MarkovProcess1$regime1) # browse the results
#>   mean        sd
#> 1  0.0 0.0000000
#> 2  0.0 0.0000000
#> 3  0.4 0.5477226
#> 4  0.0 0.0000000
#> 5  0.0 0.0000000
#> 6  0.0 0.0000000
```
