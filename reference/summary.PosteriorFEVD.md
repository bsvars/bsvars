# Provides posterior summary of forecast error variance decompositions

Provides posterior means of the forecast error variance decompositions
of each variable at all horizons.

## Usage

``` r
# S3 method for class 'PosteriorFEVD'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorFEVD obtained using the
  [`compute_variance_decompositions()`](https://bsvars.org/bsvars/reference/compute_variance_decompositions.md)
  function containing draws from the posterior distribution of the
  forecast error variance decompositions.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean of the forecast error variance
decompositions of each variable at all horizons.

## See also

[`compute_variance_decompositions`](https://bsvars.org/bsvars/reference/compute_variance_decompositions.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
specification  = specify_bsvar$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.
burn_in        = estimate(specification, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
posterior      = estimate(burn_in, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# compute forecast error variance decompositions
fevd           = compute_variance_decompositions(posterior, horizon = 4)
fevd_summary   = summary(fevd)
fevd_summary
#> $ttr
#>      shock1    shock2    shock3
#> 0 100.00000 0.0000000 0.0000000
#> 1  99.43724 0.2560217 0.3067412
#> 2  98.33701 0.7605530 0.9024371
#> 3  96.93680 1.4091218 1.6540773
#> 4  95.41996 2.1197409 2.4603035
#> 
#> $gs
#>     shock1   shock2    shock3
#> 0 55.61809 44.38191 0.0000000
#> 1 63.59489 35.89773 0.5073713
#> 2 70.77477 27.65662 1.5686096
#> 3 76.71242 20.32366 2.9639206
#> 4 81.20708 14.33730 4.4556228
#> 
#> $gdp
#>     shock1    shock2   shock3
#> 0 15.05642 2.4215960 82.52199
#> 1 19.19316 1.8853635 78.92147
#> 2 24.39733 1.4186309 74.18404
#> 3 30.74730 1.0655037 68.18719
#> 4 38.16513 0.8747406 60.96013
#> 

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar$new() |>
  estimate(S = 5) |> 
  estimate(S = 5) |> 
  compute_variance_decompositions(horizon = 4) |>
  summary() -> fevd_summary
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
fevd_summary
#> $ttr
#>      shock1      shock2   shock3
#> 0 100.00000 0.000000000 0.000000
#> 1  99.36034 0.007992811 0.631672
#> 2  97.88854 0.024890000 2.086571
#> 3  95.65433 0.048476147 4.297197
#> 4  92.77946 0.076333948 7.144211
#> 
#> $gs
#>      shock1   shock2    shock3
#> 0 0.1490424 99.85096 0.0000000
#> 1 0.1704331 99.70179 0.1277802
#> 2 0.2351337 99.36065 0.4042162
#> 3 0.3348644 98.85803 0.8071078
#> 4 0.4621152 98.22243 1.3154511
#> 
#> $gdp
#>     shock1     shock2   shock3
#> 0 2.308170 0.05156762 97.64026
#> 1 2.155167 0.05196710 97.79287
#> 2 2.020634 0.05288986 97.92648
#> 3 1.902007 0.05431130 98.04368
#> 4 1.797122 0.05620656 98.14667
#> 
```
