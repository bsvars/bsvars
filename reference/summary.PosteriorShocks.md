# Provides posterior summary of structural shocks

Provides posterior summary of the structural shocks including their
mean, standard deviations, as well as 5 and 95 percentiles.

## Usage

``` r
# S3 method for class 'PosteriorShocks'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorShocks obtained using the
  [`compute_structural_shocks()`](https://bsvars.org/bsvars/reference/compute_structural_shocks.md)
  function containing draws the posterior distribution of the structural
  shocks.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the structural shocks for each of the equations
and periods.

## See also

[`compute_structural_shocks`](https://bsvars.org/bsvars/reference/compute_structural_shocks.md)

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

# compute structural shocks
shocks         = compute_structural_shocks(posterior)
shocks_summary = summary(shocks)
head(shocks_summary$shock1)
#>         mean        sd 5% quantile 95% quantile
#> 1 -0.4217146 0.1150629  -0.5751845   -0.3245687
#> 2 -0.4978940 0.1131395  -0.6489869   -0.3974478
#> 3 -0.4174761 0.1146373  -0.5710140   -0.3165630
#> 4 -0.3786269 0.1125896  -0.5298784   -0.2810264
#> 5 -0.2561990 0.1109281  -0.4062320   -0.1645447
#> 6 -0.2698613 0.1084453  -0.4166679   -0.1804146

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar$new() |>
  estimate(S = 5) |> 
  estimate(S = 5) |> 
  compute_structural_shocks() |>
  summary() -> shocks_summary
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
head(shocks_summary$shock1)
#>         mean        sd 5% quantile 95% quantile
#> 1 -2.3394479 0.9047605   -3.265555   -1.3608028
#> 2 -1.3358308 0.9112341   -2.316532   -0.3906967
#> 3 -0.3336049 0.7120380   -1.145945    0.3616316
#> 4 -0.4283641 0.7971758   -1.325051    0.3577650
#> 5 -0.6147006 0.9141129   -1.623623    0.2963682
#> 6 -0.9106707 1.1216110   -2.123154    0.2233347
```
