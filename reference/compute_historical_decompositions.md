# Computes posterior draws of historical decompositions

Each of the draws from the posterior estimation of models from packages
bsvars or bsvarSIGNs is transformed into a draw from the posterior
distribution of the historical decompositions. IMPORTANT! The historical
decompositions are interpreted correctly for covariance stationary data.
Application to unit-root non-stationary data might result in
non-interpretable outcomes.

## Usage

``` r
compute_historical_decompositions(posterior, show_progress = TRUE)
```

## Arguments

- posterior:

  posterior estimation outcome obtained by running the `estimate`
  function. The interpretation depends on the normalisation of the
  shocks using function
  [`normalise()`](https://bsvars.org/bsvars/reference/normalise.md).
  Verify if the default settings are appropriate.

- show_progress:

  a logical value, if `TRUE` the estimation progress bar is visible

## Value

An object of class `PosteriorHD`, that is, an `NxNxTxS` array with
attribute `PosteriorHD` containing `S` draws of the historical
decompositions.

## References

Kilian, L., & Lütkepohl, H. (2017). Structural VAR Tools, Chapter 4, In:
Structural vector autoregressive analysis. Cambridge University Press.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`normalise`](https://bsvars.org/bsvars/reference/normalise.md),
[`summary`](https://rdrr.io/r/base/summary.html)

## Author

Tomasz Woźniak <wozniak.tom@pm.me> and Xiaolei Wang
<adamwang15@gmail.com>

## Examples

``` r
# specify the model
specification  = specify_bsvar$new(diff(us_fiscal_lsuw), p = 1)
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

# compute historical decompositions
hd            = compute_historical_decompositions(posterior)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Computing historical decomposition               |
#> **************************************************|
#>  This might take a little while :)                
#> **************************************************|

# workflow with the pipe |>
############################################################
diff(us_fiscal_lsuw) |>
  specify_bsvar$new(p = 1) |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  compute_historical_decompositions() -> hd
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
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Computing historical decomposition               |
#> **************************************************|
#>  This might take a little while :)                
#> **************************************************|
```
