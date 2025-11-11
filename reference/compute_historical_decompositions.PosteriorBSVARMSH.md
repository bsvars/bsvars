# Computes posterior draws of historical decompositions

Each of the draws from the posterior estimation of models from packages
bsvars or bsvarSIGNs is transformed into a draw from the posterior
distribution of the historical decompositions. IMPORTANT! The historical
decompositions are interpreted correctly for covariance stationary data.
Application to unit-root non-stationary data might result in
non-interpretable outcomes.

## Usage

``` r
# S3 method for class 'PosteriorBSVARMSH'
compute_historical_decompositions(posterior, show_progress = TRUE)
```

## Arguments

- posterior:

  posterior estimation outcome - an object of class `PosteriorBSVARMSH`
  obtained by running the `estimate` function.

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

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# specify the model
specification  = specify_bsvar_msh$new(us_fiscal_lsuw, p = 1, M = 2)
#> The identification is set to the default option of lower-triangular structural matrix.

# run the burn-in
burn_in        = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# estimate the model
posterior      = estimate(burn_in, 20)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 20 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# compute historical decompositions
hd             = compute_historical_decompositions(posterior)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Computing historical decomposition               |
#> **************************************************|
#>  This might take a little while :)                
#> **************************************************|

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_msh$new(p = 1, M = 2) |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  compute_historical_decompositions() -> hds
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 20 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Computing historical decomposition               |
#> **************************************************|
#>  This might take a little while :)                
#> **************************************************|
  
```
