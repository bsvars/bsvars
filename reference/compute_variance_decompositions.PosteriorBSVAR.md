# Computes posterior draws of the forecast error variance decomposition

Each of the draws from the posterior estimation of the model is
transformed into a draw from the posterior distribution of the forecast
error variance decomposition.

## Usage

``` r
# S3 method for class 'PosteriorBSVAR'
compute_variance_decompositions(posterior, horizon)
```

## Arguments

- posterior:

  posterior estimation outcome - an object of class `PosteriorBSVAR`
  obtained by running the `estimate` function.

- horizon:

  a positive integer number denoting the forecast horizon for the
  forecast error variance decomposition computations.

## Value

An object of class PosteriorFEVD, that is, an `NxNx(horizon+1)xS` array
with attribute PosteriorFEVD containing `S` draws of the forecast error
variance decomposition.

## References

Kilian, L., & Lütkepohl, H. (2017). Structural VAR Tools, Chapter 4, In:
Structural vector autoregressive analysis. Cambridge University Press.

## See also

[`compute_impulse_responses`](https://bsvars.org/bsvars/reference/compute_impulse_responses.md),
[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`normalise`](https://bsvars.org/bsvars/reference/normalise.md),
[`summary`](https://rdrr.io/r/base/summary.html)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# specify the model
specification  = specify_bsvar$new(us_fiscal_lsuw, p = 1)
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

# compute forecast error variance decomposition 2 years ahead
fevd           = compute_variance_decompositions(posterior, horizon = 8)

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar$new(p = 1) |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  compute_variance_decompositions(horizon = 8) -> fevd
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
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 20 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
```
