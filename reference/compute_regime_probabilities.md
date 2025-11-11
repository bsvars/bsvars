# Computes posterior draws of regime probabilities

Each of the draws from the posterior estimation of a model is
transformed into a draw from the posterior distribution of the regime
probabilities. These represent either the realisations of the regime
indicators, when `type = "realized"`, filtered probabilities, when
`type = "filtered"`, forecasted regime probabilities, when
`type = "forecasted"`, or the smoothed probabilities, when
`type = "smoothed"`, .

## Usage

``` r
compute_regime_probabilities(
  posterior,
  type = c("realized", "filtered", "forecasted", "smoothed")
)
```

## Arguments

- posterior:

  posterior estimation outcome of regime-dependent heteroskedastic
  models - an object of either of the classes: PosteriorBSVARMSH, or
  PosteriorBSVARMIX obtained by running the `estimate` function.

- type:

  one of the values `"realized"`, `"filtered"`, `"forecasted"`, or
  `"smoothed"` denoting the type of probabilities to be computed.

## Value

An object of class PosteriorRegimePr, that is, an `MxTxS` array with
attribute PosteriorRegimePr containing `S` draws of the regime
probabilities.

## References

Song, Y., and Woźniak, T., (2021) Markov Switching. *Oxford Research
Encyclopedia of Economics and Finance*, Oxford University Press,
[doi:10.1093/acrefore/9780190625979.013.174](https://doi.org/10.1093/acrefore/9780190625979.013.174)
.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`summary`](https://rdrr.io/r/base/summary.html)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# specify the model
specification  = specify_bsvar_msh$new(us_fiscal_lsuw)
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

# compute the posterior draws of realized regime indicators
regimes        = compute_regime_probabilities(posterior)

# compute the posterior draws of filtered probabilities
filtered       = compute_regime_probabilities(posterior, "filtered")

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_msh$new() |>
  estimate(S = 10) |> 
  estimate(S = 20) -> posterior
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
regimes        = compute_regime_probabilities(posterior)
filtered       = compute_regime_probabilities(posterior, "filtered")
```
