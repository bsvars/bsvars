# Plots structural shocks' conditional standard deviations

Plots of structural shocks' conditional standard deviations including
their median and percentiles.

## Usage

``` r
# S3 method for class 'PosteriorSigma'
plot(
  x,
  probability = 0.9,
  shock_names,
  col = "#ff69b4",
  main,
  xlab,
  mar.multi = c(1, 4.6, 0, 2.1),
  oma.multi = c(6, 0, 5, 0),
  ...
)
```

## Arguments

- x:

  an object of class PosteriorSigma obtained using the
  [`compute_conditional_sd()`](https://bsvars.org/bsvars/reference/compute_conditional_sd.md)
  function containing posterior draws of conditional standard deviations
  of structural shocks.

- probability:

  a parameter determining the interval to be plotted. The interval
  stretches from the `0.5 * (1 - probability)` to
  `1 - 0.5 * (1 - probability)` percentile of the posterior
  distribution.

- shock_names:

  a vector of length `N` containing names of the structural shocks.

- col:

  a colour of the plot line and the ribbon

- main:

  an alternative main title for the plot

- xlab:

  an alternative x-axis label for the plot

- mar.multi:

  the default `mar` argument setting in
  [`graphics::par`](https://rdrr.io/r/graphics/par.html). Modify with
  care!

- oma.multi:

  the default `oma` argument setting in
  [`graphics::par`](https://rdrr.io/r/graphics/par.html). Modify with
  care!

- ...:

  additional arguments affecting the summary produced.

## See also

[`compute_conditional_sd`](https://bsvars.org/bsvars/reference/compute_conditional_sd.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
data(us_fiscal_lsuw)                                  # upload data
set.seed(123)                                         # set seed
specification  = specify_bsvar_sv$new(us_fiscal_lsuw) # specify model
#> The identification is set to the default option of lower-triangular structural matrix.
burn_in        = estimate(specification, 5)           # run the burn-in
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
posterior      = estimate(burn_in, 5)                 # estimate the model
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# compute structural shocks' conditional standard deviations
sigma          = compute_conditional_sd(posterior)
plot(sigma)                                            # plot conditional sds


# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar_sv$new(p = 1) |>
  estimate(S = 5) |> 
  estimate(S = 5) |> 
  compute_conditional_sd() |>
  plot()
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
```
