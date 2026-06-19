# Plots the median and an interval between two specified percentiles for a sequence of `K` random variables

Plots the median and an interval between two specified percentiles for a
sequence of `K` random variables based on the `S` posterior draws
provided for each of them.

## Usage

``` r
plot_ribbon(
  draws,
  probability = 0.9,
  col = "#ff69b4",
  ylim,
  ylab,
  xlab,
  start_at = 0,
  add = FALSE,
  ...
)
```

## Arguments

- draws:

  a `K x S` matrix with `S` posterior draws of `K` random variables, or
  a `K x S x N` array with `N` such matrices

- probability:

  a number from interval `(0,1)` denoting the probability content of the
  plotted interval. The interval stretches from the
  `0.5 * (1 - probability)` to `1 - 0.5 * (1 - probability)` percentile
  of the posterior distribution.

- col:

  a colour of the plot

- ylim:

  the range of the `y` axis

- ylab:

  the label of the `y` axis

- xlab:

  the label of the `x` axis

- start_at:

  an integer to denote the beginning of the `x` axis range

- add:

  a logical value. If `TRUE` the current ribbon plot is added to an
  existing plot

- ...:

  other graphical parameters to be passed to
  [`base::plot`](https://rdrr.io/r/base/plot.html)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
data(us_fiscal_lsuw)                                               # upload data
set.seed(123)                                                      # set seed
specification  = specify_bsvar$new(us_fiscal_lsuw)                 # specify model
#> The identification is set to the default option of lower-triangular structural matrix.

burn_in        = estimate(specification, 10)                       # run the burn-in
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
posterior      = estimate(burn_in, 20, thin = 1)                   # estimate the model
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
irf            = compute_impulse_responses(posterior, horizon = 4) # impulse responses
plot_ribbon(irf[1,1,,])

```
