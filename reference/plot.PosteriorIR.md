# Plots impulse responses

Plots of of all variables to all shocks including their median and
percentiles.

## Usage

``` r
# S3 method for class 'PosteriorIR'
plot(
  x,
  probability = 0.9,
  shock_names,
  col = "#ff69b4",
  main,
  xlab,
  mar.multi = c(1, 4.1, 0, 1.1),
  oma.multi = c(6, 0, 5, 0),
  ...
)
```

## Arguments

- x:

  an object of class PosteriorIR obtained using the
  [`compute_impulse_responses()`](https://bsvars.org/bsvars/reference/compute_impulse_responses.md)
  function containing posterior draws of impulse responses.

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

[`compute_impulse_responses`](https://bsvars.org/bsvars/reference/compute_impulse_responses.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
data(us_fiscal_lsuw)                                  # upload data
set.seed(123)                                         # set seed
specification  = specify_bsvar$new(us_fiscal_lsuw)    # specify model
#> The identification is set to the default option of lower-triangular structural matrix.
burn_in        = estimate(specification, 10)          # run the burn-in
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
posterior      = estimate(burn_in, 20, thin = 1)      # estimate the model
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 20 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# compute impulse responses
fitted         = compute_impulse_responses(posterior, horizon = 4)
plot(fitted)                                          # plot


# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar$new() |>
  estimate(S = 10) |> 
  estimate(S = 20, thin = 1) |> 
  compute_impulse_responses(horizon = 4) |>
  plot()
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
