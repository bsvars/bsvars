# Plots forecast error variance decompositions

Plots of the posterior means of the forecast error variance
decompositions.

## Usage

``` r
# S3 method for class 'PosteriorFEVD'
plot(
  x,
  shock_names,
  cols,
  main,
  xlab,
  mar.multi = c(1, 4.6, 0, 4.6),
  oma.multi = c(6, 0, 5, 0),
  ...
)
```

## Arguments

- x:

  an object of class PosteriorFEVD obtained using the
  [`compute_variance_decompositions()`](https://bsvars.org/bsvars/reference/compute_variance_decompositions.md)
  function containing posterior draws of forecast error variance
  decompositions.

- shock_names:

  a vector of length `N` containing names of the structural shocks.

- cols:

  an `N`-vector with colours of the plot

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

[`compute_variance_decompositions`](https://bsvars.org/bsvars/reference/compute_variance_decompositions.md)

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

# compute forecast error variance decompositions
fevd           = compute_variance_decompositions(posterior, horizon = 4)
plot(fevd)


# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar$new() |>
  estimate(S = 10) |> 
  estimate(S = 20, thin = 1) |> 
  compute_variance_decompositions(horizon = 4) |>
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
