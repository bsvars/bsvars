# R6 Class Representing Forecasts

R6 class representing draws from the predictive density of a Bayesian
Structural Vector Autoregression model.

## Value

An object of class `Forecasts`.

## Details

The class contains the following objects:

- `forecasts`:

  An `N x horizon x S` array containing draws from the predictive
  density.

- `forecast_mean`:

  An `N x horizon x S` array containing the conditional means of the
  predictive density.

- `forecast_covariance`:

  An `N x N x horizon x S` array containing the conditional covariance
  matrices of the predictive density.

- `Y`:

  An `N x T` matrix containing the data on the dependent variables used
  for estimation.

The method `as_list()` returns the contents of the `Forecasts` object as
a list.

## Public fields

- `forecasts`:

  An `N x horizon x S` numeric array containing draws from the
  predictive density.

- `forecast_mean`:

  An `N x horizon x S` numeric array containing the conditional means of
  the predictive density.

- `forecast_covariance`:

  An `N x N x horizon x S` numeric array containing the conditional
  covariance matrices of the predictive density.

- `Y`:

  An `N x T` numeric matrix containing the data on the dependent
  variables used for estimation.

## Methods

### Public methods

- [`Forecasts$new()`](#method-Forecasts-initialize)

- [`Forecasts$get_forecasts()`](#method-Forecasts-get_forecasts)

- [`Forecasts$clone()`](#method-Forecasts-clone)

------------------------------------------------------------------------

### `Forecasts$new()`

Creates a new `Forecasts` object from the output of the forecasting
procedure.

#### Usage

    Forecasts$new(output, Y)

#### Arguments

- `output`:

  A list containing the forecasting output, including `forecasts`,
  `forecast_mean`, and `forecast_cov`.

- `Y`:

  An `N x T` matrix containing the data on the dependent variables.

#### Returns

An object of class `Forecasts`.

------------------------------------------------------------------------

### `Forecasts$get_forecasts()`

Converts the `Forecasts` object to a list.

#### Usage

    Forecasts$get_forecasts()

#### Returns

A list containing `forecasts`, `forecast_mean`, `forecast_covariance`,
and `Y`.

------------------------------------------------------------------------

### `Forecasts$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Forecasts$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
spec = specify_bsvar$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.
burn = estimate(spec, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
post = estimate(burn, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
fore = forecast(post, 4)
apply(fore$forecasts, 1:2, mean) # compute mean forecasts 
#>           [,1]      [,2]      [,3]      [,4]
#> [1,] -8.888228 -8.932025 -8.956138 -8.960784
#> [2,] -9.705272 -9.521921 -9.485589 -9.430052
#> [3,] -7.081218 -7.174442 -7.199131 -7.224809
```
