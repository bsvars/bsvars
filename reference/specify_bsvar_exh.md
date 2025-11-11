# R6 Class representing the specification of the BSVAREXH model with exogenous heteroskedastic regime change.

The class BSVAREXH presents complete specification for the BSVAR model
with exogenous heteroskedastic regime change.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_posterior_bsvar_exh`](https://bsvars.org/bsvars/reference/specify_posterior_bsvar_exh.md)

## Public fields

- `p`:

  a non-negative integer specifying the autoregressive lag order of the
  model.

- `identification`:

  an object IdentificationBSVARs with the identifying restrictions.

- `prior`:

  an object PriorBSVAREXH with the prior specification.

- `data_matrices`:

  an object DataMatricesBSVAR with the data matrices.

- `starting_values`:

  an object StartingValuesBSVAREXH with the starting values.

- `variance_regimes`:

  a `T`-vector with exogenous regime indicators that are integer numbers
  associating the time observation with heteroskedastic regime.

## Methods

### Public methods

- [`specify_bsvar_exh$new()`](#method-BSVAREXH-new)

- [`specify_bsvar_exh$get_normal()`](#method-BSVAREXH-get_normal)

- [`specify_bsvar_exh$get_data_matrices()`](#method-BSVAREXH-get_data_matrices)

- [`specify_bsvar_exh$get_identification()`](#method-BSVAREXH-get_identification)

- [`specify_bsvar_exh$get_prior()`](#method-BSVAREXH-get_prior)

- [`specify_bsvar_exh$get_starting_values()`](#method-BSVAREXH-get_starting_values)

- [`specify_bsvar_exh$clone()`](#method-BSVAREXH-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new specification of the BSVAR model with Markov Switching
Heteroskedasticity, BSVAREXH.

#### Usage

    specify_bsvar_exh$new(
      data,
      p = 1L,
      B,
      A,
      distribution = c("norm", "t"),
      exogenous = NULL,
      stationary = rep(FALSE, ncol(data)),
      variance_regimes = NULL
    )

#### Arguments

- `data`:

  a `(T+p)xN` matrix with time series data.

- `p`:

  a positive integer providing model's autoregressive lag order.

- `B`:

  a logical `NxN` matrix containing value `TRUE` for the elements of the
  structural matrix \\B\\ to be estimated and value `FALSE` for
  exclusion restrictions to be set to zero.

- `A`:

  a logical `NxK` matrix containing value `TRUE` for the elements of the
  autoregressive matrix \\A\\ to be estimated and value `FALSE` for
  exclusion restrictions to be set to zero.

- `distribution`:

  a character string specifying the conditional distribution of
  structural shocks. Value `"norm"` sets it to the normal distribution,
  while value `"t"` sets the Student-t distribution.

- `exogenous`:

  a `(T+p)xd` matrix of exogenous variables.

- `stationary`:

  an `N` logical vector - its element set to `FALSE` sets the prior mean
  for the autoregressive parameters of the `N`th equation to the white
  noise process, otherwise to random walk.

- `variance_regimes`:

  a `T`-vector with exogenous regime indicators that are integer numbers
  associating the time observation with heteroskedastic regime.

#### Returns

A new complete specification for the bsvar model with exogenous
heteroskedastic regime change, BSVAREXH.

------------------------------------------------------------------------

### Method `get_normal()`

Returns the logical value of whether the conditional shock distribution
is normal.

#### Usage

    specify_bsvar_exh$get_normal()

#### Examples

    spec = specify_bsvar_exh$new(us_fiscal_lsuw)
    spec$get_normal()

------------------------------------------------------------------------

### Method `get_data_matrices()`

Returns the data matrices as the DataMatricesBSVAR object.

#### Usage

    specify_bsvar_exh$get_data_matrices()

#### Examples

    spec = specify_bsvar_exh$new(us_fiscal_lsuw)
    spec$get_data_matrices()

------------------------------------------------------------------------

### Method `get_identification()`

Returns the identifying restrictions as the IdentificationBSVARs object.

#### Usage

    specify_bsvar_exh$get_identification()

#### Examples

    spec = specify_bsvar_exh$new(us_fiscal_lsuw)
    spec$get_identification()

------------------------------------------------------------------------

### Method `get_prior()`

Returns the prior specification as the PriorBSVAREXH object.

#### Usage

    specify_bsvar_exh$get_prior()

#### Examples

    spec = specify_bsvar_exh$new(us_fiscal_lsuw)
    spec$get_prior()

------------------------------------------------------------------------

### Method `get_starting_values()`

Returns the starting values as the StartingValuesBSVAREXH object.

#### Usage

    specify_bsvar_exh$get_starting_values()

#### Examples

    spec = specify_bsvar_exh$new(us_fiscal_lsuw)
    spec$get_starting_values()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    specify_bsvar_exh$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
spec = specify_bsvar_exh$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.


## ------------------------------------------------
## Method `specify_bsvar_exh$get_normal`
## ------------------------------------------------

spec = specify_bsvar_exh$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.
spec$get_normal()
#> [1] TRUE


## ------------------------------------------------
## Method `specify_bsvar_exh$get_data_matrices`
## ------------------------------------------------

spec = specify_bsvar_exh$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.
spec$get_data_matrices()
#> <DataMatricesBSVAR>
#>   Public:
#>     X: -10.5411029495397 -10.7783202280655 -8.43070154932426 1  ...
#>     Y: -10.623035131035 -10.7433212969095 -8.41622699343472 -10 ...
#>     clone: function (deep = FALSE) 
#>     get_data_matrices: function () 
#>     initialize: function (data, p = 1L, exogenous = NULL) 


## ------------------------------------------------
## Method `specify_bsvar_exh$get_identification`
## ------------------------------------------------

spec = specify_bsvar_exh$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.
spec$get_identification()
#> <IdentificationBSVARs>
#>   Public:
#>     VA: list
#>     VB: list
#>     clone: function (deep = FALSE) 
#>     get_identification: function () 
#>     initialize: function (B, A, N, K) 
#>     set_identification: function (B, A, N, K) 


## ------------------------------------------------
## Method `specify_bsvar_exh$get_prior`
## ------------------------------------------------

spec = specify_bsvar_exh$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.
spec$get_prior()
#> <PriorBSVAREXH>
#>   Inherits from: <PriorBSVAR>
#>   Public:
#>     A: 1 0 0 0 1 0 0 0 1 0 0 0
#>     A_V_inv: 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1
#>     B_V_inv: 1 0 0 0 1 0 0 0 1
#>     B_nu: 3
#>     clone: function (deep = FALSE) 
#>     get_prior: function () 
#>     hyper_a_A: 10
#>     hyper_a_B: 10
#>     hyper_nu_A: 10
#>     hyper_nu_AA: 10
#>     hyper_nu_B: 10
#>     hyper_nu_BB: 1
#>     hyper_s_AA: 10
#>     hyper_s_BB: 100
#>     initialize: function (N, p, d = 0, stationary = rep(FALSE, N)) 
#>     sigma_nu: 3
#>     sigma_s: 1


## ------------------------------------------------
## Method `specify_bsvar_exh$get_starting_values`
## ------------------------------------------------

spec = specify_bsvar_exh$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.
spec$get_starting_values()
#> <StartingValuesBSVAREXH>
#>   Inherits from: <StartingValuesBSVAR>
#>   Public:
#>     A: 0.652855827938765 0 0 0 0.172617509029806 0 0 0 0.284573 ...
#>     B: 0.47622753563337 0 0 0 0.0934937363490462 0 0 0 0.418628 ...
#>     clone: function (deep = FALSE) 
#>     df: 3 3 3
#>     get_starting_values: function () 
#>     hyper: 10 10 10 10 10 10 10 10 10 10 10 10 10 10
#>     initialize: function (A, B, N, p, T, d = 0, variance_regimes = rep(1, T)) 
#>     lambda: 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  ...
#>     set_starting_values: function (last_draw) 
#>     sigma2: 1 1 1
#>     xi: 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  ...
```
