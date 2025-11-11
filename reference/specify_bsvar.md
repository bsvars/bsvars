# R6 Class representing the specification of the homoskedastic BSVAR model

The class BSVAR presents complete specification for the homoskedastic
bsvar model.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_posterior_bsvar`](https://bsvars.org/bsvars/reference/specify_posterior_bsvar.md)

## Public fields

- `p`:

  a non-negative integer specifying the autoregressive lag order of the
  model.

- `identification`:

  an object IdentificationBSVAR with the identifying restrictions.

- `prior`:

  an object PriorBSVAR with the prior specification.

- `data_matrices`:

  an object DataMatricesBSVAR with the data matrices.

- `starting_values`:

  an object StartingValuesBSVAR with the starting values.

## Methods

### Public methods

- [`specify_bsvar$new()`](#method-BSVAR-new)

- [`specify_bsvar$get_normal()`](#method-BSVAR-get_normal)

- [`specify_bsvar$get_data_matrices()`](#method-BSVAR-get_data_matrices)

- [`specify_bsvar$get_identification()`](#method-BSVAR-get_identification)

- [`specify_bsvar$get_prior()`](#method-BSVAR-get_prior)

- [`specify_bsvar$get_starting_values()`](#method-BSVAR-get_starting_values)

- [`specify_bsvar$clone()`](#method-BSVAR-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new specification of the homoskedastic bsvar model BSVAR.

#### Usage

    specify_bsvar$new(
      data,
      p = 1L,
      B,
      A,
      distribution = c("norm", "t"),
      exogenous = NULL,
      stationary = rep(FALSE, ncol(data))
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

#### Returns

A new complete specification for the homoskedastic bsvar model BSVAR.

------------------------------------------------------------------------

### Method `get_normal()`

Returns the logical value of whether the conditional shock distribution
is normal.

#### Usage

    specify_bsvar$get_normal()

#### Examples

    spec = specify_bsvar$new(us_fiscal_lsuw)
    spec$get_normal()

------------------------------------------------------------------------

### Method `get_data_matrices()`

Returns the data matrices as the DataMatricesBSVAR object.

#### Usage

    specify_bsvar$get_data_matrices()

#### Examples

    data(us_fiscal_lsuw)
    spec = specify_bsvar$new(
       data = us_fiscal_lsuw,
       p = 4
    )
    spec$get_data_matrices()

------------------------------------------------------------------------

### Method `get_identification()`

Returns the identifying restrictions as the IdentificationBSVARs object.

#### Usage

    specify_bsvar$get_identification()

#### Examples

    data(us_fiscal_lsuw)
    spec = specify_bsvar$new(
       data = us_fiscal_lsuw,
       p = 4
    )
    spec$get_identification()

------------------------------------------------------------------------

### Method `get_prior()`

Returns the prior specification as the PriorBSVAR object.

#### Usage

    specify_bsvar$get_prior()

#### Examples

    data(us_fiscal_lsuw)
    spec = specify_bsvar$new(
       data = us_fiscal_lsuw,
       p = 4
    )
    spec$get_prior()

------------------------------------------------------------------------

### Method `get_starting_values()`

Returns the starting values as the StartingValuesBSVAR object.

#### Usage

    specify_bsvar$get_starting_values()

#### Examples

    data(us_fiscal_lsuw)
    spec = specify_bsvar$new(
       data = us_fiscal_lsuw,
       p = 4
    )
    spec$get_starting_values()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    specify_bsvar$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
data(us_fiscal_lsuw)
spec = specify_bsvar$new(
   data = us_fiscal_lsuw,
   p = 4
)
#> The identification is set to the default option of lower-triangular structural matrix.


## ------------------------------------------------
## Method `specify_bsvar$get_normal`
## ------------------------------------------------

spec = specify_bsvar$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.
spec$get_normal()
#> [1] TRUE


## ------------------------------------------------
## Method `specify_bsvar$get_data_matrices`
## ------------------------------------------------

data(us_fiscal_lsuw)
spec = specify_bsvar$new(
   data = us_fiscal_lsuw,
   p = 4
)
#> The identification is set to the default option of lower-triangular structural matrix.
spec$get_data_matrices()
#> <DataMatricesBSVAR>
#>   Public:
#>     X: -10.6660327703266 -10.6935259109068 -8.41420587851191 -1 ...
#>     Y: -10.6725356416822 -10.6388624265757 -8.42993325328991 -1 ...
#>     clone: function (deep = FALSE) 
#>     get_data_matrices: function () 
#>     initialize: function (data, p = 1L, exogenous = NULL) 


## ------------------------------------------------
## Method `specify_bsvar$get_identification`
## ------------------------------------------------

data(us_fiscal_lsuw)
spec = specify_bsvar$new(
   data = us_fiscal_lsuw,
   p = 4
)
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
## Method `specify_bsvar$get_prior`
## ------------------------------------------------

data(us_fiscal_lsuw)
spec = specify_bsvar$new(
   data = us_fiscal_lsuw,
   p = 4
)
#> The identification is set to the default option of lower-triangular structural matrix.
spec$get_prior()
#> <PriorBSVAR>
#>   Public:
#>     A: 1 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  ...
#>     A_V_inv: 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0  ...
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


## ------------------------------------------------
## Method `specify_bsvar$get_starting_values`
## ------------------------------------------------

data(us_fiscal_lsuw)
spec = specify_bsvar$new(
   data = us_fiscal_lsuw,
   p = 4
)
#> The identification is set to the default option of lower-triangular structural matrix.
spec$get_starting_values()
#> <StartingValuesBSVAR>
#>   Public:
#>     A: 0.68509255698882 0 0 0 0.897445304319263 0 0 0 0.0795983 ...
#>     B: 0.273720157565549 0 0 0 0.805774891516194 0 0 0 0.296683 ...
#>     clone: function (deep = FALSE) 
#>     df: 3 3 3
#>     get_starting_values: function () 
#>     hyper: 10 10 10 10 10 10 10 10 10 10 10 10 10 10
#>     initialize: function (A, B, N, T, p, d = 0) 
#>     lambda: 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  ...
#>     set_starting_values: function (last_draw) 
```
