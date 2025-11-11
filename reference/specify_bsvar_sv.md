# R6 Class representing the specification of the BSVAR model with Stochastic Volatility heteroskedasticity.

The class BSVARSV presents complete specification for the BSVAR model
with Stochastic Volatility heteroskedasticity.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_posterior_bsvar_sv`](https://bsvars.org/bsvars/reference/specify_posterior_bsvar_sv.md)

## Public fields

- `p`:

  a non-negative integer specifying the autoregressive lag order of the
  model.

- `identification`:

  an object IdentificationBSVARs with the identifying restrictions.

- `prior`:

  an object PriorBSVARSV with the prior specification.

- `data_matrices`:

  an object DataMatricesBSVAR with the data matrices.

- `starting_values`:

  an object StartingValuesBSVARSV with the starting values.

- `centred_sv`:

  a logical value - if true a centred parameterisation of the Stochastic
  Volatility process is estimated. Otherwise, its non-centred
  parameterisation is estimated. See Lütkepohl, Shang, Uzeda,
  Woźniak (2022) for more info.

## Methods

### Public methods

- [`specify_bsvar_sv$new()`](#method-BSVARSV-new)

- [`specify_bsvar_sv$get_normal()`](#method-BSVARSV-get_normal)

- [`specify_bsvar_sv$get_data_matrices()`](#method-BSVARSV-get_data_matrices)

- [`specify_bsvar_sv$get_identification()`](#method-BSVARSV-get_identification)

- [`specify_bsvar_sv$get_prior()`](#method-BSVARSV-get_prior)

- [`specify_bsvar_sv$get_starting_values()`](#method-BSVARSV-get_starting_values)

- [`specify_bsvar_sv$clone()`](#method-BSVARSV-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new specification of the BSVAR model with Stochastic Volatility
heteroskedasticity, BSVARSV.

#### Usage

    specify_bsvar_sv$new(
      data,
      p = 1L,
      B,
      A,
      distribution = c("norm", "t"),
      exogenous = NULL,
      centred_sv = FALSE,
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

- `centred_sv`:

  a logical value. If `FALSE` a non-centred Stochastic Volatility
  processes for conditional variances are estimated. Otherwise, a
  centred process is estimated.

- `stationary`:

  an `N` logical vector - its element set to `FALSE` sets the prior mean
  for the autoregressive parameters of the `N`th equation to the white
  noise process, otherwise to random walk.

#### Returns

A new complete specification for the bsvar model with Stochastic
Volatility heteroskedasticity, BSVARSV.

------------------------------------------------------------------------

### Method `get_normal()`

Returns the logical value of whether the conditional shock distribution
is normal.

#### Usage

    specify_bsvar_sv$get_normal()

#### Examples

    spec = specify_bsvar_sv$new(us_fiscal_lsuw)
    spec$get_normal()

------------------------------------------------------------------------

### Method `get_data_matrices()`

Returns the data matrices as the DataMatricesBSVAR object.

#### Usage

    specify_bsvar_sv$get_data_matrices()

#### Examples

    data(us_fiscal_lsuw)
    spec = specify_bsvar_sv$new(
       data = us_fiscal_lsuw,
       p = 4
    )
    spec$get_data_matrices()

------------------------------------------------------------------------

### Method `get_identification()`

Returns the identifying restrictions as the IdentificationBSVARs object.

#### Usage

    specify_bsvar_sv$get_identification()

#### Examples

    data(us_fiscal_lsuw)
    spec = specify_bsvar_sv$new(
       data = us_fiscal_lsuw,
       p = 4
    )
    spec$get_identification()

------------------------------------------------------------------------

### Method `get_prior()`

Returns the prior specification as the PriorBSVARSV object.

#### Usage

    specify_bsvar_sv$get_prior()

#### Examples

    data(us_fiscal_lsuw)
    spec = specify_bsvar_sv$new(
       data = us_fiscal_lsuw,
       p = 4
    )
    spec$get_prior()

------------------------------------------------------------------------

### Method `get_starting_values()`

Returns the starting values as the StartingValuesBSVARSV object.

#### Usage

    specify_bsvar_sv$get_starting_values()

#### Examples

    data(us_fiscal_lsuw)
    spec = specify_bsvar_sv$new(
       data = us_fiscal_lsuw,
       p = 4
    )
    spec$get_starting_values()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    specify_bsvar_sv$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
data(us_fiscal_lsuw)
spec = specify_bsvar_sv$new(
   data = us_fiscal_lsuw,
   p = 4
)
#> The identification is set to the default option of lower-triangular structural matrix.


## ------------------------------------------------
## Method `specify_bsvar_sv$get_normal`
## ------------------------------------------------

spec = specify_bsvar_sv$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.
spec$get_normal()
#> [1] TRUE


## ------------------------------------------------
## Method `specify_bsvar_sv$get_data_matrices`
## ------------------------------------------------

data(us_fiscal_lsuw)
spec = specify_bsvar_sv$new(
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
## Method `specify_bsvar_sv$get_identification`
## ------------------------------------------------

data(us_fiscal_lsuw)
spec = specify_bsvar_sv$new(
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
## Method `specify_bsvar_sv$get_prior`
## ------------------------------------------------

data(us_fiscal_lsuw)
spec = specify_bsvar_sv$new(
   data = us_fiscal_lsuw,
   p = 4
)
#> The identification is set to the default option of lower-triangular structural matrix.
spec$get_prior()
#> <PriorBSVARSV>
#>   Inherits from: <PriorBSVAR>
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
#>     sv_a_: 1
#>     sv_s_: 0.1


## ------------------------------------------------
## Method `specify_bsvar_sv$get_starting_values`
## ------------------------------------------------

data(us_fiscal_lsuw)
spec = specify_bsvar_sv$new(
   data = us_fiscal_lsuw,
   p = 4
)
#> The identification is set to the default option of lower-triangular structural matrix.
spec$get_starting_values()
#> <StartingValuesBSVARSV>
#>   Inherits from: <StartingValuesBSVAR>
#>   Public:
#>     A: 0.1538001219742 0 0 0 0.76917446567677 0 0 0 0.114089946 ...
#>     B: 0.161096099996939 0 0 0 0.506002071779221 0 0 0 0.096362 ...
#>     S: 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  ...
#>     clone: function (deep = FALSE) 
#>     df: 3 3 3
#>     get_starting_values: function () 
#>     h: -0.00559455713761091 -0.00724952784695868 0.002686607386 ...
#>     hyper: 10 10 10 10 10 10 10 10 10 10 10 10 10 10
#>     initialize: function (A, B, N, p, T, d = 0) 
#>     lambda: 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  ...
#>     omega: 0.1 0.1 0.1
#>     rho: 0.5 0.5 0.5
#>     s_: 0.05 0.05 0.05
#>     set_starting_values: function (last_draw) 
#>     sigma2_omega: 1 1 1
#>     sigma2v: 0.01 0.01 0.01
```
