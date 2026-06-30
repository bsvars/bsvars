# R6 Class representing the specification of the BSVARHMSH model with Heterogeneous Markov Switching Heteroskedasticity.

The class BSVARHMSH presents complete specification for the BSVAR model
with Heterogeneous Markov Switching Heteroskedasticity.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_posterior_bsvar_hmsh`](https://bsvars.org/bsvars/reference/specify_posterior_bsvar_hmsh.md)

## Public fields

- `p`:

  a non-negative integer specifying the autoregressive lag order of the
  model.

- `identification`:

  an object IdentificationBSVARs with the identifying restrictions.

- `prior`:

  an object PriorBSVARMSH with the prior specification.

- `data_matrices`:

  an object DataMatricesBSVAR with the data matrices.

- `starting_values`:

  an object StartingValuesBSVARHMSH with the starting values.

- `finiteM`:

  a logical value - if true a stationary Markov switching model is
  estimated. Otherwise, a sparse Markov switching model is estimated in
  which `M=20` and the number of visited states is estimated.

## Methods

### Public methods

- [`BSVARHMSH$new()`](#method-BSVARHMSH-initialize)

- [`BSVARHMSH$get_normal()`](#method-BSVARHMSH-get_normal)

- [`BSVARHMSH$get_data_matrices()`](#method-BSVARHMSH-get_data_matrices)

- [`BSVARHMSH$get_identification()`](#method-BSVARHMSH-get_identification)

- [`BSVARHMSH$get_prior()`](#method-BSVARHMSH-get_prior)

- [`BSVARHMSH$get_starting_values()`](#method-BSVARHMSH-get_starting_values)

- [`BSVARHMSH$clone()`](#method-BSVARHMSH-clone)

------------------------------------------------------------------------

### `BSVARHMSH$new()`

Create a new specification of the BSVAR model with Heterogeneous Markov
Switching Heteroskedasticity, BSVARHMSH.

#### Usage

    BSVARHMSH$new(
      data,
      p = 1L,
      M = 2L,
      B,
      A,
      distribution = c("norm", "t"),
      exogenous = NULL,
      stationary = rep(FALSE, ncol(data)),
      finiteM = TRUE
    )

#### Arguments

- `data`:

  a `(T+p)xN` matrix with time series data.

- `p`:

  a positive integer providing model's autoregressive lag order.

- `M`:

  an integer greater than 1 - the number of Markov process'
  heteroskedastic regimes.

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

- `finiteM`:

  a logical value - if true a stationary Markov switching model is
  estimated. Otherwise, a sparse Markov switching model is estimated in
  which `M=20` and the number of visited states is estimated.

#### Returns

A new complete specification for the bsvar model with Heterogeneous
Markov Switching Heteroskedasticity, BSVARHMSH.

------------------------------------------------------------------------

### `BSVARHMSH$get_normal()`

Returns the logical value of whether the conditional shock distribution
is normal.

#### Usage

    BSVARHMSH$get_normal()

#### Examples

    spec = specify_bsvar_hmsh$new(us_fiscal_lsuw)
    spec$get_normal()

------------------------------------------------------------------------

### `BSVARHMSH$get_data_matrices()`

Returns the data matrices as the DataMatricesBSVAR object.

#### Usage

    BSVARHMSH$get_data_matrices()

#### Examples

    spec = specify_bsvar_hmsh$new(
       data = us_fiscal_lsuw,
       p = 4,
       M = 2
    )
    spec$get_data_matrices()

------------------------------------------------------------------------

### `BSVARHMSH$get_identification()`

Returns the identifying restrictions as the IdentificationBSVARs object.

#### Usage

    BSVARHMSH$get_identification()

#### Examples

    spec = specify_bsvar_hmsh$new(
       data = us_fiscal_lsuw,
       p = 4,
       M = 2
    )
    spec$get_identification()

------------------------------------------------------------------------

### `BSVARHMSH$get_prior()`

Returns the prior specification as the PriorBSVARMSH object.

#### Usage

    BSVARHMSH$get_prior()

#### Examples

    spec = specify_bsvar_hmsh$new(
       data = us_fiscal_lsuw,
       p = 4,
       M = 2
    )
    spec$get_prior()

------------------------------------------------------------------------

### `BSVARHMSH$get_starting_values()`

Returns the starting values as the StartingValuesBSVARHMSH object.

#### Usage

    BSVARHMSH$get_starting_values()

#### Examples

    spec = specify_bsvar_hmsh$new(
       data = us_fiscal_lsuw,
       p = 4,
       M = 2
    )
    spec$get_starting_values()

------------------------------------------------------------------------

### `BSVARHMSH$clone()`

The objects of this class are cloneable with this method.

#### Usage

    BSVARHMSH$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
spec = specify_bsvar_hmsh$new(
   data = us_fiscal_lsuw,
   p = 4,
   M = 2
)
#> The identification is set to the default option of lower-triangular structural matrix.


## ------------------------------------------------
## Method `BSVARHMSH$get_normal()`
## ------------------------------------------------

spec = specify_bsvar_hmsh$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.
spec$get_normal()
#> [1] TRUE


## ------------------------------------------------
## Method `BSVARHMSH$get_data_matrices()`
## ------------------------------------------------

spec = specify_bsvar_hmsh$new(
   data = us_fiscal_lsuw,
   p = 4,
   M = 2
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
## Method `BSVARHMSH$get_identification()`
## ------------------------------------------------

spec = specify_bsvar_hmsh$new(
   data = us_fiscal_lsuw,
   p = 4,
   M = 2
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
## Method `BSVARHMSH$get_prior()`
## ------------------------------------------------

spec = specify_bsvar_hmsh$new(
   data = us_fiscal_lsuw,
   p = 4,
   M = 2
)
#> The identification is set to the default option of lower-triangular structural matrix.
spec$get_prior()
#> <PriorBSVARMSH>
#>   Inherits from: <PriorBSVAR>
#>   Public:
#>     A: 1 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  ...
#>     A_V_inv: 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0  ...
#>     B_V_inv: 1 0 0 0 1 0 0 0 1
#>     B_nu: 3
#>     PR_TR: 1 1 1 1
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
#>     initialize: function (N, p, d = 0, M, stationary = rep(FALSE, N)) 
#>     sigma_nu: 3
#>     sigma_s: 1


## ------------------------------------------------
## Method `BSVARHMSH$get_starting_values()`
## ------------------------------------------------

spec = specify_bsvar_hmsh$new(
   data = us_fiscal_lsuw,
   p = 4,
   M = 2
)
#> The identification is set to the default option of lower-triangular structural matrix.
spec$get_starting_values()
#> <StartingValuesBSVARHMSH>
#>   Inherits from: <StartingValuesBSVAR>
#>   Public:
#>     A: 0.317735717864707 0 0 0 0.329257216304541 0 0 0 0.161105 ...
#>     B: 0.134332926711068 0 0 0 0.00929720932617784 0 0 0 0.6143 ...
#>     PR_TR: 1 0 0 1 1 0 0 1 1 0 0 1
#>     clone: function (deep = FALSE) 
#>     df: 3 3 3
#>     get_starting_values: function () 
#>     hyper: 10 10 10 10 10 10 10 10 10 10 10 10 10 10
#>     initialize: function (A, B, N, p, M, T, d = 0, finiteM = TRUE) 
#>     lambda: 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  ...
#>     pi_0: 0.5 0.5 0.5 0.5 0.5 0.5
#>     set_starting_values: function (last_draw) 
#>     sigma2: 1 1 1 1 1 1
#>     xi: 1 0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 0 1 0 1 0  ...
```
