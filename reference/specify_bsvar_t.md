# R6 Class representing the specification of the BSVAR model with t-distributed structural shocks.

The class BSVART presents complete specification for the BSVAR model
with t-distributed structural shocks.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_posterior_bsvar_t`](https://bsvars.org/bsvars/reference/specify_posterior_bsvar_t.md)

## Super class

`bsvars::BSVAR` -\> `BSVART`

## Public fields

- `p`:

  a non-negative integer specifying the autoregressive lag order of the
  model.

- `identification`:

  an object IdentificationBSVARs with the identifying restrictions.

- `prior`:

  an object PriorBSVART with the prior specification.

- `data_matrices`:

  an object DataMatricesBSVAR with the data matrices.

- `starting_values`:

  an object StartingValuesBSVART with the starting values.

- `adaptiveMH`:

  a vector of two values setting the Robust Adaptive Metropolis sampler
  for df: target acceptance rate and adaptive rate.

## Methods

### Public methods

- [`specify_bsvar_t$new()`](#method-BSVART-new)

- [`specify_bsvar_t$clone()`](#method-BSVART-clone)

Inherited methods

- [`bsvars::BSVAR$get_data_matrices()`](https://bsvars.org/bsvars/reference/BSVAR.html#method-get_data_matrices)
- [`bsvars::BSVAR$get_identification()`](https://bsvars.org/bsvars/reference/BSVAR.html#method-get_identification)
- [`bsvars::BSVAR$get_normal()`](https://bsvars.org/bsvars/reference/BSVAR.html#method-get_normal)
- [`bsvars::BSVAR$get_prior()`](https://bsvars.org/bsvars/reference/BSVAR.html#method-get_prior)
- [`bsvars::BSVAR$get_starting_values()`](https://bsvars.org/bsvars/reference/BSVAR.html#method-get_starting_values)

------------------------------------------------------------------------

### Method `new()`

Create a new specification of the BSVAR model with t-distributed
structural shocks, BSVART.

#### Usage

    specify_bsvar_t$new(
      data,
      p = 1L,
      B,
      A,
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

- `exogenous`:

  a `(T+p)xd` matrix of exogenous variables.

- `stationary`:

  an `N` logical vector - its element set to `FALSE` sets the prior mean
  for the autoregressive parameters of the `N`th equation to the white
  noise process, otherwise to random walk.

#### Returns

A new complete specification for the bsvar model with t-distributed
structural shocks, BSVART.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    specify_bsvar_t$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
data(us_fiscal_lsuw)
spec = specify_bsvar_t$new(
   data = us_fiscal_lsuw,
   p = 4
)
#> The identification is set to the default option of lower-triangular structural matrix.
```
