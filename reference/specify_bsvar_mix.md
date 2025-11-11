# R6 Class representing the specification of the BSVAR model with a zero-mean mixture of normals model for structural shocks.

The class BSVARMIX presents complete specification for the BSVAR model
with a zero-mean mixture of normals model for structural shocks.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_posterior_bsvar_mix`](https://bsvars.org/bsvars/reference/specify_posterior_bsvar_mix.md)

## Super class

`bsvars::BSVARMSH` -\> `BSVARMIX`

## Public fields

- `p`:

  a non-negative integer specifying the autoregressive lag order of the
  model.

- `identification`:

  an object IdentificationBSVARs with the identifying restrictions.

- `prior`:

  an object PriorBSVARMIX with the prior specification.

- `data_matrices`:

  an object DataMatricesBSVAR with the data matrices.

- `starting_values`:

  an object StartingValuesBSVARMIX with the starting values.

- `finiteM`:

  a logical value - if true a finite mixture model is estimated.
  Otherwise, a sparse mixture model is estimated in which `M=20` and the
  number of visited states is estimated.

## Methods

### Public methods

- [`specify_bsvar_mix$new()`](#method-BSVARMIX-new)

- [`specify_bsvar_mix$clone()`](#method-BSVARMIX-clone)

Inherited methods

- [`bsvars::BSVARMSH$get_data_matrices()`](https://bsvars.org/bsvars/reference/BSVARMSH.html#method-get_data_matrices)
- [`bsvars::BSVARMSH$get_identification()`](https://bsvars.org/bsvars/reference/BSVARMSH.html#method-get_identification)
- [`bsvars::BSVARMSH$get_normal()`](https://bsvars.org/bsvars/reference/BSVARMSH.html#method-get_normal)
- [`bsvars::BSVARMSH$get_prior()`](https://bsvars.org/bsvars/reference/BSVARMSH.html#method-get_prior)
- [`bsvars::BSVARMSH$get_starting_values()`](https://bsvars.org/bsvars/reference/BSVARMSH.html#method-get_starting_values)

------------------------------------------------------------------------

### Method `new()`

Create a new specification of the BSVAR model with a zero-mean mixture
of normals model for structural shocks, BSVARMIX.

#### Usage

    specify_bsvar_mix$new(
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

  an integer greater than 1 - the number of components of the mixture of
  normals.

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

  a logical value - if true a finite mixture model is estimated.
  Otherwise, a sparse mixture model is estimated in which `M=20` and the
  number of visited states is estimated.

#### Returns

A new complete specification for the bsvar model with a zero-mean
mixture of normals model for structural shocks, BSVARMIX.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    specify_bsvar_mix$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
data(us_fiscal_lsuw)
spec = specify_bsvar_mix$new(
   data = us_fiscal_lsuw,
   p = 4,
   M = 2
)
#> The identification is set to the default option of lower-triangular structural matrix.
```
