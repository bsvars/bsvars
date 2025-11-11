# R6 Class Representing PriorBSVART

The class PriorBSVART presents a prior specification for the bsvar model
with t-distributed structural shocks.

## Super class

`bsvars::PriorBSVAR` -\> `PriorBSVART`

## Public fields

- `A`:

  an `NxK` matrix, the mean of the normal prior distribution for the
  parameter matrix \\A\\.

- `A_V_inv`:

  a `KxK` precision matrix of the normal prior distribution for each of
  the row of the parameter matrix \\A\\. This precision matrix is
  equation invariant.

- `B_V_inv`:

  an `NxN` precision matrix of the generalised-normal prior distribution
  for the structural matrix \\B\\. This precision matrix is equation
  invariant.

- `B_nu`:

  a positive integer greater of equal than `N`, a shape parameter of the
  generalised-normal prior distribution for the structural matrix \\B\\.

- `hyper_nu_B`:

  a positive scalar, the shape parameter of the inverted-gamma 2 prior
  for the overall shrinkage parameter for matrix \\B\\.

- `hyper_a_B`:

  a positive scalar, the shape parameter of the gamma prior for the
  second-level hierarchy for the overall shrinkage parameter for matrix
  \\B\\.

- `hyper_s_BB`:

  a positive scalar, the scale parameter of the inverted-gamma 2 prior
  for the third-level of hierarchy for overall shrinkage parameter for
  matrix \\B\\.

- `hyper_nu_BB`:

  a positive scalar, the shape parameter of the inverted-gamma 2 prior
  for the third-level of hierarchy for overall shrinkage parameter for
  matrix \\B\\.

- `hyper_nu_A`:

  a positive scalar, the shape parameter of the inverted-gamma 2 prior
  for the overall shrinkage parameter for matrix \\A\\.

- `hyper_a_A`:

  a positive scalar, the shape parameter of the gamma prior for the
  second-level hierarchy for the overall shrinkage parameter for matrix
  \\A\\.

- `hyper_s_AA`:

  a positive scalar, the scale parameter of the inverted-gamma 2 prior
  for the third-level of hierarchy for overall shrinkage parameter for
  matrix \\A\\.

- `hyper_nu_AA`:

  a positive scalar, the shape parameter of the inverted-gamma 2 prior
  for the third-level of hierarchy for overall shrinkage parameter for
  matrix \\A\\.

## Methods

### Public methods

- [`specify_prior_bsvar_t$clone()`](#method-PriorBSVART-clone)

Inherited methods

- [`bsvars::PriorBSVAR$get_prior()`](https://bsvars.org/bsvars/reference/PriorBSVAR.html#method-get_prior)
- [`bsvars::PriorBSVAR$initialize()`](https://bsvars.org/bsvars/reference/PriorBSVAR.html#method-initialize)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    specify_prior_bsvar_t$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
prior = specify_prior_bsvar_t$new(N = 3, p = 1)  # specify the prior
prior$A                                        # show autoregressive prior mean
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0    0    0
#> [2,]    0    1    0    0
#> [3,]    0    0    1    0
```
