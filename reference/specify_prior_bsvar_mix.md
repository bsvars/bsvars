# R6 Class Representing PriorBSVARMIX

The class PriorBSVARMIX presents a prior specification for the bsvar
model with a zero-mean mixture of normals model for structural shocks.

## Super classes

`bsvars::PriorBSVAR` -\> `bsvars::PriorBSVARMSH` -\> `PriorBSVARMIX`

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

- `sigma_nu`:

  a positive scalar, the shape parameter of the inverted-gamma 2 for
  mixture component-dependent variances of the structural shocks,
  \\\sigma^2\_{n.s_t}\\.

- `sigma_s`:

  a positive scalar, the scale parameter of the inverted-gamma 2 for
  mixture component-dependent variances of the structural shocks,
  \\\sigma^2\_{n.s_t}\\.

- `PR_TR`:

  an `MxM` matrix, the matrix of hyper-parameters of the row-specific
  Dirichlet prior distribution for the state probabilities the Markov
  process \\s_t\\. Its rows must be identical.

## Methods

### Public methods

- [`specify_prior_bsvar_mix$clone()`](#method-PriorBSVARMIX-clone)

Inherited methods

- [`bsvars::PriorBSVARMSH$get_prior()`](https://bsvars.org/bsvars/reference/PriorBSVARMSH.html#method-get_prior)
- [`bsvars::PriorBSVARMSH$initialize()`](https://bsvars.org/bsvars/reference/PriorBSVARMSH.html#method-initialize)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    specify_prior_bsvar_mix$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
prior = specify_prior_bsvar_mix$new(N = 3, p = 1, M = 2)  # specify the prior
prior$A                                        # show autoregressive prior mean
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0    0    0
#> [2,]    0    1    0    0
#> [3,]    0    0    1    0
```
