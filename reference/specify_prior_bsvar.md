# R6 Class Representing PriorBSVAR

The class PriorBSVAR presents a prior specification for the
homoskedastic bsvar model.

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

- [`specify_prior_bsvar$new()`](#method-PriorBSVAR-new)

- [`specify_prior_bsvar$get_prior()`](#method-PriorBSVAR-get_prior)

- [`specify_prior_bsvar$clone()`](#method-PriorBSVAR-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new prior specification PriorBSVAR.

#### Usage

    specify_prior_bsvar$new(N, p, d = 0, stationary = rep(FALSE, N))

#### Arguments

- `N`:

  a positive integer - the number of dependent variables in the model.

- `p`:

  a positive integer - the autoregressive lag order of the SVAR model.

- `d`:

  a positive integer - the number of `exogenous` variables in the model.

- `stationary`:

  an `N` logical vector - its element set to `FALSE` sets the prior mean
  for the autoregressive parameters of the `N`th equation to the white
  noise process, otherwise to random walk.

#### Returns

A new prior specification PriorBSVAR.

#### Examples

    # a prior for 3-variable example with one lag and stationary data
    prior = specify_prior_bsvar$new(N = 3, p = 1, stationary = rep(TRUE, 3))
    prior$A # show autoregressive prior mean

------------------------------------------------------------------------

### Method `get_prior()`

Returns the elements of the prior specification PriorBSVAR as a `list`.

#### Usage

    specify_prior_bsvar$get_prior()

#### Examples

    # a prior for 3-variable example with four lags
    prior = specify_prior_bsvar$new(N = 3, p = 4)
    prior$get_prior() # show the prior as list

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    specify_prior_bsvar$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
prior = specify_prior_bsvar$new(N = 3, p = 1)  # a prior for 3-variable example with one lag
prior$A                                        # show autoregressive prior mean
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0    0    0
#> [2,]    0    1    0    0
#> [3,]    0    0    1    0


## ------------------------------------------------
## Method `specify_prior_bsvar$new`
## ------------------------------------------------

# a prior for 3-variable example with one lag and stationary data
prior = specify_prior_bsvar$new(N = 3, p = 1, stationary = rep(TRUE, 3))
prior$A # show autoregressive prior mean
#>      [,1] [,2] [,3] [,4]
#> [1,]    0    0    0    0
#> [2,]    0    0    0    0
#> [3,]    0    0    0    0


## ------------------------------------------------
## Method `specify_prior_bsvar$get_prior`
## ------------------------------------------------

# a prior for 3-variable example with four lags
prior = specify_prior_bsvar$new(N = 3, p = 4)
prior$get_prior() # show the prior as list
#> $A
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
#> [1,]    1    0    0    0    0    0    0    0    0     0     0     0     0
#> [2,]    0    1    0    0    0    0    0    0    0     0     0     0     0
#> [3,]    0    0    1    0    0    0    0    0    0     0     0     0     0
#> 
#> $A_V_inv
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
#>  [1,]    1    0    0    0    0    0    0    0    0     0     0     0     0
#>  [2,]    0    1    0    0    0    0    0    0    0     0     0     0     0
#>  [3,]    0    0    1    0    0    0    0    0    0     0     0     0     0
#>  [4,]    0    0    0    4    0    0    0    0    0     0     0     0     0
#>  [5,]    0    0    0    0    4    0    0    0    0     0     0     0     0
#>  [6,]    0    0    0    0    0    4    0    0    0     0     0     0     0
#>  [7,]    0    0    0    0    0    0    9    0    0     0     0     0     0
#>  [8,]    0    0    0    0    0    0    0    9    0     0     0     0     0
#>  [9,]    0    0    0    0    0    0    0    0    9     0     0     0     0
#> [10,]    0    0    0    0    0    0    0    0    0    16     0     0     0
#> [11,]    0    0    0    0    0    0    0    0    0     0    16     0     0
#> [12,]    0    0    0    0    0    0    0    0    0     0     0    16     0
#> [13,]    0    0    0    0    0    0    0    0    0     0     0     0     1
#> 
#> $B_V_inv
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    0    1    0
#> [3,]    0    0    1
#> 
#> $B_nu
#> [1] 3
#> 
#> $hyper_nu_B
#> [1] 10
#> 
#> $hyper_a_B
#> [1] 10
#> 
#> $hyper_s_BB
#> [1] 100
#> 
#> $hyper_nu_BB
#> [1] 1
#> 
#> $hyper_nu_A
#> [1] 10
#> 
#> $hyper_a_A
#> [1] 10
#> 
#> $hyper_s_AA
#> [1] 10
#> 
#> $hyper_nu_AA
#> [1] 10
#> 
```
