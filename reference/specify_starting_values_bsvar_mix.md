# R6 Class Representing StartingValuesBSVARMIX

The class StartingValuesBSVARMIX presents starting values for the bsvar
model with a zero-mean mixture of normals model for structural shocks.

## Super classes

`bsvars::StartingValuesBSVAR` -\> `bsvars::StartingValuesBSVARMSH` -\>
`StartingValuesBSVARMIX`

## Public fields

- `A`:

  an `NxK` matrix of starting values for the parameter \\A\\.

- `B`:

  an `NxN` matrix of starting values for the parameter \\B\\.

- `hyper`:

  a `(2*N+1)x2` matrix of starting values for the shrinkage
  hyper-parameters of the hierarchical prior distribution.

- `sigma2`:

  an `NxM` matrix of starting values for the MS state-specific variances
  of the structural shocks. Its elements sum to value `M` over the rows.

- `PR_TR`:

  an `MxM` matrix of starting values for the probability matrix of the
  Markov process. Its rows must be identical and the elements of each
  row sum to 1 over the rows.

- `xi`:

  an `MxT` matrix of starting values for the Markov process indicator.
  Its columns are a chosen column of an identity matrix of order `M`.

- `pi_0`:

  an `M`-vector of starting values for mixture components state
  probabilities. Its elements sum to 1.

- `lambda`:

  a `NxT` matrix of starting values for latent variables.

- `df`:

  an `Nx1` vector of positive numbers with starting values for the
  equation-specific degrees of freedom parameters of the Student-t
  conditional distribution of structural shocks.

## Methods

### Public methods

- [`specify_starting_values_bsvar_mix$new()`](#method-StartingValuesBSVARMIX-new)

- [`specify_starting_values_bsvar_mix$clone()`](#method-StartingValuesBSVARMIX-clone)

Inherited methods

- [`bsvars::StartingValuesBSVARMSH$get_starting_values()`](https://bsvars.org/bsvars/reference/StartingValuesBSVARMSH.html#method-get_starting_values)
- [`bsvars::StartingValuesBSVARMSH$set_starting_values()`](https://bsvars.org/bsvars/reference/StartingValuesBSVARMSH.html#method-set_starting_values)

------------------------------------------------------------------------

### Method `new()`

Create new starting values StartingValuesBSVARMIX.

#### Usage

    specify_starting_values_bsvar_mix$new(A, B, N, p, M, T, d = 0, finiteM = TRUE)

#### Arguments

- `A`:

  a logical `NxK` matrix containing value `TRUE` for the elements of the
  autoregressive matrix \\A\\ to be estimated and value `FALSE` for
  exclusion restrictions to be set to zero.

- `B`:

  a logical `NxN` matrix containing value `TRUE` for the elements of the
  staructural matrix \\B\\ to be estimated and value `FALSE` for
  exclusion restrictions to be set to zero.

- `N`:

  a positive integer - the number of dependent variables in the model.

- `p`:

  a positive integer - the autoregressive lag order of the SVAR model.

- `M`:

  an integer greater than 1 - the number of components of the mixture of
  normals.

- `T`:

  a positive integer - the the time series dimension of the dependent
  variable matrix \\Y\\.

- `d`:

  a positive integer - the number of `exogenous` variables in the model.

- `finiteM`:

  a logical value - if true a finite mixture model is estimated.
  Otherwise, a sparse mixture model is estimated in which `M=20` and the
  number of visited states is estimated.

#### Returns

Starting values StartingValuesBSVARMIX.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    specify_starting_values_bsvar_mix$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# starting values for a bsvar model for a 3-variable system
A = matrix(TRUE, 3, 4)
B = matrix(TRUE, 3, 3)
sv = specify_starting_values_bsvar_mix$new(A = A, B = B, N = 3, p = 1, M = 2, T = 100)
```
