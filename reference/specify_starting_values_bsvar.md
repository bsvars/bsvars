# R6 Class Representing StartingValuesBSVAR

The class StartingValuesBSVAR presents starting values for the
homoskedastic bsvar model.

## Public fields

- `A`:

  an `NxK` matrix of starting values for the parameter \\A\\.

- `B`:

  an `NxN` matrix of starting values for the parameter \\B\\.

- `hyper`:

  a `(2*N+1)x2` matrix of starting values for the shrinkage
  hyper-parameters of the hierarchical prior distribution.

- `lambda`:

  a `NxT` matrix of starting values for latent variables.

- `df`:

  an `Nx1` vector of positive numbers with starting values for the
  equation-specific degrees of freedom parameters of the Student-t
  conditional distribution of structural shocks.

## Methods

### Public methods

- [`specify_starting_values_bsvar$new()`](#method-StartingValuesBSVAR-new)

- [`specify_starting_values_bsvar$get_starting_values()`](#method-StartingValuesBSVAR-get_starting_values)

- [`specify_starting_values_bsvar$set_starting_values()`](#method-StartingValuesBSVAR-set_starting_values)

- [`specify_starting_values_bsvar$clone()`](#method-StartingValuesBSVAR-clone)

------------------------------------------------------------------------

### Method `new()`

Create new starting values StartingValuesBSVAR.

#### Usage

    specify_starting_values_bsvar$new(A, B, N, T, p, d = 0)

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

- `T`:

  a positive integer - the number of time periods in the data.

- `p`:

  a positive integer - the autoregressive lag order of the SVAR model.

- `d`:

  a positive integer - the number of `exogenous` variables in the model.

#### Returns

Starting values StartingValuesBSVAR.

#### Examples

    # starting values for a homoskedastic bsvar with 4 lags for a 3-variable system
    A = matrix(TRUE, 3, 13)
    B = matrix(TRUE, 3, 3)
    sv = specify_starting_values_bsvar$new(A = A, B = B, N = 3, T = 120, p = 4)

------------------------------------------------------------------------

### Method `get_starting_values()`

Returns the elements of the starting values StartingValuesBSVAR as a
`list`.

#### Usage

    specify_starting_values_bsvar$get_starting_values()

#### Examples

    # starting values for a homoskedastic bsvar with 1 lag for a 3-variable system
    A = matrix(TRUE, 3, 4)
    B = matrix(TRUE, 3, 3)
    sv = specify_starting_values_bsvar$new(A = A, B = B, N = 3, T = 120, p = 1)
    sv$get_starting_values()   # show starting values as list

------------------------------------------------------------------------

### Method `set_starting_values()`

Returns the elements of the starting values StartingValuesBSVAR as a
`list`.

#### Usage

    specify_starting_values_bsvar$set_starting_values(last_draw)

#### Arguments

- `last_draw`:

  a list containing the last draw of elements `B` - an `NxN` matrix,
  `A` - an `NxK` matrix, and `hyper` - a vector of 5 positive real
  numbers.

#### Returns

An object of class StartingValuesBSVAR including the last draw of the
current MCMC as the starting value to be passed to the continuation of
the MCMC estimation using
[`estimate()`](https://bsvars.org/bsvars/reference/estimate.md).

#### Examples

    # starting values for a homoskedastic bsvar with 1 lag for a 3-variable system
    A = matrix(TRUE, 3, 4)
    B = matrix(TRUE, 3, 3)
    sv = specify_starting_values_bsvar$new(A = A, B = B, N = 3, T = 120, p = 1)

    # Modify the starting values by:
    sv_list = sv$get_starting_values()   # getting them as list
    sv_list$A <- matrix(rnorm(12), 3, 4) # modifying the entry
    sv$set_starting_values(sv_list)      # providing to the class object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    specify_starting_values_bsvar$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# starting values for a homoskedastic bsvar for a 3-variable system
A = matrix(TRUE, 3, 4)
B = matrix(TRUE, 3, 3)
sv = specify_starting_values_bsvar$new(A = A, B = B, N = 3, T = 120, p = 1)


## ------------------------------------------------
## Method `specify_starting_values_bsvar$new`
## ------------------------------------------------

# starting values for a homoskedastic bsvar with 4 lags for a 3-variable system
A = matrix(TRUE, 3, 13)
B = matrix(TRUE, 3, 3)
sv = specify_starting_values_bsvar$new(A = A, B = B, N = 3, T = 120, p = 4)


## ------------------------------------------------
## Method `specify_starting_values_bsvar$get_starting_values`
## ------------------------------------------------

# starting values for a homoskedastic bsvar with 1 lag for a 3-variable system
A = matrix(TRUE, 3, 4)
B = matrix(TRUE, 3, 3)
sv = specify_starting_values_bsvar$new(A = A, B = B, N = 3, T = 120, p = 1)
sv$get_starting_values()   # show starting values as list
#> $B
#>            [,1]      [,2]      [,3]
#> [1,] 0.07788095 0.0000000 0.0000000
#> [2,] 0.00000000 0.3449137 0.0000000
#> [3,] 0.00000000 0.0000000 0.0876102
#> 
#> $A
#>           [,1]      [,2]      [,3] [,4]
#> [1,] 0.1795321 0.0000000 0.0000000    0
#> [2,] 0.0000000 0.4760595 0.0000000    0
#> [3,] 0.0000000 0.0000000 0.4901922    0
#> 
#> $hyper
#>      [,1] [,2]
#> [1,]   10   10
#> [2,]   10   10
#> [3,]   10   10
#> [4,]   10   10
#> [5,]   10   10
#> [6,]   10   10
#> [7,]   10   10
#> 
#> $lambda
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
#> [1,]    1    1    1    1    1    1    1    1    1     1     1     1     1     1
#> [2,]    1    1    1    1    1    1    1    1    1     1     1     1     1     1
#> [3,]    1    1    1    1    1    1    1    1    1     1     1     1     1     1
#>      [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25] [,26]
#> [1,]     1     1     1     1     1     1     1     1     1     1     1     1
#> [2,]     1     1     1     1     1     1     1     1     1     1     1     1
#> [3,]     1     1     1     1     1     1     1     1     1     1     1     1
#>      [,27] [,28] [,29] [,30] [,31] [,32] [,33] [,34] [,35] [,36] [,37] [,38]
#> [1,]     1     1     1     1     1     1     1     1     1     1     1     1
#> [2,]     1     1     1     1     1     1     1     1     1     1     1     1
#> [3,]     1     1     1     1     1     1     1     1     1     1     1     1
#>      [,39] [,40] [,41] [,42] [,43] [,44] [,45] [,46] [,47] [,48] [,49] [,50]
#> [1,]     1     1     1     1     1     1     1     1     1     1     1     1
#> [2,]     1     1     1     1     1     1     1     1     1     1     1     1
#> [3,]     1     1     1     1     1     1     1     1     1     1     1     1
#>      [,51] [,52] [,53] [,54] [,55] [,56] [,57] [,58] [,59] [,60] [,61] [,62]
#> [1,]     1     1     1     1     1     1     1     1     1     1     1     1
#> [2,]     1     1     1     1     1     1     1     1     1     1     1     1
#> [3,]     1     1     1     1     1     1     1     1     1     1     1     1
#>      [,63] [,64] [,65] [,66] [,67] [,68] [,69] [,70] [,71] [,72] [,73] [,74]
#> [1,]     1     1     1     1     1     1     1     1     1     1     1     1
#> [2,]     1     1     1     1     1     1     1     1     1     1     1     1
#> [3,]     1     1     1     1     1     1     1     1     1     1     1     1
#>      [,75] [,76] [,77] [,78] [,79] [,80] [,81] [,82] [,83] [,84] [,85] [,86]
#> [1,]     1     1     1     1     1     1     1     1     1     1     1     1
#> [2,]     1     1     1     1     1     1     1     1     1     1     1     1
#> [3,]     1     1     1     1     1     1     1     1     1     1     1     1
#>      [,87] [,88] [,89] [,90] [,91] [,92] [,93] [,94] [,95] [,96] [,97] [,98]
#> [1,]     1     1     1     1     1     1     1     1     1     1     1     1
#> [2,]     1     1     1     1     1     1     1     1     1     1     1     1
#> [3,]     1     1     1     1     1     1     1     1     1     1     1     1
#>      [,99] [,100] [,101] [,102] [,103] [,104] [,105] [,106] [,107] [,108]
#> [1,]     1      1      1      1      1      1      1      1      1      1
#> [2,]     1      1      1      1      1      1      1      1      1      1
#> [3,]     1      1      1      1      1      1      1      1      1      1
#>      [,109] [,110] [,111] [,112] [,113] [,114] [,115] [,116] [,117] [,118]
#> [1,]      1      1      1      1      1      1      1      1      1      1
#> [2,]      1      1      1      1      1      1      1      1      1      1
#> [3,]      1      1      1      1      1      1      1      1      1      1
#>      [,119] [,120]
#> [1,]      1      1
#> [2,]      1      1
#> [3,]      1      1
#> 
#> $df
#> [1] 3 3 3
#> 


## ------------------------------------------------
## Method `specify_starting_values_bsvar$set_starting_values`
## ------------------------------------------------

# starting values for a homoskedastic bsvar with 1 lag for a 3-variable system
A = matrix(TRUE, 3, 4)
B = matrix(TRUE, 3, 3)
sv = specify_starting_values_bsvar$new(A = A, B = B, N = 3, T = 120, p = 1)

# Modify the starting values by:
sv_list = sv$get_starting_values()   # getting them as list
sv_list$A <- matrix(rnorm(12), 3, 4) # modifying the entry
sv$set_starting_values(sv_list)      # providing to the class object
```
