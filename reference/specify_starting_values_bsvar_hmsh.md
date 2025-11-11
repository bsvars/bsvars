# R6 Class Representing StartingValuesBSVAHMSH

The class StartingValuesBSVARHMSH presents starting values for the bsvar
model with Heterogeneous Markov Switching Heteroskedasticity.

## Super class

`bsvars::StartingValuesBSVAR` -\> `StartingValuesBSVARHMSH`

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

  an `MxMxN` array of starting values for the transition probability
  matrix of the Markov process. Its elements sum to 1 over the rows.

- `xi`:

  an `MxTxN` array of starting values for the Markov process indicator.
  Its columns are a chosen column of an identity matrix of order `M`.

- `pi_0`:

  an `MxN` matrix of starting values for state probability at time
  `t=0`. Its elements sum to 1 in columns.

- `lambda`:

  a `NxT` matrix of starting values for latent variables.

- `df`:

  an `Nx1` vector of positive numbers with starting values for the
  equation-specific degrees of freedom parameters of the Student-t
  conditional distribution of structural shocks.

## Methods

### Public methods

- [`specify_starting_values_bsvar_hmsh$new()`](#method-StartingValuesBSVARHMSH-new)

- [`specify_starting_values_bsvar_hmsh$get_starting_values()`](#method-StartingValuesBSVARHMSH-get_starting_values)

- [`specify_starting_values_bsvar_hmsh$set_starting_values()`](#method-StartingValuesBSVARHMSH-set_starting_values)

- [`specify_starting_values_bsvar_hmsh$clone()`](#method-StartingValuesBSVARHMSH-clone)

------------------------------------------------------------------------

### Method `new()`

Create new starting values StartingValuesBSVARHMSH.

#### Usage

    specify_starting_values_bsvar_hmsh$new(A, B, N, p, M, T, d = 0, finiteM = TRUE)

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

  an integer greater than 1 - the number of Markov process'
  heteroskedastic regimes.

- `T`:

  a positive integer - the the time series dimension of the dependent
  variable matrix \\Y\\.

- `d`:

  a positive integer - the number of `exogenous` variables in the model.

- `finiteM`:

  a logical value - if true a stationary Markov switching model is
  estimated. Otherwise, a sparse Markov switching model is estimated in
  which `M=20` and the number of visited states is estimated.

#### Returns

Starting values StartingValuesBSVARHMSH.

------------------------------------------------------------------------

### Method `get_starting_values()`

Returns the elements of the starting values StartingValuesBSVARHMSH as a
`list`.

#### Usage

    specify_starting_values_bsvar_hmsh$get_starting_values()

#### Examples

    # starting values for a homoskedastic bsvar with 1 lag for a 3-variable system
    A = matrix(TRUE, 3, 4)
    B = matrix(TRUE, 3, 3)
    sv = specify_starting_values_bsvar_hmsh$new(A = A, B = B, N = 3, p = 1, M = 2, T = 100)
    sv$get_starting_values()   # show starting values as list

------------------------------------------------------------------------

### Method `set_starting_values()`

Returns the elements of the starting values StartingValuesBSVARHMSH as a
`list`.

#### Usage

    specify_starting_values_bsvar_hmsh$set_starting_values(last_draw)

#### Arguments

- `last_draw`:

  a list containing the last draw.

#### Returns

An object of class StartingValuesBSVARHMSH including the last draw of
the current MCMC as the starting value to be passed to the continuation
of the MCMC estimation using
[`estimate()`](https://bsvars.org/bsvars/reference/estimate.md).

#### Examples

    # starting values for a bsvar model with 1 lag for a 3-variable system
    A = matrix(TRUE, 3, 4)
    B = matrix(TRUE, 3, 3)
    sv = specify_starting_values_bsvar_hmsh$new(A = A, B = B, N = 3, p = 1, M = 2, T = 100)

    # Modify the starting values by:
    sv_list = sv$get_starting_values()   # getting them as list
    sv_list$A <- matrix(rnorm(12), 3, 4) # modifying the entry
    sv$set_starting_values(sv_list)      # providing to the class object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    specify_starting_values_bsvar_hmsh$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# starting values for a bsvar model for a 3-variable system
A = matrix(TRUE, 3, 4)
B = matrix(TRUE, 3, 3)
sv = specify_starting_values_bsvar_hmsh$new(A = A, B = B, N = 3, p = 1, M = 2, T = 100)


## ------------------------------------------------
## Method `specify_starting_values_bsvar_hmsh$get_starting_values`
## ------------------------------------------------

# starting values for a homoskedastic bsvar with 1 lag for a 3-variable system
A = matrix(TRUE, 3, 4)
B = matrix(TRUE, 3, 3)
sv = specify_starting_values_bsvar_hmsh$new(A = A, B = B, N = 3, p = 1, M = 2, T = 100)
sv$get_starting_values()   # show starting values as list
#> $B
#>           [,1]      [,2]      [,3]
#> [1,] 0.3092308 0.0000000 0.0000000
#> [2,] 0.0000000 0.4222861 0.0000000
#> [3,] 0.0000000 0.0000000 0.3000771
#> 
#> $A
#>           [,1]     [,2]      [,3] [,4]
#> [1,] 0.6714213 0.000000 0.0000000    0
#> [2,] 0.0000000 0.389152 0.0000000    0
#> [3,] 0.0000000 0.000000 0.2289869    0
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
#>      [,99] [,100]
#> [1,]     1      1
#> [2,]     1      1
#> [3,]     1      1
#> 
#> $df
#> [1] 3 3 3
#> 
#> $sigma2
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    1    1
#> [3,]    1    1
#> 
#> $PR_TR
#> , , 1
#> 
#>      [,1] [,2]
#> [1,]    1    0
#> [2,]    0    1
#> 
#> , , 2
#> 
#>      [,1] [,2]
#> [1,]    1    0
#> [2,]    0    1
#> 
#> , , 3
#> 
#>      [,1] [,2]
#> [1,]    1    0
#> [2,]    0    1
#> 
#> 
#> $xi
#> , , 1
#> 
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
#> [1,]    0    1    0    0    1    1    0    0    0     1     1     0     1     0
#> [2,]    1    0    1    1    0    0    1    1    1     0     0     1     0     1
#>      [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25] [,26]
#> [1,]     0     1     0     0     1     0     1     0     1     1     1     0
#> [2,]     1     0     1     1     0     1     0     1     0     0     0     1
#>      [,27] [,28] [,29] [,30] [,31] [,32] [,33] [,34] [,35] [,36] [,37] [,38]
#> [1,]     1     1     0     1     0     0     1     0     1     1     0     0
#> [2,]     0     0     1     0     1     1     0     1     0     0     1     1
#>      [,39] [,40] [,41] [,42] [,43] [,44] [,45] [,46] [,47] [,48] [,49] [,50]
#> [1,]     1     0     0     0     1     0     0     0     1     1     0     0
#> [2,]     0     1     1     1     0     1     1     1     0     0     1     1
#>      [,51] [,52] [,53] [,54] [,55] [,56] [,57] [,58] [,59] [,60] [,61] [,62]
#> [1,]     1     0     0     0     0     0     0     1     0     1     0     1
#> [2,]     0     1     1     1     1     1     1     0     1     0     1     0
#>      [,63] [,64] [,65] [,66] [,67] [,68] [,69] [,70] [,71] [,72] [,73] [,74]
#> [1,]     1     0     0     0     0     1     1     1     1     1     0     0
#> [2,]     0     1     1     1     1     0     0     0     0     0     1     1
#>      [,75] [,76] [,77] [,78] [,79] [,80] [,81] [,82] [,83] [,84] [,85] [,86]
#> [1,]     0     1     1     0     1     1     0     1     0     1     1     1
#> [2,]     1     0     0     1     0     0     1     0     1     0     0     0
#>      [,87] [,88] [,89] [,90] [,91] [,92] [,93] [,94] [,95] [,96] [,97] [,98]
#> [1,]     0     1     0     1     1     0     1     1     0     1     0     0
#> [2,]     1     0     1     0     0     1     0     0     1     0     1     1
#>      [,99] [,100]
#> [1,]     1      0
#> [2,]     0      1
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
#> [1,]    1    1    0    0    0    0    0    0    0     0     1     1     1     1
#> [2,]    0    0    1    1    1    1    1    1    1     1     0     0     0     0
#>      [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25] [,26]
#> [1,]     1     0     0     1     1     0     0     1     0     0     0     0
#> [2,]     0     1     1     0     0     1     1     0     1     1     1     1
#>      [,27] [,28] [,29] [,30] [,31] [,32] [,33] [,34] [,35] [,36] [,37] [,38]
#> [1,]     0     0     1     0     0     0     1     0     0     0     0     0
#> [2,]     1     1     0     1     1     1     0     1     1     1     1     1
#>      [,39] [,40] [,41] [,42] [,43] [,44] [,45] [,46] [,47] [,48] [,49] [,50]
#> [1,]     1     0     0     0     0     0     0     1     1     0     0     1
#> [2,]     0     1     1     1     1     1     1     0     0     1     1     0
#>      [,51] [,52] [,53] [,54] [,55] [,56] [,57] [,58] [,59] [,60] [,61] [,62]
#> [1,]     0     0     0     1     1     0     0     0     0     0     1     0
#> [2,]     1     1     1     0     0     1     1     1     1     1     0     1
#>      [,63] [,64] [,65] [,66] [,67] [,68] [,69] [,70] [,71] [,72] [,73] [,74]
#> [1,]     0     1     0     1     1     0     1     0     1     0     1     0
#> [2,]     1     0     1     0     0     1     0     1     0     1     0     1
#>      [,75] [,76] [,77] [,78] [,79] [,80] [,81] [,82] [,83] [,84] [,85] [,86]
#> [1,]     0     1     0     1     0     1     1     1     0     1     1     0
#> [2,]     1     0     1     0     1     0     0     0     1     0     0     1
#>      [,87] [,88] [,89] [,90] [,91] [,92] [,93] [,94] [,95] [,96] [,97] [,98]
#> [1,]     1     0     0     1     1     0     1     1     1     0     0     1
#> [2,]     0     1     1     0     0     1     0     0     0     1     1     0
#>      [,99] [,100]
#> [1,]     1      0
#> [2,]     0      1
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
#> [1,]    1    1    0    1    1    0    1    0    0     1     1     1     0     0
#> [2,]    0    0    1    0    0    1    0    1    1     0     0     0     1     1
#>      [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25] [,26]
#> [1,]     0     1     0     1     0     0     0     0     1     1     1     1
#> [2,]     1     0     1     0     1     1     1     1     0     0     0     0
#>      [,27] [,28] [,29] [,30] [,31] [,32] [,33] [,34] [,35] [,36] [,37] [,38]
#> [1,]     0     1     1     1     0     1     0     0     0     1     0     0
#> [2,]     1     0     0     0     1     0     1     1     1     0     1     1
#>      [,39] [,40] [,41] [,42] [,43] [,44] [,45] [,46] [,47] [,48] [,49] [,50]
#> [1,]     1     0     1     0     1     1     0     0     1     1     1     1
#> [2,]     0     1     0     1     0     0     1     1     0     0     0     0
#>      [,51] [,52] [,53] [,54] [,55] [,56] [,57] [,58] [,59] [,60] [,61] [,62]
#> [1,]     1     0     0     1     0     0     1     0     0     1     0     0
#> [2,]     0     1     1     0     1     1     0     1     1     0     1     1
#>      [,63] [,64] [,65] [,66] [,67] [,68] [,69] [,70] [,71] [,72] [,73] [,74]
#> [1,]     0     1     1     0     1     1     0     1     0     1     0     1
#> [2,]     1     0     0     1     0     0     1     0     1     0     1     0
#>      [,75] [,76] [,77] [,78] [,79] [,80] [,81] [,82] [,83] [,84] [,85] [,86]
#> [1,]     0     1     0     0     0     0     1     0     1     1     0     1
#> [2,]     1     0     1     1     1     1     0     1     0     0     1     0
#>      [,87] [,88] [,89] [,90] [,91] [,92] [,93] [,94] [,95] [,96] [,97] [,98]
#> [1,]     1     0     1     0     0     0     1     1     1     0     0     1
#> [2,]     0     1     0     1     1     1     0     0     0     1     1     0
#>      [,99] [,100]
#> [1,]     0      0
#> [2,]     1      1
#> 
#> 
#> $pi_0
#>      [,1] [,2] [,3]
#> [1,]  0.5  0.5  0.5
#> [2,]  0.5  0.5  0.5
#> 


## ------------------------------------------------
## Method `specify_starting_values_bsvar_hmsh$set_starting_values`
## ------------------------------------------------

# starting values for a bsvar model with 1 lag for a 3-variable system
A = matrix(TRUE, 3, 4)
B = matrix(TRUE, 3, 3)
sv = specify_starting_values_bsvar_hmsh$new(A = A, B = B, N = 3, p = 1, M = 2, T = 100)

# Modify the starting values by:
sv_list = sv$get_starting_values()   # getting them as list
sv_list$A <- matrix(rnorm(12), 3, 4) # modifying the entry
sv$set_starting_values(sv_list)      # providing to the class object
```
