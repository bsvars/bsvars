# R6 Class Representing IdentificationBSVARs

The class IdentificationBSVARs presents the identifying restrictions for
the bsvar models.

## Public fields

- `VB`:

  a list of `N` matrices determining the unrestricted elements of matrix
  \\B\\.

- `VA`:

  a list of `N` matrices determining the unrestricted elements of matrix
  \\A\\.

## Methods

### Public methods

- [`specify_identification_bsvars$new()`](#method-IdentificationBSVARs-new)

- [`specify_identification_bsvars$get_identification()`](#method-IdentificationBSVARs-get_identification)

- [`specify_identification_bsvars$set_identification()`](#method-IdentificationBSVARs-set_identification)

- [`specify_identification_bsvars$clone()`](#method-IdentificationBSVARs-clone)

------------------------------------------------------------------------

### Method `new()`

Create new identifying restrictions IdentificationBSVARs.

#### Usage

    specify_identification_bsvars$new(B, A, N, K)

#### Arguments

- `B`:

  a logical `NxN` matrix containing value `TRUE` for the elements of the
  structural matrix \\B\\ to be estimated and value `FALSE` for
  exclusion restrictions to be set to zero.

- `A`:

  a logical `NxK` matrix containing value `TRUE` for the elements of the
  autoregressive matrix \\A\\ to be estimated and value `FALSE` for
  exclusion restrictions to be set to zero.

- `N`:

  a positive integer - the number of dependent variables in the model.

- `K`:

  a positive integer - the number of parameters in a row of
  autoregressive matrix.

#### Returns

Identifying restrictions IdentificationBSVARs.

------------------------------------------------------------------------

### Method `get_identification()`

Returns the elements of the identification pattern IdentificationBSVARs
as a `list`.

#### Usage

    specify_identification_bsvars$get_identification()

#### Examples

    B    = matrix(c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE), 3, 3); B
    spec = specify_identification_bsvars$new(B = B, N = 3, K = 4)
    spec$get_identification()

------------------------------------------------------------------------

### Method `set_identification()`

Set new starting values StartingValuesBSVAR.

#### Usage

    specify_identification_bsvars$set_identification(B, A, N, K)

#### Arguments

- `B`:

  a logical `NxN` matrix containing value `TRUE` for the elements of the
  structural matrix \\B\\ to be estimated and value `FALSE` for
  exclusion restrictions to be set to zero.

- `A`:

  a logical `NxK` matrix containing value `TRUE` for the elements of the
  autoregressive matrix \\A\\ to be estimated and value `FALSE` for
  exclusion restrictions to be set to zero.

- `N`:

  a positive integer - the number of dependent variables in the model.

- `K`:

  a positive integer - the number of parameters in a row of
  autoregressive matrix.

#### Examples

    spec = specify_identification_bsvars$new(N = 3, K = 4) # specify a model with the default option
    B    = matrix(c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE), 3, 3); B
    spec$set_identification(B = B, N = 3, K = 4)  # modify an existing specification
    spec$get_identification()              # check the outcome

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    specify_identification_bsvars$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
specify_identification_bsvars$new(N = 3, K = 4) # recursive specification for a 3-variable system
#> <IdentificationBSVARs>
#>   Public:
#>     VA: list
#>     VB: list
#>     clone: function (deep = FALSE) 
#>     get_identification: function () 
#>     initialize: function (B, A, N, K) 
#>     set_identification: function (B, A, N, K) 

B = matrix(c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE), 3, 3); B
#>      [,1]  [,2]  [,3]
#> [1,] TRUE FALSE FALSE
#> [2,] TRUE FALSE  TRUE
#> [3,] TRUE  TRUE  TRUE
specify_identification_bsvars$new(B = B, N = 3, K = 4) # an alternative identification pattern
#> <IdentificationBSVARs>
#>   Public:
#>     VA: list
#>     VB: list
#>     clone: function (deep = FALSE) 
#>     get_identification: function () 
#>     initialize: function (B, A, N, K) 
#>     set_identification: function (B, A, N, K) 


## ------------------------------------------------
## Method `specify_identification_bsvars$get_identification`
## ------------------------------------------------

B    = matrix(c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE), 3, 3); B
#>      [,1]  [,2]  [,3]
#> [1,] TRUE FALSE FALSE
#> [2,] TRUE FALSE  TRUE
#> [3,] TRUE  TRUE  TRUE
spec = specify_identification_bsvars$new(B = B, N = 3, K = 4)
spec$get_identification()
#> $VB
#> $VB[[1]]
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> 
#> $VB[[2]]
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    0    0    1
#> 
#> $VB[[3]]
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    0    1    0
#> [3,]    0    0    1
#> 
#> 
#> $VA
#> $VA[[1]]
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0    0    0
#> [2,]    0    1    0    0
#> [3,]    0    0    1    0
#> [4,]    0    0    0    1
#> 
#> $VA[[2]]
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0    0    0
#> [2,]    0    1    0    0
#> [3,]    0    0    1    0
#> [4,]    0    0    0    1
#> 
#> $VA[[3]]
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0    0    0
#> [2,]    0    1    0    0
#> [3,]    0    0    1    0
#> [4,]    0    0    0    1
#> 
#> 


## ------------------------------------------------
## Method `specify_identification_bsvars$set_identification`
## ------------------------------------------------

spec = specify_identification_bsvars$new(N = 3, K = 4) # specify a model with the default option
B    = matrix(c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE), 3, 3); B
#>      [,1]  [,2]  [,3]
#> [1,] TRUE FALSE FALSE
#> [2,] TRUE FALSE  TRUE
#> [3,] TRUE  TRUE  TRUE
spec$set_identification(B = B, N = 3, K = 4)  # modify an existing specification
spec$get_identification()              # check the outcome
#> $VB
#> $VB[[1]]
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> 
#> $VB[[2]]
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    0    0    1
#> 
#> $VB[[3]]
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    0    1    0
#> [3,]    0    0    1
#> 
#> 
#> $VA
#> $VA[[1]]
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0    0    0
#> [2,]    0    1    0    0
#> [3,]    0    0    1    0
#> [4,]    0    0    0    1
#> 
#> $VA[[2]]
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0    0    0
#> [2,]    0    1    0    0
#> [3,]    0    0    1    0
#> [4,]    0    0    0    1
#> 
#> $VA[[3]]
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0    0    0
#> [2,]    0    1    0    0
#> [3,]    0    0    1    0
#> [4,]    0    0    0    1
#> 
#> 
```
