# R6 Class Representing StartingValuesBSVARSV

The class StartingValuesBSVARSV presents starting values for the bsvar
model with Stochastic Volatility heteroskedasticity.

## Super class

`bsvars::StartingValuesBSVAR` -\> `StartingValuesBSVARSV`

## Public fields

- `A`:

  an `NxK` matrix of starting values for the parameter \\A\\.

- `B`:

  an `NxN` matrix of starting values for the parameter \\B\\.

- `hyper`:

  a `(2*N+1)x2` matrix of starting values for the shrinkage
  hyper-parameters of the hierarchical prior distribution.

- `h`:

  an `NxT` matrix with the starting values of the log-volatility
  processes.

- `rho`:

  an `N`-vector with values of SV autoregressive parameters.

- `omega`:

  an `N`-vector with values of SV process conditional standard
  deviations.

- `sigma2v`:

  an `N`-vector with values of SV process conditional variances.

- `S`:

  an `NxT` integer matrix with the auxiliary mixture component
  indicators.

- `sigma2_omega`:

  an `N`-vector with variances of the zero-mean normal prior for
  \\\omega_n\\.

- `s_`:

  a positive scalar with the scale of the gamma prior of the
  hierarchical prior for \\\sigma^2\_{\omega}\\.

- `lambda`:

  a `NxT` matrix of starting values for latent variables.

- `df`:

  an `Nx1` vector of positive numbers with starting values for the
  equation-specific degrees of freedom parameters of the Student-t
  conditional distribution of structural shocks.

## Methods

### Public methods

- [`specify_starting_values_bsvar_sv$new()`](#method-StartingValuesBSVARSV-new)

- [`specify_starting_values_bsvar_sv$get_starting_values()`](#method-StartingValuesBSVARSV-get_starting_values)

- [`specify_starting_values_bsvar_sv$set_starting_values()`](#method-StartingValuesBSVARSV-set_starting_values)

- [`specify_starting_values_bsvar_sv$clone()`](#method-StartingValuesBSVARSV-clone)

------------------------------------------------------------------------

### Method `new()`

Create new starting values StartingValuesBSVARSV.

#### Usage

    specify_starting_values_bsvar_sv$new(A, B, N, p, T, d = 0)

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

- `T`:

  a positive integer - the the time series dimension of the dependent
  variable matrix \\Y\\.

- `d`:

  a positive integer - the number of `exogenous` variables in the model.

#### Returns

Starting values StartingValuesBSVARSV.

------------------------------------------------------------------------

### Method `get_starting_values()`

Returns the elements of the starting values StartingValuesBSVARSV as a
`list`.

#### Usage

    specify_starting_values_bsvar_sv$get_starting_values()

#### Examples

    # starting values for a bsvar model with 1 lag for a 3-variable system
    A = matrix(TRUE, 3, 4)
    B = matrix(TRUE, 3, 3)
    sv = specify_starting_values_bsvar_sv$new(A = A, B = B, N = 3, p = 1, T = 100)
    sv$get_starting_values()   # show starting values as list

------------------------------------------------------------------------

### Method `set_starting_values()`

Returns the elements of the starting values StartingValuesBSVAR_SV as a
`list`.

#### Usage

    specify_starting_values_bsvar_sv$set_starting_values(last_draw)

#### Arguments

- `last_draw`:

  a list containing the last draw of the current MCMC run.

#### Returns

An object of class StartingValuesBSVAR including the last draw of the
current MCMC as the starting value to be passed to the continuation of
the MCMC estimation using
[`estimate()`](https://bsvars.org/bsvars/reference/estimate.md).

#### Examples

    # starting values for a bsvar model with 1 lag for a 3-variable system
    A = matrix(TRUE, 3, 4)
    B = matrix(TRUE, 3, 3)
    sv = specify_starting_values_bsvar_sv$new(A = A, B = B, N = 3, p = 1, T = 100)

    # Modify the starting values by:
    sv_list = sv$get_starting_values()   # getting them as list
    sv_list$A <- matrix(rnorm(12), 3, 4) # modifying the entry
    sv$set_starting_values(sv_list)      # providing to the class object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    specify_starting_values_bsvar_sv$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# starting values for a bsvar model for a 3-variable system
A = matrix(TRUE, 3, 4)
B = matrix(TRUE, 3, 3)
sv = specify_starting_values_bsvar_sv$new(A = A, B = B, N = 3, p = 1, T = 100)


## ------------------------------------------------
## Method `specify_starting_values_bsvar_sv$get_starting_values`
## ------------------------------------------------

# starting values for a bsvar model with 1 lag for a 3-variable system
A = matrix(TRUE, 3, 4)
B = matrix(TRUE, 3, 3)
sv = specify_starting_values_bsvar_sv$new(A = A, B = B, N = 3, p = 1, T = 100)
sv$get_starting_values()   # show starting values as list
#> $B
#>           [,1]      [,2]      [,3]
#> [1,] 0.9451146 0.0000000 0.0000000
#> [2,] 0.0000000 0.4026784 0.0000000
#> [3,] 0.0000000 0.0000000 0.8173679
#> 
#> $A
#>          [,1]      [,2]      [,3] [,4]
#> [1,] 0.514014 0.0000000 0.0000000    0
#> [2,] 0.000000 0.1022952 0.0000000    0
#> [3,] 0.000000 0.0000000 0.5184321    0
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
#> $h
#>              [,1]         [,2]         [,3]          [,4]          [,5]
#> [1,] -0.003136211 0.0004486727 -0.007849919 -0.0027648461  0.0010699068
#> [2,]  0.006593895 0.0011708094  0.007669743 -0.0004979153 -0.0048142218
#> [3,] -0.005633859 0.0163056370 -0.005934213  0.0031216286 -0.0001270987
#>              [,6]         [,7]          [,8]         [,9]       [,10]
#> [1,]  0.002531345 -0.002208731  0.0005200123 -0.013192285 -0.01104889
#> [2,]  0.004180498  0.018015934  0.0099572629  0.011701875  0.01477725
#> [3,] -0.013416174 -0.005682184 -0.0121992061  0.006283033  0.01007955
#>            [,11]         [,12]        [,13]         [,14]        [,15]
#> [1,]  0.01749528  0.0009816162  0.007387407  0.0001268182  0.004924303
#> [2,] -0.01209617 -0.0191261665  0.007671694 -0.0186405224  0.017470740
#> [3,]  0.01041710 -0.0084539360 -0.014745555  0.0088529813 -0.002619993
#>             [,16]       [,17]       [,18]        [,19]       [,20]       [,21]
#> [1,]  0.004997546 0.009807005 0.004566625  0.001893418 -0.01318152 0.006001131
#> [2,] -0.006888988 0.009233165 0.003169418 -0.013194094  0.01246549 0.007982218
#> [3,] -0.009825800 0.003306873 0.004091469 -0.020012100 -0.01119783 0.001838808
#>            [,22]         [,23]       [,24]        [,25]        [,26]
#> [1,] 0.003238030 -1.953124e-05 0.009381559 -0.013512000  0.005053471
#> [2,] 0.006504944 -3.088218e-03 0.001026791 -0.004557810  0.006540223
#> [3,] 0.015829795 -6.231227e-03 0.010879801 -0.002508356 -0.002153623
#>             [,27]         [,28]        [,29]        [,30]        [,31]
#> [1,]  0.002267423 -0.0025862055 -0.009931054 -0.003188478  0.010685086
#> [2,] -0.011573391 -0.0009818012  0.008689231  0.001248408 -0.004821798
#> [3,]  0.020328003 -0.0035316125 -0.018163843 -0.014689920  0.006596155
#>              [,32]        [,33]         [,34]       [,35]        [,36]
#> [1,] -0.0181763579 -0.013506908 -0.0009461619 0.006702862 -0.004165526
#> [2,] -0.0027858312  0.009194744  0.0017830377 0.001821383  0.008743172
#> [3,]  0.0006773525  0.002725277 -0.0005391045 0.015386113  0.001409836
#>             [,37]        [,38]         [,39]       [,40]        [,41]
#> [1,] 0.0216178727 -0.003526023 -0.0013603668 0.006156816 -0.021795508
#> [2,] 0.0002761923 -0.020868780  0.0009219266 0.013618260  0.008098478
#> [3,] 0.0012834286 -0.019738655 -0.0069026356 0.017131362  0.009466466
#>             [,42]        [,43]        [,44]         [,45]        [,46]
#> [1,]  0.006929014 -0.002879527  0.008928262 -0.0069941525  0.005381417
#> [2,]  0.011278569  0.010193279 -0.008016859  0.0088278759  0.023760439
#> [3,] -0.001473972 -0.006766171 -0.005871781 -0.0005988829 -0.009941079
#>             [,47]       [,48]        [,49]        [,50]       [,51]
#> [1,] 0.0132718965 -0.01181221 -0.007646915  0.004402012 0.021767122
#> [2,] 0.0062375965  0.02845465  0.011259196 -0.015665725 0.001983127
#> [3,] 0.0001541559 -0.01658575 -0.015538693  0.004531512 0.004812077
#>             [,52]        [,53]        [,54]         [,55]        [,56]
#> [1,]  0.007265324 -0.005312849 -0.000757480 -0.0005681243 -0.001658012
#> [2,] -0.007958980  0.003566891  0.007995912  0.0017473900 -0.012049237
#> [3,] -0.009604183 -0.010722186 -0.005480709 -0.0161110951 -0.002541748
#>             [,57]       [,58]         [,59]         [,60]        [,61]
#> [1,]  0.005061513 -0.01203853 -0.0110986784 -0.0062909926 -0.012843229
#> [2,]  0.006269690  0.01777208 -0.0032484166 -0.0135334551 -0.004979266
#> [3,] -0.008894353 -0.01918459 -0.0007271959 -0.0009391309  0.004225503
#>            [,62]        [,63]        [,64]         [,65]        [,66]
#> [1,] 0.010301895 -0.002754462  0.006852824  0.0002006718 -0.021436208
#> [2,] 0.011010950 -0.017559854 -0.022864094  0.0191413994  0.008243604
#> [3,] 0.009100311 -0.015618643  0.007416705 -0.0001065326  0.010943847
#>             [,67]         [,68]      [,69]        [,70]       [,71]
#> [1,] -0.004102339  0.0025544231 0.01983125 -0.004042825 0.007212838
#> [2,] -0.002949917 -0.0001434963 0.01098606 -0.004116793 0.017835047
#> [3,]  0.004039416  0.0005525418 0.02047589 -0.003102681 0.004226926
#>             [,72]        [,73]        [,74]        [,75]        [,76]
#> [1,] -0.003179447 -0.001905920 -0.004198992  0.004813065 -0.002355130
#> [2,]  0.003082526 -0.009693720 -0.010162900 -0.001580232  0.009881257
#> [3,] -0.003358239  0.001425795  0.000116297 -0.002532018  0.007250623
#>              [,77]        [,78]       [,79]        [,80]        [,81]
#> [1,] -0.0002633796 -0.012683966 0.014879268 -0.004650096 -0.001995618
#> [2,] -0.0090847470  0.010971446 0.010912767 -0.001174730  0.011632631
#> [3,]  0.0005193351  0.003698361 0.004591722 -0.011910524  0.001678959
#>             [,82]        [,83]         [,84]        [,85]         [,86]
#> [1,]  0.001615220  0.005267630  0.0110759013 -0.003472572  0.0262951179
#> [2,] -0.004342447 -0.008503478 -0.0026044028  0.012857180 -0.0197724580
#> [3,]  0.013247524  0.002884781  0.0004022606  0.001187416 -0.0004592378
#>             [,87]         [,88]        [,89]        [,90]         [,91]
#> [1,]  0.008905162 -0.0001113261 -0.002260265  0.014887659 -0.0001089553
#> [2,] -0.002577318 -0.0103429070  0.001064343 -0.006860265 -0.0115305303
#> [3,]  0.020668447 -0.0043972133  0.001614253 -0.004623534  0.0020813177
#>             [,92]        [,93]        [,94]        [,95]        [,96]
#> [1,]  0.002823116 -0.000692910 -0.005643253 -0.007706044  0.022556903
#> [2,] -0.012008094 -0.006799170 -0.000786210  0.009210972 -0.003168277
#> [3,] -0.012474076  0.007212092  0.004421909  0.003053614 -0.019712965
#>             [,97]        [,98]         [,99]       [,100]
#> [1,] 1.171932e-02 -0.009663941 -0.0020306976  0.019873206
#> [2,] 9.805043e-05  0.026389645  0.0111578512 -0.005958237
#> [3,] 1.605414e-02  0.014163289 -0.0007844158  0.001700496
#> 
#> $rho
#> [1] 0.5 0.5 0.5
#> 
#> $omega
#> [1] 0.1 0.1 0.1
#> 
#> $sigma2v
#> [1] 0.01 0.01 0.01
#> 
#> $S
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
#> $sigma2_omega
#> [1] 1 1 1
#> 
#> $s_
#> [1] 0.05 0.05 0.05
#> 


## ------------------------------------------------
## Method `specify_starting_values_bsvar_sv$set_starting_values`
## ------------------------------------------------

# starting values for a bsvar model with 1 lag for a 3-variable system
A = matrix(TRUE, 3, 4)
B = matrix(TRUE, 3, 3)
sv = specify_starting_values_bsvar_sv$new(A = A, B = B, N = 3, p = 1, T = 100)

# Modify the starting values by:
sv_list = sv$get_starting_values()   # getting them as list
sv_list$A <- matrix(rnorm(12), 3, 4) # modifying the entry
sv$set_starting_values(sv_list)      # providing to the class object
```
