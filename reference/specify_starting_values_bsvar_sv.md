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
#>            [,1]      [,2]      [,3]
#> [1,] 0.07313893 0.0000000 0.0000000
#> [2,] 0.00000000 0.8767546 0.0000000
#> [3,] 0.00000000 0.0000000 0.9229325
#> 
#> $A
#>           [,1]      [,2]       [,3] [,4]
#> [1,] 0.9240939 0.0000000 0.00000000    0
#> [2,] 0.0000000 0.1528961 0.00000000    0
#> [3,] 0.0000000 0.0000000 0.05094829    0
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
#>              [,1]          [,2]         [,3]         [,4]          [,5]
#> [1,] -0.001868055  0.0013964253  0.028561309 0.0009707993  0.0020285432
#> [2,] -0.012110649 -0.0004271323 -0.006564740 0.0017098212 -0.0007878937
#> [3,]  0.015285587  0.0043683994  0.004478331 0.0111503425  0.0031562345
#>              [,6]         [,7]         [,8]         [,9]       [,10]
#> [1,] -0.007137466 -0.007965252  0.004923450 -0.010295373 0.013747950
#> [2,]  0.018750151 -0.018882792 -0.014065694  0.001314308 0.018616425
#> [3,]  0.018597520  0.019516963 -0.008108645 -0.002753414 0.009095813
#>              [,11]        [,12]        [,13]        [,14]         [,15]
#> [1,] -0.0002122446 -0.006496985 -0.007753197  0.011342384 -0.0006390322
#> [2,]  0.0111766912  0.000244211  0.003712137 -0.003512995  0.0136192184
#> [3,] -0.0074761247 -0.001855075 -0.006249434 -0.003609300  0.0061107487
#>             [,16]         [,17]        [,18]        [,19]        [,20]
#> [1,]  0.003642131  0.0114673194 -0.005881579 -0.019974391 0.0026959244
#> [2,]  0.010250788  0.0000875398 -0.003656503 -0.003469134 0.0001818889
#> [3,] -0.010842649 -0.0144350388  0.015196378 -0.009648888 0.0105559186
#>            [,21]        [,22]        [,23]        [,24]        [,25]
#> [1,] 0.009910495 -0.010816867 -0.005830639  0.008144417 -0.010969546
#> [2,] 0.007126701 -0.004925338 -0.012492405 -0.005166661 -0.012554751
#> [3,] 0.017956447  0.009645974  0.003500025 -0.026922644 -0.006372108
#>             [,26]        [,27]       [,28]         [,29]        [,30]
#> [1,] -0.003909162 -0.006110093 -0.01491585  0.0003414736 -0.025848955
#> [2,]  0.009944220 -0.007388167 -0.01545728 -0.0058249245  0.002712292
#> [3,] -0.014230197  0.014290716  0.01039556 -0.0072136774 -0.001419930
#>             [,31]        [,32]       [,33]        [,34]       [,35]
#> [1,]  0.011039173  0.002524558 0.009917127 -0.000557616 0.007547433
#> [2,] -0.004760957  0.007410500 0.007829877 -0.010848367 0.008815685
#> [3,]  0.010090495 -0.017959741 0.013135519 -0.001419747 0.013296579
#>             [,36]         [,37]        [,38]       [,39]        [,40]
#> [1,] -0.000663639 -0.0267943701 -0.013040325 -0.00835431  0.003198381
#> [2,]  0.015785981  0.0022881353  0.001378863  0.01399265 -0.004568556
#> [3,] -0.007884133 -0.0003547721 -0.012360968 -0.03058123 -0.006221569
#>             [,41]        [,42]        [,43]         [,44]        [,45]
#> [1,]  0.001306136 -0.013873720 -0.012685813 -0.0056338590  0.016305637
#> [2,] -0.004827002  0.015992239 -0.003136211  0.0004486727 -0.007849919
#> [3,] -0.005339347  0.009053797  0.006593895  0.0011708094  0.007669743
#>              [,46]        [,47]         [,48]        [,49]         [,50]
#> [1,] -0.0059342125  0.003121629 -0.0001270987 -0.013416174 -0.0056821836
#> [2,] -0.0027648461  0.001069907  0.0025313452 -0.002208731  0.0005200123
#> [3,] -0.0004979153 -0.004814222  0.0041804983  0.018015934  0.0099572629
#>            [,51]        [,52]       [,53]         [,54]        [,55]
#> [1,] -0.01219921  0.006283033  0.01007955  0.0104171040 -0.008453936
#> [2,] -0.01319228 -0.011048891  0.01749528  0.0009816162  0.007387407
#> [3,]  0.01170188  0.014777247 -0.01209617 -0.0191261665  0.007671694
#>              [,56]       [,57]        [,58]        [,59]       [,60]
#> [1,] -0.0147455554 0.008852981 -0.002619993 -0.009825800 0.003306873
#> [2,]  0.0001268182 0.004924303  0.004997546  0.009807005 0.004566625
#> [3,] -0.0186405224 0.017470740 -0.006888988  0.009233165 0.003169418
#>             [,61]       [,62]        [,63]       [,64]         [,65]
#> [1,]  0.004091469 -0.02001210 -0.011197829 0.001838808  1.582980e-02
#> [2,]  0.001893418 -0.01318152  0.006001131 0.003238030 -1.953124e-05
#> [3,] -0.013194094  0.01246549  0.007982218 0.006504944 -3.088218e-03
#>             [,66]       [,67]        [,68]        [,69]         [,70]
#> [1,] -0.006231227  0.01087980 -0.002508356 -0.002153623  0.0203280028
#> [2,]  0.009381559 -0.01351200  0.005053471  0.002267423 -0.0025862055
#> [3,]  0.001026791 -0.00455781  0.006540223 -0.011573391 -0.0009818012
#>             [,71]        [,72]        [,73]        [,74]         [,75]
#> [1,] -0.003531613 -0.018163843 -0.014689920  0.006596155  0.0006773525
#> [2,] -0.009931054 -0.003188478  0.010685086 -0.018176358 -0.0135069077
#> [3,]  0.008689231  0.001248408 -0.004821798 -0.002785831  0.0091947436
#>              [,76]         [,77]        [,78]        [,79]        [,80]
#> [1,]  0.0027252774 -0.0005391045  0.015386113 0.0014098364  0.001283429
#> [2,] -0.0009461619  0.0067028624 -0.004165526 0.0216178727 -0.003526023
#> [3,]  0.0017830377  0.0018213833  0.008743172 0.0002761923 -0.020868780
#>              [,81]        [,82]        [,83]       [,84]        [,85]
#> [1,] -0.0197386551 -0.006902636  0.017131362 0.009466466 -0.001473972
#> [2,] -0.0013603668  0.006156816 -0.021795508 0.006929014 -0.002879527
#> [3,]  0.0009219266  0.013618260  0.008098478 0.011278569  0.010193279
#>             [,86]        [,87]         [,88]        [,89]         [,90]
#> [1,] -0.006766171 -0.005871781 -0.0005988829 -0.009941079  0.0001541559
#> [2,]  0.008928262 -0.006994153  0.0053814173  0.013271896 -0.0118122099
#> [3,] -0.008016859  0.008827876  0.0237604385  0.006237596  0.0284546509
#>             [,91]        [,92]       [,93]        [,94]        [,95]
#> [1,] -0.016585747 -0.015538693 0.004531512  0.004812077 -0.009604183
#> [2,] -0.007646915  0.004402012 0.021767122  0.007265324 -0.005312849
#> [3,]  0.011259196 -0.015665725 0.001983127 -0.007958980  0.003566891
#>             [,96]         [,97]        [,98]        [,99]       [,100]
#> [1,] -0.010722186 -0.0054807088 -0.016111095 -0.002541748 -0.008894353
#> [2,] -0.000757480 -0.0005681243 -0.001658012  0.005061513 -0.012038525
#> [3,]  0.007995912  0.0017473900 -0.012049237  0.006269690  0.017772076
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
