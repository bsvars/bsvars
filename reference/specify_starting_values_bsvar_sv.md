# R6 Class Representing StartingValuesBSVARSV

The class StartingValuesBSVARSV presents starting values for the bsvar
model with Stochastic Volatility heteroskedasticity.

## Super class

`StartingValuesBSVAR` -\> `StartingValuesBSVARSV`

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

- [`StartingValuesBSVARSV$new()`](#method-StartingValuesBSVARSV-initialize)

- [`StartingValuesBSVARSV$get_starting_values()`](#method-StartingValuesBSVARSV-get_starting_values)

- [`StartingValuesBSVARSV$set_starting_values()`](#method-StartingValuesBSVARSV-set_starting_values)

- [`StartingValuesBSVARSV$clone()`](#method-StartingValuesBSVARSV-clone)

------------------------------------------------------------------------

### `StartingValuesBSVARSV$new()`

Create new starting values StartingValuesBSVARSV.

#### Usage

    StartingValuesBSVARSV$new(A, B, N, p, T, d = 0)

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

### `StartingValuesBSVARSV$get_starting_values()`

Returns the elements of the starting values StartingValuesBSVARSV as a
`list`.

#### Usage

    StartingValuesBSVARSV$get_starting_values()

#### Examples

    # starting values for a bsvar model with 1 lag for a 3-variable system
    A = matrix(TRUE, 3, 4)
    B = matrix(TRUE, 3, 3)
    sv = specify_starting_values_bsvar_sv$new(A = A, B = B, N = 3, p = 1, T = 100)
    sv$get_starting_values()   # show starting values as list

------------------------------------------------------------------------

### `StartingValuesBSVARSV$set_starting_values()`

Returns the elements of the starting values StartingValuesBSVAR_SV as a
`list`.

#### Usage

    StartingValuesBSVARSV$set_starting_values(last_draw)

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

### `StartingValuesBSVARSV$clone()`

The objects of this class are cloneable with this method.

#### Usage

    StartingValuesBSVARSV$clone(deep = FALSE)

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
## Method `StartingValuesBSVARSV$get_starting_values()`
## ------------------------------------------------

# starting values for a bsvar model with 1 lag for a 3-variable system
A = matrix(TRUE, 3, 4)
B = matrix(TRUE, 3, 3)
sv = specify_starting_values_bsvar_sv$new(A = A, B = B, N = 3, p = 1, T = 100)
sv$get_starting_values()   # show starting values as list
#> $B
#>           [,1]      [,2]       [,3]
#> [1,] 0.1253368 0.0000000 0.00000000
#> [2,] 0.0000000 0.6368316 0.00000000
#> [3,] 0.0000000 0.0000000 0.09213944
#> 
#> $A
#>          [,1]      [,2]      [,3] [,4]
#> [1,] 0.792304 0.0000000 0.0000000    0
#> [2,] 0.000000 0.9423444 0.0000000    0
#> [3,] 0.000000 0.0000000 0.3026946    0
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
#>             [,1]         [,2]         [,3]          [,4]          [,5]
#> [1,] 0.001586002 -0.001959584  0.006973567 -0.0027799106  0.0008071335
#> [2,] 0.002468006  0.008060773 -0.014658319 -0.0007690625  0.0030741878
#> [3,] 0.008551892 -0.002688575 -0.009065460 -0.0109279029 -0.0006604455
#>              [,6]          [,7]         [,8]         [,9]        [,10]
#> [1,] -0.025988322 -0.0004931417  0.010535522 -0.002313793  0.013683515
#> [2,]  0.005006088 -0.0130490457 -0.009557360  0.002920696 -0.008350857
#> [3,] -0.001142038  0.0008151688 -0.009628865 -0.001219566  0.009405262
#>             [,11]         [,12]        [,13]        [,14]        [,15]
#> [1,] -0.006738378  0.0005762468 -0.005296950  0.004850041  0.002562127
#> [2,] -0.004244322 -0.0019554997 -0.002383590  0.007221457 -0.010394717
#> [3,] -0.003386700 -0.0109413297  0.001037197 -0.006733164 -0.010313228
#>            [,16]       [,17]        [,18]         [,19]        [,20]
#> [1,]  0.01276040 0.006646059  0.006579828 -0.0024642046 -0.002085663
#> [2,] -0.00131197 0.004055527  0.003339784  0.0003513515  0.009522185
#> [3,] -0.00719698 0.007717015 -0.015558006  0.0004621894  0.007489627
#>             [,21]        [,22]        [,23]        [,24]        [,25]
#> [1,]  0.002712385  0.006371708  0.004268542 -0.004804640 -0.005018426
#> [2,] -0.009015792 -0.002979180 -0.001648510  0.003531347  0.014736120
#> [3,] -0.011363422  0.019181291  0.013150421  0.007952058  0.003230339
#>             [,26]        [,27]        [,28]        [,29]        [,30]
#> [1,]  0.023271720 -0.005051968 -0.002339592 -0.018469069  0.008113231
#> [2,] -0.018512306 -0.004112318  0.003604842 -0.028989819 -0.009056730
#> [3,] -0.001250003  0.012659289  0.009832353 -0.007527549 -0.005785304
#>            [,31]        [,32]         [,33]        [,34]        [,35]
#> [1,] -0.00416199  0.015530051  0.0052672425 -0.002042852  0.010184235
#> [2,] -0.01028187 -0.006948704 -0.0003322662 -0.004914286 -0.008387760
#> [3,]  0.01681821 -0.011301243 -0.0170645890  0.006280111 -0.004545076
#>             [,36]        [,37]        [,38]        [,39]         [,40]
#> [1,]  0.017554500 -0.002484048  0.007787015 -0.004175903  0.0051774842
#> [2,] -0.004392324  0.004330788 -0.008815197 -0.003191363 -0.0008322375
#> [3,] -0.003345518 -0.001558631 -0.007166717 -0.016513314 -0.0178622326
#>             [,41]        [,42]        [,43]        [,44]        [,45]
#> [1,] -0.008772847  0.008904230 -0.008010406  0.024362991  0.007519323
#> [2,] -0.023545807 -0.009269405  0.003961496 -0.007605190 -0.008470970
#> [3,] -0.001252458 -0.006291860 -0.006881802  0.007297203 -0.010088575
#>             [,46]         [,47]        [,48]       [,49]       [,50]
#> [1,] -0.011117093  0.0068375198  0.007016741 0.018100995  0.01115491
#> [2,] -0.003353466 -0.0008553451 -0.006307961 0.002859330 -0.01706344
#> [3,]  0.001638306 -0.0139068551  0.001119349 0.007691079 -0.01143343
#>              [,51]        [,52]       [,53]         [,54]        [,55]
#> [1,] -0.0009036094  0.006349164 0.007397245 -0.0050868632 -0.003856017
#> [2,]  0.0085305799 -0.010084867 0.006757753 -0.0006657934  0.008325462
#> [3,] -0.0052366176  0.005350925 0.011928145  0.0009248856 -0.001321149
#>             [,56]        [,57]        [,58]        [,59]       [,60]
#> [1,] -0.004975413  0.024834344 -0.013576131 -0.002174427 0.008571999
#> [2,] -0.001881182 -0.016927635  0.002193936 -0.011357575 0.013831800
#> [3,] -0.008521174 -0.008762938 -0.013024631  0.002179846 0.001988764
#>             [,61]       [,62]        [,63]        [,64]        [,65]
#> [1,] -0.012551325 0.002780421 -0.005090965 -0.001417669 -0.002301040
#> [2,] -0.005208800 0.012267904  0.002442190  0.008635234 -0.005486897
#> [3,] -0.008278696 0.001696786 -0.002274264  0.017824703  0.006309791
#>             [,66]        [,67]         [,68]        [,69]        [,70]
#> [1,] -0.002180366 -0.011252262 -0.0014182561  0.011457710  0.016785255
#> [2,] -0.012214447  0.011112463  0.0100970995 -0.003749346 -0.015778568
#> [3,] -0.012730505  0.002706287 -0.0006477971 -0.001755193  0.003382569
#>              [,71]        [,72]        [,73]         [,74]         [,75]
#> [1,] -0.0006338523 0.0075111395 -0.005150771 -0.0148344963  0.0001514876
#> [2,]  0.0018866302 0.0007087337  0.002690497  0.0001823942 -0.0121708298
#> [3,]  0.0075360274 0.0126844227 -0.004616881  0.0068383286 -0.0038645104
#>             [,76]        [,77]        [,78]        [,79]        [,80]
#> [1,]  0.001176115  0.016059379 -0.001632821 -0.025013485 -0.002982255
#> [2,]  0.009183790 -0.013042386  0.005120209  0.005664534  0.002629808
#> [3,] -0.012305392 -0.006269906  0.001213185 -0.001691717  0.017857245
#>             [,81]         [,82]        [,83]         [,84]       [,85]
#> [1,]  0.007877916 -8.461057e-05 -0.005835448  0.0009914774 0.004870329
#> [2,] -0.002608528 -1.222898e-02 -0.003064195 -0.0046207448 0.003763251
#> [3,] -0.001731047  1.144024e-03  0.004985786  0.0068712079 0.011257888
#>             [,86]        [,87]        [,88]        [,89]        [,90]
#> [1,] -0.004913678  0.014667650  0.009307644  0.004545742 -0.003438949
#> [2,] -0.015054010  0.004788626 -0.013157592 -0.004775991  0.012734993
#> [3,]  0.005589135 -0.002957022  0.007730117  0.003105876  0.007076176
#>            [,91]         [,92]         [,93]         [,94]        [,95]
#> [1,] 0.003688921 -0.0023900959 -0.0003214758  0.0039425616 -0.014129802
#> [2,] 0.026199220  0.0003469506 -0.0101052510  0.0038138246 -0.005087583
#> [3,] 0.007759920  0.0049210871  0.0085761538 -0.0006319774 -0.006951083
#>            [,96]        [,97]         [,98]        [,99]       [,100]
#> [1,] 0.012318805  0.002421345  7.119029e-03 -0.007261603 -0.005350946
#> [2,] 0.006795369 -0.004854821 -2.380888e-03 -0.006259667 -0.006162465
#> [3,] 0.021174117  0.004352126 -7.728916e-05 -0.014290852 -0.007507318
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
## Method `StartingValuesBSVARSV$set_starting_values()`
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
