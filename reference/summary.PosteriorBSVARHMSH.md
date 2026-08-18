# Provides posterior summary of heteroskedastic Structural VAR estimation

Provides posterior mean, standard deviations, as well as 5 and 95
percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper parameters.

## Usage

``` r
# S3 method for class 'PosteriorBSVARHMSH'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorBSVARHMSH obtained using the
  [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md)
  function applied to heteroskedastic Bayesian Structural VAR model
  specification set by function `specify_bsvar_hmsh$new()` containing
  draws from the posterior distribution of the parameters.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper-parameters.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_bsvar_hmsh`](https://bsvars.org/bsvars/reference/specify_bsvar_hmsh.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# specify the model
specification  = specify_bsvar_msh$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.

# run the burn-in
burn_in        = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# estimate the model
posterior      = estimate(burn_in, 20)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 20 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
summary(posterior)
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Posterior summary of the parameters             |
#>  **************************************************|
#> $B
#> $B$equation1
#>            mean       sd 5% quantile 95% quantile
#> B[1,1] 2.574614 0.144473    2.384583     2.804976
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -20.39402 1.379384   -22.36939    -18.47744
#> B[2,2]  16.73904 1.099274    15.29693     18.01396
#> 
#> $B$equation3
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -19.33998 1.956446  -22.124000   -16.866035
#> B[3,2] -17.74748 2.049598  -21.064512   -14.679228
#> B[3,3]   4.68217 0.413478    4.228154     5.358559
#> 
#> 
#> $A
#> $A$equation1
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1  0.92646168 0.019850992  0.90392885  0.960927598
#> lag1_var2 -0.00551145 0.009635754 -0.01747300  0.008245878
#> lag1_var3  0.01628133 0.023301156 -0.02198854  0.043268019
#> const     -0.25517548 0.097413428 -0.35011403 -0.101507883
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.02335944 0.02515941 -0.05952912   0.02122673
#> lag1_var2  1.01147683 0.01363388  0.99405273   1.03307132
#> lag1_var3 -0.06665054 0.02847427 -0.11991092  -0.02901474
#> const     -0.16038098 0.13453109 -0.34705577   0.04797854
#> 
#> $A$equation3
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.04853166 0.15487319 -0.25149302    0.1685018
#> lag1_var2  0.24646199 0.04995229  0.17501470    0.3256340
#> lag1_var3  0.35295580 0.17321337  0.09448979    0.5712072
#> const      0.33403915 0.57699036 -0.48685470    1.4055016
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean         sd 5% quantile 95% quantile
#> B[1,]_shrinkage        174.0127   54.83972    72.26748     235.2996
#> B[2,]_shrinkage        354.2453  216.94849   139.74729     744.9786
#> B[3,]_shrinkage        244.8327  150.38795   117.76067     437.3552
#> B[1,]_shrinkage_scale 1770.9465  606.97746   906.97401    2829.1384
#> B[2,]_shrinkage_scale 2280.8076 1265.70477  1174.64665    3663.8748
#> B[3,]_shrinkage_scale 1880.5862  530.07387  1314.69986    2826.5422
#> B_global_scale         184.7527   52.45009   128.64268     264.5924
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.5567576 0.2336739   0.3349472    0.8480766
#> A[2,]_shrinkage       0.4503216 0.2654851   0.1862303    0.8866481
#> A[3,]_shrinkage       0.9030718 0.6525881   0.4183811    2.0574741
#> A[1,]_shrinkage_scale 6.7714075 1.8028410   4.5631601   10.9114874
#> A[2,]_shrinkage_scale 6.6518548 3.0558540   2.7826432   11.1100915
#> A[3,]_shrinkage_scale 8.7770262 3.0921066   5.3737529   15.2245075
#> A_global_scale        0.7975223 0.1567753   0.6052355    1.0595295
#> 
#> 

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_msh$new() |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  summary()
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 20 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Posterior summary of the parameters             |
#>  **************************************************|
#> $B
#> $B$equation1
#>             mean          sd 5% quantile 95% quantile
#> B[1,1] 0.1586786 0.005863031   0.1505878    0.1675674
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -21.85601 1.606955   -24.78632     -19.6508
#> B[2,2]  23.87238 1.755696    21.45449      27.0658
#> 
#> $B$equation3
#>              mean       sd 5% quantile 95% quantile
#> B[3,1] -12.632682 1.453157  -14.764564   -10.627100
#> B[3,2]  -6.263308 1.090452   -7.696084    -4.486498
#> B[3,3]  80.488067 6.732478   71.379473    89.090736
#> 
#> 
#> $A
#> $A$equation1
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.8088599 0.03675090  0.75066976    0.8565163
#> lag1_var2 -0.2082930 0.04122119 -0.25638461   -0.1526830
#> lag1_var3 -0.2896474 0.05415873 -0.36351477   -0.2142306
#> const      0.2187183 0.26068387 -0.09963121    0.5978315
#> 
#> $A$equation2
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.1653351 0.02771195  -0.1996819   -0.1333890
#> lag1_var2  0.8007962 0.04095450   0.7482906    0.8736825
#> lag1_var3 -0.2726101 0.04563976  -0.3264476   -0.2233687
#> const      0.1455167 0.25834655  -0.1707217    0.6124081
#> 
#> $A$equation3
#>                    mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.0377867202 0.007199439 -0.04935941  -0.02966751
#> lag1_var2 -0.0536699373 0.008691311 -0.06304276  -0.03893942
#> lag1_var3  0.9276286140 0.010605551  0.91238159   0.94348514
#> const     -0.0002401391 0.052799064 -0.06642993   0.08527627
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        612.0310  596.8364   101.56084     1442.455
#> B[2,]_shrinkage        569.5228  432.4682   183.06754     1342.908
#> B[3,]_shrinkage        977.8721  378.7460   425.27275     1572.054
#> B[1,]_shrinkage_scale 4951.6041 4274.8645   961.16909    15498.105
#> B[2,]_shrinkage_scale 5182.0226 4832.3062  1277.89218    13798.434
#> B[3,]_shrinkage_scale 4912.4765 2913.6677  1343.32528    11639.491
#> B_global_scale         457.2563  345.0791    88.38897     1048.445
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3860925 0.2088861   0.1144824    0.6910185
#> A[2,]_shrinkage       0.6570420 0.9067221   0.1321760    1.8220874
#> A[3,]_shrinkage       0.5889400 0.5743984   0.1423931    1.6474507
#> A[1,]_shrinkage_scale 4.9941734 2.7372695   1.5618327    9.1552342
#> A[2,]_shrinkage_scale 6.1130819 6.3288427   1.8185997   19.3927552
#> A[3,]_shrinkage_scale 5.6827239 4.8496862   1.4624805   12.7751060
#> A_global_scale        0.6219977 0.3596173   0.3241355    1.4660936
#> 
#> 
```
