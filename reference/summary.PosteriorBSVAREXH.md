# Provides posterior summary of heteroskedastic Structural VAR estimation

Provides posterior mean, standard deviations, as well as 5 and 95
percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper parameters.

## Usage

``` r
# S3 method for class 'PosteriorBSVAREXH'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorBSVAREXH obtained using the
  [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md)
  function applied to heteroskedastic Bayesian Structural VAR model
  specification set by function `specify_bsvar_exh$new()` containing
  draws from the posterior distribution of the parameters.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper-parameters.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_bsvar_exh`](https://bsvars.org/bsvars/reference/specify_bsvar_exh.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# specify the model and set seed
spec  = specify_bsvar_exh$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.

# run the burn-in
burn  = estimate(spec, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-exH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# estimate the model
post  = estimate(burn, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-exH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
summary(post)
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Posterior summary of the parameters             |
#>  **************************************************|
#> $B
#> $B$equation1
#>             mean         sd 5% quantile 95% quantile
#> B[1,1] 0.1500464 0.00574714   0.1406376    0.1564044
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -20.33273 0.7361694   -21.26737    -19.27431
#> B[2,2]  33.02478 1.1940813    31.31051     34.54699
#> 
#> $B$equation3
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -31.03035 1.815784   -32.95178    -28.21721
#> B[3,2] -16.98975 1.355915   -19.00551    -15.47634
#> B[3,3]  53.88299 1.917661    50.57332     55.68128
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.90267694 0.03685417  0.84222014   0.95171741
#> lag1_var2 -0.07153050 0.02038476 -0.09864049  -0.04886138
#> lag1_var3 -0.66015954 0.05389997 -0.72712569  -0.57105108
#> const     -0.06049641 0.12611060 -0.23417068   0.07725025
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.04100879 0.02857462 -0.08518953  -0.01529998
#> lag1_var2  0.91857686 0.01442287  0.90013570   0.93830654
#> lag1_var3 -0.42538855 0.03959889 -0.46354376  -0.36522433
#> const     -0.37461862 0.11092631 -0.51930359  -0.22750725
#> 
#> $A$equation3
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.03274243 0.03188900 -0.08468196 -0.003541695
#> lag1_var2 -0.05958421 0.01239694 -0.07699580 -0.046596844
#> lag1_var3  0.44019168 0.04558761  0.39955664  0.514458137
#> const     -0.08009976 0.08362290 -0.19758014  0.014261742
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         62.62118  55.65877    20.16962     164.1777
#> B[2,]_shrinkage        235.54791  85.94756   132.79187     365.3642
#> B[3,]_shrinkage        400.51066 179.50515   199.01561     691.2248
#> B[1,]_shrinkage_scale  651.24365 550.07928   294.06594    1551.6097
#> B[2,]_shrinkage_scale 1171.90306 728.95747   429.01510    2318.0108
#> B[3,]_shrinkage_scale 1109.92038 699.15749   394.93481    2119.0549
#> B_global_scale          87.10413  45.79613    35.48301     154.4504
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.2087313 0.06123965   0.1387797    0.2991104
#> A[2,]_shrinkage       0.1956115 0.16477427   0.0806252    0.4751806
#> A[3,]_shrinkage       0.2291007 0.08281508   0.1054627    0.3311664
#> A[1,]_shrinkage_scale 2.6905529 0.69830706   1.9446185    3.8631827
#> A[2,]_shrinkage_scale 2.4002853 1.11576703   1.3053678    4.3034369
#> A[3,]_shrinkage_scale 2.7117288 0.60185565   1.9535141    3.4953545
#> A_global_scale        0.3866745 0.05046984   0.3388702    0.4604775
#> 
#> 

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_exh$new() |>
  estimate(S = 10) |> 
  estimate(S = 10) |> 
  summary()
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-exH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-exH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
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
#> B[1,1] 0.1765072 0.005491943   0.1679323    0.1814797
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -26.18006 1.144236   -27.86979    -24.69267
#> B[2,2]  27.99093 1.213093    26.41454     29.77801
#> 
#> $B$equation3
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -26.47189 1.984648   -29.68828    -24.34898
#> B[3,2] -25.69252 1.494296   -27.63330    -23.90043
#> B[3,3]  40.32808 1.238623    39.34902     42.29325
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  1.047984878 0.03964866  0.98557639   1.09020696
#> lag1_var2  0.015919854 0.01751104 -0.00660022   0.04145172
#> lag1_var3 -0.832468689 0.04623207 -0.88401051  -0.76167941
#> const     -0.007573382 0.13250143 -0.18847073   0.17323738
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.09917676 0.03260085  0.05063825    0.1399284
#> lag1_var2  0.97213944 0.02067506  0.94376521    1.0001772
#> lag1_var3 -0.83618179 0.03845652 -0.88944805   -0.7846864
#> const     -0.35517234 0.16152747 -0.59446304   -0.1477010
#> 
#> $A$equation3
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.149721104 0.04560864  0.08093969   0.20207531
#> lag1_var2  0.020311629 0.02816597 -0.01038425   0.06824140
#> lag1_var3 -0.153173973 0.05187630 -0.21768829  -0.08240612
#> const      0.006425927 0.22640429 -0.24997453   0.36729800
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         88.74027  48.81889    32.16602     153.8711
#> B[2,]_shrinkage        249.70109 152.11769   117.02930     497.1375
#> B[3,]_shrinkage        395.03487 167.97712   175.33894     663.3382
#> B[1,]_shrinkage_scale  792.16672 397.96422   345.09398    1298.3610
#> B[2,]_shrinkage_scale 1117.26698 624.26426   452.78914    2130.6887
#> B[3,]_shrinkage_scale 1422.73438 750.85913   478.68855    2449.6517
#> B_global_scale          92.45479  54.06481    36.59292     171.4619
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.4326819 0.2102607   0.2057495    0.7266808
#> A[2,]_shrinkage       0.5547390 0.4581423   0.2127924    1.2924374
#> A[3,]_shrinkage       0.5519388 0.2377144   0.3391093    0.9518022
#> A[1,]_shrinkage_scale 5.1354306 1.6359586   3.2488473    7.5117034
#> A[2,]_shrinkage_scale 5.4208158 2.4958584   3.0448236    9.6369800
#> A[3,]_shrinkage_scale 5.0041181 1.2107337   3.5389357    6.8430814
#> A_global_scale        0.6154722 0.1161814   0.4455839    0.7647622
#> 
#> 
```
