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
#>             mean          sd 5% quantile 95% quantile
#> B[1,1] 0.1186827 0.003214882   0.1148036    0.1235951
#> 
#> $B$equation2
#>             mean         sd 5% quantile 95% quantile
#> B[2,1] -2.375302 0.08461678   -2.515124    -2.279495
#> B[2,2] 38.613613 1.26967280   37.248097    40.688064
#> 
#> $B$equation3
#>               mean       sd 5% quantile 95% quantile
#> B[3,1] -34.3719418 1.176715  -35.877102   -32.902578
#> B[3,2]  -0.4070549 1.818362   -2.796607     1.937935
#> B[3,3]  69.2410458 2.436525   66.030104    72.219744
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  1.04801957 0.01859803  1.02517792   1.07250029
#> lag1_var2  0.07276879 0.01710247  0.05092473   0.09628538
#> lag1_var3 -1.17393053 0.02130958 -1.21068065  -1.15043396
#> const      0.72584738 0.08961318  0.59816997   0.85557831
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.02756030 0.01518650 -0.05060450 -0.009553035
#> lag1_var2  0.96075766 0.01246638  0.94035024  0.974656393
#> lag1_var3 -0.02990366 0.01971962 -0.05515446 -0.001746796
#> const     -0.35880937 0.09619447 -0.52074257 -0.266087124
#> 
#> $A$equation3
#>                 mean          sd 5% quantile 95% quantile
#> lag1_var1 0.04941729 0.008972796  0.03530967   0.05914532
#> lag1_var2 0.02817736 0.007354138  0.01891603   0.03911239
#> lag1_var3 0.38701223 0.011866181  0.37303765   0.40524555
#> const     0.29973463 0.039761323  0.24471449   0.35322136
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        133.3350  68.11497    57.47855     244.6557
#> B[2,]_shrinkage        267.3135 161.39062   155.49960     552.7087
#> B[3,]_shrinkage        876.6243 481.54998   390.43911    1628.9220
#> B[1,]_shrinkage_scale 1214.3086 480.81992   593.85936    1840.0323
#> B[2,]_shrinkage_scale 1421.9740 455.79579   991.21218    2173.3222
#> B[3,]_shrinkage_scale 1690.9419 494.70828  1051.37108    2383.3930
#> B_global_scale         141.6229  36.55620   101.54385     196.4104
#> 
#> $hyper$A
#>                             mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage        1.6929423 0.9918462   0.8326696    3.1991606
#> A[2,]_shrinkage        1.0437923 0.3505429   0.6550454    1.5665306
#> A[3,]_shrinkage        0.6684905 0.1758330   0.4687726    0.9331951
#> A[1,]_shrinkage_scale 12.8823339 4.9096264   8.6065174   21.4180564
#> A[2,]_shrinkage_scale 10.9629990 3.7423400   6.9063508   16.6564068
#> A[3,]_shrinkage_scale  9.0181747 1.6372850   6.9717232   11.4052882
#> A_global_scale         1.1698315 0.1548472   0.9706936    1.3829503
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
#>             mean         sd 5% quantile 95% quantile
#> B[1,1] 0.1474609 0.00534023   0.1405138    0.1548521
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -29.38231 1.2368558   -31.37445    -28.06368
#> B[2,2]  21.65885 0.9064454    20.68853     23.11751
#> 
#> $B$equation3
#>              mean       sd 5% quantile 95% quantile
#> B[3,1] -10.638938 1.522505  -12.315045    -8.119630
#> B[3,2]   6.285582 1.121012    4.451295     7.526795
#> B[3,3]  92.234058 2.717768   89.253021    96.258646
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile  95% quantile
#> lag1_var1  1.03061902 0.05279111   0.9619320  1.0912443193
#> lag1_var2 -0.61220276 0.01952387  -0.6393350 -0.5839910902
#> lag1_var3 -0.08246527 0.06265655  -0.1539550  0.0006147082
#> const      0.52772587 0.22096708   0.2575431  0.8615454641
#> 
#> $A$equation2
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.1169141 0.06067619  0.03866382    0.2033203
#> lag1_var2  0.1102524 0.02362802  0.07907432    0.1459269
#> lag1_var3 -0.1979724 0.07120474 -0.29434982   -0.0986293
#> const      0.1840286 0.28404474 -0.15046459    0.6291116
#> 
#> $A$equation3
#>                   mean          sd 5% quantile  95% quantile
#> lag1_var1 -0.014478931 0.009537335 -0.02660174  0.0001810202
#> lag1_var2 -0.014293164 0.004472183 -0.02010323 -0.0081437395
#> lag1_var3  1.014936518 0.012014751  0.99779398  1.0314383777
#> const     -0.003388276 0.044351848 -0.06650067  0.0633157039
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean         sd 5% quantile 95% quantile
#> B[1,]_shrinkage        140.2157  120.07963    56.53255     321.1766
#> B[2,]_shrinkage        276.7461   83.86127   169.31157     400.6441
#> B[3,]_shrinkage        944.8312  376.69129   513.77069    1497.3037
#> B[1,]_shrinkage_scale 1246.2153  623.45540   562.97528    2201.2957
#> B[2,]_shrinkage_scale 1752.8499  974.48701   647.61608    3269.4548
#> B[3,]_shrinkage_scale 1768.8907 1118.52435   606.99707    3576.1839
#> B_global_scale         147.4883   87.66842    42.51116     273.0407
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.5896421 0.27112399   0.2696961    0.9545009
#> A[2,]_shrinkage       0.6014087 0.20549780   0.3742268    0.8782232
#> A[3,]_shrinkage       0.4050385 0.24433449   0.1695794    0.7719811
#> A[1,]_shrinkage_scale 4.5739942 2.34883864   2.3854856    8.2179968
#> A[2,]_shrinkage_scale 5.1703304 1.30462120   3.7761702    6.9828991
#> A[3,]_shrinkage_scale 4.5136788 1.31008872   2.9615145    6.6021026
#> A_global_scale        0.5448415 0.09949215   0.4278288    0.6941276
#> 
#> 
```
