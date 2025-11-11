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
#>             mean         sd 5% quantile 95% quantile
#> B[1,1] 0.2743287 0.01588252    0.256715    0.2939072
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -32.64234 2.0952318   -35.94272    -29.71982
#> B[2,2]  13.24450 0.8491833    12.06122     14.56570
#> 
#> $B$equation3
#>             mean        sd 5% quantile 95% quantile
#> B[3,1] -17.79893 2.7532280   -22.10029    -13.52839
#> B[3,2] -12.04822 0.9784532   -13.30906    -10.33407
#> B[3,3]  90.52374 5.0200535    84.46739     95.28848
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.87352713 0.01385041  0.84817089   0.89753935
#> lag1_var2 -0.04530412 0.02221656 -0.08170846  -0.01991191
#> lag1_var3 -0.23386607 0.01610476 -0.25676936  -0.21227747
#> const      0.23191530 0.18739844 -0.08057405   0.47499309
#> 
#> $A$equation2
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.2306291 0.04165684  -0.2828527   -0.1657709
#> lag1_var2  0.8937392 0.02905505   0.8518903    0.9347414
#> lag1_var3 -0.6776803 0.05439270  -0.7573685   -0.5989530
#> const      0.6161031 0.21578122   0.3191438    0.8247872
#> 
#> $A$equation3
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.05099322 0.007655073 -0.06051419  -0.03841220
#> lag1_var2 -0.02127319 0.006110524 -0.03061959  -0.01240758
#> lag1_var3  0.85677488 0.009370934  0.84000416   0.86874269
#> const      0.13841675 0.058459689  0.05405095   0.22306358
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        439.1129  405.8352    81.16233    1148.5071
#> B[2,]_shrinkage        503.7732  375.0847   157.40628    1082.7085
#> B[3,]_shrinkage       1056.5910  537.4412   386.80139    2098.7796
#> B[1,]_shrinkage_scale 3756.7934 2873.5256   446.39467    9857.2009
#> B[2,]_shrinkage_scale 3597.1408 2481.2219   860.91633    7845.7348
#> B[3,]_shrinkage_scale 4338.4102 2680.4263   875.32089    8291.1336
#> B_global_scale         359.5301  239.7759    67.22402     710.5362
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.4623162 0.1892743   0.1929038    0.7096629
#> A[2,]_shrinkage       0.6053795 0.3104104   0.3010823    1.0771298
#> A[3,]_shrinkage       0.3599383 0.1734202   0.1897951    0.7146256
#> A[1,]_shrinkage_scale 4.7750379 1.7130224   2.3099149    7.9297202
#> A[2,]_shrinkage_scale 5.3418201 1.4924609   3.6089253    7.7653745
#> A[3,]_shrinkage_scale 4.5415413 1.4500408   2.2480596    6.8320035
#> A_global_scale        0.5817083 0.1340768   0.4045001    0.7364376
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
#>            mean          sd 5% quantile 95% quantile
#> B[1,1] 0.108383 0.004947153   0.1025713    0.1170936
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -15.84137 1.172821   -17.22798    -13.87978
#> B[2,2]  33.21818 2.457184    29.09728     36.12924
#> 
#> $B$equation3
#>              mean       sd 5% quantile 95% quantile
#> B[3,1] -20.652148 1.689837  -23.138139   -18.007012
#> B[3,2]  -7.661103 2.114200   -9.732063    -5.709595
#> B[3,3]  88.051674 4.113210   80.846152    93.987732
#> 
#> 
#> $A
#> $A$equation1
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.8188055 0.02418454  0.79424920    0.8575818
#> lag1_var2 -0.0417235 0.02113270 -0.07351310   -0.0141320
#> lag1_var3 -0.9032176 0.03722756 -0.96952576   -0.8621753
#> const      0.2418116 0.18097959 -0.05828947    0.4593825
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.08459643 0.01757135  -0.1158197  -0.06475915
#> lag1_var2  0.94967805 0.01337220   0.9285268   0.97178762
#> lag1_var3 -0.42690890 0.02685391  -0.4598952  -0.37973717
#> const     -0.14303635 0.12262321  -0.3001921   0.04845682
#> 
#> $A$equation3
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.04582737 0.009687393 -0.05906442 -0.030507715
#> lag1_var2 -0.01631263 0.007197432 -0.02610400 -0.004992447
#> lag1_var3  0.74511609 0.013466997  0.72766704  0.763502410
#> const      0.01943882 0.065980262 -0.07488064  0.105594247
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        298.1538  274.7040    22.02932     883.2276
#> B[2,]_shrinkage        400.4307  295.9042   127.84071     907.9234
#> B[3,]_shrinkage       1033.5368  502.5654   362.43077    1798.5237
#> B[1,]_shrinkage_scale 2920.0007 2477.5064   237.72682    6431.1972
#> B[2,]_shrinkage_scale 2845.0038 2266.3171   284.38541    6154.6701
#> B[3,]_shrinkage_scale 4132.5474 3285.8532   450.03541    9434.0225
#> B_global_scale         314.8752  243.7283    25.35984     628.9314
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.6053090 0.3482830   0.1347855    1.2015578
#> A[2,]_shrinkage       0.4450613 0.2528918   0.2156834    0.7406520
#> A[3,]_shrinkage       0.5640893 0.2962490   0.2203496    1.1252471
#> A[1,]_shrinkage_scale 5.2546384 1.9959191   2.3152579    7.8909551
#> A[2,]_shrinkage_scale 5.4141882 1.9639476   3.1745701    9.1476605
#> A[3,]_shrinkage_scale 6.2994294 2.4566890   3.2365380   10.8445391
#> A_global_scale        0.6636447 0.1650886   0.4853614    0.9688531
#> 
#> 
```
