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
#> B[1,1] 0.3852761 0.01471279   0.3586985    0.4063304
#> 
#> $B$equation2
#>              mean       sd 5% quantile 95% quantile
#> B[2,1] -27.897355 2.092978  -30.881783    -25.11643
#> B[2,2]   9.818165 0.736744    8.840335     10.85900
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -12.130966 2.1308712  -15.214039   -10.038339
#> B[3,2]  -5.295156 0.7802141   -6.481636    -4.094314
#> B[3,3]  85.007447 4.8190913   78.147767    92.463956
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.85486065 0.01658456  0.82971084   0.87440246
#> lag1_var2 -0.04424853 0.01449059 -0.06661577  -0.02799295
#> lag1_var3 -0.09526212 0.01946764 -0.12001630  -0.06610349
#> const      0.02024173 0.14408134 -0.19672724   0.19278070
#> 
#> $A$equation2
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.3017236 0.05548207 -0.39680289    -0.235756
#> lag1_var2  0.9444787 0.05187456  0.86858195     1.010155
#> lag1_var3 -0.4150081 0.06636958 -0.49534837    -0.297406
#> const      0.6969129 0.50676537 -0.04275689     1.319905
#> 
#> $A$equation3
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.03507834 0.009300998 -0.05578997 -0.026356717
#> lag1_var2 -0.01322327 0.005143183 -0.01966911 -0.007038159
#> lag1_var3  0.95489170 0.011215337  0.94413772  0.978346267
#> const      0.01468737 0.050385163 -0.06037826  0.072894650
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        283.1179  205.6734    79.41999     647.0722
#> B[2,]_shrinkage        467.5507  258.6317   170.13556     822.5960
#> B[3,]_shrinkage       1120.3756  381.5685   587.90925    1714.2030
#> B[1,]_shrinkage_scale 2628.8997 1107.7253   892.47047    4165.1635
#> B[2,]_shrinkage_scale 3539.9672 1779.2430  1663.96643    5457.2580
#> B[3,]_shrinkage_scale 4552.5747 3278.2825  1742.93055    9446.8926
#> B_global_scale         336.9374  175.2179   143.59521     549.1987
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.2537263 0.10889805  0.09197495    0.4494974
#> A[2,]_shrinkage       0.3156833 0.17276346  0.14490307    0.5784276
#> A[3,]_shrinkage       0.3117962 0.21587809  0.13443278    0.8439176
#> A[1,]_shrinkage_scale 3.4314447 1.40436929  1.66681726    5.5644203
#> A[2,]_shrinkage_scale 3.3404316 1.23618547  1.73467593    5.4068846
#> A[3,]_shrinkage_scale 3.4281003 1.34264512  1.59475569    5.7205695
#> A_global_scale        0.4268623 0.09390261  0.29146425    0.5637277
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
#> B[1,1] 0.1736061 0.005914342    0.165765    0.1820453
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -23.26099 2.028458   -26.02737    -20.05127
#> B[2,2]  17.27775 1.506725    14.90235     19.31834
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -12.938444 1.7015173  -15.533814   -10.960020
#> B[3,2]  -7.482745 0.9668193   -9.028061    -6.123141
#> B[3,3]  73.697482 6.5575885   63.968748    82.292014
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.89317904 0.02711776  0.86048122   0.93259412
#> lag1_var2 -0.07134705 0.00924454 -0.08838481  -0.05900668
#> lag1_var3 -0.49940796 0.03864021 -0.55499502  -0.45479919
#> const      0.29953291 0.08314971  0.14953596   0.41793045
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.08735566 0.03245943  -0.1296448  -0.04167211
#> lag1_var2  0.91759964 0.02142312   0.8870876   0.95019703
#> lag1_var3 -0.74168633 0.04909401  -0.8092055  -0.65352196
#> const      0.54225773 0.17006903   0.3438720   0.81349226
#> 
#> $A$equation3
#>                  mean          sd  5% quantile 95% quantile
#> lag1_var1 -0.02635973 0.010321598 -0.039921586   -0.0106042
#> lag1_var2 -0.02588165 0.004276231 -0.033158844   -0.0207778
#> lag1_var3  0.83473565 0.013675331  0.813099256    0.8525833
#> const      0.05621270 0.040920077  0.000834828    0.1000612
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        82.61460  72.29332    11.30700     204.7983
#> B[2,]_shrinkage       179.06685 121.03805    68.30080     415.3279
#> B[3,]_shrinkage       611.59327 227.25106   311.43375     991.0192
#> B[1,]_shrinkage_scale 622.85681 412.40414   130.95045    1465.0136
#> B[2,]_shrinkage_scale 809.79418 571.75630   250.71216    1886.6565
#> B[3,]_shrinkage_scale 880.25404 498.70996   275.50956    1931.9599
#> B_global_scale         66.83374  45.56024    21.68812     147.2186
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.5651056 0.3986255   0.1741924    1.1330252
#> A[2,]_shrinkage       0.5645369 0.2318132   0.2761928    0.9792615
#> A[3,]_shrinkage       0.3535861 0.1638326   0.1811787    0.5919052
#> A[1,]_shrinkage_scale 4.7562501 1.5065579   3.0324452    7.1316973
#> A[2,]_shrinkage_scale 5.8101129 1.5611030   3.8696419    8.2087788
#> A[3,]_shrinkage_scale 4.6209136 1.8560764   2.6761964    8.5223870
#> A_global_scale        0.5971316 0.1277657   0.4438947    0.8089932
#> 
#> 
```
