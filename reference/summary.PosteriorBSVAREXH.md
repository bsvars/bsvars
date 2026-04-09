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
#>            mean        sd 5% quantile 95% quantile
#> B[1,1] 5.749783 0.1938326    5.439853     5.954017
#> 
#> $B$equation2
#>              mean        sd 5% quantile 95% quantile
#> B[2,1] -33.744831 1.0632454  -34.903364   -32.187470
#> B[2,2]   1.795314 0.0447733    1.734957     1.853742
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1]  -4.232686 2.2668853   -7.332903    -1.420974
#> B[3,2] -38.488787 0.9838067  -40.109052   -37.247616
#> B[3,3]  18.872336 0.5017798   18.236562    19.689836
#> 
#> 
#> $A
#> $A$equation1
#>                    mean          sd 5% quantile 95% quantile
#> lag1_var1  0.9118501421 0.014668140  0.88978016   0.93043221
#> lag1_var2 -0.0007275159 0.008412255 -0.00976606   0.01234599
#> lag1_var3  0.0850005545 0.019968956  0.05800662   0.11245503
#> const     -0.0271060206 0.062669669 -0.08698203   0.07809794
#> 
#> $A$equation2
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.1871885 0.02277577  -0.2170248   -0.1517003
#> lag1_var2  0.9793956 0.01565940   0.9608505    1.0024932
#> lag1_var3 -0.2165115 0.03150847  -0.2648856   -0.1774594
#> const     -0.3541337 0.11540478  -0.4827489   -0.1886364
#> 
#> $A$equation3
#>                  mean         sd  5% quantile 95% quantile
#> lag1_var1 -0.33986623 0.03011450 -0.384583418   -0.3020010
#> lag1_var2  0.04860724 0.03343760  0.007737815    0.0946607
#> lag1_var3  0.48767491 0.04014538  0.440271247    0.5442280
#> const      0.08118711 0.25144475 -0.220706203    0.4132663
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         87.40012  76.53544    25.38009     222.4703
#> B[2,]_shrinkage        179.66797  38.64603   124.76875     228.7738
#> B[3,]_shrinkage        281.57612 107.17875   168.33279     440.0990
#> B[1,]_shrinkage_scale  606.69353 180.91011   331.35604     825.6715
#> B[2,]_shrinkage_scale  871.46964 292.40066   564.42463    1295.1194
#> B[3,]_shrinkage_scale 1021.66290 332.26258   634.56440    1565.7040
#> B_global_scale          78.89192  20.49748    46.09538     104.1703
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.5352850 0.3319084   0.2813669    1.0637461
#> A[2,]_shrinkage       0.7937538 0.8030465   0.2594349    2.0446705
#> A[3,]_shrinkage       0.4541261 0.2218712   0.1923428    0.7701345
#> A[1,]_shrinkage_scale 6.7777081 2.0877072   4.0114236    9.9675143
#> A[2,]_shrinkage_scale 8.0766367 5.5813297   3.8127870   17.4456886
#> A[3,]_shrinkage_scale 6.4124918 2.8925133   3.5447530   10.6230618
#> A_global_scale        0.8776608 0.3686819   0.5797221    1.5395776
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
#> B[1,1] 0.1209625 0.003903127   0.1160647    0.1265312
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -22.47714 1.046601   -23.54928    -20.81280
#> B[2,2]  30.67443 1.415790    28.42524     32.11893
#> 
#> $B$equation3
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -28.76823 1.222999   -30.54431    -27.41912
#> B[3,2] -17.22867 1.513954   -19.80128    -15.63967
#> B[3,3]  65.99116 2.850264    62.99648     70.74693
#> 
#> 
#> $A
#> $A$equation1
#>                 mean          sd 5% quantile 95% quantile
#> lag1_var1  0.9266452 0.056303052   0.8456058   0.98953312
#> lag1_var2  0.0404513 0.009137871   0.0307806   0.05589086
#> lag1_var3 -1.0193105 0.066801702  -1.0948305  -0.92537623
#> const      0.3805956 0.085080217   0.2665109   0.49444249
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.03555481 0.03270830 -0.07867204   0.01357759
#> lag1_var2  0.97838455 0.01098236  0.96374922   0.99283516
#> lag1_var3 -0.76284437 0.03922775 -0.81836382  -0.71132549
#> const     -0.18161753 0.12688193 -0.32472357   0.02065539
#> 
#> $A$equation3
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.01017607 0.026076966 -0.04709795   0.01956522
#> lag1_var2  0.01633176 0.004311044  0.01001308   0.02163090
#> lag1_var3  0.31822344 0.031355061  0.28137838   0.36141581
#> const      0.16844103 0.049251047  0.09477383   0.23049269
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         59.77855  53.35694    15.96248     153.5574
#> B[2,]_shrinkage        232.51474  65.72658   145.63529     334.0628
#> B[3,]_shrinkage        772.93003 241.23120   561.86698    1142.1497
#> B[1,]_shrinkage_scale  676.44131 318.61852   325.14100    1090.4309
#> B[2,]_shrinkage_scale 1430.53358 431.46495   728.51737    1919.0277
#> B[3,]_shrinkage_scale 1926.12535 628.97502  1081.04294    2785.0398
#> B_global_scale         120.75085  34.63651    64.10951     155.1692
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3831704 0.14187558   0.1945153    0.5457176
#> A[2,]_shrinkage       0.3563792 0.26870756   0.1532991    0.8318858
#> A[3,]_shrinkage       0.4548902 0.14170682   0.2995978    0.6688261
#> A[1,]_shrinkage_scale 3.3366022 1.17395729   1.7668151    4.8440147
#> A[2,]_shrinkage_scale 3.4276031 1.28093197   2.1087843    5.6564258
#> A[3,]_shrinkage_scale 4.4866798 1.30396499   2.9064572    6.5161362
#> A_global_scale        0.4274401 0.06696715   0.3472145    0.5235640
#> 
#> 
```
