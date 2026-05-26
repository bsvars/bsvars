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
#> B[1,1] 5.343371 0.2732404    5.062721     5.784489
#> 
#> $B$equation2
#>              mean         sd 5% quantile 95% quantile
#> B[2,1] -33.387448 1.22501728  -35.050149   -32.082289
#> B[2,2]   1.916628 0.08746938    1.811279     2.041461
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1]  -2.922693 2.3466240   -6.026064    0.1932598
#> B[3,2] -38.820347 0.8317722  -39.902274  -37.6564415
#> B[3,3]  19.011306 0.4171704   18.426824   19.5355095
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.904981631 0.01852344  0.88053359   0.93275615
#> lag1_var2 -0.009388841 0.01378587 -0.02533916   0.01224426
#> lag1_var3  0.091128848 0.02823772  0.05203743   0.12966960
#> const     -0.117972137 0.08783352 -0.20964438   0.02267328
#> 
#> $A$equation2
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.2830114 0.04257477  -0.3317216  -0.21850677
#> lag1_var2  0.9821764 0.01384807   0.9612930   0.99948728
#> lag1_var3 -0.1158841 0.04679028  -0.1805734  -0.05649201
#> const     -0.4792043 0.09810598  -0.5934021  -0.34176240
#> 
#> $A$equation3
#>                  mean         sd  5% quantile 95% quantile
#> lag1_var1 -0.54350972 0.07341799 -0.630088792  -0.43874419
#> lag1_var2  0.04956331 0.02631296  0.006618189   0.08066511
#> lag1_var3  0.70750570 0.07995959  0.595083140   0.80302242
#> const     -0.19032592 0.18154340 -0.423398176   0.08163001
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean         sd 5% quantile 95% quantile
#> B[1,]_shrinkage        215.6059  142.37224    34.77481     398.7236
#> B[2,]_shrinkage        213.2807   83.38848   148.10135     361.0218
#> B[3,]_shrinkage        375.8480  209.40770   172.82036     733.9143
#> B[1,]_shrinkage_scale 1805.1226 1167.89858   489.71029    3526.3479
#> B[2,]_shrinkage_scale 1934.4603 1019.32096  1014.14136    3456.6357
#> B[3,]_shrinkage_scale 2523.3981 2013.92090   836.35286    6123.7904
#> B_global_scale         206.8060  142.38856    69.99530     449.3435
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.7442759 0.2695940   0.4132291    1.1216257
#> A[2,]_shrinkage       0.5948530 0.2556390   0.3157888    1.0005898
#> A[3,]_shrinkage       0.6101471 0.2220096   0.3620865    0.9637211
#> A[1,]_shrinkage_scale 9.0653302 1.8149117   6.5401765   11.1917361
#> A[2,]_shrinkage_scale 7.6018518 3.8535396   4.4065918   14.0831455
#> A[3,]_shrinkage_scale 7.5276683 2.7900081   4.9145680   12.3310055
#> A_global_scale        0.9256983 0.3166623   0.5715400    1.4483814
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
#>            mean          sd 5% quantile 95% quantile
#> B[1,1] 0.201074 0.006315607   0.1928644    0.2092911
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -30.68189 1.589045   -32.07145    -27.86859
#> B[2,2]  19.78813 1.003198    18.01939     20.66862
#> 
#> $B$equation3
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -21.51388 2.103518   -24.11324    -18.50693
#> B[3,2] -21.99304 1.579790   -23.65761    -19.67758
#> B[3,3]  70.02661 2.088627    67.59169     73.35109
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.92056287 0.03083807   0.8684128   0.95135090
#> lag1_var2 -0.04820091 0.01556041  -0.0717495  -0.02939603
#> lag1_var3 -0.52022573 0.03849881  -0.5585591  -0.45689355
#> const     -0.15732652 0.13765213  -0.3572723   0.01024140
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.02816683 0.01853731  -0.0507482 -0.002552504
#> lag1_var2  0.87923358 0.01485485   0.8557138  0.896798958
#> lag1_var3 -0.91587692 0.02202487  -0.9468925 -0.885830199
#> const     -0.63122414 0.11572127  -0.7971494 -0.479555515
#> 
#> $A$equation3
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.01855769 0.018195235 -0.04543094  0.004334449
#> lag1_var2 -0.04281031 0.006416017 -0.05137234 -0.033787229
#> lag1_var3  0.53147291 0.022941988  0.50295709  0.564307665
#> const     -0.16540840 0.059285086 -0.23739865 -0.077625278
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         94.80689  48.92909    38.31910     168.7732
#> B[2,]_shrinkage        183.87358  47.22360   122.70254     250.8441
#> B[3,]_shrinkage        591.94435 206.95331   317.92296     844.0047
#> B[1,]_shrinkage_scale 1004.41974 368.35729   436.69836    1431.3634
#> B[2,]_shrinkage_scale 1077.66053 343.24261   741.25379    1548.5744
#> B[3,]_shrinkage_scale 1582.78460 644.87214   737.95244    2469.9290
#> B_global_scale         112.42642  38.64114    67.63859     171.8276
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.2780258 0.09423521   0.1587954    0.4156396
#> A[2,]_shrinkage       0.8921047 0.38807981   0.4879647    1.5504480
#> A[3,]_shrinkage       0.2758145 0.10870430   0.1601606    0.4378647
#> A[1,]_shrinkage_scale 3.8143067 0.75626127   2.7468760    4.7309702
#> A[2,]_shrinkage_scale 8.0482277 4.07263774   3.9978834   14.4296920
#> A[3,]_shrinkage_scale 3.8594502 1.22845754   2.5881136    5.5702136
#> A_global_scale        0.6412337 0.17274452   0.4654017    0.9065662
#> 
#> 
```
