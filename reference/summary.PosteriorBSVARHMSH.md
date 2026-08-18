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
#> B[1,1] 0.3853328 0.01457033   0.3567097    0.4027469
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -33.54326 2.4373508   -36.85067    -29.19941
#> B[2,2]  11.80449 0.8578265    10.27438     12.96893
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -15.753183 1.9593453  -18.623924   -12.837039
#> B[3,2]  -5.556697 0.7803345   -7.130457    -4.655243
#> B[3,3]  98.671338 6.9971668   89.050480   110.036140
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile  95% quantile
#> lag1_var1  0.86521584 0.01974412  0.84190252  8.925023e-01
#> lag1_var2 -0.02687072 0.01870284 -0.05426296  4.643938e-05
#> lag1_var3 -0.10754893 0.02377106 -0.13788427 -7.698992e-02
#> const      0.19732664 0.17577338 -0.05481197  4.472557e-01
#> 
#> $A$equation2
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.3102555 0.06307564  -0.4292953   -0.2321380
#> lag1_var2  0.9195360 0.05755583   0.8268476    1.0109734
#> lag1_var3 -0.3963879 0.07795889  -0.4740019   -0.2466128
#> const      0.5076814 0.52038437  -0.2343482    1.3276922
#> 
#> $A$equation3
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.03786471 0.009139496 -0.05499497 -0.025791179
#> lag1_var2 -0.01111095 0.005928736 -0.01856340 -0.001876851
#> lag1_var3  0.95851570 0.010901218  0.94503751  0.979588382
#> const      0.03617406 0.055344083 -0.03865442  0.105178192
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        337.0595  249.3645    93.93354     813.6142
#> B[2,]_shrinkage        595.0112  344.5434   210.61102    1079.0086
#> B[3,]_shrinkage       1465.2472  525.1932   805.88814    2374.5832
#> B[1,]_shrinkage_scale 3133.3596 1370.9621  1030.81010    4998.4745
#> B[2,]_shrinkage_scale 4331.2135 2291.6176  1913.20633    6728.6735
#> B[3,]_shrinkage_scale 5603.7705 4205.7517  2042.07467   12261.5589
#> B_global_scale         408.3072  226.7050   168.52948     694.8201
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.2603737 0.1122241   0.1043530    0.4542336
#> A[2,]_shrinkage       0.2909881 0.1578567   0.1292169    0.5534467
#> A[3,]_shrinkage       0.3098866 0.2132958   0.1348239    0.8336472
#> A[1,]_shrinkage_scale 3.4739660 1.4230965   1.6891431    5.5857257
#> A[2,]_shrinkage_scale 3.2274768 1.2346710   1.5919947    5.3046965
#> A[3,]_shrinkage_scale 3.4100502 1.3353529   1.6006235    5.7372884
#> A_global_scale        0.4245528 0.0949522   0.2907515    0.5651041
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
#> B[1,1] 0.1734068 0.004715948   0.1666015     0.180933
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -29.89547 1.885541   -32.24481    -27.07431
#> B[2,2]  22.21004 1.401858    20.09244     23.93457
#> 
#> $B$equation3
#>              mean       sd 5% quantile 95% quantile
#> B[3,1] -16.592520 1.936733   -19.46461    -13.92313
#> B[3,2]  -9.191697 1.224113   -10.88921     -7.13711
#> B[3,3]  92.759606 6.964637    83.18457    104.70331
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.86825418 0.02489169  0.83526756   0.90530148
#> lag1_var2 -0.05977646 0.01207643 -0.08173901  -0.04147239
#> lag1_var3 -0.47647495 0.03614652 -0.53410703  -0.43285833
#> const      0.35383703 0.10908349  0.20105803   0.51773597
#> 
#> $A$equation2
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.1347156 0.03173980 -0.17751086  -0.08379142
#> lag1_var2  0.9025239 0.02460605  0.86959024   0.94070200
#> lag1_var3 -0.6898542 0.04791672 -0.76236143  -0.62823398
#> const      0.3375343 0.19591805  0.05280145   0.61454945
#> 
#> $A$equation3
#>                  mean          sd  5% quantile 95% quantile
#> lag1_var1 -0.03547814 0.009841473 -0.046215403  -0.01984639
#> lag1_var2 -0.02450130 0.005060478 -0.030130739  -0.01886754
#> lag1_var3  0.84386659 0.012649296  0.821958141   0.86020027
#> const      0.05326525 0.045700021  0.003423868   0.09744997
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        102.03658  91.48027    12.17691     266.5142
#> B[2,]_shrinkage        263.38521 171.04931   101.31096     596.4723
#> B[3,]_shrinkage        949.53344 335.89327   525.79192    1502.3233
#> B[1,]_shrinkage_scale  768.35398 552.22902   141.07463    1920.1338
#> B[2,]_shrinkage_scale 1035.59636 778.35443   276.75565    2497.0465
#> B[3,]_shrinkage_scale 1102.16741 689.74830   303.26415    2572.4769
#> B_global_scale          82.96873  61.98551    23.62482     191.9962
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.5636307 0.3962981   0.1754411    1.1320757
#> A[2,]_shrinkage       0.5317353 0.2228007   0.2579022    0.9219737
#> A[3,]_shrinkage       0.3503502 0.1644881   0.1785716    0.5909468
#> A[1,]_shrinkage_scale 4.7208708 1.4938744   2.9752879    7.0168566
#> A[2,]_shrinkage_scale 5.6495841 1.5479198   3.7434360    7.9878028
#> A[3,]_shrinkage_scale 4.5790551 1.8673661   2.6533209    8.5072699
#> A_global_scale        0.5904636 0.1272573   0.4378610    0.7974904
#> 
#> 
```
