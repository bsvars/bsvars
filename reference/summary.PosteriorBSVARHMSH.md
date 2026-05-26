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
#>             mean          sd 5% quantile 95% quantile
#> B[1,1] 0.1238984 0.004734257   0.1182627    0.1316987
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -24.08458 1.243187   -25.83596    -22.10325
#> B[2,2]  27.73900 1.434295    25.45606     29.75568
#> 
#> $B$equation3
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -28.87493 2.256956   -31.74976    -24.99113
#> B[3,2] -19.06422 1.632141   -21.31681    -16.73165
#> B[3,3]  64.07437 3.527353    58.34937     68.42081
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.98913300 0.04540564  0.92418718   1.06529184
#> lag1_var2  0.01190062 0.03363202 -0.04273914   0.05346106
#> lag1_var3 -1.04934254 0.06611394 -1.14979676  -0.95208319
#> const      0.05724878 0.25849607 -0.34902852   0.39818426
#> 
#> $A$equation2
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.002238381 0.04879679 -0.07666015   0.08213784
#> lag1_var2  0.979082091 0.03136988  0.93352468   1.02645702
#> lag1_var3 -0.921109443 0.06760566 -1.02696397  -0.81454620
#> const     -0.229904914 0.25158125 -0.62401017   0.20363538
#> 
#> $A$equation3
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 0.02391950 0.03243674 -0.02463961    0.0828692
#> lag1_var2 0.01461823 0.02455483 -0.02270578    0.0443832
#> lag1_var3 0.21547125 0.04756061  0.13716925    0.2897548
#> const     0.09589629 0.18857924 -0.17302321    0.3523027
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean         sd 5% quantile 95% quantile
#> B[1,]_shrinkage        255.8157  207.56468    85.89370     680.9672
#> B[2,]_shrinkage        272.7336   98.95809   125.14710     464.4315
#> B[3,]_shrinkage        763.7288  344.87134   489.59796    1525.3393
#> B[1,]_shrinkage_scale 2151.9425 1373.81906   817.49200    4727.9367
#> B[2,]_shrinkage_scale 1960.6292  789.14042   898.30840    3393.5401
#> B[3,]_shrinkage_scale 2434.9739 1297.55393   933.42953    4507.0525
#> B_global_scale         200.0956  107.82995    90.27763     359.7516
#> 
#> $hyper$A
#>                             mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage        1.3396782 0.6220677   0.5403634     2.282280
#> A[2,]_shrinkage        0.7245254 0.3856964   0.2872096     1.346958
#> A[3,]_shrinkage        0.6662068 0.3031588   0.2639221     1.199410
#> A[1,]_shrinkage_scale 13.5096029 4.8521734   8.4293337    19.923585
#> A[2,]_shrinkage_scale  7.9844338 3.4055844   2.8178122    12.717727
#> A[3,]_shrinkage_scale  8.5283617 3.3146985   5.4044614    13.884996
#> A_global_scale         1.1931663 0.3636237   0.7741481     1.813736
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
#>             mean       sd 5% quantile 95% quantile
#> B[1,1] 0.1612303 0.006348   0.1524519    0.1702681
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -28.56273 2.144112   -32.18281    -26.36576
#> B[2,2]  22.35546 1.675788    20.63918     25.16526
#> 
#> $B$equation3
#>               mean       sd 5% quantile 95% quantile
#> B[3,1] -13.2230154 2.553517  -17.178044    -9.547765
#> B[3,2]   0.4497161 1.920482   -1.919255     3.878832
#> B[3,3]  93.4541302 5.337804   86.444129   101.417637
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.90013741 0.04180275   0.8513660   0.95938384
#> lag1_var2 -0.45946818 0.02524568  -0.4975828  -0.43059065
#> lag1_var3 -0.08703615 0.05234694  -0.1609678  -0.02853216
#> const     -0.12429527 0.24025863  -0.4458878   0.20593099
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.09366947 0.04970327  -0.1592857 -0.013625279
#> lag1_var2  0.38949047 0.03126715   0.3370369  0.436146085
#> lag1_var3 -0.14937465 0.06237759  -0.2534936 -0.071906650
#> const     -0.36647349 0.29618946  -0.8365011 -0.008912798
#> 
#> $A$equation3
#>                  mean          sd 5% quantile  95% quantile
#> lag1_var1 -0.01634469 0.010403535 -0.03212505 -1.530964e-05
#> lag1_var2 -0.06841720 0.006853267 -0.08466678 -6.158519e-02
#> lag1_var3  0.99166178 0.013619259  0.96882383  1.012354e+00
#> const     -0.07619514 0.062391210 -0.22836762 -4.549972e-03
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean         sd 5% quantile 95% quantile
#> B[1,]_shrinkage        121.3530   77.17146    43.19371     264.7696
#> B[2,]_shrinkage        311.7631  218.62796   120.04603     593.5526
#> B[3,]_shrinkage        948.2676  419.07730   558.36069    1551.4428
#> B[1,]_shrinkage_scale 1315.6985  739.71888   580.65058    2499.1660
#> B[2,]_shrinkage_scale 1729.2779  970.81897   733.23561    3304.4826
#> B[3,]_shrinkage_scale 2151.2141 1313.62535   805.50578    4153.4031
#> B_global_scale         158.3027   84.62027    58.15977     292.5794
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3458039 0.1666495   0.1638222    0.6666005
#> A[2,]_shrinkage       0.4310210 0.1947428   0.1786902    0.7549628
#> A[3,]_shrinkage       0.3437345 0.2834410   0.1156164    0.5843893
#> A[1,]_shrinkage_scale 4.0291963 1.6219576   2.0617055    6.9072618
#> A[2,]_shrinkage_scale 4.2037187 1.3057183   2.7846334    6.4127459
#> A[3,]_shrinkage_scale 3.9397433 1.6029548   2.2867105    6.8222458
#> A_global_scale        0.5090746 0.1935632   0.3376099    0.9186670
#> 
#> 
```
