# Provides posterior summary of heteroskedastic Structural VAR estimation

Provides posterior mean, standard deviations, as well as 5 and 95
percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper parameters.

## Usage

``` r
# S3 method for class 'PosteriorBSVARMSH'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorBSVARMSH obtained using the
  [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md)
  function applied to heteroskedastic Bayesian Structural VAR model
  specification set by function `specify_bsvar_msh$new()` containing
  draws from the posterior distribution of the parameters.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper-parameters.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_bsvar_msh`](https://bsvars.org/bsvars/reference/specify_bsvar_msh.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
set.seed(123)
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
#>            mean         sd 5% quantile 95% quantile
#> B[1,1] 0.905559 0.04765416   0.8401119    0.9666544
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -18.24134 1.222942   -19.86753    -16.43178
#> B[2,2]  34.13613 2.281377    30.70614     37.28311
#> 
#> $B$equation3
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -32.83921 2.123394  -36.018317   -30.040749
#> B[3,2] -16.08219 1.536692  -17.968571   -13.995490
#> B[3,3]   6.45492 0.339832    5.972701     6.920447
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.871138101 0.02127644  0.83022010   0.90086559
#> lag1_var2 -0.004261879 0.01196716 -0.02255421   0.01318017
#> lag1_var3  0.005727761 0.02777996 -0.02995239   0.05824378
#> const     -0.107299499 0.11098347 -0.29827663   0.05619071
#> 
#> $A$equation2
#>                    mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.0627683560 0.01859482 -0.08523879  -0.03735508
#> lag1_var2  0.9652773755 0.01132283  0.95154202   0.98582857
#> lag1_var3  0.0004893731 0.02326335 -0.02802197   0.02901127
#> const     -0.3437180278 0.09697503 -0.46767103  -0.22263456
#> 
#> $A$equation3
#>                  mean        sd 5% quantile 95% quantile
#> lag1_var1 -0.48228780 0.1500522 -0.68800323  -0.25201524
#> lag1_var2  0.03938369 0.0404859 -0.02453474   0.09176835
#> lag1_var3  0.60277760 0.1911127  0.36560913   0.85261413
#> const     -0.06805704 0.3642663 -0.66043277   0.53352302
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        62.73660  21.41002    28.30505     93.04222
#> B[2,]_shrinkage       181.95075  63.57409   109.34169    290.93041
#> B[3,]_shrinkage       207.00506 104.55333    81.61105    449.56415
#> B[1,]_shrinkage_scale 653.55290 225.23994   363.07370   1007.20692
#> B[2,]_shrinkage_scale 767.96933 198.97013   458.14684   1081.48506
#> B[3,]_shrinkage_scale 763.00944 192.87495   483.22029   1108.38586
#> B_global_scale         63.99689  13.22256    41.82352     81.24698
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.2450042 0.07598887   0.1419385    0.3724916
#> A[2,]_shrinkage       0.3617389 0.18307322   0.1664882    0.7328828
#> A[3,]_shrinkage       0.4007919 0.20903195   0.1756399    0.8086641
#> A[1,]_shrinkage_scale 3.4403430 1.37194243   1.7001660    5.1360618
#> A[2,]_shrinkage_scale 4.4887431 2.02099829   2.4046754    9.3567792
#> A[3,]_shrinkage_scale 4.6239420 2.19980208   2.2788978    7.6073994
#> A_global_scale        0.5076981 0.12931529   0.3340523    0.6944414
#> 
#> 

# workflow with the pipe |>
############################################################
set.seed(123)
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
#>            mean         sd 5% quantile 95% quantile
#> B[1,1] 0.905559 0.04765416   0.8401119    0.9666544
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -18.24134 1.222942   -19.86753    -16.43178
#> B[2,2]  34.13613 2.281377    30.70614     37.28311
#> 
#> $B$equation3
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -32.83921 2.123394  -36.018317   -30.040749
#> B[3,2] -16.08219 1.536692  -17.968571   -13.995490
#> B[3,3]   6.45492 0.339832    5.972701     6.920447
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.871138101 0.02127644  0.83022010   0.90086559
#> lag1_var2 -0.004261879 0.01196716 -0.02255421   0.01318017
#> lag1_var3  0.005727761 0.02777996 -0.02995239   0.05824378
#> const     -0.107299499 0.11098347 -0.29827663   0.05619071
#> 
#> $A$equation2
#>                    mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.0627683560 0.01859482 -0.08523879  -0.03735508
#> lag1_var2  0.9652773755 0.01132283  0.95154202   0.98582857
#> lag1_var3  0.0004893731 0.02326335 -0.02802197   0.02901127
#> const     -0.3437180278 0.09697503 -0.46767103  -0.22263456
#> 
#> $A$equation3
#>                  mean        sd 5% quantile 95% quantile
#> lag1_var1 -0.48228780 0.1500522 -0.68800323  -0.25201524
#> lag1_var2  0.03938369 0.0404859 -0.02453474   0.09176835
#> lag1_var3  0.60277760 0.1911127  0.36560913   0.85261413
#> const     -0.06805704 0.3642663 -0.66043277   0.53352302
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        62.73660  21.41002    28.30505     93.04222
#> B[2,]_shrinkage       181.95075  63.57409   109.34169    290.93041
#> B[3,]_shrinkage       207.00506 104.55333    81.61105    449.56415
#> B[1,]_shrinkage_scale 653.55290 225.23994   363.07370   1007.20692
#> B[2,]_shrinkage_scale 767.96933 198.97013   458.14684   1081.48506
#> B[3,]_shrinkage_scale 763.00944 192.87495   483.22029   1108.38586
#> B_global_scale         63.99689  13.22256    41.82352     81.24698
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.2450042 0.07598887   0.1419385    0.3724916
#> A[2,]_shrinkage       0.3617389 0.18307322   0.1664882    0.7328828
#> A[3,]_shrinkage       0.4007919 0.20903195   0.1756399    0.8086641
#> A[1,]_shrinkage_scale 3.4403430 1.37194243   1.7001660    5.1360618
#> A[2,]_shrinkage_scale 4.4887431 2.02099829   2.4046754    9.3567792
#> A[3,]_shrinkage_scale 4.6239420 2.19980208   2.2788978    7.6073994
#> A_global_scale        0.5076981 0.12931529   0.3340523    0.6944414
#> 
#> 
```
