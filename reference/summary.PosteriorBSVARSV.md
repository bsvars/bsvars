# Provides posterior summary of heteroskedastic Structural VAR estimation

Provides posterior mean, standard deviations, as well as 5 and 95
percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper parameters.

## Usage

``` r
# S3 method for class 'PosteriorBSVARSV'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorBSVARSV obtained using the
  [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md)
  function applied to heteroskedastic Bayesian Structural VAR model
  specification set by function `specify_bsvar_sv$new()` containing
  draws from the posterior distribution of the parameters.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper-parameters.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_bsvar_sv`](https://bsvars.org/bsvars/reference/specify_bsvar_sv.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
set.seed(123)
specification  = specify_bsvar_sv$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.

# run the burn-in
burn_in        = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
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
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
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
#> B[1,1] 0.8880404 0.03234065   0.8479733    0.9383081
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -20.17502 0.9693794   -21.42646    -18.71845
#> B[2,2]  37.08301 1.7993540    34.29442     39.46582
#> 
#> $B$equation3
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -34.59738 2.382150  -38.686991   -31.321648
#> B[3,2] -24.09785 2.846351  -28.638397   -18.278661
#> B[3,3]   7.30565 0.452349    6.707626     8.106959
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.899967561 0.03551699  0.85464012  0.943045930
#> lag1_var2 -0.014320738 0.01393344 -0.03390220  0.009389615
#> lag1_var3 -0.006007665 0.04775672 -0.06107548  0.060241145
#> const     -0.029455152 0.10113999 -0.15418754  0.112781089
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.03198116 0.03029792 -0.07762588  0.008155534
#> lag1_var2  0.95879244 0.01572494  0.93690606  0.978005177
#> lag1_var3 -0.02573547 0.03702043 -0.07259681  0.033201972
#> const     -0.30691405 0.11850837 -0.45941175 -0.152133242
#> 
#> $A$equation3
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.2444069 0.21914051  -0.5450416   0.03577259
#> lag1_var2 -0.1149835 0.09135831  -0.2495413   0.02049132
#> lag1_var3  0.4606354 0.27921693   0.1260540   0.95131199
#> const     -0.3449846 0.59024236  -1.2004776   0.46610615
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean         sd 5% quantile 95% quantile
#> B[1,]_shrinkage        250.3652  162.52073    91.61837     506.3389
#> B[2,]_shrinkage        459.1242  214.70685   204.45730     791.4350
#> B[3,]_shrinkage        367.9512  122.51937   189.39306     579.1157
#> B[1,]_shrinkage_scale 2235.7355  811.03623  1398.34069    3582.9416
#> B[2,]_shrinkage_scale 2861.2018 1128.14505  1568.09236    4895.4584
#> B[3,]_shrinkage_scale 2690.1121  977.43651  1671.00497    4174.6417
#> B_global_scale         261.2355   85.48032   124.23103     427.0576
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.4372191 0.1811184   0.2237841    0.7001949
#> A[2,]_shrinkage       0.3298539 0.1469032   0.1706468    0.6286073
#> A[3,]_shrinkage       0.5421574 0.3199806   0.2761048    1.3019186
#> A[1,]_shrinkage_scale 5.2433237 1.3672152   3.5632384    7.2064302
#> A[2,]_shrinkage_scale 4.5462876 1.4383627   2.4604007    6.7931260
#> A[3,]_shrinkage_scale 5.8194970 2.2453102   3.4039836   11.1139229
#> A_global_scale        0.6070179 0.1382992   0.4596807    0.8676449
#> 
#> 

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar_sv$new() |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  summary()
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
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
#>             mean         sd 5% quantile 95% quantile
#> B[1,1] 0.8880404 0.03234065   0.8479733    0.9383081
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -20.17502 0.9693794   -21.42646    -18.71845
#> B[2,2]  37.08301 1.7993540    34.29442     39.46582
#> 
#> $B$equation3
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -34.59738 2.382150  -38.686991   -31.321648
#> B[3,2] -24.09785 2.846351  -28.638397   -18.278661
#> B[3,3]   7.30565 0.452349    6.707626     8.106959
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.899967561 0.03551699  0.85464012  0.943045930
#> lag1_var2 -0.014320738 0.01393344 -0.03390220  0.009389615
#> lag1_var3 -0.006007665 0.04775672 -0.06107548  0.060241145
#> const     -0.029455152 0.10113999 -0.15418754  0.112781089
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.03198116 0.03029792 -0.07762588  0.008155534
#> lag1_var2  0.95879244 0.01572494  0.93690606  0.978005177
#> lag1_var3 -0.02573547 0.03702043 -0.07259681  0.033201972
#> const     -0.30691405 0.11850837 -0.45941175 -0.152133242
#> 
#> $A$equation3
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.2444069 0.21914051  -0.5450416   0.03577259
#> lag1_var2 -0.1149835 0.09135831  -0.2495413   0.02049132
#> lag1_var3  0.4606354 0.27921693   0.1260540   0.95131199
#> const     -0.3449846 0.59024236  -1.2004776   0.46610615
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean         sd 5% quantile 95% quantile
#> B[1,]_shrinkage        250.3652  162.52073    91.61837     506.3389
#> B[2,]_shrinkage        459.1242  214.70685   204.45730     791.4350
#> B[3,]_shrinkage        367.9512  122.51937   189.39306     579.1157
#> B[1,]_shrinkage_scale 2235.7355  811.03623  1398.34069    3582.9416
#> B[2,]_shrinkage_scale 2861.2018 1128.14505  1568.09236    4895.4584
#> B[3,]_shrinkage_scale 2690.1121  977.43651  1671.00497    4174.6417
#> B_global_scale         261.2355   85.48032   124.23103     427.0576
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.4372191 0.1811184   0.2237841    0.7001949
#> A[2,]_shrinkage       0.3298539 0.1469032   0.1706468    0.6286073
#> A[3,]_shrinkage       0.5421574 0.3199806   0.2761048    1.3019186
#> A[1,]_shrinkage_scale 5.2433237 1.3672152   3.5632384    7.2064302
#> A[2,]_shrinkage_scale 4.5462876 1.4383627   2.4604007    6.7931260
#> A[3,]_shrinkage_scale 5.8194970 2.2453102   3.4039836   11.1139229
#> A_global_scale        0.6070179 0.1382992   0.4596807    0.8676449
#> 
#> 
```
