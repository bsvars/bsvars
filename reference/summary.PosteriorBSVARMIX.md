# Provides posterior summary of non-normal Structural VAR estimation

Provides posterior mean, standard deviations, as well as 5 and 95
percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper parameters.

## Usage

``` r
# S3 method for class 'PosteriorBSVARMIX'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorBSVARMIX obtained using the
  [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md)
  function applied to non-normal Bayesian Structural VAR model
  specification set by function `specify_bsvar_mix$new()` containing
  draws from the posterior distribution of the parameters.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper-parameters.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_bsvar_mix`](https://bsvars.org/bsvars/reference/specify_bsvar_mix.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
set.seed(123)
specification  = specify_bsvar_mix$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.

# run the burn-in
burn_in        = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
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
#>  Gibbs sampler for the SVAR-finiteMIX model             |
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
#> B[1,1] 0.8967006 0.03879291   0.8433696    0.9551249
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -18.85626 0.9746227   -20.33543    -17.28310
#> B[2,2]  35.14343 1.7429014    32.25311     37.35872
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -32.085299 3.0755051  -37.081978    -28.53957
#> B[3,2] -18.390197 1.8462261  -21.421983    -15.69987
#> B[3,3]   6.468515 0.5356921    5.870899      7.47901
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.945504968 0.03968795  0.89066284  0.992411975
#> lag1_var2 -0.016388984 0.01581058 -0.03857151  0.008955671
#> lag1_var3 -0.058719754 0.05357932 -0.12196471  0.018447222
#> const     -0.009105614 0.13889383 -0.22998939  0.185803215
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.03317368 0.02613627 -0.06295247   0.01138269
#> lag1_var2  0.94980710 0.01173394  0.93463655   0.96717528
#> lag1_var3 -0.02007414 0.03138173 -0.06495403   0.02314170
#> const     -0.36931443 0.07930524 -0.48444250  -0.26597363
#> 
#> $A$equation3
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.07324249 0.24349307  -0.4707120   0.26450632
#> lag1_var2 -0.07770681 0.07532946  -0.1781722   0.04882174
#> lag1_var3  0.27909970 0.32694268  -0.1747812   0.80721347
#> const      0.28062978 0.53767138  -0.6114248   0.90529272
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         72.96549  42.13777    18.80975     153.5424
#> B[2,]_shrinkage        268.64239 117.79267   154.37438     432.4578
#> B[3,]_shrinkage        247.43538 157.40591   105.41323     571.8471
#> B[1,]_shrinkage_scale  703.03709 268.66474   305.57230    1172.6337
#> B[2,]_shrinkage_scale 1127.29781 558.62716   504.68986    1939.7711
#> B[3,]_shrinkage_scale 1043.11854 565.57744   564.56926    2147.3494
#> B_global_scale          89.86399  31.57773    36.76980     134.5608
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3129552 0.18276142   0.1307058    0.6701249
#> A[2,]_shrinkage       0.4603458 0.18033249   0.2728004    0.8437418
#> A[3,]_shrinkage       0.5578918 0.48686457   0.2577584    1.2016304
#> A[1,]_shrinkage_scale 3.9606532 1.69461580   2.2483460    6.4860586
#> A[2,]_shrinkage_scale 4.8879340 1.02979093   3.7272382    6.3484314
#> A[3,]_shrinkage_scale 5.0302865 1.46938222   3.0881576    7.5713120
#> A_global_scale        0.5203514 0.09896409   0.3578862    0.6535425
#> 
#> 

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar_mix$new() |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  summary()
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
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
#> B[1,1] 0.8967006 0.03879291   0.8433696    0.9551249
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -18.85626 0.9746227   -20.33543    -17.28310
#> B[2,2]  35.14343 1.7429014    32.25311     37.35872
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -32.085299 3.0755051  -37.081978    -28.53957
#> B[3,2] -18.390197 1.8462261  -21.421983    -15.69987
#> B[3,3]   6.468515 0.5356921    5.870899      7.47901
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.945504968 0.03968795  0.89066284  0.992411975
#> lag1_var2 -0.016388984 0.01581058 -0.03857151  0.008955671
#> lag1_var3 -0.058719754 0.05357932 -0.12196471  0.018447222
#> const     -0.009105614 0.13889383 -0.22998939  0.185803215
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.03317368 0.02613627 -0.06295247   0.01138269
#> lag1_var2  0.94980710 0.01173394  0.93463655   0.96717528
#> lag1_var3 -0.02007414 0.03138173 -0.06495403   0.02314170
#> const     -0.36931443 0.07930524 -0.48444250  -0.26597363
#> 
#> $A$equation3
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.07324249 0.24349307  -0.4707120   0.26450632
#> lag1_var2 -0.07770681 0.07532946  -0.1781722   0.04882174
#> lag1_var3  0.27909970 0.32694268  -0.1747812   0.80721347
#> const      0.28062978 0.53767138  -0.6114248   0.90529272
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         72.96549  42.13777    18.80975     153.5424
#> B[2,]_shrinkage        268.64239 117.79267   154.37438     432.4578
#> B[3,]_shrinkage        247.43538 157.40591   105.41323     571.8471
#> B[1,]_shrinkage_scale  703.03709 268.66474   305.57230    1172.6337
#> B[2,]_shrinkage_scale 1127.29781 558.62716   504.68986    1939.7711
#> B[3,]_shrinkage_scale 1043.11854 565.57744   564.56926    2147.3494
#> B_global_scale          89.86399  31.57773    36.76980     134.5608
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3129552 0.18276142   0.1307058    0.6701249
#> A[2,]_shrinkage       0.4603458 0.18033249   0.2728004    0.8437418
#> A[3,]_shrinkage       0.5578918 0.48686457   0.2577584    1.2016304
#> A[1,]_shrinkage_scale 3.9606532 1.69461580   2.2483460    6.4860586
#> A[2,]_shrinkage_scale 4.8879340 1.02979093   3.7272382    6.3484314
#> A[3,]_shrinkage_scale 5.0302865 1.46938222   3.0881576    7.5713120
#> A_global_scale        0.5203514 0.09896409   0.3578862    0.6535425
#> 
#> 
```
