# Provides posterior summary of homoskedastic Structural VAR estimation

Provides posterior mean, standard deviations, as well as 5 and 95
percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper parameters.

## Usage

``` r
# S3 method for class 'PosteriorBSVAR'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorBSVAR obtained using the
  [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md)
  function applied to homoskedastic Bayesian Structural VAR model
  specification set by function `specify_bsvar$new()` containing draws
  from the posterior distribution of the parameters.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper-parameters.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_bsvar`](https://bsvars.org/bsvars/reference/specify_bsvar.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
set.seed(123)
specification  = specify_bsvar$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.

# run the burn-in
burn_in        = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#>  s: 0
#>  s: 1
#>  s: 2
#>  s: 3
#>  s: 4
#>  s: 5
#>  s: 6
#>  s: 7
#>  s: 8
#>  s: 9

# estimate the model
posterior      = estimate(burn_in, 20)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 20 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#>  s: 0
#>  s: 1
#>  s: 2
#>  s: 3
#>  s: 4
#>  s: 5
#>  s: 6
#>  s: 7
#>  s: 8
#>  s: 9
#>  s: 10
#>  s: 11
#>  s: 12
#>  s: 13
#>  s: 14
#>  s: 15
#>  s: 16
#>  s: 17
#>  s: 18
#>  s: 19
summary(posterior)
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Posterior summary of the parameters             |
#>  **************************************************|
#> $B
#> $B$equation1
#>            mean       sd 5% quantile 95% quantile
#> B[1,1] 35.13751 1.662341    32.27821     37.50053
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1]  0.203966 2.655375   -4.348831     3.947681
#> B[2,2] 39.740603 2.372283   36.592392    44.182503
#> 
#> $B$equation3
#>               mean       sd 5% quantile 95% quantile
#> B[3,1] -14.4700193 2.014671  -17.385533   -10.647916
#> B[3,2]   0.1415374 2.833932   -3.523736     5.518732
#> B[3,3]  97.3047726 4.439215   89.751257   103.234698
#> 
#> 
#> $A
#> $A$equation1
#>                    mean         sd 5% quantile 95% quantile
#> lag1_var1  0.9167708118 0.01640180  0.88214215   0.93203902
#> lag1_var2 -0.0004550805 0.01615128 -0.01989142   0.02228304
#> lag1_var3  0.1001715940 0.01759817  0.08269715   0.13531303
#> const     -0.0318755378 0.10709924 -0.16898375   0.11080539
#> 
#> $A$equation2
#>                  mean         sd  5% quantile 95% quantile
#> lag1_var1 -0.01833963 0.01597960 -0.046712831  0.002780159
#> lag1_var2  0.95086407 0.01089483  0.938073012  0.965589069
#> lag1_var3  0.03082624 0.01865446  0.006820681  0.065005874
#> const     -0.42743084 0.08489123 -0.545315764 -0.320154885
#> 
#> $A$equation3
#>                   mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.018877234 0.007530174 -0.03313477 -0.007254350
#> lag1_var2 -0.006350861 0.005097487 -0.01318196  0.002060092
#> lag1_var3  1.021516048 0.009286359  1.00772587  1.038552888
#> const     -0.075791286 0.033186019 -0.11701377 -0.031810667
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        510.2088  263.0525   168.07034    1038.1049
#> B[2,]_shrinkage        452.0448  305.6327   197.26518    1073.6906
#> B[3,]_shrinkage       1699.6873  770.4732   626.17848    2934.4914
#> B[1,]_shrinkage_scale 4045.5003 2731.9036   981.13487    9372.5363
#> B[2,]_shrinkage_scale 4012.7786 3050.4982   772.60778   10132.2920
#> B[3,]_shrinkage_scale 5186.2669 3512.8791  1024.76310   11525.6665
#> B_global_scale         413.3765  282.8363    77.45248     992.7636
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3568490 0.1772647   0.1334462    0.6404623
#> A[2,]_shrinkage       0.6151523 0.2447963   0.3701411    1.1217956
#> A[3,]_shrinkage       0.3470430 0.1864993   0.1611084    0.6624150
#> A[1,]_shrinkage_scale 4.4528489 1.7080962   1.8428982    7.2273215
#> A[2,]_shrinkage_scale 6.7418749 1.7549855   4.5590076    9.7030230
#> A[3,]_shrinkage_scale 3.9009423 1.7932977   1.8901989    6.6540362
#> A_global_scale        0.5848222 0.1364776   0.4156844    0.7342223
#> 
#> 

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar$new() |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  summary()
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#>  s: 0
#>  s: 1
#>  s: 2
#>  s: 3
#>  s: 4
#>  s: 5
#>  s: 6
#>  s: 7
#>  s: 8
#>  s: 9
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 20 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#>  s: 0
#>  s: 1
#>  s: 2
#>  s: 3
#>  s: 4
#>  s: 5
#>  s: 6
#>  s: 7
#>  s: 8
#>  s: 9
#>  s: 10
#>  s: 11
#>  s: 12
#>  s: 13
#>  s: 14
#>  s: 15
#>  s: 16
#>  s: 17
#>  s: 18
#>  s: 19
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Posterior summary of the parameters             |
#>  **************************************************|
#> $B
#> $B$equation1
#>            mean       sd 5% quantile 95% quantile
#> B[1,1] 35.13751 1.662341    32.27821     37.50053
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1]  0.203966 2.655375   -4.348831     3.947681
#> B[2,2] 39.740603 2.372283   36.592392    44.182503
#> 
#> $B$equation3
#>               mean       sd 5% quantile 95% quantile
#> B[3,1] -14.4700193 2.014671  -17.385533   -10.647916
#> B[3,2]   0.1415374 2.833932   -3.523736     5.518732
#> B[3,3]  97.3047726 4.439215   89.751257   103.234698
#> 
#> 
#> $A
#> $A$equation1
#>                    mean         sd 5% quantile 95% quantile
#> lag1_var1  0.9167708118 0.01640180  0.88214215   0.93203902
#> lag1_var2 -0.0004550805 0.01615128 -0.01989142   0.02228304
#> lag1_var3  0.1001715940 0.01759817  0.08269715   0.13531303
#> const     -0.0318755378 0.10709924 -0.16898375   0.11080539
#> 
#> $A$equation2
#>                  mean         sd  5% quantile 95% quantile
#> lag1_var1 -0.01833963 0.01597960 -0.046712831  0.002780159
#> lag1_var2  0.95086407 0.01089483  0.938073012  0.965589069
#> lag1_var3  0.03082624 0.01865446  0.006820681  0.065005874
#> const     -0.42743084 0.08489123 -0.545315764 -0.320154885
#> 
#> $A$equation3
#>                   mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.018877234 0.007530174 -0.03313477 -0.007254350
#> lag1_var2 -0.006350861 0.005097487 -0.01318196  0.002060092
#> lag1_var3  1.021516048 0.009286359  1.00772587  1.038552888
#> const     -0.075791286 0.033186019 -0.11701377 -0.031810667
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        510.2088  263.0525   168.07034    1038.1049
#> B[2,]_shrinkage        452.0448  305.6327   197.26518    1073.6906
#> B[3,]_shrinkage       1699.6873  770.4732   626.17848    2934.4914
#> B[1,]_shrinkage_scale 4045.5003 2731.9036   981.13487    9372.5363
#> B[2,]_shrinkage_scale 4012.7786 3050.4982   772.60778   10132.2920
#> B[3,]_shrinkage_scale 5186.2669 3512.8791  1024.76310   11525.6665
#> B_global_scale         413.3765  282.8363    77.45248     992.7636
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3568490 0.1772647   0.1334462    0.6404623
#> A[2,]_shrinkage       0.6151523 0.2447963   0.3701411    1.1217956
#> A[3,]_shrinkage       0.3470430 0.1864993   0.1611084    0.6624150
#> A[1,]_shrinkage_scale 4.4528489 1.7080962   1.8428982    7.2273215
#> A[2,]_shrinkage_scale 6.7418749 1.7549855   4.5590076    9.7030230
#> A[3,]_shrinkage_scale 3.9009423 1.7932977   1.8901989    6.6540362
#> A_global_scale        0.5848222 0.1364776   0.4156844    0.7342223
#> 
#> 
```
