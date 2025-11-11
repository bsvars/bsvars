# Provides posterior summary of Structural VAR with t-distributed shocks estimation

Provides posterior mean, standard deviations, as well as 5 and 95
percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, hyper-parameters, and Student-t
degrees-of-freedom parameter \\\nu\\.

## Usage

``` r
# S3 method for class 'PosteriorBSVART'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorBSVART obtained using the
  [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md)
  function applied to homoskedastic Bayesian Structural VAR model
  specification set by function `specify_bsvar$new()` containing draws
  from the posterior distribution of the parameters.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, hyper-parameters, and Student-t
degrees-of-freedom parameter \\\nu\\.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_bsvar_t`](https://bsvars.org/bsvars/reference/specify_bsvar_t.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
set.seed(123)
specification  = specify_bsvar_t$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.

# run the burn-in
burn_in        = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#>     with t-distributed structural skocks          |
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
#>  Gibbs sampler for the SVAR model                 |
#>     with t-distributed structural skocks          |
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
#>            mean        sd 5% quantile 95% quantile
#> B[1,1] 4.222967 0.1964742    3.989853      4.49404
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1]  6.593547 0.5411135    5.749397     7.214492
#> B[2,2] 37.587160 2.3214388   33.820026    41.254613
#> 
#> $B$equation3
#>              mean       sd 5% quantile 95% quantile
#> B[3,1] -42.244748 3.267630  -47.804792    -39.16199
#> B[3,2]   5.810698 1.953078    3.585059      9.35430
#> B[3,3]  26.332988 3.040438   22.959886     31.05234
#> 
#> 
#> $A
#> $A$equation1
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  1.1201885 0.04588607   1.0582065    1.1801962
#> lag1_var2 -0.4833022 0.01867071  -0.5098365   -0.4633290
#> lag1_var3  0.3526924 0.04764077   0.2900609    0.4170120
#> const     -0.9429959 0.10942846  -1.1110774   -0.8121359
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.04308159 0.01596251 -0.06073241 -0.018898082
#> lag1_var2  1.05439557 0.01032555  1.03620953  1.067885583
#> lag1_var3 -0.03018862 0.02099680 -0.06534685 -0.004031450
#> const     -0.10434944 0.06923318 -0.22121529 -0.007137917
#> 
#> $A$equation3
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.2693696 0.08999488   0.1763414    0.3957086
#> lag1_var2 -0.7744645 0.06863910  -0.8671303   -0.6761031
#> lag1_var3  1.4979477 0.06604537   1.4018902    1.5816431
#> const     -1.2937179 0.27372126  -1.6650121   -0.8391539
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        248.0825  332.9210    55.23892     979.4765
#> B[2,]_shrinkage        454.8399  220.0051   187.27918     735.6476
#> B[3,]_shrinkage        529.0521  447.9865   181.20646    1185.2744
#> B[1,]_shrinkage_scale 2145.8668 2118.9236   641.70027    5331.3981
#> B[2,]_shrinkage_scale 2472.7233 1400.2042  1102.23454    4612.6050
#> B[3,]_shrinkage_scale 2473.9050 1510.6194   888.15990    5530.5929
#> B_global_scale         209.7715  127.9414    86.86448     421.5153
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.6079679 0.2813793   0.3156344     0.957362
#> A[2,]_shrinkage       0.6615013 0.3375035   0.2807152     1.210557
#> A[3,]_shrinkage       0.9621696 0.5610722   0.3799474     1.797524
#> A[1,]_shrinkage_scale 6.6547005 1.8431775   4.0259320     9.333616
#> A[2,]_shrinkage_scale 7.4279970 3.0960142   4.2797804    13.331077
#> A[3,]_shrinkage_scale 7.5908333 2.2848186   4.8216838    11.471378
#> A_global_scale        0.8145013 0.1667969   0.5890240     1.055296
#> 
#> 
#> $df
#>         mean           sd  5% quantile 95% quantile 
#>            3            0            3            3 
#> 

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar_t$new() |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  summary()
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#>     with t-distributed structural skocks          |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#>     with t-distributed structural skocks          |
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
#>            mean        sd 5% quantile 95% quantile
#> B[1,1] 4.222967 0.1964742    3.989853      4.49404
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1]  6.593547 0.5411135    5.749397     7.214492
#> B[2,2] 37.587160 2.3214388   33.820026    41.254613
#> 
#> $B$equation3
#>              mean       sd 5% quantile 95% quantile
#> B[3,1] -42.244748 3.267630  -47.804792    -39.16199
#> B[3,2]   5.810698 1.953078    3.585059      9.35430
#> B[3,3]  26.332988 3.040438   22.959886     31.05234
#> 
#> 
#> $A
#> $A$equation1
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  1.1201885 0.04588607   1.0582065    1.1801962
#> lag1_var2 -0.4833022 0.01867071  -0.5098365   -0.4633290
#> lag1_var3  0.3526924 0.04764077   0.2900609    0.4170120
#> const     -0.9429959 0.10942846  -1.1110774   -0.8121359
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.04308159 0.01596251 -0.06073241 -0.018898082
#> lag1_var2  1.05439557 0.01032555  1.03620953  1.067885583
#> lag1_var3 -0.03018862 0.02099680 -0.06534685 -0.004031450
#> const     -0.10434944 0.06923318 -0.22121529 -0.007137917
#> 
#> $A$equation3
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.2693696 0.08999488   0.1763414    0.3957086
#> lag1_var2 -0.7744645 0.06863910  -0.8671303   -0.6761031
#> lag1_var3  1.4979477 0.06604537   1.4018902    1.5816431
#> const     -1.2937179 0.27372126  -1.6650121   -0.8391539
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        248.0825  332.9210    55.23892     979.4765
#> B[2,]_shrinkage        454.8399  220.0051   187.27918     735.6476
#> B[3,]_shrinkage        529.0521  447.9865   181.20646    1185.2744
#> B[1,]_shrinkage_scale 2145.8668 2118.9236   641.70027    5331.3981
#> B[2,]_shrinkage_scale 2472.7233 1400.2042  1102.23454    4612.6050
#> B[3,]_shrinkage_scale 2473.9050 1510.6194   888.15990    5530.5929
#> B_global_scale         209.7715  127.9414    86.86448     421.5153
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.6079679 0.2813793   0.3156344     0.957362
#> A[2,]_shrinkage       0.6615013 0.3375035   0.2807152     1.210557
#> A[3,]_shrinkage       0.9621696 0.5610722   0.3799474     1.797524
#> A[1,]_shrinkage_scale 6.6547005 1.8431775   4.0259320     9.333616
#> A[2,]_shrinkage_scale 7.4279970 3.0960142   4.2797804    13.331077
#> A[3,]_shrinkage_scale 7.5908333 2.2848186   4.8216838    11.471378
#> A_global_scale        0.8145013 0.1667969   0.5890240     1.055296
#> 
#> 
#> $df
#>         mean           sd  5% quantile 95% quantile 
#>            3            0            3            3 
#> 
```
