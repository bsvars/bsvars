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
#>             mean         sd 5% quantile 95% quantile
#> B[1,1] 0.9014852 0.04761176   0.8370849    0.9755561
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -13.84954 1.471242   -15.63182    -11.49321
#> B[2,2]  26.30151 2.778722    22.03868     29.72077
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -23.973117 3.0121821  -28.446783   -19.546220
#> B[3,2]  -9.387167 1.3814414  -11.562553    -7.745330
#> B[3,3]   4.454882 0.5356209    3.646738     5.277948
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.988194381 0.01420223  0.96739208   1.00938963
#> lag1_var2 -0.006817076 0.02251535 -0.03982698   0.02446020
#> lag1_var3 -0.119128883 0.01731301 -0.14339257  -0.09114892
#> const      0.031385067 0.18045031 -0.22930438   0.26979971
#> 
#> $A$equation2
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.002447902 0.01673029 -0.03394412  0.015833323
#> lag1_var2  0.978693143 0.01366916  0.95825314  0.996245605
#> lag1_var3 -0.067082825 0.02127130 -0.09158115 -0.019726130
#> const     -0.159580822 0.12162907 -0.36512751  0.002627932
#> 
#> $A$equation3
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.2888432 0.08208022  0.16948913    0.4045256
#> lag1_var2  0.1297106 0.13825009 -0.07466392    0.3274477
#> lag1_var3 -0.2517527 0.10825443 -0.41109165   -0.1420077
#> const      1.7486550 1.10883698 -0.07359455    3.1952987
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        53.93075  27.72985    15.85971     96.63204
#> B[2,]_shrinkage       155.17792  70.05367    85.57759    285.20564
#> B[3,]_shrinkage       155.94513  70.48152    59.84251    253.91530
#> B[1,]_shrinkage_scale 549.97589 228.16350   258.36175    907.38951
#> B[2,]_shrinkage_scale 805.81866 436.01484   324.84329   1312.50268
#> B[3,]_shrinkage_scale 827.47056 378.42681   299.22009   1264.60583
#> B_global_scale         69.46559  30.36773    25.51776    117.90987
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.4532666 0.2747942   0.1699936    0.8577125
#> A[2,]_shrinkage       0.6252758 0.4785711   0.2255387    1.1648535
#> A[3,]_shrinkage       1.2736705 0.5951927   0.6162898    2.1693282
#> A[1,]_shrinkage_scale 5.0345683 1.6451767   2.9265072    8.3588304
#> A[2,]_shrinkage_scale 6.2189126 2.4047309   3.5662781   11.4408306
#> A[3,]_shrinkage_scale 8.7869766 3.6125068   4.6369545   16.6781454
#> A_global_scale        0.7280418 0.1871912   0.5590932    1.1051042
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
#>             mean         sd 5% quantile 95% quantile
#> B[1,1] 0.9014852 0.04761176   0.8370849    0.9755561
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -13.84954 1.471242   -15.63182    -11.49321
#> B[2,2]  26.30151 2.778722    22.03868     29.72077
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -23.973117 3.0121821  -28.446783   -19.546220
#> B[3,2]  -9.387167 1.3814414  -11.562553    -7.745330
#> B[3,3]   4.454882 0.5356209    3.646738     5.277948
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.988194381 0.01420223  0.96739208   1.00938963
#> lag1_var2 -0.006817076 0.02251535 -0.03982698   0.02446020
#> lag1_var3 -0.119128883 0.01731301 -0.14339257  -0.09114892
#> const      0.031385067 0.18045031 -0.22930438   0.26979971
#> 
#> $A$equation2
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.002447902 0.01673029 -0.03394412  0.015833323
#> lag1_var2  0.978693143 0.01366916  0.95825314  0.996245605
#> lag1_var3 -0.067082825 0.02127130 -0.09158115 -0.019726130
#> const     -0.159580822 0.12162907 -0.36512751  0.002627932
#> 
#> $A$equation3
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.2888432 0.08208022  0.16948913    0.4045256
#> lag1_var2  0.1297106 0.13825009 -0.07466392    0.3274477
#> lag1_var3 -0.2517527 0.10825443 -0.41109165   -0.1420077
#> const      1.7486550 1.10883698 -0.07359455    3.1952987
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        53.93075  27.72985    15.85971     96.63204
#> B[2,]_shrinkage       155.17792  70.05367    85.57759    285.20564
#> B[3,]_shrinkage       155.94513  70.48152    59.84251    253.91530
#> B[1,]_shrinkage_scale 549.97589 228.16350   258.36175    907.38951
#> B[2,]_shrinkage_scale 805.81866 436.01484   324.84329   1312.50268
#> B[3,]_shrinkage_scale 827.47056 378.42681   299.22009   1264.60583
#> B_global_scale         69.46559  30.36773    25.51776    117.90987
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.4532666 0.2747942   0.1699936    0.8577125
#> A[2,]_shrinkage       0.6252758 0.4785711   0.2255387    1.1648535
#> A[3,]_shrinkage       1.2736705 0.5951927   0.6162898    2.1693282
#> A[1,]_shrinkage_scale 5.0345683 1.6451767   2.9265072    8.3588304
#> A[2,]_shrinkage_scale 6.2189126 2.4047309   3.5662781   11.4408306
#> A[3,]_shrinkage_scale 8.7869766 3.6125068   4.6369545   16.6781454
#> A_global_scale        0.7280418 0.1871912   0.5590932    1.1051042
#> 
#> 
```
