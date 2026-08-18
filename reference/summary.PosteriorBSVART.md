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
#> B[1,1] 6.874157 0.5727951    6.088172     7.673111
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1]  7.836857 0.8141895    6.728916     9.055742
#> B[2,2] 37.586310 3.0090169   33.105569    42.793989
#> 
#> $B$equation3
#>              mean       sd 5% quantile 95% quantile
#> B[3,1] -40.741416 2.131288  -43.055546   -36.953523
#> B[3,2]   4.293506 2.069149    1.772986     7.710992
#> B[3,3]  61.937427 3.941843   56.562602    67.931794
#> 
#> 
#> $A
#> $A$equation1
#>                 mean          sd 5% quantile 95% quantile
#> lag1_var1  1.2081301 0.029983943   1.1555770    1.2490144
#> lag1_var2 -0.4964526 0.009049639  -0.5085969   -0.4864345
#> lag1_var3  0.1486820 0.032594573   0.1080837    0.2021617
#> const     -1.8051261 0.126704409  -2.0130801   -1.6546147
#> 
#> $A$equation2
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.065680293 0.01186167 -0.08418947  -0.04360319
#> lag1_var2  1.076507539 0.01333718  1.05534593   1.09603967
#> lag1_var3  0.000117873 0.01539591 -0.02029663   0.02664165
#> const      0.132870116 0.08413076 -0.00299472   0.23501504
#> 
#> $A$equation3
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.1670583 0.02986867   0.1231209    0.2038507
#> lag1_var2 -0.3405645 0.02167798  -0.3746757   -0.3146420
#> lag1_var3  1.0717777 0.01861683   1.0471055    1.0975245
#> const     -1.2365188 0.05579230  -1.3192176   -1.1393586
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        241.0620  293.5613    24.93243     818.5321
#> B[2,]_shrinkage        337.0645  266.8272   157.83869     599.1109
#> B[3,]_shrinkage        713.4146  371.5052   334.94957    1360.6430
#> B[1,]_shrinkage_scale 1736.5659 1299.3166   459.73026    3172.2360
#> B[2,]_shrinkage_scale 2161.6278 1622.7257   558.19029    4808.2048
#> B[3,]_shrinkage_scale 2555.6215 1742.1132   704.38189    5845.7968
#> B_global_scale         206.3980  136.2975    54.47058     439.4526
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.9610685 0.3226963   0.4432092     1.483598
#> A[2,]_shrinkage       0.7597520 0.4117221   0.2864751     1.420687
#> A[3,]_shrinkage       0.9188517 0.6366080   0.3073197     2.257162
#> A[1,]_shrinkage_scale 8.8220047 3.4387099   4.6142399    13.042708
#> A[2,]_shrinkage_scale 9.2271003 3.7770771   4.9667734    14.755176
#> A[3,]_shrinkage_scale 7.9703695 3.3133032   3.9752469    12.795768
#> A_global_scale        0.9271225 0.2334228   0.6326487     1.285765
#> 
#> 
#> $df
#>         mean           sd  5% quantile 95% quantile 
#>    3.7499529    0.5473584    3.0691856    4.3848037 
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
#> B[1,1] 6.874157 0.5727951    6.088172     7.673111
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1]  7.836857 0.8141895    6.728916     9.055742
#> B[2,2] 37.586310 3.0090169   33.105569    42.793989
#> 
#> $B$equation3
#>              mean       sd 5% quantile 95% quantile
#> B[3,1] -40.741416 2.131288  -43.055546   -36.953523
#> B[3,2]   4.293506 2.069149    1.772986     7.710992
#> B[3,3]  61.937427 3.941843   56.562602    67.931794
#> 
#> 
#> $A
#> $A$equation1
#>                 mean          sd 5% quantile 95% quantile
#> lag1_var1  1.2081301 0.029983943   1.1555770    1.2490144
#> lag1_var2 -0.4964526 0.009049639  -0.5085969   -0.4864345
#> lag1_var3  0.1486820 0.032594573   0.1080837    0.2021617
#> const     -1.8051261 0.126704409  -2.0130801   -1.6546147
#> 
#> $A$equation2
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.065680293 0.01186167 -0.08418947  -0.04360319
#> lag1_var2  1.076507539 0.01333718  1.05534593   1.09603967
#> lag1_var3  0.000117873 0.01539591 -0.02029663   0.02664165
#> const      0.132870116 0.08413076 -0.00299472   0.23501504
#> 
#> $A$equation3
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.1670583 0.02986867   0.1231209    0.2038507
#> lag1_var2 -0.3405645 0.02167798  -0.3746757   -0.3146420
#> lag1_var3  1.0717777 0.01861683   1.0471055    1.0975245
#> const     -1.2365188 0.05579230  -1.3192176   -1.1393586
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        241.0620  293.5613    24.93243     818.5321
#> B[2,]_shrinkage        337.0645  266.8272   157.83869     599.1109
#> B[3,]_shrinkage        713.4146  371.5052   334.94957    1360.6430
#> B[1,]_shrinkage_scale 1736.5659 1299.3166   459.73026    3172.2360
#> B[2,]_shrinkage_scale 2161.6278 1622.7257   558.19029    4808.2048
#> B[3,]_shrinkage_scale 2555.6215 1742.1132   704.38189    5845.7968
#> B_global_scale         206.3980  136.2975    54.47058     439.4526
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.9610685 0.3226963   0.4432092     1.483598
#> A[2,]_shrinkage       0.7597520 0.4117221   0.2864751     1.420687
#> A[3,]_shrinkage       0.9188517 0.6366080   0.3073197     2.257162
#> A[1,]_shrinkage_scale 8.8220047 3.4387099   4.6142399    13.042708
#> A[2,]_shrinkage_scale 9.2271003 3.7770771   4.9667734    14.755176
#> A[3,]_shrinkage_scale 7.9703695 3.3133032   3.9752469    12.795768
#> A_global_scale        0.9271225 0.2334228   0.6326487     1.285765
#> 
#> 
#> $df
#>         mean           sd  5% quantile 95% quantile 
#>    3.7499529    0.5473584    3.0691856    4.3848037 
#> 
```
