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
#> B[1,1] 4.291442 0.2263348    3.967889     4.631731
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1]  7.106019 0.5340295     6.35443     7.863146
#> B[2,2] 35.245359 1.9727200    32.87128    38.445668
#> 
#> $B$equation3
#>              mean       sd 5% quantile 95% quantile
#> B[3,1] -43.804877 3.850641  -50.834308   -39.816151
#> B[3,2]   4.800904 1.995549    2.021863     7.672945
#> B[3,3]  25.754540 2.830211   22.771364    30.817280
#> 
#> 
#> $A
#> $A$equation1
#>                 mean          sd 5% quantile 95% quantile
#> lag1_var1  0.9202501 0.014823580   0.8984030   0.94220284
#> lag1_var2 -0.3868765 0.009219533  -0.4034938  -0.37677036
#> lag1_var3  0.5774997 0.018185517   0.5518274   0.60242159
#> const     -0.1754493 0.089349636  -0.3347188  -0.08151862
#> 
#> $A$equation2
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.01301584 0.011302323 -0.03081566  0.002765722
#> lag1_var2  1.04742740 0.009390958  1.03652294  1.059832449
#> lag1_var3 -0.07591845 0.015927405 -0.09656774 -0.049866691
#> const     -0.23696075 0.064991791 -0.31963739 -0.130684069
#> 
#> $A$equation3
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.05782055 0.02137966  -0.0852735  -0.03121736
#> lag1_var2 -0.63401548 0.02599957  -0.6804676  -0.59825376
#> lag1_var3  1.90600030 0.05453960   1.8323608   1.98899845
#> const      0.09897185 0.13014300  -0.1378877   0.22091244
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         81.50130  69.96320    28.56362     159.1066
#> B[2,]_shrinkage        238.51343 116.80879   100.70195     422.7964
#> B[3,]_shrinkage        302.05388 137.58233   118.47011     534.4334
#> B[1,]_shrinkage_scale  788.75176 533.12293   324.95963    1834.4692
#> B[2,]_shrinkage_scale  973.75138 536.42454   326.27158    1737.1093
#> B[3,]_shrinkage_scale 1089.84436 563.62341   480.92471    2212.7035
#> B_global_scale          91.40884  57.92974    36.24661     193.4078
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.5687436 0.3912574   0.2257400    1.2521293
#> A[2,]_shrinkage       0.4414599 0.2935231   0.1430329    1.0146412
#> A[3,]_shrinkage       0.4726430 0.1992010   0.2402957    0.7810567
#> A[1,]_shrinkage_scale 5.7316146 2.1350591   3.1474089    9.0968993
#> A[2,]_shrinkage_scale 5.4479443 2.9000498   2.0956174   12.0345288
#> A[3,]_shrinkage_scale 5.0556677 1.5467299   2.6130485    7.5921862
#> A_global_scale        0.6176345 0.1921040   0.3629447    0.9227255
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
#> B[1,1] 4.291442 0.2263348    3.967889     4.631731
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1]  7.106019 0.5340295     6.35443     7.863146
#> B[2,2] 35.245359 1.9727200    32.87128    38.445668
#> 
#> $B$equation3
#>              mean       sd 5% quantile 95% quantile
#> B[3,1] -43.804877 3.850641  -50.834308   -39.816151
#> B[3,2]   4.800904 1.995549    2.021863     7.672945
#> B[3,3]  25.754540 2.830211   22.771364    30.817280
#> 
#> 
#> $A
#> $A$equation1
#>                 mean          sd 5% quantile 95% quantile
#> lag1_var1  0.9202501 0.014823580   0.8984030   0.94220284
#> lag1_var2 -0.3868765 0.009219533  -0.4034938  -0.37677036
#> lag1_var3  0.5774997 0.018185517   0.5518274   0.60242159
#> const     -0.1754493 0.089349636  -0.3347188  -0.08151862
#> 
#> $A$equation2
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.01301584 0.011302323 -0.03081566  0.002765722
#> lag1_var2  1.04742740 0.009390958  1.03652294  1.059832449
#> lag1_var3 -0.07591845 0.015927405 -0.09656774 -0.049866691
#> const     -0.23696075 0.064991791 -0.31963739 -0.130684069
#> 
#> $A$equation3
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.05782055 0.02137966  -0.0852735  -0.03121736
#> lag1_var2 -0.63401548 0.02599957  -0.6804676  -0.59825376
#> lag1_var3  1.90600030 0.05453960   1.8323608   1.98899845
#> const      0.09897185 0.13014300  -0.1378877   0.22091244
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         81.50130  69.96320    28.56362     159.1066
#> B[2,]_shrinkage        238.51343 116.80879   100.70195     422.7964
#> B[3,]_shrinkage        302.05388 137.58233   118.47011     534.4334
#> B[1,]_shrinkage_scale  788.75176 533.12293   324.95963    1834.4692
#> B[2,]_shrinkage_scale  973.75138 536.42454   326.27158    1737.1093
#> B[3,]_shrinkage_scale 1089.84436 563.62341   480.92471    2212.7035
#> B_global_scale          91.40884  57.92974    36.24661     193.4078
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.5687436 0.3912574   0.2257400    1.2521293
#> A[2,]_shrinkage       0.4414599 0.2935231   0.1430329    1.0146412
#> A[3,]_shrinkage       0.4726430 0.1992010   0.2402957    0.7810567
#> A[1,]_shrinkage_scale 5.7316146 2.1350591   3.1474089    9.0968993
#> A[2,]_shrinkage_scale 5.4479443 2.9000498   2.0956174   12.0345288
#> A[3,]_shrinkage_scale 5.0556677 1.5467299   2.6130485    7.5921862
#> A_global_scale        0.6176345 0.1921040   0.3629447    0.9227255
#> 
#> 
#> $df
#>         mean           sd  5% quantile 95% quantile 
#>            3            0            3            3 
#> 
```
