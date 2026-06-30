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
#> B[1,1] 6.807055 0.4334005    6.260366     7.482888
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1]  9.689544 0.780879    8.494743     10.68037
#> B[2,2] 35.862940 2.132174   33.354895     39.01922
#> 
#> $B$equation3
#>              mean       sd 5% quantile 95% quantile
#> B[3,1] -39.201311 2.642121   -43.16831    -35.48670
#> B[3,2]   4.394354 1.797714     2.24981      6.37283
#> B[3,3]  49.731485 4.331414    44.16253     57.33723
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.77776960 0.01724371   0.7516370    0.8006332
#> lag1_var2 -0.27411649 0.01065245  -0.2935573   -0.2593147
#> lag1_var3  0.63832071 0.02781919   0.5937239    0.6734300
#> const      0.02698535 0.09153624  -0.1301465    0.1482897
#> 
#> $A$equation2
#>                  mean         sd  5% quantile 95% quantile
#> lag1_var1  0.02840277 0.01231118  0.007357957   0.04961415
#> lag1_var2  1.04597141 0.01012515  1.032878846   1.05857368
#> lag1_var3 -0.13108327 0.02122889 -0.162166341  -0.09618983
#> const     -0.27228151 0.06437521 -0.353283697  -0.17058954
#> 
#> $A$equation3
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.1535182 0.01947162 -0.18231858   -0.1301205
#> lag1_var2 -0.2174352 0.01432718 -0.24299765   -0.1975668
#> lag1_var3  1.4889859 0.04680643  1.42692112    1.5607225
#> const      0.1061443 0.07531598 -0.04857662    0.1938735
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        104.3044  91.23878    35.16646     206.1714
#> B[2,]_shrinkage        269.4972 136.18459   103.21596     487.3926
#> B[3,]_shrinkage        440.7935 203.94793   184.37917     791.0151
#> B[1,]_shrinkage_scale  986.0346 695.11374   378.97487    2349.0881
#> B[2,]_shrinkage_scale 1178.0627 682.21753   368.52983    2139.8116
#> B[3,]_shrinkage_scale 1367.2452 751.05049   577.02688    2851.0557
#> B_global_scale         112.6477  75.30146    41.97984     245.5163
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.5370921 0.3701242   0.2152264    1.1673410
#> A[2,]_shrinkage       0.4157029 0.2716956   0.1329030    0.9460132
#> A[3,]_shrinkage       0.3374986 0.1444055   0.1523719    0.5748506
#> A[1,]_shrinkage_scale 5.3599817 1.9862160   2.9519458    8.4109778
#> A[2,]_shrinkage_scale 5.0967048 2.6683731   1.9858168   11.1967920
#> A[3,]_shrinkage_scale 4.2357586 1.3608940   2.0916630    6.8231923
#> A_global_scale        0.5734975 0.1772462   0.3355454    0.8663955
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
#> B[1,1] 6.807055 0.4334005    6.260366     7.482888
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1]  9.689544 0.780879    8.494743     10.68037
#> B[2,2] 35.862940 2.132174   33.354895     39.01922
#> 
#> $B$equation3
#>              mean       sd 5% quantile 95% quantile
#> B[3,1] -39.201311 2.642121   -43.16831    -35.48670
#> B[3,2]   4.394354 1.797714     2.24981      6.37283
#> B[3,3]  49.731485 4.331414    44.16253     57.33723
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.77776960 0.01724371   0.7516370    0.8006332
#> lag1_var2 -0.27411649 0.01065245  -0.2935573   -0.2593147
#> lag1_var3  0.63832071 0.02781919   0.5937239    0.6734300
#> const      0.02698535 0.09153624  -0.1301465    0.1482897
#> 
#> $A$equation2
#>                  mean         sd  5% quantile 95% quantile
#> lag1_var1  0.02840277 0.01231118  0.007357957   0.04961415
#> lag1_var2  1.04597141 0.01012515  1.032878846   1.05857368
#> lag1_var3 -0.13108327 0.02122889 -0.162166341  -0.09618983
#> const     -0.27228151 0.06437521 -0.353283697  -0.17058954
#> 
#> $A$equation3
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.1535182 0.01947162 -0.18231858   -0.1301205
#> lag1_var2 -0.2174352 0.01432718 -0.24299765   -0.1975668
#> lag1_var3  1.4889859 0.04680643  1.42692112    1.5607225
#> const      0.1061443 0.07531598 -0.04857662    0.1938735
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        104.3044  91.23878    35.16646     206.1714
#> B[2,]_shrinkage        269.4972 136.18459   103.21596     487.3926
#> B[3,]_shrinkage        440.7935 203.94793   184.37917     791.0151
#> B[1,]_shrinkage_scale  986.0346 695.11374   378.97487    2349.0881
#> B[2,]_shrinkage_scale 1178.0627 682.21753   368.52983    2139.8116
#> B[3,]_shrinkage_scale 1367.2452 751.05049   577.02688    2851.0557
#> B_global_scale         112.6477  75.30146    41.97984     245.5163
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.5370921 0.3701242   0.2152264    1.1673410
#> A[2,]_shrinkage       0.4157029 0.2716956   0.1329030    0.9460132
#> A[3,]_shrinkage       0.3374986 0.1444055   0.1523719    0.5748506
#> A[1,]_shrinkage_scale 5.3599817 1.9862160   2.9519458    8.4109778
#> A[2,]_shrinkage_scale 5.0967048 2.6683731   1.9858168   11.1967920
#> A[3,]_shrinkage_scale 4.2357586 1.3608940   2.0916630    6.8231923
#> A_global_scale        0.5734975 0.1772462   0.3355454    0.8663955
#> 
#> 
#> $df
#>         mean           sd  5% quantile 95% quantile 
#>            3            0            3            3 
#> 
```
