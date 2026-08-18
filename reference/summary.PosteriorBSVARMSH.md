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
#>             mean        sd 5% quantile 95% quantile
#> B[1,1] 0.8922138 0.0477047   0.8264662    0.9717608
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -14.49071 0.9875185   -15.61207    -12.79523
#> B[2,2]  26.90914 1.8373176    23.76385     29.11126
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -23.996931 2.3522835  -27.673534   -21.015857
#> B[3,2] -10.984500 1.7397953  -13.323235    -8.208214
#> B[3,3]   4.596468 0.4175689    4.069288     5.203820
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.87753463 0.01313540  0.85838911  0.896214016
#> lag1_var2 -0.03447078 0.01251521 -0.05291571 -0.008888101
#> lag1_var3  0.02300533 0.01784935  0.00121014  0.047272723
#> const     -0.22212021 0.11181106 -0.38858746  0.019782327
#> 
#> $A$equation2
#>                   mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.056067328 0.014604066 -0.07509863   -0.0293102
#> lag1_var2  0.961763099 0.008785521  0.95139585    0.9774430
#> lag1_var3  0.003256145 0.017511644 -0.03118934    0.0274313
#> const     -0.292860461 0.073215668 -0.38787963   -0.1712194
#> 
#> $A$equation3
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.40992828 0.10211680  -0.5536190  -0.26226242
#> lag1_var2 -0.06529016 0.05484545  -0.1517178   0.01875258
#> lag1_var3  0.65784203 0.12608189   0.4655599   0.82698937
#> const      0.05833696 0.42538393  -0.5388004   0.65748802
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         70.15923  43.09292    29.06199     151.0048
#> B[2,]_shrinkage        198.80041 110.87535    92.24472     404.0567
#> B[3,]_shrinkage        157.16639  71.93056    75.68872     266.8984
#> B[1,]_shrinkage_scale  723.18834 251.04274   355.80754    1095.2819
#> B[2,]_shrinkage_scale 1040.53754 404.88367   577.62787    1713.3236
#> B[3,]_shrinkage_scale 1034.53145 465.38245   515.70061    1587.9710
#> B_global_scale          90.94845  28.10301    54.30209     125.8784
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3686391 0.3132535  0.08466927    1.0572789
#> A[2,]_shrinkage       0.4880153 0.4072878  0.14681456    1.0791576
#> A[3,]_shrinkage       0.4808486 0.2796726  0.17491733    1.0045363
#> A[1,]_shrinkage_scale 4.7709972 3.3554323  1.51356995   12.9150911
#> A[2,]_shrinkage_scale 5.1344187 2.3463922  2.51909852   10.0000213
#> A[3,]_shrinkage_scale 5.3122042 1.7126181  3.31394919    8.3943399
#> A_global_scale        0.5899128 0.1588825  0.42386561    0.8721331
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
#>             mean        sd 5% quantile 95% quantile
#> B[1,1] 0.8922138 0.0477047   0.8264662    0.9717608
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -14.49071 0.9875185   -15.61207    -12.79523
#> B[2,2]  26.90914 1.8373176    23.76385     29.11126
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -23.996931 2.3522835  -27.673534   -21.015857
#> B[3,2] -10.984500 1.7397953  -13.323235    -8.208214
#> B[3,3]   4.596468 0.4175689    4.069288     5.203820
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.87753463 0.01313540  0.85838911  0.896214016
#> lag1_var2 -0.03447078 0.01251521 -0.05291571 -0.008888101
#> lag1_var3  0.02300533 0.01784935  0.00121014  0.047272723
#> const     -0.22212021 0.11181106 -0.38858746  0.019782327
#> 
#> $A$equation2
#>                   mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.056067328 0.014604066 -0.07509863   -0.0293102
#> lag1_var2  0.961763099 0.008785521  0.95139585    0.9774430
#> lag1_var3  0.003256145 0.017511644 -0.03118934    0.0274313
#> const     -0.292860461 0.073215668 -0.38787963   -0.1712194
#> 
#> $A$equation3
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.40992828 0.10211680  -0.5536190  -0.26226242
#> lag1_var2 -0.06529016 0.05484545  -0.1517178   0.01875258
#> lag1_var3  0.65784203 0.12608189   0.4655599   0.82698937
#> const      0.05833696 0.42538393  -0.5388004   0.65748802
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         70.15923  43.09292    29.06199     151.0048
#> B[2,]_shrinkage        198.80041 110.87535    92.24472     404.0567
#> B[3,]_shrinkage        157.16639  71.93056    75.68872     266.8984
#> B[1,]_shrinkage_scale  723.18834 251.04274   355.80754    1095.2819
#> B[2,]_shrinkage_scale 1040.53754 404.88367   577.62787    1713.3236
#> B[3,]_shrinkage_scale 1034.53145 465.38245   515.70061    1587.9710
#> B_global_scale          90.94845  28.10301    54.30209     125.8784
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3686391 0.3132535  0.08466927    1.0572789
#> A[2,]_shrinkage       0.4880153 0.4072878  0.14681456    1.0791576
#> A[3,]_shrinkage       0.4808486 0.2796726  0.17491733    1.0045363
#> A[1,]_shrinkage_scale 4.7709972 3.3554323  1.51356995   12.9150911
#> A[2,]_shrinkage_scale 5.1344187 2.3463922  2.51909852   10.0000213
#> A[3,]_shrinkage_scale 5.3122042 1.7126181  3.31394919    8.3943399
#> A_global_scale        0.5899128 0.1588825  0.42386561    0.8721331
#> 
#> 
```
