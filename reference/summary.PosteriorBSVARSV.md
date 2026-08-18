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
#> B[1,1] 0.9010828 0.03931268    0.845086    0.9416097
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -20.32154 0.8811852   -21.70338    -18.89911
#> B[2,2]  37.55274 1.6682267    34.83031     39.64311
#> 
#> $B$equation3
#>             mean        sd 5% quantile 95% quantile
#> B[3,1] -36.11011 2.0925789  -39.165452    -33.67605
#> B[3,2] -22.61624 2.0293449  -25.184979    -18.56204
#> B[3,3]   7.36517 0.3343643    6.929173      8.05906
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.94482526 0.03000910   0.8973338   0.98871896
#> lag1_var2 -0.01185949 0.02362927  -0.0459075   0.02189547
#> lag1_var3 -0.05856802 0.03624834  -0.1078734  -0.00620204
#> const      0.01550931 0.15803930  -0.2484914   0.22025425
#> 
#> $A$equation2
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.007129929 0.02925805 -0.04773913  0.043713219
#> lag1_var2  0.953512069 0.01451848  0.93525975  0.981128762
#> lag1_var3 -0.052726492 0.03609757 -0.10599244 -0.001051484
#> const     -0.334538540 0.11046242 -0.49125226 -0.113851450
#> 
#> $A$equation3
#>                  mean        sd 5% quantile 95% quantile
#> lag1_var1 -0.01451545 0.1896650 -0.33958919    0.2772483
#> lag1_var2 -0.06432895 0.1064509 -0.18580566    0.1109638
#> lag1_var3  0.18229300 0.2216627 -0.09698352    0.5226799
#> const      0.23199793 0.6292501 -0.46771287    1.3369096
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        451.1608  366.8023    37.44892    1028.4623
#> B[2,]_shrinkage        863.7923  564.1945   174.62707    1867.1855
#> B[3,]_shrinkage        602.6441  439.0055   128.27483    1308.1047
#> B[1,]_shrinkage_scale 4441.8916 3776.1816   430.88099   10458.4282
#> B[2,]_shrinkage_scale 5588.4236 4291.1873   739.66960   11796.4100
#> B[3,]_shrinkage_scale 4172.2767 2960.0798   733.88134    8744.7633
#> B_global_scale         465.9150  364.3860    61.85200     892.1007
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3625670 0.2900119   0.1117519    0.9896129
#> A[2,]_shrinkage       0.4051586 0.1437654   0.2390495    0.5826352
#> A[3,]_shrinkage       0.5908001 0.4419567   0.2181729    1.2666547
#> A[1,]_shrinkage_scale 4.2454506 1.7597568   2.0196427    6.4034493
#> A[2,]_shrinkage_scale 4.7005225 1.5253075   3.1703050    7.2423883
#> A[3,]_shrinkage_scale 4.8874853 1.8363896   2.8666085    8.5235213
#> A_global_scale        0.5514944 0.1344630   0.3929914    0.7634523
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
#> B[1,1] 0.9010828 0.03931268    0.845086    0.9416097
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -20.32154 0.8811852   -21.70338    -18.89911
#> B[2,2]  37.55274 1.6682267    34.83031     39.64311
#> 
#> $B$equation3
#>             mean        sd 5% quantile 95% quantile
#> B[3,1] -36.11011 2.0925789  -39.165452    -33.67605
#> B[3,2] -22.61624 2.0293449  -25.184979    -18.56204
#> B[3,3]   7.36517 0.3343643    6.929173      8.05906
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.94482526 0.03000910   0.8973338   0.98871896
#> lag1_var2 -0.01185949 0.02362927  -0.0459075   0.02189547
#> lag1_var3 -0.05856802 0.03624834  -0.1078734  -0.00620204
#> const      0.01550931 0.15803930  -0.2484914   0.22025425
#> 
#> $A$equation2
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.007129929 0.02925805 -0.04773913  0.043713219
#> lag1_var2  0.953512069 0.01451848  0.93525975  0.981128762
#> lag1_var3 -0.052726492 0.03609757 -0.10599244 -0.001051484
#> const     -0.334538540 0.11046242 -0.49125226 -0.113851450
#> 
#> $A$equation3
#>                  mean        sd 5% quantile 95% quantile
#> lag1_var1 -0.01451545 0.1896650 -0.33958919    0.2772483
#> lag1_var2 -0.06432895 0.1064509 -0.18580566    0.1109638
#> lag1_var3  0.18229300 0.2216627 -0.09698352    0.5226799
#> const      0.23199793 0.6292501 -0.46771287    1.3369096
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        451.1608  366.8023    37.44892    1028.4623
#> B[2,]_shrinkage        863.7923  564.1945   174.62707    1867.1855
#> B[3,]_shrinkage        602.6441  439.0055   128.27483    1308.1047
#> B[1,]_shrinkage_scale 4441.8916 3776.1816   430.88099   10458.4282
#> B[2,]_shrinkage_scale 5588.4236 4291.1873   739.66960   11796.4100
#> B[3,]_shrinkage_scale 4172.2767 2960.0798   733.88134    8744.7633
#> B_global_scale         465.9150  364.3860    61.85200     892.1007
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3625670 0.2900119   0.1117519    0.9896129
#> A[2,]_shrinkage       0.4051586 0.1437654   0.2390495    0.5826352
#> A[3,]_shrinkage       0.5908001 0.4419567   0.2181729    1.2666547
#> A[1,]_shrinkage_scale 4.2454506 1.7597568   2.0196427    6.4034493
#> A[2,]_shrinkage_scale 4.7005225 1.5253075   3.1703050    7.2423883
#> A[3,]_shrinkage_scale 4.8874853 1.8363896   2.8666085    8.5235213
#> A_global_scale        0.5514944 0.1344630   0.3929914    0.7634523
#> 
#> 
```
