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
#> B[1,1] 0.8870916 0.03528028   0.8352888    0.9401435
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -18.75312 1.056281   -20.41557    -17.02559
#> B[2,2]  34.48929 1.963238    31.31556     37.44283
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -31.734265 1.9821749  -34.881283   -29.181130
#> B[3,2] -17.571403 1.7504964  -20.532657   -15.311888
#> B[3,3]   6.365612 0.3687844    5.860931     6.857711
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  1.03901184 0.02218447  1.01328983  1.075651697
#> lag1_var2 -0.03904007 0.02200608 -0.06678832 -0.007636447
#> lag1_var3 -0.18076547 0.03043116 -0.23582994 -0.151711898
#> const     -0.27756297 0.18578483 -0.47499354 -0.039775724
#> 
#> $A$equation2
#>                  mean         sd  5% quantile 95% quantile
#> lag1_var1  0.02059436 0.01895236 -0.005474478   0.04624105
#> lag1_var2  0.94770420 0.01664362  0.925140927   0.97450649
#> lag1_var3 -0.09296846 0.02617207 -0.138934713  -0.06145795
#> const     -0.42762202 0.12984901 -0.572657427  -0.23667741
#> 
#> $A$equation3
#>                 mean        sd 5% quantile 95% quantile
#> lag1_var1  0.5963884 0.1224723   0.4778464   0.81983478
#> lag1_var2 -0.1691176 0.1283751  -0.3249726   0.03341021
#> lag1_var3 -0.6047201 0.1773131  -0.9097106  -0.40411847
#> const     -1.0056562 1.0444947  -2.5912512   0.54467338
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         76.39409  44.33062    16.97432     137.5948
#> B[2,]_shrinkage        257.17279 123.77474   113.07231     487.9517
#> B[3,]_shrinkage        226.76803  92.28633   116.84255     353.7844
#> B[1,]_shrinkage_scale  906.38304 566.44932   154.48393    1777.2144
#> B[2,]_shrinkage_scale 1412.19482 977.67492   529.64550    2776.4937
#> B[3,]_shrinkage_scale 1335.76674 789.10138   341.52358    2506.0519
#> B_global_scale         120.12864  72.29638    44.55243     205.5031
#> 
#> $hyper$A
#>                             mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage        1.2830810 1.0157458   0.4160975     2.213314
#> A[2,]_shrinkage        0.9348893 0.4440348   0.4890715     1.974114
#> A[3,]_shrinkage        1.3726551 0.6605221   0.6523746     2.465475
#> A[1,]_shrinkage_scale 13.2416233 5.5866821   7.6280199    19.457357
#> A[2,]_shrinkage_scale 11.6025068 5.4662643   6.7526322    20.190082
#> A[3,]_shrinkage_scale 13.4877808 6.7442862   7.1299776    29.559229
#> A_global_scale         1.3599467 0.5999579   0.7837480     2.842465
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
#> B[1,1] 0.8870916 0.03528028   0.8352888    0.9401435
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -18.75312 1.056281   -20.41557    -17.02559
#> B[2,2]  34.48929 1.963238    31.31556     37.44283
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -31.734265 1.9821749  -34.881283   -29.181130
#> B[3,2] -17.571403 1.7504964  -20.532657   -15.311888
#> B[3,3]   6.365612 0.3687844    5.860931     6.857711
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  1.03901184 0.02218447  1.01328983  1.075651697
#> lag1_var2 -0.03904007 0.02200608 -0.06678832 -0.007636447
#> lag1_var3 -0.18076547 0.03043116 -0.23582994 -0.151711898
#> const     -0.27756297 0.18578483 -0.47499354 -0.039775724
#> 
#> $A$equation2
#>                  mean         sd  5% quantile 95% quantile
#> lag1_var1  0.02059436 0.01895236 -0.005474478   0.04624105
#> lag1_var2  0.94770420 0.01664362  0.925140927   0.97450649
#> lag1_var3 -0.09296846 0.02617207 -0.138934713  -0.06145795
#> const     -0.42762202 0.12984901 -0.572657427  -0.23667741
#> 
#> $A$equation3
#>                 mean        sd 5% quantile 95% quantile
#> lag1_var1  0.5963884 0.1224723   0.4778464   0.81983478
#> lag1_var2 -0.1691176 0.1283751  -0.3249726   0.03341021
#> lag1_var3 -0.6047201 0.1773131  -0.9097106  -0.40411847
#> const     -1.0056562 1.0444947  -2.5912512   0.54467338
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         76.39409  44.33062    16.97432     137.5948
#> B[2,]_shrinkage        257.17279 123.77474   113.07231     487.9517
#> B[3,]_shrinkage        226.76803  92.28633   116.84255     353.7844
#> B[1,]_shrinkage_scale  906.38304 566.44932   154.48393    1777.2144
#> B[2,]_shrinkage_scale 1412.19482 977.67492   529.64550    2776.4937
#> B[3,]_shrinkage_scale 1335.76674 789.10138   341.52358    2506.0519
#> B_global_scale         120.12864  72.29638    44.55243     205.5031
#> 
#> $hyper$A
#>                             mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage        1.2830810 1.0157458   0.4160975     2.213314
#> A[2,]_shrinkage        0.9348893 0.4440348   0.4890715     1.974114
#> A[3,]_shrinkage        1.3726551 0.6605221   0.6523746     2.465475
#> A[1,]_shrinkage_scale 13.2416233 5.5866821   7.6280199    19.457357
#> A[2,]_shrinkage_scale 11.6025068 5.4662643   6.7526322    20.190082
#> A[3,]_shrinkage_scale 13.4877808 6.7442862   7.1299776    29.559229
#> A_global_scale         1.3599467 0.5999579   0.7837480     2.842465
#> 
#> 
```
