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
#> B[1,1] 0.8944901 0.04329999   0.8437655    0.9481897
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -19.78672 0.6975307   -21.04460    -19.06134
#> B[2,2]  36.47211 1.2977585    34.92646     38.66404
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -36.650509 1.8252466   -39.53264    -34.47758
#> B[3,2] -25.566357 2.7191922   -29.33088    -22.30127
#> B[3,3]   7.805895 0.3709497     7.40875      8.53956
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd  5% quantile 95% quantile
#> lag1_var1  0.99985704 0.03777154  0.937778179   1.03918233
#> lag1_var2  0.02495513 0.01422577  0.003469983   0.04220646
#> lag1_var3 -0.15749888 0.05228315 -0.211098145  -0.07132667
#> const      0.16387723 0.17092523 -0.099324763   0.35964459
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.01381346 0.01991514 -0.01687093   0.03647887
#> lag1_var2  0.97402501 0.01474427  0.95390044   0.99070111
#> lag1_var3 -0.09813321 0.02837265 -0.13585892  -0.06010610
#> const     -0.26959970 0.14438809 -0.47284089  -0.10086530
#> 
#> $A$equation3
#>                  mean        sd   5% quantile 95% quantile
#> lag1_var1  0.31157647 0.2472599 -0.1253183370    0.6213498
#> lag1_var2  0.08629612 0.0487997  0.0004133584    0.1581725
#> lag1_var3 -0.37950991 0.3547239 -0.7965059758    0.2392769
#> const      0.52580266 0.6985614 -0.5889117689    1.3004466
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        196.8530  139.5110    41.81654     493.6660
#> B[2,]_shrinkage        336.1313  107.9521   196.85789     515.1511
#> B[3,]_shrinkage        430.0687  265.3743   175.45948     850.4441
#> B[1,]_shrinkage_scale 1979.6226 1426.9540   765.92515    4743.5424
#> B[2,]_shrinkage_scale 2236.3105 1171.0611   688.08291    4008.7853
#> B[3,]_shrinkage_scale 2835.1100 2688.4045   908.53525    9023.6264
#> B_global_scale         210.8891  153.9426    72.09251     448.1864
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.4683357 0.3234273  0.16740868    1.0269211
#> A[2,]_shrinkage       0.2524850 0.1465537  0.06978192    0.5088692
#> A[3,]_shrinkage       0.7565351 0.3363800  0.41971438    1.5160093
#> A[1,]_shrinkage_scale 5.0000043 2.1525082  2.96741579    7.8343394
#> A[2,]_shrinkage_scale 3.4020582 0.9675611  1.69856005    5.0462721
#> A[3,]_shrinkage_scale 6.3208681 2.5425256  3.75175401   11.7113931
#> A_global_scale        0.5850116 0.1302251  0.45027139    0.8411225
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
#> B[1,1] 0.8944901 0.04329999   0.8437655    0.9481897
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -19.78672 0.6975307   -21.04460    -19.06134
#> B[2,2]  36.47211 1.2977585    34.92646     38.66404
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -36.650509 1.8252466   -39.53264    -34.47758
#> B[3,2] -25.566357 2.7191922   -29.33088    -22.30127
#> B[3,3]   7.805895 0.3709497     7.40875      8.53956
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd  5% quantile 95% quantile
#> lag1_var1  0.99985704 0.03777154  0.937778179   1.03918233
#> lag1_var2  0.02495513 0.01422577  0.003469983   0.04220646
#> lag1_var3 -0.15749888 0.05228315 -0.211098145  -0.07132667
#> const      0.16387723 0.17092523 -0.099324763   0.35964459
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.01381346 0.01991514 -0.01687093   0.03647887
#> lag1_var2  0.97402501 0.01474427  0.95390044   0.99070111
#> lag1_var3 -0.09813321 0.02837265 -0.13585892  -0.06010610
#> const     -0.26959970 0.14438809 -0.47284089  -0.10086530
#> 
#> $A$equation3
#>                  mean        sd   5% quantile 95% quantile
#> lag1_var1  0.31157647 0.2472599 -0.1253183370    0.6213498
#> lag1_var2  0.08629612 0.0487997  0.0004133584    0.1581725
#> lag1_var3 -0.37950991 0.3547239 -0.7965059758    0.2392769
#> const      0.52580266 0.6985614 -0.5889117689    1.3004466
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        196.8530  139.5110    41.81654     493.6660
#> B[2,]_shrinkage        336.1313  107.9521   196.85789     515.1511
#> B[3,]_shrinkage        430.0687  265.3743   175.45948     850.4441
#> B[1,]_shrinkage_scale 1979.6226 1426.9540   765.92515    4743.5424
#> B[2,]_shrinkage_scale 2236.3105 1171.0611   688.08291    4008.7853
#> B[3,]_shrinkage_scale 2835.1100 2688.4045   908.53525    9023.6264
#> B_global_scale         210.8891  153.9426    72.09251     448.1864
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.4683357 0.3234273  0.16740868    1.0269211
#> A[2,]_shrinkage       0.2524850 0.1465537  0.06978192    0.5088692
#> A[3,]_shrinkage       0.7565351 0.3363800  0.41971438    1.5160093
#> A[1,]_shrinkage_scale 5.0000043 2.1525082  2.96741579    7.8343394
#> A[2,]_shrinkage_scale 3.4020582 0.9675611  1.69856005    5.0462721
#> A[3,]_shrinkage_scale 6.3208681 2.5425256  3.75175401   11.7113931
#> A_global_scale        0.5850116 0.1302251  0.45027139    0.8411225
#> 
#> 
```
