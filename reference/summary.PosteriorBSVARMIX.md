# Provides posterior summary of non-normal Structural VAR estimation

Provides posterior mean, standard deviations, as well as 5 and 95
percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper parameters.

## Usage

``` r
# S3 method for class 'PosteriorBSVARMIX'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorBSVARMIX obtained using the
  [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md)
  function applied to non-normal Bayesian Structural VAR model
  specification set by function `specify_bsvar_mix$new()` containing
  draws from the posterior distribution of the parameters.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper-parameters.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_bsvar_mix`](https://bsvars.org/bsvars/reference/specify_bsvar_mix.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
set.seed(123)
specification  = specify_bsvar_mix$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.

# run the burn-in
burn_in        = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
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
#>  Gibbs sampler for the SVAR-finiteMIX model             |
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
#> B[1,1] 0.8894811 0.0447447    0.824223    0.9684456
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -18.54120 0.8592834   -19.79780    -17.21552
#> B[2,2]  33.77741 1.5588390    31.28291     36.04172
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -31.792893 2.2661429   -35.58117   -28.769614
#> B[3,2] -18.045404 2.0774032   -20.31942   -15.269654
#> B[3,3]   6.361845 0.3706728     6.04125     6.952776
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  1.003436044 0.01641994  0.98141592   1.02693651
#> lag1_var2 -0.004868417 0.01811926 -0.03829297   0.02280775
#> lag1_var3 -0.151360723 0.01999838 -0.17517301  -0.12516992
#> const     -0.056247266 0.15733371 -0.34701296   0.20599041
#> 
#> $A$equation2
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.006530092 0.01581767 -0.02439232   0.01989140
#> lag1_var2  0.961906724 0.01571793  0.93986735   0.98531024
#> lag1_var3 -0.068661651 0.02066835 -0.10018555  -0.04295973
#> const     -0.355858780 0.12238095 -0.51068166  -0.19416307
#> 
#> $A$equation3
#>                 mean        sd 5% quantile 95% quantile
#> lag1_var1  0.2871848 0.1033761  0.08748286   0.41714645
#> lag1_var2  0.0271416 0.1332837 -0.16895281   0.19014622
#> lag1_var3 -0.3249173 0.1456577 -0.54056738  -0.03487728
#> const      0.1832235 1.0921882 -1.17072628   1.64415812
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         91.96295  54.15372    41.07132     166.0348
#> B[2,]_shrinkage        223.71449  99.92321   127.91237     442.4787
#> B[3,]_shrinkage        254.46937 103.81410   140.33180     415.7662
#> B[1,]_shrinkage_scale  847.06741 304.78655   389.10800    1321.5747
#> B[2,]_shrinkage_scale 1133.43622 439.25000   532.57450    1646.5706
#> B[3,]_shrinkage_scale 1167.56634 361.22079   773.32394    1642.2000
#> B_global_scale         100.17572  33.29230    58.53005     143.4988
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.7728640 0.2923582   0.4726655     1.365764
#> A[2,]_shrinkage       0.9401181 0.7173775   0.3652034     2.308926
#> A[3,]_shrinkage       0.9910831 0.4105958   0.5479981     1.646646
#> A[1,]_shrinkage_scale 8.8133545 2.3847344   5.7530171    10.981245
#> A[2,]_shrinkage_scale 8.5778060 3.2181695   5.5765056    14.589027
#> A[3,]_shrinkage_scale 9.5380368 2.3680308   6.0444277    13.409178
#> A_global_scale        0.9559161 0.1709353   0.7526526     1.280365
#> 
#> 

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar_mix$new() |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  summary()
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
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
#> B[1,1] 0.8894811 0.0447447    0.824223    0.9684456
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -18.54120 0.8592834   -19.79780    -17.21552
#> B[2,2]  33.77741 1.5588390    31.28291     36.04172
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -31.792893 2.2661429   -35.58117   -28.769614
#> B[3,2] -18.045404 2.0774032   -20.31942   -15.269654
#> B[3,3]   6.361845 0.3706728     6.04125     6.952776
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  1.003436044 0.01641994  0.98141592   1.02693651
#> lag1_var2 -0.004868417 0.01811926 -0.03829297   0.02280775
#> lag1_var3 -0.151360723 0.01999838 -0.17517301  -0.12516992
#> const     -0.056247266 0.15733371 -0.34701296   0.20599041
#> 
#> $A$equation2
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.006530092 0.01581767 -0.02439232   0.01989140
#> lag1_var2  0.961906724 0.01571793  0.93986735   0.98531024
#> lag1_var3 -0.068661651 0.02066835 -0.10018555  -0.04295973
#> const     -0.355858780 0.12238095 -0.51068166  -0.19416307
#> 
#> $A$equation3
#>                 mean        sd 5% quantile 95% quantile
#> lag1_var1  0.2871848 0.1033761  0.08748286   0.41714645
#> lag1_var2  0.0271416 0.1332837 -0.16895281   0.19014622
#> lag1_var3 -0.3249173 0.1456577 -0.54056738  -0.03487728
#> const      0.1832235 1.0921882 -1.17072628   1.64415812
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         91.96295  54.15372    41.07132     166.0348
#> B[2,]_shrinkage        223.71449  99.92321   127.91237     442.4787
#> B[3,]_shrinkage        254.46937 103.81410   140.33180     415.7662
#> B[1,]_shrinkage_scale  847.06741 304.78655   389.10800    1321.5747
#> B[2,]_shrinkage_scale 1133.43622 439.25000   532.57450    1646.5706
#> B[3,]_shrinkage_scale 1167.56634 361.22079   773.32394    1642.2000
#> B_global_scale         100.17572  33.29230    58.53005     143.4988
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.7728640 0.2923582   0.4726655     1.365764
#> A[2,]_shrinkage       0.9401181 0.7173775   0.3652034     2.308926
#> A[3,]_shrinkage       0.9910831 0.4105958   0.5479981     1.646646
#> A[1,]_shrinkage_scale 8.8133545 2.3847344   5.7530171    10.981245
#> A[2,]_shrinkage_scale 8.5778060 3.2181695   5.5765056    14.589027
#> A[3,]_shrinkage_scale 9.5380368 2.3680308   6.0444277    13.409178
#> A_global_scale        0.9559161 0.1709353   0.7526526     1.280365
#> 
#> 
```
