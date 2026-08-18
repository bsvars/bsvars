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
#>             mean         sd 5% quantile 95% quantile
#> B[1,1] 0.8967928 0.04482659   0.8147042    0.9570954
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -14.17398 1.028317   -15.76040    -12.67834
#> B[2,2]  26.39888 1.908314    23.76328     29.50050
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -23.694788 2.6353361  -28.239449   -20.719872
#> B[3,2] -11.134222 1.4241846  -13.018737    -8.795515
#> B[3,3]   4.568193 0.4646663    3.913666     5.328111
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.95739972 0.03053433  0.91557735  0.995721532
#> lag1_var2 -0.02458595 0.01489954 -0.04835821 -0.005417496
#> lag1_var3 -0.06905409 0.04109182 -0.11722572 -0.006069715
#> const     -0.05701818 0.13825344 -0.32391418  0.119631956
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.02815296 0.02219668 -0.05658754  0.002757778
#> lag1_var2  0.94923943 0.01346040  0.93304146  0.970188311
#> lag1_var3 -0.02537166 0.02675665 -0.06024612  0.014268257
#> const     -0.36766742 0.09169323 -0.49907987 -0.224923283
#> 
#> $A$equation3
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.001485227 0.19971138  -0.3324904  0.272978459
#> lag1_var2 -0.087957140 0.06293511  -0.1791977 -0.001902791
#> lag1_var3  0.196649682 0.26485539  -0.1665208  0.628053723
#> const      0.268292570 0.48601057  -0.5914119  0.837472311
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        56.26643  31.07645    17.76610    118.80584
#> B[2,]_shrinkage       172.39495  76.22372    90.76747    289.10603
#> B[3,]_shrinkage       149.78640  97.15134    61.35022    324.81472
#> B[1,]_shrinkage_scale 544.62531 202.26084   295.50139    921.33806
#> B[2,]_shrinkage_scale 828.82876 370.98555   459.55431   1347.95324
#> B[3,]_shrinkage_scale 754.54907 379.66366   455.53708   1514.60161
#> B_global_scale         67.97084  20.02695    34.37002     94.74695
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3134593 0.1854088   0.1320172    0.6803524
#> A[2,]_shrinkage       0.4609831 0.1802980   0.2765942    0.8404632
#> A[3,]_shrinkage       0.5612189 0.4902699   0.2594689    1.1859130
#> A[1,]_shrinkage_scale 3.9630053 1.7043572   2.2556554    6.5200519
#> A[2,]_shrinkage_scale 4.8940468 1.0340090   3.6948409    6.3531734
#> A[3,]_shrinkage_scale 5.0543796 1.4884525   3.1076338    7.6332231
#> A_global_scale        0.5212198 0.1006025   0.3572412    0.6561833
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
#>             mean         sd 5% quantile 95% quantile
#> B[1,1] 0.8967928 0.04482659   0.8147042    0.9570954
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -14.17398 1.028317   -15.76040    -12.67834
#> B[2,2]  26.39888 1.908314    23.76328     29.50050
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -23.694788 2.6353361  -28.239449   -20.719872
#> B[3,2] -11.134222 1.4241846  -13.018737    -8.795515
#> B[3,3]   4.568193 0.4646663    3.913666     5.328111
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.95739972 0.03053433  0.91557735  0.995721532
#> lag1_var2 -0.02458595 0.01489954 -0.04835821 -0.005417496
#> lag1_var3 -0.06905409 0.04109182 -0.11722572 -0.006069715
#> const     -0.05701818 0.13825344 -0.32391418  0.119631956
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.02815296 0.02219668 -0.05658754  0.002757778
#> lag1_var2  0.94923943 0.01346040  0.93304146  0.970188311
#> lag1_var3 -0.02537166 0.02675665 -0.06024612  0.014268257
#> const     -0.36766742 0.09169323 -0.49907987 -0.224923283
#> 
#> $A$equation3
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.001485227 0.19971138  -0.3324904  0.272978459
#> lag1_var2 -0.087957140 0.06293511  -0.1791977 -0.001902791
#> lag1_var3  0.196649682 0.26485539  -0.1665208  0.628053723
#> const      0.268292570 0.48601057  -0.5914119  0.837472311
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        56.26643  31.07645    17.76610    118.80584
#> B[2,]_shrinkage       172.39495  76.22372    90.76747    289.10603
#> B[3,]_shrinkage       149.78640  97.15134    61.35022    324.81472
#> B[1,]_shrinkage_scale 544.62531 202.26084   295.50139    921.33806
#> B[2,]_shrinkage_scale 828.82876 370.98555   459.55431   1347.95324
#> B[3,]_shrinkage_scale 754.54907 379.66366   455.53708   1514.60161
#> B_global_scale         67.97084  20.02695    34.37002     94.74695
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3134593 0.1854088   0.1320172    0.6803524
#> A[2,]_shrinkage       0.4609831 0.1802980   0.2765942    0.8404632
#> A[3,]_shrinkage       0.5612189 0.4902699   0.2594689    1.1859130
#> A[1,]_shrinkage_scale 3.9630053 1.7043572   2.2556554    6.5200519
#> A[2,]_shrinkage_scale 4.8940468 1.0340090   3.6948409    6.3531734
#> A[3,]_shrinkage_scale 5.0543796 1.4884525   3.1076338    7.6332231
#> A_global_scale        0.5212198 0.1006025   0.3572412    0.6561833
#> 
#> 
```
