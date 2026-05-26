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
#> B[1,1] 0.9003053 0.03987537   0.8312135     0.952878
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -18.61152 1.002927   -20.11836    -17.10357
#> B[2,2]  35.03624 1.912348    32.22472     37.96064
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -32.962188 2.0491907  -36.177626   -30.660783
#> B[3,2] -18.166325 2.7024687  -22.298675   -13.907214
#> B[3,3]   6.599233 0.3602898    6.134195     7.146167
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  1.008429711 0.02833650  0.97565888   1.05298928
#> lag1_var2 -0.004288468 0.01765189 -0.02580327   0.01468302
#> lag1_var3 -0.157809089 0.03684345 -0.21584127  -0.11536259
#> const     -0.042291088 0.16061383 -0.24753813   0.17284098
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.00137204 0.02318991 -0.02865933   0.03957584
#> lag1_var2  0.96614418 0.01520545  0.94540356   0.98934696
#> lag1_var3 -0.07294382 0.02920323 -0.12686514  -0.03544369
#> const     -0.31194436 0.13187041 -0.48955731  -0.09827047
#> 
#> $A$equation3
#>                  mean        sd 5% quantile 95% quantile
#> lag1_var1  0.36871771 0.2038692  0.17571398    0.6833710
#> lag1_var2  0.05575071 0.1054406 -0.09708806    0.2123233
#> lag1_var3 -0.40600703 0.2540374 -0.77944532   -0.1619167
#> const      0.57759167 1.0191738 -0.54319931    2.1502143
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        351.6787  257.1096    147.0736     605.9621
#> B[2,]_shrinkage        482.9041  292.5469    243.8955    1112.1412
#> B[3,]_shrinkage        537.1220  365.8285    203.1924    1110.2056
#> B[1,]_shrinkage_scale 3444.0321 2073.6781   1402.0987    7010.8981
#> B[2,]_shrinkage_scale 3312.6475 1349.0962   1659.3641    6051.2121
#> B[3,]_shrinkage_scale 4007.4765 2256.3827   1008.1006    8305.1223
#> B_global_scale         345.5292  198.6099    125.4823     767.3886
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.4745683 0.2368989   0.2113871    0.9165366
#> A[2,]_shrinkage       0.3158615 0.1676562   0.1053388    0.5895658
#> A[3,]_shrinkage       1.0182251 0.6572590   0.4495779    1.5814405
#> A[1,]_shrinkage_scale 5.4275714 2.4856162   3.3268787   10.8905441
#> A[2,]_shrinkage_scale 3.8655222 1.6119619   1.8010736    6.3557546
#> A[3,]_shrinkage_scale 7.6224765 2.7176836   4.9840333   10.9745628
#> A_global_scale        0.6340627 0.1818693   0.4294298    0.9466126
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
#> B[1,1] 0.9003053 0.03987537   0.8312135     0.952878
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -18.61152 1.002927   -20.11836    -17.10357
#> B[2,2]  35.03624 1.912348    32.22472     37.96064
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -32.962188 2.0491907  -36.177626   -30.660783
#> B[3,2] -18.166325 2.7024687  -22.298675   -13.907214
#> B[3,3]   6.599233 0.3602898    6.134195     7.146167
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  1.008429711 0.02833650  0.97565888   1.05298928
#> lag1_var2 -0.004288468 0.01765189 -0.02580327   0.01468302
#> lag1_var3 -0.157809089 0.03684345 -0.21584127  -0.11536259
#> const     -0.042291088 0.16061383 -0.24753813   0.17284098
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.00137204 0.02318991 -0.02865933   0.03957584
#> lag1_var2  0.96614418 0.01520545  0.94540356   0.98934696
#> lag1_var3 -0.07294382 0.02920323 -0.12686514  -0.03544369
#> const     -0.31194436 0.13187041 -0.48955731  -0.09827047
#> 
#> $A$equation3
#>                  mean        sd 5% quantile 95% quantile
#> lag1_var1  0.36871771 0.2038692  0.17571398    0.6833710
#> lag1_var2  0.05575071 0.1054406 -0.09708806    0.2123233
#> lag1_var3 -0.40600703 0.2540374 -0.77944532   -0.1619167
#> const      0.57759167 1.0191738 -0.54319931    2.1502143
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        351.6787  257.1096    147.0736     605.9621
#> B[2,]_shrinkage        482.9041  292.5469    243.8955    1112.1412
#> B[3,]_shrinkage        537.1220  365.8285    203.1924    1110.2056
#> B[1,]_shrinkage_scale 3444.0321 2073.6781   1402.0987    7010.8981
#> B[2,]_shrinkage_scale 3312.6475 1349.0962   1659.3641    6051.2121
#> B[3,]_shrinkage_scale 4007.4765 2256.3827   1008.1006    8305.1223
#> B_global_scale         345.5292  198.6099    125.4823     767.3886
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.4745683 0.2368989   0.2113871    0.9165366
#> A[2,]_shrinkage       0.3158615 0.1676562   0.1053388    0.5895658
#> A[3,]_shrinkage       1.0182251 0.6572590   0.4495779    1.5814405
#> A[1,]_shrinkage_scale 5.4275714 2.4856162   3.3268787   10.8905441
#> A[2,]_shrinkage_scale 3.8655222 1.6119619   1.8010736    6.3557546
#> A[3,]_shrinkage_scale 7.6224765 2.7176836   4.9840333   10.9745628
#> A_global_scale        0.6340627 0.1818693   0.4294298    0.9466126
#> 
#> 
```
