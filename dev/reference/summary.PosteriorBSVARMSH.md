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
  [`estimate()`](https://bsvars.org/bsvars/dev/reference/estimate.md)
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

[`estimate`](https://bsvars.org/bsvars/dev/reference/estimate.md),
[`specify_bsvar_msh`](https://bsvars.org/bsvars/dev/reference/specify_bsvar_msh.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
specification  = specify_bsvar_msh$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.
burn_in        = estimate(specification, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
posterior      = estimate(burn_in, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
summ           = summary(posterior)
summ
#> $B
#> $B$ttr
#>             mean         sd 5% quantile 95% quantile
#> B[1,1] 0.4292294 0.01404635   0.4153701    0.4466668
#> 
#> $B$gs
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -29.12424 3.904193   -33.96807    -25.29244
#> B[2,2]  12.82009 1.714862    11.13812     14.95000
#> 
#> $B$gdp
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -17.83714 1.788928   -19.89527    -16.15634
#> B[3,2] -30.45503 2.755931   -34.21038    -28.26813
#> B[3,3]  29.72554 1.923628    28.28258     32.31364
#> 
#> 
#> $A
#> $A$ttr
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1  1.00602701 0.007437419  0.99648471   1.01299426
#> lag1_var2 -0.06428092 0.015602124 -0.08384561  -0.04892312
#> lag1_var3 -0.25384795 0.012586041 -0.26372543  -0.23677080
#> const     -0.21678170 0.126992851 -0.37083911  -0.08165535
#> 
#> $A$gs
#>                 mean          sd 5% quantile 95% quantile
#> lag1_var1  0.1059402 0.004148042   0.1016575    0.1106049
#> lag1_var2  0.8367035 0.006642448   0.8297499    0.8443272
#> lag1_var3 -0.6923226 0.004025565  -0.6963276   -0.6872817
#> const     -0.6737482 0.081757248  -0.7589066   -0.5784268
#> 
#> $A$gdp
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1  0.16657972 0.008308436   0.1579634   0.17706320
#> lag1_var2 -0.16438135 0.002356904  -0.1673922  -0.16198063
#> lag1_var3  0.06751015 0.010381530   0.0550153   0.07866399
#> const     -0.42671785 0.015757897  -0.4399150  -0.40555641
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        33.67953  24.67710    12.02679     66.43441
#> B[2,]_shrinkage        92.30335  48.58045    60.12220    159.27170
#> B[3,]_shrinkage       210.55945 109.61786   127.84816    358.56982
#> B[1,]_shrinkage_scale 276.44647 169.05986   104.74775    469.65451
#> B[2,]_shrinkage_scale 227.38521  85.18297   139.05712    324.85526
#> B[3,]_shrinkage_scale 265.63844  72.12961   196.35830    359.31971
#> B_global_scale         22.51800  10.40744    12.45363     35.94220
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.4390106 0.19355975   0.2797854    0.6997295
#> A[2,]_shrinkage       0.6577394 0.24146568   0.3735556    0.9136468
#> A[3,]_shrinkage       0.5057656 0.08789633   0.4113233    0.6073115
#> A[1,]_shrinkage_scale 5.7118868 1.25855292   4.1987768    7.0997722
#> A[2,]_shrinkage_scale 6.7365079 1.54596095   4.7212705    8.1111866
#> A[3,]_shrinkage_scale 7.0191122 1.84457825   4.8526346    9.0959976
#> A_global_scale        0.7712549 0.24265980   0.5606933    1.0876566
#> 
#> 

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_msh$new() |>
  estimate(S = 5) |> 
  estimate(S = 5) |> 
  summary() -> summ
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
summ
#> $B
#> $B$ttr
#>             mean         sd 5% quantile 95% quantile
#> B[1,1] 0.3401241 0.01624611   0.3178432    0.3498222
#> 
#> $B$gs
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -28.39515 4.354443   -32.39447    -22.63473
#> B[2,2]  17.12052 2.633582    13.64019     19.54640
#> 
#> $B$gdp
#>             mean         sd 5% quantile 95% quantile
#> B[3,1] -4.518997  1.3958803   -5.997186    -2.851342
#> B[3,2]  2.083694  0.7650306    1.168386     2.848872
#> B[3,3] 67.411864 11.4907721   56.748490    82.361399
#> 
#> 
#> $A
#> $A$ttr
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.8498161 0.01217798   0.8377872   0.86352056
#> lag1_var2 -0.2931411 0.02703018  -0.3253693  -0.26323670
#> lag1_var3  0.1637872 0.01346556   0.1515283   0.18158797
#> const     -0.1431922 0.20401883  -0.3953274   0.06538535
#> 
#> $A$gs
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.1644450 0.02611732  -0.1991484   -0.1433256
#> lag1_var2  0.4704910 0.02863743   0.4344874    0.4978704
#> lag1_var3  0.1754827 0.02694952   0.1536093    0.2117748
#> const     -0.5994123 0.23618396  -0.8783437   -0.3447495
#> 
#> $A$gdp
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.01579289 0.002824773 -0.01944425  -0.01303294
#> lag1_var2 -0.01826245 0.002515737 -0.02128345  -0.01613033
#> lag1_var3  1.02053126 0.002819248  1.01767726   1.02410553
#> const     -0.12470010 0.022502821 -0.15298625  -0.10407875
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean         sd 5% quantile 95% quantile
#> B[1,]_shrinkage        27.71604  14.914879    8.808264     41.38118
#> B[2,]_shrinkage       207.67877  86.219172  107.464262    302.20242
#> B[3,]_shrinkage       281.36856  95.485819  173.006066    388.20394
#> B[1,]_shrinkage_scale 222.48799 133.393145   73.287508    367.45905
#> B[2,]_shrinkage_scale 286.15890 112.396424  157.464986    406.38988
#> B[3,]_shrinkage_scale 328.80480  89.402610  207.034559    399.63718
#> B_global_scale         23.64678   9.158511   11.894722     30.76179
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.4123150 0.07691661   0.3203607    0.4912766
#> A[2,]_shrinkage       0.5677337 0.07222853   0.4783330    0.6324089
#> A[3,]_shrinkage       0.3744769 0.08634127   0.2663945    0.4566396
#> A[1,]_shrinkage_scale 6.7011377 1.53667541   4.7750182    8.2522204
#> A[2,]_shrinkage_scale 6.9352255 1.62306990   5.6533331    9.0506366
#> A[3,]_shrinkage_scale 5.3664359 2.06407487   3.0124286    7.5032875
#> A_global_scale        0.8011152 0.21831936   0.5331786    1.0384187
#> 
#> 
```
