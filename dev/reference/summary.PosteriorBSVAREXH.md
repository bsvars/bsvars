# Provides posterior summary of heteroskedastic Structural VAR estimation

Provides posterior mean, standard deviations, as well as 5 and 95
percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper parameters.

## Usage

``` r
# S3 method for class 'PosteriorBSVAREXH'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorBSVAREXH obtained using the
  [`estimate()`](https://bsvars.org/bsvars/dev/reference/estimate.md)
  function applied to heteroskedastic Bayesian Structural VAR model
  specification set by function `specify_bsvar_exh$new()` containing
  draws from the posterior distribution of the parameters.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper-parameters.

## See also

[`estimate`](https://bsvars.org/bsvars/dev/reference/estimate.md),
[`specify_bsvar_exh`](https://bsvars.org/bsvars/dev/reference/specify_bsvar_exh.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
spec  = specify_bsvar_exh$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.
burn  = estimate(spec, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-exH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
post  = estimate(burn, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-exH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
summ  = summary(post)
summ
#> $B
#> $B$ttr
#>             mean          sd 5% quantile 95% quantile
#> B[1,1] 0.1762576 0.009649735   0.1667138    0.1878257
#> 
#> $B$gs
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -12.03508 0.2197975   -12.24227    -11.75751
#> B[2,2]  38.16017 0.7145051    37.26542     38.85756
#> 
#> $B$gdp
#>             mean        sd 5% quantile 95% quantile
#> B[3,1] -9.274347 0.5423291   -9.879874  -8.74039458
#> B[3,2] -1.056237 1.1008278   -2.519865  -0.07050846
#> B[3,3] 95.346271 4.7424752   89.609671 100.32827240
#> 
#> 
#> $A
#> $A$ttr
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  1.10254835 0.04950979   1.0575155    1.1561864
#> lag1_var2 -0.32703157 0.03807676  -0.3732523   -0.2899285
#> lag1_var3 -0.45939794 0.07213487  -0.5396176   -0.4005355
#> const     -0.05776938 0.26403775  -0.3387262    0.2353076
#> 
#> $A$gs
#>                 mean          sd  5% quantile 95% quantile
#> lag1_var1  0.0257838 0.019086315  0.008629869   0.05085048
#> lag1_var2  0.8523751 0.007095037  0.843409512   0.85876212
#> lag1_var3 -0.1311582 0.024507485 -0.164284572  -0.11260940
#> const     -0.4189048 0.037911696 -0.465493306  -0.37953934
#> 
#> $A$gdp
#>                   mean         sd  5% quantile 95% quantile
#> lag1_var1  0.002011602 0.01247682 -0.008570599   0.01876388
#> lag1_var2 -0.040483676 0.00671981 -0.047641329  -0.03283659
#> lag1_var3  0.963870063 0.01685006  0.940986415   0.97754612
#> const     -0.079462222 0.05923717 -0.148224008  -0.01459314
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean         sd 5% quantile 95% quantile
#> B[1,]_shrinkage        21.51484   9.072669    14.51556     33.86436
#> B[2,]_shrinkage       206.44443 100.779816   107.54670    336.28857
#> B[3,]_shrinkage       984.77220 483.129501   438.26472   1453.82419
#> B[1,]_shrinkage_scale 260.28476  90.492868   136.72315    328.46906
#> B[2,]_shrinkage_scale 597.06232 281.932244   270.39096    871.30957
#> B[3,]_shrinkage_scale 594.74606 234.653706   289.61177    793.65891
#> B_global_scale         41.49449  23.844876    16.72619     70.70697
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.6306448 0.1936209   0.4474472     0.867831
#> A[2,]_shrinkage       1.3784131 0.9293588   0.5220248     2.551558
#> A[3,]_shrinkage       0.4722017 0.1521237   0.3037819     0.625477
#> A[1,]_shrinkage_scale 8.2142387 2.8886035   6.0859143    12.095366
#> A[2,]_shrinkage_scale 9.7591647 3.8437264   7.0112725    14.818054
#> A[3,]_shrinkage_scale 6.7497373 2.9255750   4.2687227    10.687518
#> A_global_scale        0.9413422 0.2022958   0.7842115     1.216363
#> 
#> 

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_exh$new() |>
  estimate(S = 5) |> 
  estimate(S = 5) |> 
  summary() -> summ
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-exH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-exH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
summ
#> $B
#> $B$ttr
#>             mean        sd 5% quantile 95% quantile
#> B[1,1] 0.6772801 0.0277719   0.6412433    0.7043094
#> 
#> $B$gs
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -14.97996 0.6218884   -15.67173    -14.50338
#> B[2,2]  36.60626 1.4031211    35.50984     38.16553
#> 
#> $B$gdp
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -32.599375 2.1684339  -35.556163   -31.142645
#> B[3,2] -14.694120 1.7920486  -16.754682   -12.966379
#> B[3,3]   9.280258 0.4844604    8.860317     9.930967
#> 
#> 
#> $A
#> $A$ttr
#>                    mean         sd 5% quantile 95% quantile
#> lag1_var1  0.9148405270 0.03039116  0.88378537   0.95339516
#> lag1_var2  0.0009622354 0.01862093 -0.02388268   0.01608271
#> lag1_var3 -0.0941994960 0.03961495 -0.14570568  -0.05616436
#> const     -0.0603111811 0.18787151 -0.31158875   0.09164464
#> 
#> $A$gs
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.03701744 0.02734435 -0.06670091 -0.006142686
#> lag1_var2  0.96029001 0.01620226  0.94125225  0.978468901
#> lag1_var3 -0.03147040 0.03593063 -0.07259067  0.005774853
#> const     -0.39187333 0.13854636 -0.54375147 -0.230030466
#> 
#> $A$gdp
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 0.005446755 0.15908870 -0.19107321   0.17328816
#> lag1_var2 0.026947073 0.04250941 -0.02910031   0.06491875
#> lag1_var3 0.166389225 0.18898471 -0.03476442   0.39897473
#> const     0.030110622 0.47127043 -0.60386905   0.38938526
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        45.73327  43.75771    18.40535    104.84067
#> B[2,]_shrinkage       153.58389  63.43594    95.51166    235.64271
#> B[3,]_shrinkage       123.16959  23.33709    96.84909    145.68585
#> B[1,]_shrinkage_scale 401.17710 192.90696   172.09981    589.62218
#> B[2,]_shrinkage_scale 481.02037 227.11475   266.08027    772.77365
#> B[3,]_shrinkage_scale 495.11484 159.06085   311.18951    660.06278
#> B_global_scale         38.25823  18.24212    17.61124     57.42393
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.4055815 0.26428953   0.1246307    0.7046854
#> A[2,]_shrinkage       0.4765268 0.16233958   0.3007998    0.6254538
#> A[3,]_shrinkage       0.8246637 0.47682368   0.3879185    1.4333542
#> A[1,]_shrinkage_scale 5.5646779 1.52823990   3.7296848    7.2551743
#> A[2,]_shrinkage_scale 5.3740295 1.83041373   3.2432132    7.4711532
#> A[3,]_shrinkage_scale 6.9795172 2.70739527   4.7726932   10.0779321
#> A_global_scale        0.7186764 0.07717518   0.6403026    0.8148988
#> 
#> 
```
