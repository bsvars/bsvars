# Provides posterior summary of homoskedastic Structural VAR estimation

Provides posterior mean, standard deviations, as well as 5 and 95
percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper parameters.

## Usage

``` r
# S3 method for class 'PosteriorBSVAR'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorBSVAR obtained using the
  [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md)
  function applied to homoskedastic Bayesian Structural VAR model
  specification set by function `specify_bsvar$new()` containing draws
  from the posterior distribution of the parameters.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper-parameters.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_bsvar`](https://bsvars.org/bsvars/reference/specify_bsvar.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
specification = specify_bsvar$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.
burn_in      = estimate(specification, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
posterior    = estimate(burn_in, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
summ         = summary(posterior)
summ
#> $B
#> $B$ttr
#>            mean        sd 5% quantile 95% quantile
#> B[1,1] 7.536293 0.1734733    7.347356     7.725792
#> 
#> $B$gs
#>            mean        sd 5% quantile 95% quantile
#> B[2,1] 29.99247 1.2279334    29.00884     31.63226
#> B[2,2] 14.79522 0.5633139    14.19704     15.35689
#> 
#> $B$gdp
#>             mean        sd 5% quantile 95% quantile
#> B[3,1] -17.77855 2.0789029   -20.11016    -15.27935
#> B[3,2]  33.30661 0.9252079    32.33007     34.46135
#> B[3,3]  26.91952 1.1070105    25.78426     28.24276
#> 
#> 
#> $A
#> $A$ttr
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1  0.98553353 0.023781304  0.96363863    1.0169604
#> lag1_var2 -0.04355804 0.005533047 -0.05040167   -0.0381727
#> lag1_var3  0.10423575 0.027883763  0.06926430    0.1331322
#> const      0.10404030 0.067844541  0.03975012    0.1834415
#> 
#> $A$gs
#>                  mean         sd  5% quantile 95% quantile
#> lag1_var1 -0.17550683 0.02342221 -0.202538619   -0.1514954
#> lag1_var2  1.06266801 0.00772124  1.054056693    1.0718664
#> lag1_var3  0.02045757 0.02943655 -0.008364567    0.0540878
#> const     -0.63125678 0.05786431 -0.708445332   -0.5835502
#> 
#> $A$gdp
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.2082288 0.02010503   0.1885361    0.2321626
#> lag1_var2 -0.1757823 0.01593815  -0.1930951   -0.1573425
#> lag1_var3  1.0557357 0.02233679   1.0305241    1.0782180
#> const      0.2571219 0.11760452   0.1159001    0.3893288
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        43.16168  18.44983    23.27314     64.39388
#> B[2,]_shrinkage       106.41331  19.97912    85.07403    129.48793
#> B[3,]_shrinkage       309.76003 108.22973   190.12689    420.51022
#> B[1,]_shrinkage_scale 239.27420  87.87394   155.17205    347.29033
#> B[2,]_shrinkage_scale 395.79483 151.56464   206.53399    553.06373
#> B[3,]_shrinkage_scale 414.86727 191.78389   247.28154    668.76734
#> B_global_scale         29.01334  13.40196    18.12956     47.13273
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.4393611 0.3436592   0.1382114    0.8648923
#> A[2,]_shrinkage       0.5360280 0.2060812   0.3333879    0.8011904
#> A[3,]_shrinkage       0.2544574 0.1221727   0.1460636    0.4178035
#> A[1,]_shrinkage_scale 6.1774844 2.5182867   4.0308846    9.4575260
#> A[2,]_shrinkage_scale 6.1271055 2.1587770   3.6108420    8.5764945
#> A[3,]_shrinkage_scale 3.9260006 1.1935848   2.9756340    5.5288085
#> A_global_scale        0.6253810 0.1040401   0.4933588    0.7257941
#> 
#> 

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar$new() |>
  estimate(S = 5) |> 
  estimate(S = 5) |> 
  summary() -> summ
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
summ
#> $B
#> $B$ttr
#>            mean        sd 5% quantile 95% quantile
#> B[1,1] 6.546383 0.7658193    5.717065      7.38822
#> 
#> $B$gs
#>            mean        sd 5% quantile 95% quantile
#> B[2,1] 31.80119 0.7612268    31.13962     32.65541
#> B[2,2] 12.41192 1.0700444    11.49411     13.59234
#> 
#> $B$gdp
#>              mean       sd 5% quantile 95% quantile
#> B[3,1] -11.502720 3.517164  -14.496895    -7.185701
#> B[3,2]  -5.773033 1.538792   -7.003716    -3.873503
#> B[3,3]  97.127922 3.494150   93.698437   101.420844
#> 
#> 
#> $A
#> $A$ttr
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.6973902 0.02320725  0.67563677    0.7263381
#> lag1_var2  0.0749102 0.01387004  0.06355813    0.0934071
#> lag1_var3  0.1885181 0.02617578  0.15774879    0.2183383
#> const     -0.5680396 0.14907146 -0.76091645   -0.4275507
#> 
#> $A$gs
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.5456248 0.06106868   0.4778457    0.6090900
#> lag1_var2  0.7778416 0.02658155   0.7488850    0.8090393
#> lag1_var3 -0.2425886 0.11612688  -0.3460075   -0.1036589
#> const      0.8244574 0.26930381   0.5534911    1.1499396
#> 
#> $A$gdp
#>                   mean          sd  5% quantile 95% quantile
#> lag1_var1 -0.004587467 0.009108945 -0.012546575  0.007110666
#> lag1_var2 -0.005200245 0.003200576 -0.009177892 -0.002793103
#> lag1_var3  1.007803610 0.011124282  0.993545011  1.017788208
#> const     -0.035712902 0.028440830 -0.072486530 -0.011945164
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        30.00012  11.69850    18.31179     44.83762
#> B[2,]_shrinkage       163.73403  81.17716   100.19646    272.86506
#> B[3,]_shrinkage       651.70725 191.66510   406.33222    822.66238
#> B[1,]_shrinkage_scale 352.02125 103.64913   226.00127    455.66870
#> B[2,]_shrinkage_scale 684.95464 278.94987   380.22148   1024.71540
#> B[3,]_shrinkage_scale 576.27268 282.00743   333.15793    934.42925
#> B_global_scale         45.95796  21.09949    21.27932     69.69094
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.6177006 0.1076198   0.4876319    0.7265400
#> A[2,]_shrinkage       1.1426718 0.5915749   0.5917430    1.9229697
#> A[3,]_shrinkage       0.4376916 0.2563026   0.1890792    0.7507777
#> A[1,]_shrinkage_scale 8.5020818 1.1870702   7.2045149    9.8888133
#> A[2,]_shrinkage_scale 9.3144880 1.1659814   7.9312613   10.5862369
#> A[3,]_shrinkage_scale 6.5007995 2.9974593   3.9282335   10.5116290
#> A_global_scale        0.8899747 0.1485249   0.7005882    1.0294250
#> 
#> 
```
