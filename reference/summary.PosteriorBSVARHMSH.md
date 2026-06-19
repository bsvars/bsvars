# Provides posterior summary of heteroskedastic Structural VAR estimation

Provides posterior mean, standard deviations, as well as 5 and 95
percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper parameters.

## Usage

``` r
# S3 method for class 'PosteriorBSVARHMSH'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorBSVARHMSH obtained using the
  [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md)
  function applied to heteroskedastic Bayesian Structural VAR model
  specification set by function `specify_bsvar_hmsh$new()` containing
  draws from the posterior distribution of the parameters.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper-parameters.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_bsvar_hmsh`](https://bsvars.org/bsvars/reference/specify_bsvar_hmsh.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# specify the model
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
#> B[1,1] 0.3858301 0.01429607   0.3626019    0.4039731
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -32.87709 1.8183725   -34.92512    -30.05164
#> B[2,2]  11.57399 0.6418358    10.57274     12.30785
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -16.175585 2.0360721  -19.056258   -12.654097
#> B[3,2]  -4.941834 0.8488482   -5.998684    -3.618954
#> B[3,3]  94.758790 4.6161934   88.960769   102.518189
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.96643270 0.03889707   0.9111644   1.01680127
#> lag1_var2 -0.10944356 0.01803938  -0.1325297  -0.07926666
#> lag1_var3 -0.15969297 0.04929852  -0.2231160  -0.09254806
#> const     -0.05296716 0.16144322  -0.2742659   0.21286537
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.01608894 0.12390673  -0.1305605    0.1782220
#> lag1_var2  0.69666770 0.03151873   0.6558518    0.7459544
#> lag1_var3 -0.59247195 0.15620614  -0.8046330   -0.4014773
#> const     -0.08443335 0.27954617  -0.4406902    0.3459477
#> 
#> $A$equation3
#>                   mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.003074835 0.013359604 -0.02691821   0.01728465
#> lag1_var2 -0.038037760 0.005965077 -0.04880218  -0.03131988
#> lag1_var3  0.939044357 0.015805716  0.91400699   0.96635557
#> const     -0.049492187 0.051011127 -0.14146909   0.01563266
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean         sd 5% quantile 95% quantile
#> B[1,]_shrinkage        184.6406   98.69646    79.32382     360.5670
#> B[2,]_shrinkage        461.6462  205.51957   205.83154     769.3691
#> B[3,]_shrinkage       1152.7025  492.29456   625.14246    1917.5389
#> B[1,]_shrinkage_scale 1890.4045  895.74998   786.82077    3240.1783
#> B[2,]_shrinkage_scale 3044.4537 1695.93264  1203.62425    6081.9687
#> B[3,]_shrinkage_scale 3063.6266 1531.85436   782.31355    5197.9277
#> B_global_scale         245.4201  114.23809    67.54973     436.9894
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.6853007 0.5180679   0.1676716    1.7255750
#> A[2,]_shrinkage       0.3858825 0.1444183   0.2176847    0.6666445
#> A[3,]_shrinkage       0.4358050 0.2959764   0.1348111    0.8640991
#> A[1,]_shrinkage_scale 6.6207675 3.2963779   2.8719575   11.7293318
#> A[2,]_shrinkage_scale 5.0335483 2.0687868   2.8067665    7.5415766
#> A[3,]_shrinkage_scale 4.9708558 1.7604745   2.3083028    7.7152698
#> A_global_scale        0.6823429 0.2399281   0.3123039    1.0598918
#> 
#> 

# workflow with the pipe |>
############################################################
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
#> B[1,1] 0.4710056 0.01900577   0.4396629    0.4966973
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -32.34958 2.2089714   -36.24607    -29.74164
#> B[2,2]  12.73309 0.8835278    11.66052     14.30261
#> 
#> $B$equation3
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -19.66228 1.963654   -22.42104    -16.67723
#> B[3,2] -26.41044 1.761362   -29.22353    -23.93199
#> B[3,3]  63.74238 3.847332    59.81175     69.61260
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd  5% quantile 95% quantile
#> lag1_var1  0.959968553 0.01446358  0.942089667   0.98606442
#> lag1_var2  0.013258726 0.01548634 -0.008255292   0.03809986
#> lag1_var3 -0.249191576 0.01837394 -0.276323049  -0.22623167
#> const     -0.007190086 0.13511175 -0.204699429   0.16192724
#> 
#> $A$equation2
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.009981620 0.02880984  -0.0609005   0.03476488
#> lag1_var2  1.033013857 0.03135136   0.9833988   1.06490894
#> lag1_var3 -0.744244291 0.04247516  -0.8036550  -0.66530352
#> const     -0.009773432 0.23164652  -0.3665954   0.22919176
#> 
#> $A$equation3
#>                   mean         sd  5% quantile 95% quantile
#> lag1_var1 0.0006517466 0.01461959 -0.016118405   0.02753346
#> lag1_var2 0.0320737901 0.01672946  0.002756688   0.05255891
#> lag1_var3 0.5903633018 0.02047775  0.550522158   0.61597894
#> const     0.1161632344 0.12732872 -0.095014513   0.28443324
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        122.5722  103.8193    38.70566     291.9160
#> B[2,]_shrinkage        280.6572  181.4243   115.84102     544.0309
#> B[3,]_shrinkage        591.9580  266.2726   283.93312    1048.5127
#> B[1,]_shrinkage_scale 1374.5938 1236.4778   224.09991    3412.8504
#> B[2,]_shrinkage_scale 2088.5542 1638.6283   453.88656    4895.0443
#> B[3,]_shrinkage_scale 2336.1795 1697.7577   451.15580    5248.7092
#> B_global_scale         186.7542  142.1608    40.14301     435.4140
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.2426710 0.12391945   0.1175700    0.4473416
#> A[2,]_shrinkage       0.3659739 0.16939605   0.1674291    0.7065550
#> A[3,]_shrinkage       0.2560133 0.14342009   0.1044590    0.4343166
#> A[1,]_shrinkage_scale 2.7642866 0.84703750   1.6216557    4.0179689
#> A[2,]_shrinkage_scale 3.6210997 1.20593801   2.2445176    5.7162164
#> A[3,]_shrinkage_scale 3.6130272 1.82297170   1.5453716    6.4096279
#> A_global_scale        0.4460977 0.07880817   0.3378004    0.5691421
#> 
#> 
```
