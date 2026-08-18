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
#> B[1,1] 0.2116311 0.01060514   0.1962503    0.2242864
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -21.54674 2.036245   -23.57757    -18.53032
#> B[2,2]  15.20813 1.439121    13.07179     16.64093
#> 
#> $B$equation3
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -16.26276 1.881174   -18.76321   -13.226860
#> B[3,2] -10.42290 1.403429   -12.66094    -8.707436
#> B[3,3]  63.82473 6.091430    56.36304    71.746663
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd  5% quantile 95% quantile
#> lag1_var1  1.08487655 0.02568477  1.044161122   1.11684885
#> lag1_var2  0.02378637 0.02190961 -0.009687296   0.05453906
#> lag1_var3 -0.69397138 0.03122368 -0.734000584  -0.64455321
#> const      0.36000735 0.18402013  0.071919156   0.61363041
#> 
#> $A$equation2
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.1839861 0.04712201   0.1064854    0.2515900
#> lag1_var2  1.0494745 0.02837551   1.0130665    1.0976114
#> lag1_var3 -1.0647202 0.05915676  -1.1468638   -0.9691386
#> const      0.6470366 0.23525527   0.3612072    1.0762151
#> 
#> $A$equation3
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 0.05953941 0.01004850 0.043150807   0.07190733
#> lag1_var2 0.01648989 0.01011538 0.001315071   0.03140280
#> lag1_var3 0.63734593 0.01296474 0.619536080   0.65763925
#> const     0.20793420 0.08502764 0.081371467   0.32031474
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean         sd 5% quantile 95% quantile
#> B[1,]_shrinkage        177.3843  128.88677    44.04111     415.5104
#> B[2,]_shrinkage        292.0554  167.50675   110.53172     539.5998
#> B[3,]_shrinkage        608.9864  298.06395   281.04294    1274.9855
#> B[1,]_shrinkage_scale 1780.2065 1003.47843   579.29029    3328.4173
#> B[2,]_shrinkage_scale 2090.5219  986.41427   960.61927    3301.9961
#> B[3,]_shrinkage_scale 3091.8500 1515.48377  1398.73940    5387.0873
#> B_global_scale         225.1973   96.71785   115.46245     362.7444
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.5783532 0.2904529   0.2249501    0.9752735
#> A[2,]_shrinkage       0.6919437 0.2063999   0.3377913    0.9932344
#> A[3,]_shrinkage       0.5850865 0.4351437   0.1380479    1.5006247
#> A[1,]_shrinkage_scale 6.1970814 2.7207538   3.3175595   12.0914192
#> A[2,]_shrinkage_scale 7.3869019 2.3398342   5.2206970   10.9145751
#> A[3,]_shrinkage_scale 6.1237569 2.7345493   2.4313459    9.3250740
#> A_global_scale        0.7330675 0.1688981   0.5313876    1.0130647
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
#> B[1,1] 0.4031323 0.02098414   0.3789747    0.4396861
#> 
#> $B$equation2
#>              mean        sd 5% quantile 95% quantile
#> B[2,1] -26.671980 2.6834650  -30.969372   -22.875419
#> B[2,2]   6.670602 0.6728108    5.715388     7.752653
#> 
#> $B$equation3
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -14.81228 1.839227   -17.92127    -11.90816
#> B[3,2] -19.35212 2.026053   -21.97608    -15.46877
#> B[3,3]  53.51118 5.110564    44.33431     60.22048
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.89962035 0.01600919  0.87268696   0.92394350
#> lag1_var2 -0.07143909 0.01167294 -0.09146801  -0.05522074
#> lag1_var3 -0.14706552 0.02291255 -0.17872162  -0.10421645
#> const     -0.32190093 0.09737315 -0.49434071  -0.16055908
#> 
#> $A$equation2
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.1717509 0.05008555  -0.2457291  -0.11015705
#> lag1_var2  0.8194909 0.03659702   0.7658510   0.86335095
#> lag1_var3 -0.8895249 0.07002888  -0.9739598  -0.78394669
#> const     -0.3664772 0.30359347  -0.8343017  -0.01019672
#> 
#> $A$equation3
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.07008915 0.02372481 -0.10547232  -0.04323772
#> lag1_var2 -0.07544959 0.01521572 -0.09960045  -0.05573545
#> lag1_var3  0.61152357 0.03352840  0.57211860   0.66176964
#> const     -0.13309592 0.12298123 -0.33425476   0.02503312
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        253.0438  195.6140    51.50772     503.0741
#> B[2,]_shrinkage        428.6956  224.3638   195.15957     932.1126
#> B[3,]_shrinkage        521.5498  347.5046   310.98622     862.9732
#> B[1,]_shrinkage_scale 2408.5189 1617.7398   497.24814    4433.7536
#> B[2,]_shrinkage_scale 3241.5784 1722.0663  1129.18791    6779.5607
#> B[3,]_shrinkage_scale 3004.3596 1732.6650   897.58943    5160.2779
#> B_global_scale         263.6398  149.7809    79.03177     445.6088
#> 
#> $hyper$A
#>                             mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage        1.2309783 0.8484448   0.6063493     2.705039
#> A[2,]_shrinkage        1.2122697 0.4712046   0.5757270     2.078995
#> A[3,]_shrinkage        0.9415989 0.5835351   0.3822900     2.218748
#> A[1,]_shrinkage_scale 12.3583071 5.3971557   6.5529989    22.947853
#> A[2,]_shrinkage_scale 12.1564900 3.1980676   9.2338305    18.325919
#> A[3,]_shrinkage_scale 10.8110563 3.7538400   5.2753914    18.565116
#> A_global_scale         1.2281601 0.3133722   0.7882670     1.719829
#> 
#> 
```
