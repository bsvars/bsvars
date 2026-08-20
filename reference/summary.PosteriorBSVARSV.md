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
specification  = specify_bsvar_sv$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.
burn_in        = estimate(specification, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
posterior      = estimate(burn_in, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
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
#> B[1,1] 0.7524348 0.02680938   0.7228779     0.784886
#> 
#> $B$gs
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -29.18650 1.455234   -31.06154    -27.82874
#> B[2,2]  22.85501 1.146352    21.81268     24.33956
#> 
#> $B$gdp
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -24.70986 2.829929   -27.61018    -21.13874
#> B[3,2] -40.03415 2.815522   -42.86223    -36.45396
#> B[3,3]  40.80452 2.849672    37.63170     43.78543
#> 
#> 
#> $A
#> $A$ttr
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.89332372 0.02624981  0.86805693  0.926407899
#> lag1_var2 -0.02199348 0.01196633 -0.03314638 -0.006506866
#> lag1_var3 -0.02161809 0.03646169 -0.06609304  0.012092153
#> const     -0.09774041 0.12071267 -0.23478019  0.040821776
#> 
#> $A$gs
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.08395921 0.017697743  -0.1033178  -0.06227824
#> lag1_var2  0.92954859 0.009041927   0.9176759   0.93774001
#> lag1_var3 -0.08444812 0.022186023  -0.1132686  -0.06331227
#> const     -0.48623468 0.084894768  -0.5917290  -0.40110168
#> 
#> $A$gdp
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.07947027 0.024964175 -0.10155669  -0.04712624
#> lag1_var2 -0.05113679 0.006341127 -0.05845706  -0.04425603
#> lag1_var3  0.81373284 0.033281303  0.76956737   0.83951408
#> const     -0.26672639 0.071372415 -0.34383689  -0.18026460
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean         sd 5% quantile 95% quantile
#> B[1,]_shrinkage        10.56150   4.417036    7.445969     16.54145
#> B[2,]_shrinkage       145.71418  43.398668  115.109980    205.47844
#> B[3,]_shrinkage       385.43400 198.772565  253.231803    659.28887
#> B[1,]_shrinkage_scale 142.57367  55.492057   88.148040    214.25706
#> B[2,]_shrinkage_scale 219.03958  62.145313  150.250820    280.88687
#> B[3,]_shrinkage_scale 292.36507 105.596079  184.011970    409.71809
#> B_global_scale         19.21773   7.523432   11.814163     28.30318
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.6659376 0.22767495   0.4126714    0.8767868
#> A[2,]_shrinkage       0.5197614 0.14036953   0.3733379    0.6861794
#> A[3,]_shrinkage       0.4473919 0.16233770   0.3268141    0.6689358
#> A[1,]_shrinkage_scale 6.1306701 0.90188535   4.9649235    6.9163193
#> A[2,]_shrinkage_scale 5.8914369 0.45376671   5.3192419    6.3272534
#> A[3,]_shrinkage_scale 4.6908156 0.95583941   3.4338075    5.5289702
#> A_global_scale        0.6194208 0.09542415   0.5074372    0.7251827
#> 
#> 

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_sv$new() |>
  estimate(S = 5) |> 
  estimate(S = 5) |> 
  summary() -> summ
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
summ
#> $B
#> $B$ttr
#>             mean         sd 5% quantile 95% quantile
#> B[1,1] 0.2260005 0.01933395     0.20775    0.2499536
#> 
#> $B$gs
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -50.03601 1.928703   -51.85349    -47.53856
#> B[2,2]  46.71821 1.810868    44.36862     48.40386
#> 
#> $B$gdp
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -40.75562 4.340580   -46.28969    -36.65580
#> B[3,2] -40.16594 2.911820   -43.37082    -37.05647
#> B[3,3]  68.24234 5.657974    62.98978     75.53306
#> 
#> 
#> $A
#> $A$ttr
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1  1.00622447 0.015032907  0.99272026    1.0237934
#> lag1_var2 -0.02461387 0.008451842 -0.03478261   -0.0158823
#> lag1_var3 -0.72667406 0.018580635 -0.74829688   -0.7107594
#> const      0.29181455 0.081875862  0.18951259    0.3749824
#> 
#> $A$gs
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1  0.05401929 0.011936999  0.03929333   0.06670229
#> lag1_var2  0.92011159 0.008929686  0.91384665   0.93242117
#> lag1_var3 -0.82675052 0.014958504 -0.84531295  -0.81148609
#> const     -0.14310303 0.071431348 -0.20829499  -0.04838837
#> 
#> $A$gdp
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1  0.07373846 0.016789478  0.05058289   0.08496491
#> lag1_var2 -0.04119476 0.007344152 -0.05074327  -0.03447128
#> lag1_var3  0.03107087 0.024003716  0.01591822   0.06429912
#> const      0.29191077 0.045625286  0.23071423   0.33157457
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         27.41060  22.62285    6.049032     56.19062
#> B[2,]_shrinkage        610.37689 319.37753  379.651974   1047.15314
#> B[3,]_shrinkage       1226.97252 406.37036  764.020658   1656.57916
#> B[1,]_shrinkage_scale  327.61610 249.87030  124.636789    662.29497
#> B[2,]_shrinkage_scale  549.12819 366.11095  280.613639   1040.54703
#> B[3,]_shrinkage_scale  809.78258 687.02034  249.032814   1707.50630
#> B_global_scale          38.63965  26.09554   16.490902     72.09242
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage        1.526854 0.3104712   1.2698616     1.907183
#> A[2,]_shrinkage        1.341085 0.5019571   0.8649977     1.974786
#> A[3,]_shrinkage        2.135608 0.9946437   0.9048409     3.071828
#> A[1,]_shrinkage_scale 15.171483 6.4602221   8.3919109    23.099540
#> A[2,]_shrinkage_scale 14.301220 4.2423530   9.9330633    19.307504
#> A[3,]_shrinkage_scale 19.165159 5.8850714  11.5946978    24.500925
#> A_global_scale         1.525911 0.6975033   1.0107929     2.476682
#> 
#> 
```
