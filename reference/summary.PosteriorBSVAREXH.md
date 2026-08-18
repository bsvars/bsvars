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
  [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md)
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

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_bsvar_exh`](https://bsvars.org/bsvars/reference/specify_bsvar_exh.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# specify the model and set seed
spec  = specify_bsvar_exh$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.

# run the burn-in
burn  = estimate(spec, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-exH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# estimate the model
post  = estimate(burn, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-exH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
summary(post)
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Posterior summary of the parameters             |
#>  **************************************************|
#> $B
#> $B$equation1
#>             mean          sd 5% quantile 95% quantile
#> B[1,1] 0.1943314 0.003573542   0.1897768    0.1998807
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -27.72623 1.172094   -29.08224    -26.15460
#> B[2,2]  24.35952 1.031839    22.98800     25.56466
#> 
#> $B$equation3
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -25.44811 2.176290   -27.75310    -22.02290
#> B[3,2] -25.94231 1.213393   -27.25237    -24.16975
#> B[3,3]  48.97031 2.008426    46.43129     52.06900
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  1.04714777 0.02417173  1.01415457   1.08129407
#> lag1_var2 -0.04510745 0.01562500 -0.06069498  -0.02119631
#> lag1_var3 -0.68738265 0.03290177 -0.73482269  -0.64263861
#> const     -0.11471322 0.12870118 -0.26877312   0.07591409
#> 
#> $A$equation2
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.1106343 0.01856961   0.0879973    0.1370479
#> lag1_var2  0.9105941 0.01732611   0.8868856    0.9283742
#> lag1_var3 -0.8451298 0.03158617  -0.8901443   -0.8115252
#> const     -0.4478280 0.11459111  -0.5951536   -0.2958949
#> 
#> $A$equation3
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1  0.12254124 0.018224647  0.10215787   0.15125910
#> lag1_var2 -0.05078994 0.008450724 -0.06215803  -0.03914794
#> lag1_var3  0.14215438 0.027819354  0.09971572   0.16966187
#> const     -0.12308697 0.063305490 -0.20011946  -0.02536216
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        117.5127  98.19577    41.37149     293.3569
#> B[2,]_shrinkage        195.1066  71.69009   113.59488     313.0553
#> B[3,]_shrinkage        464.2638 255.05408   214.44965     844.7123
#> B[1,]_shrinkage_scale  954.9601 462.54736   414.55344    1637.0272
#> B[2,]_shrinkage_scale 1331.5235 500.05928   818.49520    2141.0515
#> B[3,]_shrinkage_scale 1577.2640 655.39197   836.37670    2519.7002
#> B_global_scale         121.8244  53.07003    54.97085     188.0653
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3036732 0.08974605   0.1871829    0.4386307
#> A[2,]_shrinkage       0.4387003 0.23234651   0.1646000    0.7829274
#> A[3,]_shrinkage       0.3478454 0.12004359   0.2046977    0.5193737
#> A[1,]_shrinkage_scale 4.0203611 1.30346789   2.1262678    5.6700098
#> A[2,]_shrinkage_scale 3.9715870 1.83657985   1.8460573    6.4589963
#> A[3,]_shrinkage_scale 3.7505495 0.70775816   2.9189314    4.8056357
#> A_global_scale        0.4692067 0.09879356   0.3501500    0.6271736
#> 
#> 

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_exh$new() |>
  estimate(S = 10) |> 
  estimate(S = 10) |> 
  summary()
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-exH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-exH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
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
#> B[1,1] 0.1498624 0.00387681   0.1445524    0.1553667
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -26.40883 1.072637   -28.08844    -25.18231
#> B[2,2]  26.01809 1.056362    24.81718     27.67345
#> 
#> $B$equation3
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -26.22162 1.485965   -28.43279    -24.43803
#> B[3,2] -23.70369 1.542712   -25.38226    -21.46653
#> B[3,3]  56.36251 2.386406    53.45083     60.03375
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.73019523 0.03839422   0.6956703   0.79552768
#> lag1_var2  0.05719942 0.02247934   0.0304175   0.08691559
#> lag1_var3 -0.55410676 0.04982952  -0.6391686  -0.50412099
#> const      0.53721378 0.18821248   0.3346482   0.80518550
#> 
#> $A$equation2
#>                 mean         sd  5% quantile 95% quantile
#> lag1_var1 -0.2417666 0.03815155 -0.283862622   -0.1838159
#> lag1_var2  1.0189301 0.02064582  0.992595660    1.0444222
#> lag1_var3 -0.5944538 0.05015157 -0.670237672   -0.5409237
#> const      0.2149482 0.17287782  0.004987791    0.4456555
#> 
#> $A$equation3
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.19831135 0.02908391 -0.23425531  -0.15336645
#> lag1_var2  0.04928006 0.02008086  0.02374429   0.07669085
#> lag1_var3  0.45366095 0.03892564  0.39254719   0.50108969
#> const      0.47003849 0.16624790  0.26644802   0.70579095
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        62.14927  33.01770    27.82442     113.2151
#> B[2,]_shrinkage       184.21610  58.40251   110.52157     261.9270
#> B[3,]_shrinkage       418.95119 140.80552   266.07286     617.8268
#> B[1,]_shrinkage_scale 606.54537 361.23659   297.35387    1235.3809
#> B[2,]_shrinkage_scale 850.09109 446.51099   343.81542    1451.6957
#> B[3,]_shrinkage_scale 983.59966 595.47316   295.02256    1795.5992
#> B_global_scale         76.40387  46.56782    30.54573     150.4627
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.5349482 0.1033343   0.4229460    0.7029435
#> A[2,]_shrinkage       0.5744162 0.1550988   0.3454721    0.7881580
#> A[3,]_shrinkage       0.4856801 0.1865879   0.2123740    0.6955173
#> A[1,]_shrinkage_scale 6.1075795 1.0570574   4.7501953    7.7446518
#> A[2,]_shrinkage_scale 7.4665577 1.2828881   6.4379035    9.5525877
#> A[3,]_shrinkage_scale 6.3429249 1.7216787   3.9003970    8.7413249
#> A_global_scale        0.7610958 0.1669010   0.5741949    0.9892830
#> 
#> 
```
