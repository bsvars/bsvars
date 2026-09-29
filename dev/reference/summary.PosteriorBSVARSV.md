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
  [`estimate()`](https://bsvars.org/bsvars/dev/reference/estimate.md)
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

[`estimate`](https://bsvars.org/bsvars/dev/reference/estimate.md),
[`specify_bsvar_sv`](https://bsvars.org/bsvars/dev/reference/specify_bsvar_sv.md)

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
#> B[1,1] 0.7934921 0.02351947   0.7658708    0.8156339
#> 
#> $B$gs
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -28.41708 1.391415   -30.15974    -27.00509
#> B[2,2]  21.99638 1.091112    20.85561     23.34875
#> 
#> $B$gdp
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -28.78153 3.796443   -33.58232    -25.17240
#> B[3,2] -42.03366 2.905845   -45.77608    -39.30929
#> B[3,3]  44.36194 2.910052    40.62435     46.61471
#> 
#> 
#> $A
#> $A$ttr
#>                  mean          sd  5% quantile 95% quantile
#> lag1_var1 0.863680478 0.016654485  0.844392536   0.88136877
#> lag1_var2 0.008470705 0.009519825 -0.003650662   0.01695774
#> lag1_var3 0.009772408 0.022647277 -0.016267612   0.03199468
#> const     0.150542898 0.066566005  0.061658679   0.20950831
#> 
#> $A$gs
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.06877186 0.009126865 -0.07816959  -0.05781414
#> lag1_var2  0.95768300 0.007265691  0.94797643   0.96407659
#> lag1_var3 -0.10411705 0.011749578 -0.11614012  -0.09075305
#> const     -0.20229358 0.065501267 -0.28784070  -0.14364455
#> 
#> $A$gdp
#>                   mean          sd  5% quantile 95% quantile
#> lag1_var1 -0.127233298 0.012900727 -0.138434099 -0.110464335
#> lag1_var2 -0.001788414 0.003161205 -0.005012568  0.001736003
#> lag1_var3  0.868739298 0.017301056  0.845871778  0.883812045
#> const      0.188740378 0.028490106  0.167173244  0.227747858
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        20.79619  13.08899    10.22739     38.01958
#> B[2,]_shrinkage       143.58813  26.41079   109.03825    168.86374
#> B[3,]_shrinkage       456.31205 132.20442   348.34454    633.31711
#> B[1,]_shrinkage_scale 168.13780  51.40472   108.17284    211.74677
#> B[2,]_shrinkage_scale 446.35875 152.78048   269.13515    582.11192
#> B[3,]_shrinkage_scale 450.77926 206.99325   215.61117    683.06552
#> B_global_scale         29.72462  11.50526    15.86490     40.02968
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.4564134 0.08722311   0.3554136    0.5549601
#> A[2,]_shrinkage       0.5241820 0.07631416   0.4665181    0.6281272
#> A[3,]_shrinkage       0.4969062 0.24271739   0.3448422    0.8257764
#> A[1,]_shrinkage_scale 6.7671389 1.71454513   5.2277430    8.8188544
#> A[2,]_shrinkage_scale 7.0872304 1.70273326   5.1515486    9.0977713
#> A[3,]_shrinkage_scale 5.3064086 1.30283112   3.8649085    6.5879543
#> A_global_scale        0.7266842 0.11557729   0.6137608    0.8623070
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
#>             mean          sd 5% quantile 95% quantile
#> B[1,1] 0.1658915 0.009579287   0.1575115    0.1782538
#> 
#> $B$gs
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -32.61226 2.392159   -35.13462    -29.94118
#> B[2,2]  29.26501 2.152437    26.86436     31.53303
#> 
#> $B$gdp
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -38.66031 3.138151   -42.86557    -36.19429
#> B[3,2] -49.12746 1.399741   -50.55635    -47.35731
#> B[3,3]  83.83042 1.842257    81.76888     86.03355
#> 
#> 
#> $A
#> $A$ttr
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  1.0159257 0.01848435   0.9931062    1.0330297
#> lag1_var2 -0.1652018 0.01007586  -0.1788656   -0.1571328
#> lag1_var3 -0.6358188 0.02509660  -0.6598767   -0.6057287
#> const     -0.4847050 0.08667731  -0.6018924   -0.4201600
#> 
#> $A$gs
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1  0.08939667 0.010535050  0.07615288    0.1000771
#> lag1_var2  0.78528671 0.006918046  0.77674006    0.7906839
#> lag1_var3 -0.79196405 0.014550324 -0.80520039   -0.7729762
#> const     -0.80379100 0.058349042 -0.86890420   -0.7476414
#> 
#> $A$gdp
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1  0.09539317 0.005194016  0.08913043   0.09980417
#> lag1_var2 -0.19552611 0.005055709 -0.20164485  -0.19133589
#> lag1_var3  0.19511836 0.006368486  0.19070462   0.20387652
#> const     -0.65034912 0.032087557 -0.68662573  -0.61529300
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        29.13019  34.87838    8.324064     76.51529
#> B[2,]_shrinkage       170.44708  61.57287  103.639282    233.11938
#> B[3,]_shrinkage       851.63484 398.17552  550.595377   1392.25360
#> B[1,]_shrinkage_scale 175.75753 115.89700   86.740246    333.49178
#> B[2,]_shrinkage_scale 315.33396 138.37002  147.493410    456.39951
#> B[3,]_shrinkage_scale 381.82142 260.46348  134.600928    717.98135
#> B_global_scale         23.79956  12.38809    9.499908     37.69664
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.6228318 0.3098684   0.2612736    0.9705424
#> A[2,]_shrinkage       0.6558498 0.1847879   0.4417263    0.8710345
#> A[3,]_shrinkage       0.7497111 0.2325459   0.4986533    1.0268504
#> A[1,]_shrinkage_scale 8.1026050 3.6183497   4.1043741   12.0063491
#> A[2,]_shrinkage_scale 7.5399166 2.3994948   5.1997855   10.6166090
#> A[3,]_shrinkage_scale 6.7645410 2.4032275   4.2816445    9.7957778
#> A_global_scale        0.8247290 0.1317844   0.6669571    0.9412495
#> 
#> 
```
