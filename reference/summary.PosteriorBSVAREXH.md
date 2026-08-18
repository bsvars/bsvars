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
#> B[1,1] 0.1950818 0.007386825   0.1861644     0.206435
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -27.92516 1.138433   -29.52635    -26.51888
#> B[2,2]  24.54653 1.005829    23.31432     25.96066
#> 
#> $B$equation3
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -24.92590 2.945665   -29.80607    -21.33459
#> B[3,2] -26.66552 2.415342   -29.85635    -23.23412
#> B[3,3]  49.21074 1.529060    46.85936     51.25532
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd   5% quantile 95% quantile
#> lag1_var1  1.122728008 0.03267780  1.0866569720   1.16400814
#> lag1_var2  0.007228053 0.01573271 -0.0136945785   0.02885882
#> lag1_var3 -0.811008763 0.04318803 -0.8636640218  -0.76395300
#> const      0.191010374 0.12888434  0.0003571099   0.35473050
#> 
#> $A$equation2
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.1963453 0.04035519   0.1394641  0.242059862
#> lag1_var2  0.9626336 0.02064907   0.9287501  0.986814968
#> lag1_var3 -0.9841728 0.05428202  -1.0405745 -0.907727042
#> const     -0.1673422 0.15608413  -0.4360478 -0.007150803
#> 
#> $A$equation3
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 0.207136501 0.03778811  0.16783406   0.25414577
#> lag1_var2 0.002046394 0.01578519 -0.01761875   0.02272994
#> lag1_var3 0.007824502 0.05055534 -0.05234482   0.06176869
#> const     0.194923949 0.12282054  0.02702078   0.33972709
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        77.45673  35.86833    45.06077    138.55894
#> B[2,]_shrinkage       201.55735  79.17090    85.86337    294.74631
#> B[3,]_shrinkage       408.23187 164.55942   230.30902    684.36444
#> B[1,]_shrinkage_scale 648.24336 249.92393   264.43779    944.44491
#> B[2,]_shrinkage_scale 720.72131 330.94139   333.62969   1163.16506
#> B[3,]_shrinkage_scale 842.88862 357.15778   427.11752   1382.55387
#> B_global_scale         61.63043  20.77670    33.55365     89.28196
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.7715838 0.35323743   0.4187094    1.3127986
#> A[2,]_shrinkage       0.5150829 0.33849105   0.1987464    1.0993331
#> A[3,]_shrinkage       0.5008191 0.17353390   0.3083176    0.7833503
#> A[1,]_shrinkage_scale 6.2254699 2.36838328   3.3431848    9.5206337
#> A[2,]_shrinkage_scale 5.2408737 2.11328721   2.6976303    8.0964944
#> A[3,]_shrinkage_scale 5.6918404 1.71810008   4.6008548    8.8111871
#> A_global_scale        0.6157048 0.09386147   0.4665042    0.7214287
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
#>        mean          sd 5% quantile 95% quantile
#> B[1,1] 0.36 0.007948277   0.3517644    0.3723926
#> 
#> $B$equation2
#>            mean        sd 5% quantile 95% quantile
#> B[2,1] -30.4201 0.7000894   -31.17961    -29.29732
#> B[2,2]  18.3022 0.4179408    17.63622     18.77111
#> 
#> $B$equation3
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -19.62228 2.132645   -22.85493    -17.35016
#> B[3,2] -33.23079 1.540400   -35.19476    -30.99765
#> B[3,3]  32.61794 1.156033    31.00875     34.22015
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.878519577 0.04666489  0.82848062   0.95015567
#> lag1_var2 -0.005927987 0.01875567 -0.03092058   0.01825931
#> lag1_var3 -0.205777635 0.05544759 -0.28980097  -0.14948489
#> const      0.026307715 0.14656928 -0.16838531   0.23354672
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.08413602 0.03863600  -0.1359218  -0.03364269
#> lag1_var2  0.95304555 0.02822532   0.9118699   0.98169522
#> lag1_var3 -0.48499150 0.04202349  -0.5305355  -0.42979233
#> const     -0.29631634 0.19166524  -0.5762592  -0.08488350
#> 
#> $A$equation3
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.10936987 0.06027696 -0.17922786  -0.02405619
#> lag1_var2 -0.00480963 0.03321864 -0.05405314   0.02703940
#> lag1_var3  0.31420388 0.06868683  0.21491706   0.39609342
#> const      0.13191283 0.23680995 -0.20055004   0.37672107
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        111.88880  76.84561    28.26971     229.1695
#> B[2,]_shrinkage        134.67598  51.44436    52.25262     201.2661
#> B[3,]_shrinkage        340.80128 107.84168   215.18583     507.4172
#> B[1,]_shrinkage_scale  810.09355 375.69016   418.99333    1357.3376
#> B[2,]_shrinkage_scale  769.07024 322.49488   357.88993    1226.8057
#> B[3,]_shrinkage_scale 1075.93905 596.73971   438.00761    1932.8645
#> B_global_scale          79.80372  41.82807    40.38212     141.0267
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.6361864 0.4442558   0.1958202    1.3210973
#> A[2,]_shrinkage       0.3545144 0.1823720   0.2009763    0.6539642
#> A[3,]_shrinkage       0.5218119 0.3626142   0.1722698    1.0772999
#> A[1,]_shrinkage_scale 6.6705299 2.4607924   3.6161782   10.0247345
#> A[2,]_shrinkage_scale 5.0843742 2.5081332   2.4938621    8.8303147
#> A[3,]_shrinkage_scale 5.6335297 1.5981276   3.5397016    7.8462150
#> A_global_scale        0.6982590 0.1977076   0.4990999    1.0052183
#> 
#> 
```
