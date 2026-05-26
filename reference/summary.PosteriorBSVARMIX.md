# Provides posterior summary of non-normal Structural VAR estimation

Provides posterior mean, standard deviations, as well as 5 and 95
percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper parameters.

## Usage

``` r
# S3 method for class 'PosteriorBSVARMIX'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorBSVARMIX obtained using the
  [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md)
  function applied to non-normal Bayesian Structural VAR model
  specification set by function `specify_bsvar_mix$new()` containing
  draws from the posterior distribution of the parameters.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper-parameters.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_bsvar_mix`](https://bsvars.org/bsvars/reference/specify_bsvar_mix.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
set.seed(123)
specification  = specify_bsvar_mix$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.

# run the burn-in
burn_in        = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
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
#>  Gibbs sampler for the SVAR-finiteMIX model             |
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
#> B[1,1] 0.8904943 0.03202662     0.83927    0.9552197
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -18.25836 0.9823318   -19.79896    -16.79848
#> B[2,2]  33.71299 1.8460289    30.89029     36.86304
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -32.061312 2.4272183  -36.045891   -28.811575
#> B[3,2] -17.619554 2.1735691  -21.493629   -14.849076
#> B[3,3]   6.398558 0.4569877    5.704414     7.238467
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.95064036 0.03113784   0.9073329  0.991665988
#> lag1_var2 -0.01250789 0.02236178  -0.0561789  0.009695998
#> lag1_var3 -0.08425415 0.03946637  -0.1409958 -0.035678976
#> const     -0.12701615 0.18263637  -0.4977556  0.092453281
#> 
#> $A$equation2
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.0308342 0.02316904 -0.05893011  0.001919201
#> lag1_var2  0.9586752 0.01215821  0.93912126  0.978493212
#> lag1_var3 -0.0382990 0.02898846 -0.07938673 -0.004473503
#> const     -0.3985921 0.09597595 -0.52481143 -0.241431925
#> 
#> $A$equation3
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.036306417 0.17313369  -0.1781081    0.3217692
#> lag1_var2 -0.004172112 0.09770471  -0.1667314    0.1022118
#> lag1_var3  0.001889492 0.21885398  -0.3585211    0.2395674
#> const     -0.106963498 0.72023601  -1.1879256    0.6436369
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        42.94895  16.86169    17.18037     68.59133
#> B[2,]_shrinkage       200.59144 110.26840    91.82747    434.54175
#> B[3,]_shrinkage       204.79744  93.75785   131.69691    380.45135
#> B[1,]_shrinkage_scale 516.49858 148.18965   314.74385    733.04420
#> B[2,]_shrinkage_scale 832.72217 305.03675   409.73820   1351.59419
#> B[3,]_shrinkage_scale 800.49882 254.57401   446.80199   1256.17610
#> B_global_scale         68.90024  19.22539    46.70709    102.53140
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.4958863 0.3007960   0.2183707    0.9974715
#> A[2,]_shrinkage       0.3423374 0.1725593   0.1629529    0.6699417
#> A[3,]_shrinkage       0.6550550 0.2650492   0.3230723    1.0708791
#> A[1,]_shrinkage_scale 5.6768079 2.0244301   3.4008349    8.7193975
#> A[2,]_shrinkage_scale 4.0494576 1.1018955   2.8426300    5.7774521
#> A[3,]_shrinkage_scale 6.4202113 1.5775068   4.2152205    8.8989941
#> A_global_scale        0.6293677 0.1197714   0.4981031    0.7929023
#> 
#> 

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar_mix$new() |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  summary()
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
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
#> B[1,1] 0.8904943 0.03202662     0.83927    0.9552197
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -18.25836 0.9823318   -19.79896    -16.79848
#> B[2,2]  33.71299 1.8460289    30.89029     36.86304
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -32.061312 2.4272183  -36.045891   -28.811575
#> B[3,2] -17.619554 2.1735691  -21.493629   -14.849076
#> B[3,3]   6.398558 0.4569877    5.704414     7.238467
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.95064036 0.03113784   0.9073329  0.991665988
#> lag1_var2 -0.01250789 0.02236178  -0.0561789  0.009695998
#> lag1_var3 -0.08425415 0.03946637  -0.1409958 -0.035678976
#> const     -0.12701615 0.18263637  -0.4977556  0.092453281
#> 
#> $A$equation2
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.0308342 0.02316904 -0.05893011  0.001919201
#> lag1_var2  0.9586752 0.01215821  0.93912126  0.978493212
#> lag1_var3 -0.0382990 0.02898846 -0.07938673 -0.004473503
#> const     -0.3985921 0.09597595 -0.52481143 -0.241431925
#> 
#> $A$equation3
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.036306417 0.17313369  -0.1781081    0.3217692
#> lag1_var2 -0.004172112 0.09770471  -0.1667314    0.1022118
#> lag1_var3  0.001889492 0.21885398  -0.3585211    0.2395674
#> const     -0.106963498 0.72023601  -1.1879256    0.6436369
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        42.94895  16.86169    17.18037     68.59133
#> B[2,]_shrinkage       200.59144 110.26840    91.82747    434.54175
#> B[3,]_shrinkage       204.79744  93.75785   131.69691    380.45135
#> B[1,]_shrinkage_scale 516.49858 148.18965   314.74385    733.04420
#> B[2,]_shrinkage_scale 832.72217 305.03675   409.73820   1351.59419
#> B[3,]_shrinkage_scale 800.49882 254.57401   446.80199   1256.17610
#> B_global_scale         68.90024  19.22539    46.70709    102.53140
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.4958863 0.3007960   0.2183707    0.9974715
#> A[2,]_shrinkage       0.3423374 0.1725593   0.1629529    0.6699417
#> A[3,]_shrinkage       0.6550550 0.2650492   0.3230723    1.0708791
#> A[1,]_shrinkage_scale 5.6768079 2.0244301   3.4008349    8.7193975
#> A[2,]_shrinkage_scale 4.0494576 1.1018955   2.8426300    5.7774521
#> A[3,]_shrinkage_scale 6.4202113 1.5775068   4.2152205    8.8989941
#> A_global_scale        0.6293677 0.1197714   0.4981031    0.7929023
#> 
#> 
```
