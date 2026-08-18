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
#>            mean        sd 5% quantile 95% quantile
#> B[1,1] 0.889967 0.0458947   0.8326742    0.9619856
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -14.12034 0.7750048   -14.95219    -12.81286
#> B[2,2]  25.41608 1.3752693    23.20435     26.91029
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -22.985355 1.9279224  -25.347987   -19.571884
#> B[3,2] -10.035170 1.1630978  -11.527087    -8.693510
#> B[3,3]   4.358428 0.3635719    3.718853     4.754285
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.980601140 0.01948476  0.95688077  1.015467862
#> lag1_var2 -0.008577273 0.01219130 -0.02645992  0.004412572
#> lag1_var3 -0.124032560 0.02435285 -0.16984502 -0.096401514
#> const     -0.109508354 0.10522593 -0.29051443  0.040361239
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.01172735 0.01270114 -0.02952936   0.00838223
#> lag1_var2  0.97302217 0.01385173  0.94882614   0.99327563
#> lag1_var3 -0.06479352 0.01805428 -0.08857699  -0.03575013
#> const     -0.26161149 0.10360888 -0.42647463  -0.12378355
#> 
#> $A$equation3
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.19517847 0.06383619  0.10555866    0.2783919
#> lag1_var2  0.09602354 0.08376198 -0.01408057    0.2151415
#> lag1_var3 -0.24381823 0.09835824 -0.41933668   -0.1209382
#> const      0.56577428 0.65434182 -0.59854198    1.4872858
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        73.14744  37.33372    21.63125     126.6624
#> B[2,]_shrinkage       173.85119  95.65587    80.91903     343.2817
#> B[3,]_shrinkage       113.12801  37.16580    72.97011     167.1046
#> B[1,]_shrinkage_scale 736.05206 355.82657   366.07978    1328.8961
#> B[2,]_shrinkage_scale 942.70572 354.53763   498.11162    1251.3609
#> B[3,]_shrinkage_scale 821.32732 278.74186   444.89517    1257.7760
#> B_global_scale         81.91938  24.42881    55.79360     124.6930
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3519375 0.2333718   0.1017375    0.8648700
#> A[2,]_shrinkage       0.3959127 0.1972382   0.1668821    0.7143175
#> A[3,]_shrinkage       0.7787939 0.5072071   0.3370671    1.3880294
#> A[1,]_shrinkage_scale 5.1069695 2.9894651   1.7375816   10.8098476
#> A[2,]_shrinkage_scale 5.0930284 2.2375367   2.4011364    9.6383402
#> A[3,]_shrinkage_scale 7.1739818 3.7459229   4.2986050   13.7590109
#> A_global_scale        0.6947631 0.2639223   0.4961086    1.3464837
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
#>            mean        sd 5% quantile 95% quantile
#> B[1,1] 0.889967 0.0458947   0.8326742    0.9619856
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -14.12034 0.7750048   -14.95219    -12.81286
#> B[2,2]  25.41608 1.3752693    23.20435     26.91029
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -22.985355 1.9279224  -25.347987   -19.571884
#> B[3,2] -10.035170 1.1630978  -11.527087    -8.693510
#> B[3,3]   4.358428 0.3635719    3.718853     4.754285
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.980601140 0.01948476  0.95688077  1.015467862
#> lag1_var2 -0.008577273 0.01219130 -0.02645992  0.004412572
#> lag1_var3 -0.124032560 0.02435285 -0.16984502 -0.096401514
#> const     -0.109508354 0.10522593 -0.29051443  0.040361239
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.01172735 0.01270114 -0.02952936   0.00838223
#> lag1_var2  0.97302217 0.01385173  0.94882614   0.99327563
#> lag1_var3 -0.06479352 0.01805428 -0.08857699  -0.03575013
#> const     -0.26161149 0.10360888 -0.42647463  -0.12378355
#> 
#> $A$equation3
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.19517847 0.06383619  0.10555866    0.2783919
#> lag1_var2  0.09602354 0.08376198 -0.01408057    0.2151415
#> lag1_var3 -0.24381823 0.09835824 -0.41933668   -0.1209382
#> const      0.56577428 0.65434182 -0.59854198    1.4872858
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        73.14744  37.33372    21.63125     126.6624
#> B[2,]_shrinkage       173.85119  95.65587    80.91903     343.2817
#> B[3,]_shrinkage       113.12801  37.16580    72.97011     167.1046
#> B[1,]_shrinkage_scale 736.05206 355.82657   366.07978    1328.8961
#> B[2,]_shrinkage_scale 942.70572 354.53763   498.11162    1251.3609
#> B[3,]_shrinkage_scale 821.32732 278.74186   444.89517    1257.7760
#> B_global_scale         81.91938  24.42881    55.79360     124.6930
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3519375 0.2333718   0.1017375    0.8648700
#> A[2,]_shrinkage       0.3959127 0.1972382   0.1668821    0.7143175
#> A[3,]_shrinkage       0.7787939 0.5072071   0.3370671    1.3880294
#> A[1,]_shrinkage_scale 5.1069695 2.9894651   1.7375816   10.8098476
#> A[2,]_shrinkage_scale 5.0930284 2.2375367   2.4011364    9.6383402
#> A[3,]_shrinkage_scale 7.1739818 3.7459229   4.2986050   13.7590109
#> A_global_scale        0.6947631 0.2639223   0.4961086    1.3464837
#> 
#> 
```
