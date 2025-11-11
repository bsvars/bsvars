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
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
set.seed(123)
specification  = specify_bsvar_sv$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.

# run the burn-in
burn_in        = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
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
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
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
#> B[1,1] 0.8968551 0.03709074   0.8283078    0.9328999
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -18.46946 0.8220482   -19.16881    -17.42530
#> B[2,2]  34.92845 1.3188127    33.13628     36.49547
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -34.821946 1.9026809   -37.35330    -32.30190
#> B[3,2] -22.246437 2.5840664   -25.70848    -18.58940
#> B[3,3]   7.219321 0.3103326     6.77616      7.64351
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.87849611 0.02891186  0.82739880   0.91007584
#> lag1_var2 -0.03617474 0.01761460 -0.06249171  -0.01192026
#> lag1_var3  0.05735366 0.03579801  0.01619320   0.12359260
#> const      0.03371037 0.15888776 -0.24270495   0.24494208
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.04710790 0.02073636 -0.08511392  -0.01509528
#> lag1_var2  0.93860893 0.01242777  0.92217757   0.95508547
#> lag1_var3  0.01635927 0.02715958 -0.02548643   0.06640819
#> const     -0.34717939 0.11460452 -0.52234377  -0.20901670
#> 
#> $A$equation3
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.3910296 0.16658864  -0.5967895   -0.1809398
#> lag1_var2 -0.2398368 0.09095333  -0.3763948   -0.1112160
#> lag1_var3  0.8940474 0.20909227   0.6090205    1.1478181
#> const      0.2510930 0.76238032  -0.9746165    1.3888192
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        105.8239  58.08665    34.72296     206.8484
#> B[2,]_shrinkage        303.2644 171.11669   156.62845     515.9037
#> B[3,]_shrinkage        304.2805 146.90411   173.49668     550.8772
#> B[1,]_shrinkage_scale 1120.3314 467.34851   477.66102    1713.2537
#> B[2,]_shrinkage_scale 1553.7656 531.91145   718.95160    2323.0100
#> B[3,]_shrinkage_scale 1444.4069 567.38831   707.24566    2228.4188
#> B_global_scale         124.7393  46.92956    56.77050     203.7057
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3789412 0.2533293   0.1135425    0.8820791
#> A[2,]_shrinkage       0.3876942 0.2626464   0.1328798    0.8877898
#> A[3,]_shrinkage       0.6746070 0.3906257   0.3181639    1.4349097
#> A[1,]_shrinkage_scale 4.1076558 1.8934071   2.2052983    7.2891826
#> A[2,]_shrinkage_scale 4.7717278 2.3422141   2.5689294    9.3989777
#> A[3,]_shrinkage_scale 6.0204774 1.7744902   4.2236732   10.0204334
#> A_global_scale        0.5772347 0.1585564   0.3654327    0.8286607
#> 
#> 

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar_sv$new() |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  summary()
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
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
#> B[1,1] 0.8968551 0.03709074   0.8283078    0.9328999
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -18.46946 0.8220482   -19.16881    -17.42530
#> B[2,2]  34.92845 1.3188127    33.13628     36.49547
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -34.821946 1.9026809   -37.35330    -32.30190
#> B[3,2] -22.246437 2.5840664   -25.70848    -18.58940
#> B[3,3]   7.219321 0.3103326     6.77616      7.64351
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.87849611 0.02891186  0.82739880   0.91007584
#> lag1_var2 -0.03617474 0.01761460 -0.06249171  -0.01192026
#> lag1_var3  0.05735366 0.03579801  0.01619320   0.12359260
#> const      0.03371037 0.15888776 -0.24270495   0.24494208
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.04710790 0.02073636 -0.08511392  -0.01509528
#> lag1_var2  0.93860893 0.01242777  0.92217757   0.95508547
#> lag1_var3  0.01635927 0.02715958 -0.02548643   0.06640819
#> const     -0.34717939 0.11460452 -0.52234377  -0.20901670
#> 
#> $A$equation3
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.3910296 0.16658864  -0.5967895   -0.1809398
#> lag1_var2 -0.2398368 0.09095333  -0.3763948   -0.1112160
#> lag1_var3  0.8940474 0.20909227   0.6090205    1.1478181
#> const      0.2510930 0.76238032  -0.9746165    1.3888192
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        105.8239  58.08665    34.72296     206.8484
#> B[2,]_shrinkage        303.2644 171.11669   156.62845     515.9037
#> B[3,]_shrinkage        304.2805 146.90411   173.49668     550.8772
#> B[1,]_shrinkage_scale 1120.3314 467.34851   477.66102    1713.2537
#> B[2,]_shrinkage_scale 1553.7656 531.91145   718.95160    2323.0100
#> B[3,]_shrinkage_scale 1444.4069 567.38831   707.24566    2228.4188
#> B_global_scale         124.7393  46.92956    56.77050     203.7057
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3789412 0.2533293   0.1135425    0.8820791
#> A[2,]_shrinkage       0.3876942 0.2626464   0.1328798    0.8877898
#> A[3,]_shrinkage       0.6746070 0.3906257   0.3181639    1.4349097
#> A[1,]_shrinkage_scale 4.1076558 1.8934071   2.2052983    7.2891826
#> A[2,]_shrinkage_scale 4.7717278 2.3422141   2.5689294    9.3989777
#> A[3,]_shrinkage_scale 6.0204774 1.7744902   4.2236732   10.0204334
#> A_global_scale        0.5772347 0.1585564   0.3654327    0.8286607
#> 
#> 
```
