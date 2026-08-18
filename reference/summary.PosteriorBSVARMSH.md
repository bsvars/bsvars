# Provides posterior summary of heteroskedastic Structural VAR estimation

Provides posterior mean, standard deviations, as well as 5 and 95
percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper parameters.

## Usage

``` r
# S3 method for class 'PosteriorBSVARMSH'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorBSVARMSH obtained using the
  [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md)
  function applied to heteroskedastic Bayesian Structural VAR model
  specification set by function `specify_bsvar_msh$new()` containing
  draws from the posterior distribution of the parameters.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper-parameters.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_bsvar_msh`](https://bsvars.org/bsvars/reference/specify_bsvar_msh.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
set.seed(123)
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
#> B[1,1] 0.8996463 0.04295655   0.8342665    0.9676881
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -18.60187 1.177734   -20.10204    -16.56378
#> B[2,2]  34.59492 2.169408    30.81382     37.31989
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -30.957566 3.4702727  -35.989037   -27.154990
#> B[3,2] -18.163782 3.2722930  -23.398280   -14.026367
#> B[3,3]   6.266313 0.5859791    5.516275     7.263924
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile  95% quantile
#> lag1_var1  0.889731368 0.01637185  0.86446241  0.9112834848
#> lag1_var2 -0.018354423 0.01418959 -0.03616572 -0.0003594926
#> lag1_var3  0.003833553 0.02252568 -0.02756633  0.0373403892
#> const     -0.090768066 0.12083297 -0.22187184  0.0671269272
#> 
#> $A$equation2
#>                   mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.060278460 0.015145621 -0.07894518  -0.04166893
#> lag1_var2  0.955527940 0.008992972  0.94318446   0.96673854
#> lag1_var3  0.008579274 0.018099549 -0.01188551   0.03360481
#> const     -0.354953485 0.074411594 -0.46265152  -0.27159084
#> 
#> $A$equation3
#>                    mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.3631195181 0.11647205  -0.5691133  -0.22636009
#> lag1_var2 -0.0673211713 0.05766746  -0.1589692   0.01863379
#> lag1_var3  0.5948973901 0.14435618   0.4083480   0.83492774
#> const     -0.0004964216 0.43009180  -0.6239197   0.61661951
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         90.68717  62.51141    32.37334     215.7303
#> B[2,]_shrinkage        292.32383 159.95432   141.93074     599.6044
#> B[3,]_shrinkage        246.13513 113.24497   108.69604     452.1537
#> B[1,]_shrinkage_scale  930.70715 406.51761   448.84105    1568.5796
#> B[2,]_shrinkage_scale 1398.14316 633.32204   638.09600    2475.5915
#> B[3,]_shrinkage_scale 1415.44083 732.70506   641.04907    2282.5381
#> B_global_scale         118.99166  47.36574    56.53211     179.7842
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3633026 0.3144911  0.07945084    1.0566960
#> A[2,]_shrinkage       0.4940577 0.4084080  0.14958499    1.0946591
#> A[3,]_shrinkage       0.4830437 0.2797241  0.17526477    1.0113187
#> A[1,]_shrinkage_scale 4.7392827 3.3613080  1.50581913   12.8980132
#> A[2,]_shrinkage_scale 5.1628899 2.3400057  2.54915088   10.0244464
#> A[3,]_shrinkage_scale 5.3186527 1.6887375  3.34910792    8.3679453
#> A_global_scale        0.5901114 0.1582296  0.42473490    0.8707002
#> 
#> 

# workflow with the pipe |>
############################################################
set.seed(123)
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
#> B[1,1] 0.8996463 0.04295655   0.8342665    0.9676881
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -18.60187 1.177734   -20.10204    -16.56378
#> B[2,2]  34.59492 2.169408    30.81382     37.31989
#> 
#> $B$equation3
#>              mean        sd 5% quantile 95% quantile
#> B[3,1] -30.957566 3.4702727  -35.989037   -27.154990
#> B[3,2] -18.163782 3.2722930  -23.398280   -14.026367
#> B[3,3]   6.266313 0.5859791    5.516275     7.263924
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile  95% quantile
#> lag1_var1  0.889731368 0.01637185  0.86446241  0.9112834848
#> lag1_var2 -0.018354423 0.01418959 -0.03616572 -0.0003594926
#> lag1_var3  0.003833553 0.02252568 -0.02756633  0.0373403892
#> const     -0.090768066 0.12083297 -0.22187184  0.0671269272
#> 
#> $A$equation2
#>                   mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.060278460 0.015145621 -0.07894518  -0.04166893
#> lag1_var2  0.955527940 0.008992972  0.94318446   0.96673854
#> lag1_var3  0.008579274 0.018099549 -0.01188551   0.03360481
#> const     -0.354953485 0.074411594 -0.46265152  -0.27159084
#> 
#> $A$equation3
#>                    mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.3631195181 0.11647205  -0.5691133  -0.22636009
#> lag1_var2 -0.0673211713 0.05766746  -0.1589692   0.01863379
#> lag1_var3  0.5948973901 0.14435618   0.4083480   0.83492774
#> const     -0.0004964216 0.43009180  -0.6239197   0.61661951
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         90.68717  62.51141    32.37334     215.7303
#> B[2,]_shrinkage        292.32383 159.95432   141.93074     599.6044
#> B[3,]_shrinkage        246.13513 113.24497   108.69604     452.1537
#> B[1,]_shrinkage_scale  930.70715 406.51761   448.84105    1568.5796
#> B[2,]_shrinkage_scale 1398.14316 633.32204   638.09600    2475.5915
#> B[3,]_shrinkage_scale 1415.44083 732.70506   641.04907    2282.5381
#> B_global_scale         118.99166  47.36574    56.53211     179.7842
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3633026 0.3144911  0.07945084    1.0566960
#> A[2,]_shrinkage       0.4940577 0.4084080  0.14958499    1.0946591
#> A[3,]_shrinkage       0.4830437 0.2797241  0.17526477    1.0113187
#> A[1,]_shrinkage_scale 4.7392827 3.3613080  1.50581913   12.8980132
#> A[2,]_shrinkage_scale 5.1628899 2.3400057  2.54915088   10.0244464
#> A[3,]_shrinkage_scale 5.3186527 1.6887375  3.34910792    8.3679453
#> A_global_scale        0.5901114 0.1582296  0.42473490    0.8707002
#> 
#> 
```
