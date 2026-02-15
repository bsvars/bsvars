# Provides posterior summary of homoskedastic Structural VAR estimation

Provides posterior mean, standard deviations, as well as 5 and 95
percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper parameters.

## Usage

``` r
# S3 method for class 'PosteriorBSVAR'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorBSVAR obtained using the
  [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md)
  function applied to homoskedastic Bayesian Structural VAR model
  specification set by function `specify_bsvar$new()` containing draws
  from the posterior distribution of the parameters.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper-parameters.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_bsvar`](https://bsvars.org/bsvars/reference/specify_bsvar.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
set.seed(123)
specification  = specify_bsvar$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.

# run the burn-in
burn_in        = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
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
#>  Gibbs sampler for the SVAR model                 |
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
#>            mean       sd 5% quantile 95% quantile
#> B[1,1] 35.23577 1.821631    32.36055      38.3665
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -1.210886 2.462629   -4.903203     2.574583
#> B[2,2] 39.933172 1.577864   37.891761    42.797654
#> 
#> $B$equation3
#>               mean       sd 5% quantile 95% quantile
#> B[3,1] -14.4290114 2.183303  -18.311217   -10.870478
#> B[3,2]  -0.2140076 1.952149   -2.780451     2.623556
#> B[3,3]  95.9199823 4.768203   89.049043   101.662956
#> 
#> 
#> $A
#> $A$equation1
#>                    mean          sd  5% quantile 95% quantile
#> lag1_var1  0.9105950813 0.022897907  0.872407653   0.94277588
#> lag1_var2  0.0021691467 0.009545828 -0.008742908   0.01329558
#> lag1_var3  0.1085479404 0.029028793  0.066425311   0.15385028
#> const     -0.0006590507 0.082530820 -0.116474351   0.11584120
#> 
#> $A$equation2
#>                  mean         sd 5% quantile  95% quantile
#> lag1_var1 -0.03044777 0.01927840 -0.05355648  0.0003724801
#> lag1_var2  0.95506313 0.01016851  0.93579180  0.9719528959
#> lag1_var3  0.04324218 0.02450106  0.01037155  0.0756095277
#> const     -0.40742494 0.08489388 -0.54064748 -0.2515980366
#> 
#> $A$equation3
#>                   mean          sd 5% quantile  95% quantile
#> lag1_var1 -0.017295931 0.006078960 -0.02498971 -0.0086500943
#> lag1_var2 -0.005248387 0.003298598 -0.01062149 -0.0009931715
#> lag1_var3  1.020548413 0.007743462  1.01112865  1.0313859055
#> const     -0.056767739 0.024627906 -0.08933259 -0.0272952936
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        1057.1535  737.1846    334.7029     2674.538
#> B[2,]_shrinkage        1028.1827  860.1049    218.8209     2202.194
#> B[3,]_shrinkage        2188.8845 1390.2292    688.5259     4629.800
#> B[1,]_shrinkage_scale  9836.1378 8981.0008   1814.1446    25396.615
#> B[2,]_shrinkage_scale  9483.7197 6633.4345   1539.5779    19785.059
#> B[3,]_shrinkage_scale 11496.0191 8675.7928   2201.5938    22906.932
#> B_global_scale          990.5311  720.1240    134.9493     1849.741
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.2723898 0.1449999  0.09040047    0.4828196
#> A[2,]_shrinkage       0.3407613 0.1481311  0.15815831    0.5521471
#> A[3,]_shrinkage       0.4278662 0.1939110  0.24638296    0.8068997
#> A[1,]_shrinkage_scale 3.7690895 1.7151073  1.79984087    6.0602411
#> A[2,]_shrinkage_scale 4.7045419 1.8164610  2.44544168    7.1956499
#> A[3,]_shrinkage_scale 5.8212150 2.9892229  2.16825723   11.0994786
#> A_global_scale        0.6126614 0.2386865  0.34045996    1.0201557
#> 
#> 

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar$new() |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  summary()
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
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
#>            mean       sd 5% quantile 95% quantile
#> B[1,1] 35.23577 1.821631    32.36055      38.3665
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -1.210886 2.462629   -4.903203     2.574583
#> B[2,2] 39.933172 1.577864   37.891761    42.797654
#> 
#> $B$equation3
#>               mean       sd 5% quantile 95% quantile
#> B[3,1] -14.4290114 2.183303  -18.311217   -10.870478
#> B[3,2]  -0.2140076 1.952149   -2.780451     2.623556
#> B[3,3]  95.9199823 4.768203   89.049043   101.662956
#> 
#> 
#> $A
#> $A$equation1
#>                    mean          sd  5% quantile 95% quantile
#> lag1_var1  0.9105950813 0.022897907  0.872407653   0.94277588
#> lag1_var2  0.0021691467 0.009545828 -0.008742908   0.01329558
#> lag1_var3  0.1085479404 0.029028793  0.066425311   0.15385028
#> const     -0.0006590507 0.082530820 -0.116474351   0.11584120
#> 
#> $A$equation2
#>                  mean         sd 5% quantile  95% quantile
#> lag1_var1 -0.03044777 0.01927840 -0.05355648  0.0003724801
#> lag1_var2  0.95506313 0.01016851  0.93579180  0.9719528959
#> lag1_var3  0.04324218 0.02450106  0.01037155  0.0756095277
#> const     -0.40742494 0.08489388 -0.54064748 -0.2515980366
#> 
#> $A$equation3
#>                   mean          sd 5% quantile  95% quantile
#> lag1_var1 -0.017295931 0.006078960 -0.02498971 -0.0086500943
#> lag1_var2 -0.005248387 0.003298598 -0.01062149 -0.0009931715
#> lag1_var3  1.020548413 0.007743462  1.01112865  1.0313859055
#> const     -0.056767739 0.024627906 -0.08933259 -0.0272952936
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        1057.1535  737.1846    334.7029     2674.538
#> B[2,]_shrinkage        1028.1827  860.1049    218.8209     2202.194
#> B[3,]_shrinkage        2188.8845 1390.2292    688.5259     4629.800
#> B[1,]_shrinkage_scale  9836.1378 8981.0008   1814.1446    25396.615
#> B[2,]_shrinkage_scale  9483.7197 6633.4345   1539.5779    19785.059
#> B[3,]_shrinkage_scale 11496.0191 8675.7928   2201.5938    22906.932
#> B_global_scale          990.5311  720.1240    134.9493     1849.741
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.2723898 0.1449999  0.09040047    0.4828196
#> A[2,]_shrinkage       0.3407613 0.1481311  0.15815831    0.5521471
#> A[3,]_shrinkage       0.4278662 0.1939110  0.24638296    0.8068997
#> A[1,]_shrinkage_scale 3.7690895 1.7151073  1.79984087    6.0602411
#> A[2,]_shrinkage_scale 4.7045419 1.8164610  2.44544168    7.1956499
#> A[3,]_shrinkage_scale 5.8212150 2.9892229  2.16825723   11.0994786
#> A_global_scale        0.6126614 0.2386865  0.34045996    1.0201557
#> 
#> 
```
