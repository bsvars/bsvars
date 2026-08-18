# Provides posterior summary of Structural VAR with t-distributed shocks estimation

Provides posterior mean, standard deviations, as well as 5 and 95
percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, hyper-parameters, and Student-t
degrees-of-freedom parameter \\\nu\\.

## Usage

``` r
# S3 method for class 'PosteriorBSVART'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorBSVART obtained using the
  [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md)
  function applied to homoskedastic Bayesian Structural VAR model
  specification set by function `specify_bsvar$new()` containing draws
  from the posterior distribution of the parameters.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, hyper-parameters, and Student-t
degrees-of-freedom parameter \\\nu\\.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_bsvar_t`](https://bsvars.org/bsvars/reference/specify_bsvar_t.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
set.seed(123)
specification  = specify_bsvar_t$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.

# run the burn-in
burn_in        = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#>     with t-distributed structural skocks          |
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
#>     with t-distributed structural skocks          |
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
#> B[1,1] 7.034951 0.4166247    6.552665      7.75676
#> 
#> $B$equation2
#>            mean       sd 5% quantile 95% quantile
#> B[2,1] 12.59271 1.620784    10.36204     14.67046
#> B[2,2] 39.22914 2.505320    35.55623     42.48742
#> 
#> $B$equation3
#>              mean       sd 5% quantile 95% quantile
#> B[3,1] -40.842076 1.594068  -43.409487    -38.67083
#> B[3,2]   8.009233 1.967776    5.832431     10.94258
#> B[3,3]  52.128853 4.877992   45.472727     57.32923
#> 
#> 
#> $A
#> $A$equation1
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.7176348 0.02905666   0.6802405    0.7599792
#> lag1_var2 -0.2387549 0.00819767  -0.2526514   -0.2277487
#> lag1_var3  0.7308774 0.03325575   0.6791078    0.7738860
#> const      0.5172478 0.08707424   0.3710768    0.6299674
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.05855155 0.02033243  0.02644133   0.08747015
#> lag1_var2  1.05233870 0.02187078  1.01568198   1.07759808
#> lag1_var3 -0.19741858 0.04166237 -0.26171136  -0.12033212
#> const     -0.42928810 0.08782119 -0.57933500  -0.29366328
#> 
#> $A$equation3
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.2125312 0.02098437  -0.2589332   -0.1934901
#> lag1_var2 -0.1987338 0.01886333  -0.2272451   -0.1728379
#> lag1_var3  1.5885561 0.04061777   1.5299195    1.6437286
#> const      0.4944636 0.10368716   0.3600196    0.6851229
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        310.4691  230.9548    65.10316     563.9709
#> B[2,]_shrinkage        428.0940  168.4553   194.07053     746.7932
#> B[3,]_shrinkage        806.8825  410.8740   386.73639    1549.5007
#> B[1,]_shrinkage_scale 2861.8969 1580.9712  1190.43425    6188.6068
#> B[2,]_shrinkage_scale 3262.3162 1507.2222  1458.99499    5654.7844
#> B[3,]_shrinkage_scale 3860.4038 2489.3413  1451.92453    8087.5427
#> B_global_scale         294.0579  151.1123   128.85863     579.4997
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.7175554 0.4496179   0.2703232    1.6890561
#> A[2,]_shrinkage       0.4073541 0.2711477   0.1257121    0.8549006
#> A[3,]_shrinkage       0.4354260 0.2157549   0.1697930    0.6905040
#> A[1,]_shrinkage_scale 6.2928870 3.0219711   3.1105320   11.9042820
#> A[2,]_shrinkage_scale 5.4501569 3.3586091   1.8778479   10.9026521
#> A[3,]_shrinkage_scale 5.4807705 3.0447349   2.7175489    9.9030489
#> A_global_scale        0.6959038 0.2451832   0.4149082    1.1673854
#> 
#> 
#> $df
#>         mean           sd  5% quantile 95% quantile 
#>     4.698393     1.042636     3.715375     7.286543 
#> 

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar_t$new() |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  summary()
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#>     with t-distributed structural skocks          |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#>     with t-distributed structural skocks          |
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
#> B[1,1] 7.034951 0.4166247    6.552665      7.75676
#> 
#> $B$equation2
#>            mean       sd 5% quantile 95% quantile
#> B[2,1] 12.59271 1.620784    10.36204     14.67046
#> B[2,2] 39.22914 2.505320    35.55623     42.48742
#> 
#> $B$equation3
#>              mean       sd 5% quantile 95% quantile
#> B[3,1] -40.842076 1.594068  -43.409487    -38.67083
#> B[3,2]   8.009233 1.967776    5.832431     10.94258
#> B[3,3]  52.128853 4.877992   45.472727     57.32923
#> 
#> 
#> $A
#> $A$equation1
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.7176348 0.02905666   0.6802405    0.7599792
#> lag1_var2 -0.2387549 0.00819767  -0.2526514   -0.2277487
#> lag1_var3  0.7308774 0.03325575   0.6791078    0.7738860
#> const      0.5172478 0.08707424   0.3710768    0.6299674
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.05855155 0.02033243  0.02644133   0.08747015
#> lag1_var2  1.05233870 0.02187078  1.01568198   1.07759808
#> lag1_var3 -0.19741858 0.04166237 -0.26171136  -0.12033212
#> const     -0.42928810 0.08782119 -0.57933500  -0.29366328
#> 
#> $A$equation3
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.2125312 0.02098437  -0.2589332   -0.1934901
#> lag1_var2 -0.1987338 0.01886333  -0.2272451   -0.1728379
#> lag1_var3  1.5885561 0.04061777   1.5299195    1.6437286
#> const      0.4944636 0.10368716   0.3600196    0.6851229
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        310.4691  230.9548    65.10316     563.9709
#> B[2,]_shrinkage        428.0940  168.4553   194.07053     746.7932
#> B[3,]_shrinkage        806.8825  410.8740   386.73639    1549.5007
#> B[1,]_shrinkage_scale 2861.8969 1580.9712  1190.43425    6188.6068
#> B[2,]_shrinkage_scale 3262.3162 1507.2222  1458.99499    5654.7844
#> B[3,]_shrinkage_scale 3860.4038 2489.3413  1451.92453    8087.5427
#> B_global_scale         294.0579  151.1123   128.85863     579.4997
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.7175554 0.4496179   0.2703232    1.6890561
#> A[2,]_shrinkage       0.4073541 0.2711477   0.1257121    0.8549006
#> A[3,]_shrinkage       0.4354260 0.2157549   0.1697930    0.6905040
#> A[1,]_shrinkage_scale 6.2928870 3.0219711   3.1105320   11.9042820
#> A[2,]_shrinkage_scale 5.4501569 3.3586091   1.8778479   10.9026521
#> A[3,]_shrinkage_scale 5.4807705 3.0447349   2.7175489    9.9030489
#> A_global_scale        0.6959038 0.2451832   0.4149082    1.1673854
#> 
#> 
#> $df
#>         mean           sd  5% quantile 95% quantile 
#>     4.698393     1.042636     3.715375     7.286543 
#> 
```
